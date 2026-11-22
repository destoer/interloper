// https://www.lighterra.com/papers/graphcoloring/
// https://www.epfl.ch/labs/lamp/wp-content/uploads/2019/01/acc14_10_register-allocation_4.pdf

#include "backend/reg_alloc/reg_allocator.cpp"
#include "backend/reg_alloc/stack_only_alloc.cpp"
#include "backend/reg_alloc/liveness.cpp"

void init_regs(GraphAlloc& alloc);


GraphAlloc make_graph_alloc(b32 print,b32 stack_only, b32 debug, RegTable local, SymbolTable* table,arch_target arch)
{
    GraphAlloc alloc;

    alloc.print = print;
    alloc.stack_alloc = make_stack_alloc(print);

    alloc.arch = arch;
    alloc.local = local;
    alloc.table = table;
    alloc.stack_only = stack_only;
    alloc.debug = debug;

    init_regs(alloc);

    alloc.fpr.free_set_full = alloc.fpr.local.free_set;
    alloc.gpr.free_set_full = alloc.gpr.local.free_set;

    return alloc;
}

void destroy_graph_alloc(GraphAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}


lowered_reg_t graph_allocate_var(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, reg_arg_kind arg_kind)
{
    if(alloc.stack_only)
    {
        return stack_allocate_and_rewrite_var(alloc,block,node,slot,arg_kind);
    }

    assert(false);
}


lowered_reg_t graph_allocate_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot reg, reg_arg_kind arg_kind)
{
    switch(reg.kind)
    {
        case reg_kind::local:
        case reg_kind::global:
        {
            return graph_allocate_var(alloc,block,node,reg,arg_kind);
        }

        // Just resolve the register the upper level allocator
        // should have handled the constraints for us.
        case reg_kind::spec:
        {
            return special_reg_to_reg(alloc.arch,reg.spec);
        }
    }

    assert(false);
}

void graph_allocate_reg_span(GraphAlloc& alloc,Block& block,OpcodeNode* node, const ConstSpan<RegSlot>& regs, 
    reg_arg_kind arg_kind, Span<lowered_reg_t>& lowered)
{
    for(auto& reg : regs)
    {
        lowered[lowered.size++] = graph_allocate_reg(alloc,block,node,reg,arg_kind);
    }
}


ConstLoweredRegSpan graph_allocate_registers(GraphAlloc& alloc,Block& block,OpcodeNode* node, const ConstIrRegSpan& ir_reg)
{
    blank_lowered_reg_span(alloc.lowered_reg);

    graph_allocate_reg_span(alloc,block,node,ir_reg.src,reg_arg_kind::src,alloc.lowered_reg.src);
    graph_allocate_reg_span(alloc,block,node,ir_reg.dst_src,reg_arg_kind::dst_src,alloc.lowered_reg.dst_src);
    graph_allocate_reg_span(alloc,block,node,ir_reg.dst,reg_arg_kind::dst,alloc.lowered_reg.dst);

    return alloc.lowered_reg;
}




// We don't want a two stage allocator here, this should handle allocation decisions for EVERY variable.
// Along with spilling and loads, a big problem with the old allocator is that it has no insight
// on very costly allocation decisions.
// in the program and save the result including ones that can't be held in long term storage.
// It should also have logging that leaves no step major undocumented.
// First step is to build up the ranges and then the interference graph
// Then we need to look at doing coloring.


struct VariableGraphNode
{
    lowered_reg_t colour = REG_FREE;
    LocalRegSet adjacent_set;
    Array<LocalSlot> adjacent_list; 
    LocalSlot slot;
};

struct VariableGraph
{
    Array<VariableGraphNode> node;

    // Nodes we are currently ignoring and need to color later
    LocalRegSet to_colour_set;
    Array<LocalSlot> to_colour_list;

    Array<LocalSlot> potential_spill;

    Array<u32> spill_cost;
    u32 spilled_registers = 0;
};

VariableGraph make_variable_graph(Function& func)
{
    VariableGraph graph;
    resize(graph.node,count(func.local.registers));
    resize(graph.spill_cost,(count(func.local.registers)));

    for(u32 n = 0; n < count(graph.node); n++)
    {
        auto& node = graph.node[n];
        node.adjacent_set = make_local_reg_set(func.local);
        node.slot = {n};
    }

    graph.to_colour_set = make_local_reg_set(func.local);

    return graph;
}

void build_interference_graph(VariableGraph& graph, ProgramRange& program)
{
    for(auto& node : graph.node)
    {
        auto& var = program.var[node.slot.handle];
        for(auto& other : program.var)
        {
            if(other.slot == var.slot)
            {
                continue;
            }

            // TODO: Can add a edge to the other node here for speed
            if(variable_ranges_overlap(var,other))
            {
                add(node.adjacent_set,other.slot);
                push_var(node.adjacent_list,other.slot);
            }
        }
    }
}

void calc_spill_costs(VariableGraph& graph, GraphAlloc& alloc)
{
    for(size_t v = 0; v < count(graph.node); v++)
    {
        const auto& ir_reg = reg_from_local(alloc.local,graph.node[v].slot);

        // Total access according to loop depth / degree of node
        const u32 degree = count(graph.node[v].adjacent_list);
        graph.spill_cost[v] = ir_reg.spill_cost;

        if(degree > 1)
        {
            graph.spill_cost[v] /= degree;
        }
    }

    heap_sort(graph.spill_cost,[](const u32& v1, const u32& v2)
    {
        return v1 > v2;
    });
}

u32 get_filtered_degree(VariableGraph& graph, VariableGraphNode& node)
{
    u32 count = 0;

    for(const auto& adjacent : node.adjacent_list)
    {
        if(!contains(graph.to_colour_set,adjacent))
        {
            count += 1;
        }
    }

    return count;
}

void push_slot_to_colour(VariableGraph& graph, LocalSlot slot)
{
    add(graph.to_colour_set,slot);
    push_var(graph.to_colour_list,slot);
}

void simplify_prune_nodes(VariableGraph& graph, GraphAlloc& alloc)
{
    // Prune the graph so the nodes with the highest level of interference are colored first.
    // this is done by pushing the nodes with the lowest level of interference first.
    // so that when we come to colour the graph nodes that at most have k-1 colours are colored last.
    // where we will have a free colour to use.

    b32 modified = true;
    
    // Remove all nodes with less than k colors.
    
    // TODO: Probably better doing this selectively against nodes we have just modified.
    // but this will be foolproof for now.
    while(modified)
    {
        modified = false;
        for(auto& node : graph.node)
        {
            if(contains(graph.to_colour_set,node.slot))
            {
                continue;
            }

            auto& file = get_register_file(alloc,node.slot);

            // Add to list of nodes that are ignored if it has a degree less than max
            // this means that when we re add it, we should have a colour spare.
            if(get_filtered_degree(graph,node) < file.max_colours)
            {
                push_slot_to_colour(graph,node.slot);
                modified = true;
            }
        }
    }
}

void simplify(VariableGraph& graph, GraphAlloc& alloc)
{
    b32 done = false;

    while(!done)
    {
        done = true;
        simplify_prune_nodes(graph,alloc);

        // If the there are variables still unqueued for colouring if means one has a higher degree than our available colours
        // and we may have to spill it, pick one and add it as a candidate.
        // we will not spill it yet as it may turn out some of its incident nodes share the same colour and we can colour it anyways.
        if(count(graph.node) != count(graph.to_colour_list))
        {
            for(u32 v = 0; v < count(graph.spill_cost); v++)
            {
                LocalSlot local = {v};
                if(contains(graph.to_colour_set,local))
                {
                    continue;
                }

                push_slot_to_colour(graph,local);
                break;
            }
            done = false;
        }
    }
}


// TODO:
// - Coloring
// - Make live range analysis spill aware. 
// We require a fix for globals and for aliased variables so they can be tracked at proper ranges
// for instance if we have
// add x, y, z
// spill x
// Its range should die as soon as we see the spill 

void graph_allocate(GraphAlloc& alloc,Interloper& itl, Function& func)
{
    // Don't need to do pre computation for stack allocation
    if(alloc.stack_only)
    {
        return;
    }

    ProgramRange program = find_program_ranges(itl,func);
    VariableGraph graph = make_variable_graph(func);

    build_interference_graph(graph,program);
    calc_spill_costs(graph,alloc);

    simplify(graph,alloc);

    // TODO: Actually attempt to colour it.

    assert(false);
}

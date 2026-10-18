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

    alloc.fpr.free_set_full = alloc.fpr.free_set;
    alloc.gpr.free_set_full = alloc.gpr.free_set;

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
    lowered_reg_t color = REG_FREE;
    LocalRegSet adjacent; 
    LocalSlot slot;
};

struct VariableGraph
{
    Array<VariableGraphNode> node;
};

VariableGraph make_variable_graph(Function& func)
{
    VariableGraph graph;
    resize(graph.node,count(func.local.registers));

    for(u32 n = 0; n < count(graph.node); n++)
    {
        auto& node = graph.node[n];
        node.adjacent = make_local_reg_set(func.local);
        node.slot = {n};
    }

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

            if(variable_ranges_overlap(var,other))
            {
                add(node.adjacent,other.slot);
            }
        }
    }
}

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

    assert(false);
}

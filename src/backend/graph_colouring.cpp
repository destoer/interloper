#include "backend/stack_only_alloc.cpp"

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

    return alloc;
}

void destroy_graph_alloc(GraphAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}


ConstLoweredRegSpan graph_allocate_registers(GraphAlloc& alloc,Block& block,OpcodeNode* node, const ConstIrRegSpan& ir_reg)
{
    UNUSED(alloc); UNUSED(block); UNUSED(node); UNUSED(ir_reg);
    assert(false);
}


// We don't want a two stage allocator here, this should handle allocation decisions for EVERY variable.
// Along with spilling and loads, a big problem with the old allocator is that it has no insight
// on very costly allocation decisions.
// in the program and save the result including ones that can't be held in long term storage.
// It should also have logging that leaves no step major undocumented.

void graph_allocate(GraphAlloc& alloc,Interloper& itl, Function& func)
{
    // Don't need to do pre computation for stack allocation
    if(alloc.stack_only)
    {
        return;
    }

    UNUSED(itl); UNUSED(func);
    assert(false);
}

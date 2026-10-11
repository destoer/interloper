#pragma once

// TODO: we need to handle 16 byte alignment
// if we need to call external functions or use sse

struct StackAlloc
{
    Array<ArrayAllocation> array_allocation;

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to variables
    u32 stack_offset = 0;

    // where does each section for alloc start?
    u32 stack_alloc[4] = {0};

    // how much of each type of var is there at the moment?
    u32 size_count[4] = {0};

    // what is the total amount of space that this functions stack requires!
    u32 stack_size = 0;

    Array<RegSlot> pending_allocation;

    b32 print = false;
};


struct RegisterFile
{
    // what registers have we used total for this function?
    u32 used_set = 0;

    // what registers are we allowed to use?
    u32 free_set = 0;

    // What registers are saved by the current function?
    u32 saved_set = 0;

    // What registers are current fixed in place?
    u32 locked_set = 0;
};

struct GraphAlloc
{
    arch_target arch;

    // allocation info of locals for current function
    // NOTE: this is owned by the func and we dont have to free it
    RegTable local;
    SymbolTable* table;

    RegisterFile gpr;
    RegisterFile fpr;

    StackAlloc stack_alloc;

    b32 stack_only = false;
    b32 debug = false;

    b32 print = false;

    IrRegSpanStorage ir_reg_span_storage;
    IrRegSpan ir_reg_span = make_ir_reg_span(ir_reg_span_storage);

    LoweredRegSpanStorage lowered_reg_span_storage;
    LoweredRegSpan lowered_reg_span = make_lowered_reg_span(lowered_reg_span_storage);
};

Reg& reg_from_slot(GraphAlloc& alloc, LocalSlot slot);
Reg& reg_from_slot(GraphAlloc& alloc, RegSlot slot);

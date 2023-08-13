#pragma once

#include <destoer.h>



struct Trace
{
    u64 idx;
    u64 history_target[0x10] = {0};
    u64 history_source[0x10] = {0};
};

// memory map 
//  0x00000000 -> program memory
//  0x00000000 + program_size -> globals
//  0x20000000 + stack size -> stack
//  0x30000000 and above -> heap allocations (when implemented)
struct Interpretter
{
    u32 size = 0;

    //pc and sp are after the standard regs
    u32 regs[MACHINE_REG_SIZE + 2];

    b32 quit;

    Array<u8> program;

    Array<u8> global;

    Array<u8> stack;

    Trace trace;
};




#pragma once

#include <lib.h>


// memory map 
//  0x10000000 -> program memory
//  0x20000000 + stack size -> stack
//  0x30000000 and above -> heap allocations (when implemented)
struct Interpretter
{
    u32 size = 0;

    //pc and sp are after the standard regs
    u32 regs[MACHINE_REG_SIZE + 2];

    b32 quit;

    // 16MB stack
    std::vector<u8> stack;
};





s32 run(Interpretter& interpretter,const u8 *program, u32 size);
void reset(Interpretter &interpretter);
void execute_opcode(Interpretter& interpretter,const Opcode &opcode);

#pragma once

#include <destoer/destoer.h>

// IR SYSCALLS
static constexpr u32 SYSCALL_EXIT = 0x0;
static constexpr u32 SYSCALL_OPEN = 0x1;
static constexpr u32 SYSCALL_CLOSE = 0x2;
static constexpr u32 SYSCALL_WRITE = 0x3;
static constexpr u32 SYSCALL_READ = 0x4;
static constexpr u32 SYSCALL_ALLOC = 0x5;
static constexpr u32 SYSCALL_TIME = 0x6;
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
    u64 regs[MACHINE_REG_SIZE + 2];

    b32 quit;

    Array<u8> program;

    Array<u8> global;

    Array<u8> stack;

    // allocate pointers for cleanup by the vm
    Array<u8> alloc;

    Trace trace;
};


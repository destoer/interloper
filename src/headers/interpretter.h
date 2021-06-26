#pragma once

#include <lib.h>


// memory map 
//  0x10000000 -> program memory
//  0x20000000 + stack size -> stack
//  0x30000000 and above -> heap allocations (when implemented)
struct Interpretter final
{
    void run(const uint8_t *program, uint32_t size);

    template<typename access_type>
    access_type read_mem(uint32_t addr);

    template<typename access_type>
    void write_mem(uint32_t addr, access_type v);

    const uint8_t *program = nullptr;
    uint32_t size = 0;

    // is an extra pass to recompile the registers
    // so pc and sp are 14 & 15 
    // and to jam the immediates worthwhile or should we just leave it?
    uint32_t regs[16];

    // 16MB stack
    std::vector<uint8_t> stack;
};








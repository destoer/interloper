#pragma once
#include <destoer.h>
#include <ir.h>

using PoolSlot = Slot<slot_type::pool>;

// POOL
enum class pool_type 
{
    string_literal,
    var,
    jump_table,
};


struct PoolSection
{
    PoolSlot slot;
    pool_type type;

    u32 offset;
    u32 size; 
};


struct ConstDataPointer
{
    // pool slot of section we are taking a pointer too
    PoolSlot slot;

    // offset into this section
    u32 offset = 0;
};


struct PoolPointer
{
    ConstDataPointer pointer;
    u32 pool_offset = 0;
};

struct ConstPool
{
    Array<PoolSection> sections;
    Array<u8> buf;

    // offsets into the buffer that requires address resolution
    Array<u32> label;
    Array<PoolPointer> pool_pointer; 

    u32 base_vaddr = 0;
};

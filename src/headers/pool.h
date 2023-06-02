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

    // TODO: should these be stored with the pool globally instead?
    
    // offsets into the current section of data that requires address resolution
    Array<u32> label;
    Array<u32> pool_pointer;    
};


struct ConstPool
{
    Array<PoolSection> sections;
    Array<u8> buf;
};

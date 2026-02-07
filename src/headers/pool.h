#pragma once
#include <destoer/destoer.h>
#include <ir.h>

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

struct PoolLabel
{
    LabelSlot label_slot;
    u32 pool_offset;
};

struct ConstPool
{
    Array<PoolSection> sections;
    Array<u8> buf;

    // offsets into the buffer that requires address resolution
    Array<PoolLabel> label;
    Array<PoolPointer> pool_pointer; 

    u32 base_vaddr = 0;
};

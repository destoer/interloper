#pragma once
#include <lib.h>

struct ArenaAllocator;

struct Arena
{
    // how much have we used?
    u32 len = 0; 

    // how much do we have total?
    u32 size = 0;

    // underyling memory
    void* buf = nullptr;
};


static constexpr u32 ARENA_ALLOC_SIZE = 32;

// for now just have a single pool
// and dont deal with it getting exhausted
struct ArenaAllocator
{
    Arena arena[ARENA_ALLOC_SIZE];
    u32 size = 0;
};

void* allocate(ArenaAllocator& allocator, u32 size);
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


// for now just have a single pool
// and dont deal with it getting exhausted
struct ArenaAllocator
{
    Arena arena;
};


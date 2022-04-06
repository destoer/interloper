#include <lib.h>

struct Pool
{
    // how much have we used?
    u32 len; 

    // how much do we have total?
    u32 size;

    // underyling memory
    void* buf;
};

Pool make_pool(u32 size)
{
    Pool pool;

    pool.len = 0;
    pool.size = size;
    pool.buf = malloc(size);
}


// for now just have a single pool
// and dont deal with it getting exhausted
struct Allocator
{
    Pool pool;
};

Allocator make_allocator()
{
    Allocator allocator;

    allocator.pool = make_pool(10 * 1024 * 1024);

    return allocator;
}

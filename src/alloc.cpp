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

    assert(pool.buf);

    return pool;
}

void reset_pool(Pool &pool)
{
    pool.len = 0;
}

void destory_pool(Pool &pool)
{
    if(pool.buf)
    {
        free(pool.buf);
        pool.buf = nullptr;
    }
}

// for now just have a single pool
// and dont deal with it getting exhausted
struct Allocator
{
    Pool pool;
};

Allocator make_allocator(u32 size)
{
    Allocator allocator;

    allocator.pool = make_pool(size);

    return allocator;
}

void destory_allocator(Allocator &allocator)
{
    destory_pool(allocator.pool);
}
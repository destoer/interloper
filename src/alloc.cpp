#include <lib.h>


Arena make_arena(u32 size)
{
    Arena arena;

    arena.len = 0;
    arena.size = size;
    arena.buf = malloc(size);

    assert(arena.buf);

    return arena;
}

void reset_arena(Arena &arena)
{
    arena.len = 0;
}

void destory_arena(Arena &arena)
{
    if(arena.buf)
    {
        free(arena.buf);
        arena.buf = nullptr;
    }
    arena.len = 0;
    arena.size = 0;
}

void *allocate(Arena &arena,u32 size)
{   
    // allocation failed
    if(arena.len + size >= arena.size)
    {
        return nullptr;
    }

    // do the allocation
    u8* ptr = (u8*)(arena.buf); 

    void* alloc_ptr =  (void*)(&ptr[arena.len]);
    arena.len += size;

    return alloc_ptr;
}

ArenaAllocator make_allocator(u32 size)
{
    ArenaAllocator allocator;

    allocator.arena = make_arena(size);

    return allocator;
}

void destory_allocator(ArenaAllocator &allocator)
{
    destory_arena(allocator.arena);
}

void* allocate(ArenaAllocator& allocator, u32 size)
{
    // TODO: handle this failing and add a new area
    if(allocator.arena.len + size >= allocator.arena.size)
    {
        unimplemented("Arena OOM requested: %d %d:%d",size,allocator.arena.len,allocator.arena.size);
    }

    return allocate(allocator.arena,size);
}

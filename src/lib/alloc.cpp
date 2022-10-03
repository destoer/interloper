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

void destroy_arena(Arena &arena)
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

void reserve_end(Arena& arena,u32 size)
{
    arena.len += size;
}


bool add_arena(ArenaAllocator& allocator, u32 size)
{
    if(allocator.size == ARENA_ALLOC_SIZE)
    {
        return true;
    }

    allocator.arena[allocator.size++] = make_arena(size);
    return false;
}

ArenaAllocator make_allocator(u32 size)
{
    ArenaAllocator allocator;

    add_arena(allocator,size);

    return allocator;
}

void destroy_allocator(ArenaAllocator &allocator)
{
    for(u32 i = 0; i < allocator.size; i++)
    {
        destroy_arena(allocator.arena[i]);
    }
}

void* allocate(ArenaAllocator& allocator, u32 size)
{
    const u32 cur_arena = allocator.size - 1;

    if(allocator.arena[cur_arena].len + size >= allocator.arena[cur_arena].size)
    {
        const u32 new_size = std::max(allocator.arena[cur_arena].size,size);
        
        // next arena is double the size of the old arena, or what we need
        const u32 arena_size = new_size * 2;

        if(add_arena(allocator,arena_size))
        {
            unimplemented("Arena OOM %d : %d : %d\n",cur_arena,ARENA_ALLOC_SIZE,size);
        }
    }

    return allocate(allocator.arena[allocator.size - 1],size);
}

u32 allocator_size(const ArenaAllocator& allocator)
{
    u32 size = 0;

    for(u32 i = 0; i < allocator.size; i++)
    {
        size += allocator.arena[i].size;
    }

    return size;
}



Arena& cur_arena(ArenaAllocator& allocator)
{
    return allocator.arena[allocator.size - 1];
}
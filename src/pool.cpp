#include <pool.h>

void destroy_const_pool(ConstPool& pool)
{
    for(u32 i = 0; i < count(pool.sections); i++)
    {
        auto& section = pool.sections[i];
        destroy_arr(section.label);
        destroy_arr(section.pool_pointer);
    }

    destroy_arr(pool.sections);
    destroy_arr(pool.buf);
}

PoolSection& pool_section_from_slot(ConstPool& pool, PoolSlot slot)
{
    return pool.sections[slot.handle];
}

PoolSlot pool_slot_from_idx(u32 handle)
{
    return {handle};
}

PoolSlot reserve_const_pool_section(ConstPool& pool, pool_type type, u32 size)
{
    PoolSection section;
    section.type = type;
    section.size = size;
    section.offset = pool.buf.size;

    // Get back a slot to identify this section
    const u32 handle = count(pool.sections);
    PoolSlot slot  = {handle};

    // reserve this section inside the pool
    resize(pool.buf,count(pool.buf) + size);

    // finally add our section and return back the handle
    push_var(pool.sections,section);
    return slot;
}

void write_const_pool(ConstPool& pool, PoolSlot slot, u32 offset, const void* data,u32 size)
{
    const auto& section = pool_section_from_slot(pool,slot);
    memcpy(&pool.buf[section.offset + offset],data,size);
}

template<typename T>
void write_const_pool(ConstPool& pool, PoolSlot slot, u32 offset,const T& data)
{
    write_const_pool(pool,slot,offset,&data,sizeof(data));
}

void write_const_pool_label(ConstPool& pool, PoolSlot slot, u32 offset,LabelSlot label_slot)
{
    // mark where label is going so it can resolved later
    auto& section = pool_section_from_slot(pool,slot);
    push_var(section.label,offset);

    // write the label in
    write_const_pool(pool,slot,offset,label_slot.handle);
}

// create a pool section and push the data to it
PoolSlot push_const_pool(ConstPool& pool, pool_type type, const void* data,u32 size)
{
    // create section
    const auto slot = reserve_const_pool_section(pool,type,size);

    // copy the data into the pool
    write_const_pool(pool,slot,0,data,size);

    return slot;
}
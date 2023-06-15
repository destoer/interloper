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

// TODO: this doesn't handle alignment
// TODO: should this hold the a Type* of the stored data if it is a var?
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

void write_const_pool(ConstPool& pool, PoolSection& section, u32 offset, const void* data,u32 size)
{
    memcpy(&pool.buf[section.offset + offset],data,size);
}

template<typename T>
void write_const_pool(ConstPool& pool, PoolSection& section, u32 offset,const T& data)
{
    write_const_pool(pool,section,offset,&data,sizeof(data));
}

void write_const_pool_label(ConstPool& pool, PoolSection& section, u32 offset, LabelSlot label_slot)
{
    // mark where label is going so it can resolved later
    push_var(section.label,offset);

    // write the label in
    write_const_pool(pool,section,offset,label_slot.handle);    
}

void write_const_pool_label(ConstPool& pool, PoolSlot slot, u32 offset,LabelSlot label_slot)
{
    // mark where label is going so it can resolved later
    auto& section = pool_section_from_slot(pool,slot);

    write_const_pool_label(pool,section,offset,label_slot);
}

void write_const_pool_pointer(ConstPool& pool, PoolSection& section, u32 offset,PoolSlot pool_slot)
{
    push_var(section.pool_pointer,offset);

    // write the pool slot in
    write_const_pool(pool,section,offset,pool_slot.handle);
}


/*
template<typename T>
void write_const_pool_struct(ConstPool& pool, PoolSlot slot, const String& name, const T& data)
{

}
*/


// create a pool section and push the data to it
PoolSlot push_const_pool(ConstPool& pool, pool_type type, const void* data,u32 size)
{
    // create section
    const auto slot = reserve_const_pool_section(pool,type,size);
    auto& section = pool_section_from_slot(pool,slot);

    // copy the data into the pool
    write_const_pool(pool,section,0,data,size);

    return slot;
}

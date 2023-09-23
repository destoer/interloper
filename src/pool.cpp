#include <pool.h>

void destroy_const_pool(ConstPool& pool)
{
    destroy_arr(pool.label);
    destroy_arr(pool.pool_pointer);

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

void align_pool(ConstPool& pool, u32 align_size)
{
    const u32 unaligned = (pool.buf.size & (align_size - 1));

    // pool requires alginment
    if(unaligned != 0)
    {
        resize(pool.buf,count(pool.buf) + (align_size - unaligned));
    }
}

// TODO: should this hold the a Type* of the stored data if it is a var?
PoolSlot reserve_const_pool_section(ConstPool& pool, pool_type type, u32 size)
{
    const u32 align_size = size >= GPR_SIZE? GPR_SIZE : size;

    // TODO: this results in extra padding
    // make this do reordering at the end
    align_pool(pool,align_size);

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
    push_var(pool.label,section.offset + offset);

    //printf("wrote label L%d %d\n",label_slot.handle,count(pool.label));

    static_assert(GPR_SIZE == 8);

    // promote the handle so it can fit the addr later
    const u64 handle = label_slot.handle;

    // write the label in
    write_const_pool(pool,section,offset,handle);    
}

void write_const_pool_label(ConstPool& pool, PoolSlot slot, u32 offset,LabelSlot label_slot)
{
    // mark where label is going so it can resolved later
    auto& section = pool_section_from_slot(pool,slot);

    write_const_pool_label(pool,section,offset,label_slot);
}

void write_const_pool_pointer(ConstPool& pool, PoolSection& section, u32 offset,const ConstDataPointer& data_pointer)
{
    PoolPointer pointer;
    pointer.pointer = data_pointer;
    pointer.pool_offset = section.offset + offset;

    // write out handle to PoolPointer incase we need to do a look for const expr
    const u64 handle = count(pool.pool_pointer);
    write_const_pool(pool,section,offset,handle);

    push_var(pool.pool_pointer,pointer);
}

void write_const_pool_pointer(ConstPool& pool, PoolSection& section, u32 offset,PoolSlot slot)
{
    PoolPointer pointer;
    pointer.pointer = {slot, 0};
    pointer.pool_offset = section.offset + offset;

    push_var(pool.pool_pointer,pointer);
}

// create a pool section and push the data to it
PoolSlot push_const_pool(ConstPool& pool, pool_type type, const void* data,u32 size)
{
    u32 pool_size = size;

    if(type == pool_type::string_literal)
    {
        pool_size += 1;
    }

    // create section
    const auto slot = reserve_const_pool_section(pool,type,pool_size);
    auto& section = pool_section_from_slot(pool,slot);

    // copy the data into the pool
    write_const_pool(pool,section,0,data,size);

    // string_literals are null terminated inside the pool
    if(type == pool_type::string_literal)
    {
        write_const_pool(pool,section,size,'\0');
    }

    return slot;
}

PoolSlot push_const_pool_string(ConstPool& pool, const String& literal)
{
    return push_const_pool(pool,pool_type::string_literal,literal.buf,literal.size);
}
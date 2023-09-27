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

void write_const_pool_vla(ConstPool& const_pool, PoolSection& vla_section, u32 offset, PoolSlot data_slot, u64 len)
{
    // write in the data
    write_const_pool_pointer(const_pool,vla_section,0 + offset,data_slot); 
    write_const_pool(const_pool,vla_section,GPR_SIZE + offset,len);    
}

PoolSlot push_const_pool_vla(ConstPool& const_pool,PoolSlot data_slot, u64 len)
{
    // alloc vla struct
    const auto vla_slot = reserve_const_pool_section(const_pool,pool_type::var,GPR_SIZE * 2);
    auto& vla_section = pool_section_from_slot(const_pool,vla_slot);

    write_const_pool_vla(const_pool,vla_section,0,data_slot,len);

    return vla_slot;
}

PoolSlot push_const_pool_fixed_array(ConstPool& const_pool, PoolSlot data_slot)
{
    // add array pointer
    const auto pointer_slot = reserve_const_pool_section(const_pool,pool_type::var,GPR_SIZE);
    auto& pointer_section = pool_section_from_slot(const_pool,pointer_slot);

    write_const_pool_pointer(const_pool,pointer_section,0,data_slot);

    return pointer_slot;    
}


struct ConstData
{
    // actual data repr
    union
    {
        // inline data
        // enum, builtin types
        u64 v = 0;

        // Pointer to other const pool var
        // Used to return arrays, pointers structs
        ConstDataPointer data_pointer;
    };

    // NOTE: this indicates how to access const data, as described above
    Type* type = nullptr;
};

ConstData make_const_builtin(u64 v, Type* type)
{
    ConstData data;

    data.v = v;
    data.type = type;

    return data;
}

void write_const_pool_mem(Interloper& itl, PoolSlot slot, u32 offset, u64 v, u32 size)
{
    assert(size <= 8);

    auto& section = pool_section_from_slot(itl.const_pool,slot);

    if((offset + size) > section.size)
    {
        panic(itl,itl_error::out_of_bounds,"out of bounds write in const pool\n");
        return;
    } 

    // calc the read reqs
    const u32 addr = section.offset + offset;

    memcpy(&itl.const_pool.buf.data[addr],&v,size);
}

void write_const_builtin(Interloper& itl,PoolSlot slot, u32 offset,const ConstData& data)
{
    const u32 size = type_size(itl,data.type);
    write_const_pool_mem(itl,slot,offset,data.v,size);
}

// used for writing into compound data, i.e structs, arrays
void write_const_data(Interloper& itl, PoolSlot slot, u32 offset, const ConstData& data)
{
    // write out based on type
    Type* type = data.type;

    // builtin type can just be directly read out as is from here
    if(is_builtin(type))
    {
        write_const_builtin(itl,slot,offset,data);
    }

    // this is technically just an int
    // return in place
    else if(is_enum(type))
    {
        assert(false);
    }

    
    else if(is_array(type))
    {
        // this should only be legal for vla assigns

        assert(false);
    }

    else if(is_struct(type))
    {
        assert(false);
    }

    else if(is_pointer(type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }   
}

PoolSlot pool_slot_from_sym(const Symbol& sym)
{
    return pool_slot_from_idx(sym.reg.offset);
}

u64 read_const_pool_mem(Interloper& itl, PoolSlot slot, u32 offset, u32 size)
{
    assert(size <= 8);

    auto& section = pool_section_from_slot(itl.const_pool,slot);

    if((offset + size) > section.size)
    {
        panic(itl,itl_error::out_of_bounds,"out of bounds read in const pool\n");
        return 0;
    } 

    // calc the read reqs
    const u32 addr = section.offset + offset;

    u64 v = 0;
    memcpy(&v,&itl.const_pool.buf.data[addr],size);

    return v;
}

u64 builtin_from_const(Interloper& itl, Type* type,PoolSlot slot, u32 offset)
{
    const u32 size = type_size(itl,type);
    return read_const_pool_mem(itl,slot,offset,size);
}

ConstData read_const_data(Interloper& itl, Type* type, PoolSlot slot, u32 offset)
{
    // read out based on type

    // builtin type can just be directly read out as is from here
    if(is_builtin(type))
    {
        ConstData data;

        data.v = builtin_from_const(itl,type,slot,offset);
        data.type = type;

        return data;
    }

    // this is technically just an int
    // return in place
    else if(is_enum(type))
    {
        assert(false);
    }

    // return the array pointer
    // NOTE: this works the same as arrays normally
    // if this is a vla we will get a pointer to the struct
    // for fixed arrays this is the data pointer
    else if(is_array(type))
    {
        assert(false);
    }

    // return slot
    else if(is_struct(type))
    {
        assert(false);
    }

    // return pointer data
    else if(is_pointer(type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }        
}

ConstData read_const_sym(Interloper& itl, Symbol& sym)
{
    const auto pool_slot = pool_slot_from_idx(sym.reg.offset);

    return read_const_data(itl,sym.type,pool_slot,0);
}


b32 is_any(Interloper& itl, const Type* type)
{
    if(!itl.rtti_enable)
    {
        return false;
    }

    if(is_struct(type))
    {
        StructType* struct_type = (StructType*)type;

        return struct_type->struct_idx == itl.rtti_cache.any_idx;
    }

    return false;
}

u32 cache_struct(Interloper& itl, const String& name)
{
    TypeDecl* type_decl = lookup_type(itl,name);

    if(!type_decl)
    {
        panic(itl,itl_error::struct_error,"could not find struct %s for rtti\n",name.buf);
        return INVALID_TYPE_IDX;
    }

    if(type_decl->kind != type_kind::struct_t)
    {
        panic(itl,itl_error::struct_error,"could not find struct %s for rtti\n",name.buf);
        return INVALID_TYPE_IDX;
    }

    return type_decl->type_idx;
}

//NOTE: this silently fails
u32 cache_offset(Interloper& itl,Struct& structure, const String& member_name)
{
    auto offset_opt = member_offset(structure,member_name);

    if(!offset_opt)
    {
        panic(itl,itl_error::rtti_error,"could not find offset for %s.%s\n",structure.name.buf,member_name.buf);
        return 0;
    }

    return offset_opt.value();
}


void cache_rtti_structs(Interloper& itl)
{
    auto& rtti = itl.rtti_cache;
    
    // cache Any struct info
    rtti.any_idx = cache_struct(itl,"Any");

    if(invalid_type_idx(rtti.any_idx))
    {
        return;
    }

    auto& any_struct = itl.struct_table[rtti.any_idx];
    rtti.any_data_offset = cache_offset(itl,any_struct,"data");
    rtti.any_type_offset = cache_offset(itl,any_struct,"type");
    rtti.any_struct_size = any_struct.size;


    // cache Type struct info
    const u32 type_struct_idx = cache_struct(itl,"Type");

    if(invalid_type_idx(type_struct_idx))
    {
        return;
    }

    auto& type_struct = itl.struct_table[type_struct_idx];
    rtti.is_const_offset = cache_offset(itl,type_struct,"is_const");
    rtti.type_idx_offset = cache_offset(itl,type_struct,"type_idx");
    rtti.type_struct_size = type_struct.size;


    const u32 pointer_struct_idx = cache_struct(itl,"PointerType");

    if(invalid_type_idx(pointer_struct_idx))
    {
        return;
    }

    auto& pointer_struct = itl.struct_table[pointer_struct_idx];
    rtti.pointer_contained_offset = cache_offset(itl,pointer_struct,"contained_type");
    rtti.pointer_struct_size = pointer_struct.size;


/*
    rtti.array_idx = cache_struct(itl,"ArrayType");


    rtti.enum_idx = cache_struct(itl,"EnumType");



    rtti.struct_idx = cache_struct(itl,"StructType");
*/

}

PoolSlot make_rtti(Interloper& itl, const Type* type)
{
    const auto& rtti = itl.rtti_cache;

    // TODO: for now we wont bother trying to unique this

    switch(type->type_idx)
    {
        case ARRAY:
        {
            assert(false);
            break;
        }

        case POINTER:
        {
            // get contained type
            const PointerType* pointer_type = (PointerType*)type;
            const PoolSlot contained_type = make_rtti(itl,pointer_type->contained_type);

            // reserve room for pointer type
            const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.pointer_struct_size);
            auto& section = pool_section_from_slot(itl.const_pool,slot);

            // push in base type
            write_const_pool(itl.const_pool,section,rtti.is_const_offset,type->is_const);
            write_const_pool(itl.const_pool,section,rtti.type_idx_offset,POINTER_RTTI);            

            // push in pointer type
            write_const_pool_pointer(itl.const_pool,section,rtti.pointer_contained_offset,contained_type);

            return slot;
        }

        case ENUM:
        {
            assert(false);
            break;
        }

        case STRUCT:
        {
            assert(false);
            break;
        }

        // builtin just insert as is
        default:
        {
            // allocate a slot in the pool for us
            const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.type_struct_size);
            auto& section = pool_section_from_slot(itl.const_pool,slot);

            // write in base type struct
            write_const_pool(itl.const_pool,section,rtti.is_const_offset,type->is_const);
            write_const_pool(itl.const_pool,section,rtti.type_idx_offset,type->type_idx);

            return slot;
        }
    }

    assert(false);
    return {0xffff'ffff};
}


SymSlot aquire_rtti(Interloper& itl, Function& func, const Type* type)
{
    const PoolSlot pool_slot = make_rtti(itl,type);

    return pool_addr(func,pool_slot);
}

u32 promote_size(u32 size)
{
    if(size < GPR_SIZE)
    {
        return GPR_SIZE;
    }

    return size;
}

// any struct + sizeof type total
u32 any_size(Interloper &itl, const Type* type)
{
    u32 size = itl.rtti_cache.any_struct_size;

    // how does this play at 64 bit?
    static_assert(GPR_SIZE == sizeof(u32));

    // cannot embed directly into the data pointer...
    if(!is_trivial_copy(type))
    {
        const u32 arg_size = type_size(itl,type);

        size += promote_size(arg_size);
    }

    return size;    
}

void make_any(Interloper& itl,Function& func, SymSlot ptr_slot, u32 offset, const SymSlot src, const Type* type)
{
    auto& rtti = itl.rtti_cache;

    // aquire a copy of the typing information from the const pool
    const SymSlot rtti_ptr = aquire_rtti(itl,func,type); 

    if(is_trivial_copy(type))
    {
        // store data
        emit(func,store_ptr(src,ptr_slot,offset + rtti.any_data_offset,GPR_SIZE));

        // store type struct
        emit(func,store_ptr(rtti_ptr,ptr_slot,offset + rtti.any_type_offset,GPR_SIZE));                
    } 

    // finally the any struct
    // TODO: we should store things less than GPR_SIZE directly in the pointer...
    // store our any struct
    else if(is_array(type))
    {
        assert(false);
    }

    else if(is_struct(type))
    {
        assert(false);
    }
    
    // should never happen
    else
    {
        assert(false);
    }
}
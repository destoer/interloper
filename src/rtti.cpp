
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
/*
    rtti.array_idx = cache_struct(itl,"ArrayType");


    rtti.enum_idx = cache_struct(itl,"EnumType");



    rtti.struct_idx = cache_struct(itl,"StructType");
*/

}

PoolSlot make_rtti(Interloper& itl, const Type* type)
{
    const auto& rtti = itl.rtti_cache;

    // TODO: for now we ignore trying unique the typing information to save space..

    switch(type->type_idx)
    {
        case ARRAY:
        {
            assert(false);
            break;
        }

        // TODO: how will recursive pushing work exactly?
        // test with a pointer!
        case POINTER:
        {
            assert(false);
            break;
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
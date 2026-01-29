Result<u32,itl_error> cache_struct_index(Interloper& itl, NameSpace* name_space, const String& name)
{
    auto type_opt = lookup_type_scoped(itl,name_space,name);
    if(!type_opt)
    {
        return compile_error(itl,itl_error::struct_error,"No such type %s",name.buf);
    }

    TypeDecl* type_decl = *type_opt; 

    if(!type_decl)
    {
        return compile_error(itl,itl_error::struct_error,"could not find struct %s for rtti",name.buf);
    }

    if(type_decl->kind != type_kind::struct_t)
    {
        return compile_error(itl,itl_error::struct_error,"%s is a %s and not a struct for rtti",name.buf,TYPE_KIND_NAMES[u32(type_decl->kind)]);
    }

    return type_decl->type_idx;
}


Result<u32,itl_error> cache_offset(Interloper& itl,Struct& structure, const String& member_name)
{
    auto member_opt = get_member(structure,member_name);

    if(!member_opt)
    {
        return compile_error(itl,itl_error::rtti_error,"could not find offset for %S.%S",structure.name,member_name);
    }

    const auto member = *member_opt;
    return member.offset;
}



struct MemberCacheReq
{
    String name;
    u32* value;
};

MemberCacheReq make_member_cache_req(const char* name, u32* value)
{
    MemberCacheReq out;
    out.name = make_static_string(name,strlen(name));
    out.value = value;

    return out;
}

struct StructCacheReq
{
    String name;
    NameSpace* name_space = nullptr;
    u32* struct_idx;
    u32* struct_size;
    Array<MemberCacheReq> members;
};

void add_member_cache_req(StructCacheReq& struct_req, const char* name, u32* value)
{
    push_var(struct_req.members,make_member_cache_req(name,value));
}

StructCacheReq make_struct_cache_req(NameSpace* name_space, const char* name, u32* struct_idx, u32* struct_size)
{
    StructCacheReq out;
    out.name = make_static_string(name,strlen(name));
    out.name_space = name_space;
    out.struct_idx = struct_idx;
    out.struct_size = struct_size;

    return out;
}

void destroy_cache_req(StructCacheReq& req)
{
    destroy_arr(req.members);
}

Option<itl_error> cache_structure(Interloper& itl, StructCacheReq& req)
{
    const auto struct_res = cache_struct_index(itl,req.name_space,req.name);
    if(!struct_res)
    {
        destroy_cache_req(req);
        return struct_res.error();
    }

    const u32 struct_idx = *struct_res;

    if(req.struct_idx)
    {
        *req.struct_idx = struct_idx;
    }

    auto& structure = itl.struct_table[struct_idx];

    if(req.struct_size)
    {
        *req.struct_size = structure.size;
    }

    for(const auto& member_req : req.members)
    {
        const auto member_res = cache_offset(itl,structure,member_req.name);

        if(!member_res)
        {
            destroy_cache_req(req);
            return member_res.error();
        }

        *member_req.value = *member_res;
    }

    destroy_cache_req(req);
    return option::none;
}


Option<itl_error> cache_rtti_structs(Interloper& itl)
{
    auto& rtti = itl.rtti_cache;
    rtti.enum_type_cache = make_table<u32,TypeTrieNode>();
    
    NameSpace* rtti_name_space = find_name_space(itl,"rtti");

    StructCacheReq any_cache_req = make_struct_cache_req(rtti_name_space,"Any",&rtti.any_idx,&rtti.any_struct_size);
    add_member_cache_req(any_cache_req,"data",&rtti.any_data_offset);
    add_member_cache_req(any_cache_req,"type",&rtti.any_type_offset);
    add_member_cache_req(any_cache_req,"stored_extern",&rtti.any_stored_extern_offset);

    const auto any_err = cache_structure(itl,any_cache_req);
    if(any_err)
    {
        return any_err;
    }

    StructCacheReq builtin_cache_req = make_struct_cache_req(rtti_name_space,"BuiltinType",nullptr,&rtti.builtin_type_struct_size);
    add_member_cache_req(builtin_cache_req,"builtin",&rtti.builtin_type_offset);

    const auto builtin_err = cache_structure(itl,builtin_cache_req);
    if(builtin_err)
    {
        return builtin_err;
    }

    StructCacheReq pointer_cache_req = make_struct_cache_req(rtti_name_space,"PointerType",nullptr,&rtti.pointer_struct_size);
    add_member_cache_req(pointer_cache_req,"contained_type",&rtti.pointer_contained_offset);

    const auto pointer_err = cache_structure(itl,pointer_cache_req);
    if(pointer_err)
    {
        return pointer_err;
    }

    StructCacheReq array_cache_req = make_struct_cache_req(rtti_name_space,"ArrayType",nullptr,&rtti.array_struct_size);
    add_member_cache_req(array_cache_req,"contained_type",&rtti.array_contained_offset);
    add_member_cache_req(array_cache_req,"size",&rtti.array_size_offset);
    add_member_cache_req(array_cache_req,"sub_size",&rtti.array_sub_size_offset);

    const auto array_err = cache_structure(itl,array_cache_req);
    if(array_err)
    {
        return array_err;
    }

    StructCacheReq enum_type_cache_req = make_struct_cache_req(rtti_name_space,"EnumType",nullptr,&rtti.enum_type_struct_size);
    add_member_cache_req(enum_type_cache_req,"enumeration",&rtti.enum_type_enumeration_offset);
    const auto enum_type_err = cache_structure(itl,enum_type_cache_req);
    if(enum_type_err)
    {
        return enum_type_err;
    }

    StructCacheReq enum_struct_cache_req = make_struct_cache_req(rtti_name_space,"Enum",nullptr,&rtti.enum_struct_struct_size);
    add_member_cache_req(enum_struct_cache_req,"name",&rtti.enum_struct_name_offset);
    add_member_cache_req(enum_struct_cache_req,"member",&rtti.enum_struct_member_offset);
    const auto enum_struct_err = cache_structure(itl,enum_struct_cache_req);
    if(enum_struct_err)
    {
        return enum_struct_err;
    }


    StructCacheReq enum_member_cache_req = make_struct_cache_req(rtti_name_space,"EnumMember",nullptr,&rtti.enum_member_size);
    add_member_cache_req(enum_member_cache_req,"name",&rtti.enum_member_name_offset);
    add_member_cache_req(enum_member_cache_req,"value",&rtti.enum_member_value_offset);
    return cache_structure(itl,enum_member_cache_req);
}
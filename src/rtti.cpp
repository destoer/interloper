
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
    auto offset_opt = member_offset(structure,member_name);

    if(!offset_opt)
    {
        return compile_error(itl,itl_error::rtti_error,"could not find offset for %s.%s",structure.name.buf,member_name.buf);
    }

    return *offset_opt;
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
    
    NameSpace* rtti_name_space = find_name_space(itl,"rtti");

    StructCacheReq any_cache_req = make_struct_cache_req(rtti_name_space,"Any",&rtti.any_idx,&rtti.any_struct_size);
    add_member_cache_req(any_cache_req,"data",&rtti.any_data_offset);
    add_member_cache_req(any_cache_req,"type",&rtti.any_type_offset);

    const auto any_err = cache_structure(itl,any_cache_req);
    if(!!any_err)
    {
        return any_err;
    }

    StructCacheReq builtin_cache_req = make_struct_cache_req(rtti_name_space,"BuiltinType",nullptr,&rtti.builtin_type_struct_size);
    add_member_cache_req(builtin_cache_req,"builtin",&rtti.builtin_type_offset);

    const auto builtin_err = cache_structure(itl,builtin_cache_req);
    if(!!builtin_err)
    {
        return builtin_err;
    }

    StructCacheReq pointer_cache_req = make_struct_cache_req(rtti_name_space,"PointerType",nullptr,&rtti.pointer_struct_size);
    add_member_cache_req(pointer_cache_req,"contained_type",&rtti.pointer_contained_offset);

    const auto pointer_err = cache_structure(itl,pointer_cache_req);
    if(!!pointer_err)
    {
        return pointer_err;
    }

    StructCacheReq array_cache_req = make_struct_cache_req(rtti_name_space,"ArrayType",nullptr,&rtti.array_struct_size);
    add_member_cache_req(array_cache_req,"contained_type",&rtti.array_contained_offset);
    add_member_cache_req(array_cache_req,"size",&rtti.array_size_offset);
    add_member_cache_req(array_cache_req,"sub_size",&rtti.array_sub_size_offset);

    return cache_structure(itl,array_cache_req);

/*
    rtti.enum_idx = cache_struct(itl,"EnumType");



    rtti.struct_idx = cache_struct(itl,"StructType");
*/
}


void destroy_trie(TypeTrieNode& root)
{
    for(u32 i = 0; i < count(root.nodes); i++)
    {
        destroy_trie(root.nodes[i]);
        destroy_arr(root.nodes);

        root = {};
    }
}

void destroy_rtti_cache(RttiCache& cache)
{
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        destroy_trie(cache.builtin_type_cache[i]);
        cache.builtin_type_cache[i] = {};
    }
}

TypeTrieNode& make_rtti_internal(Interloper& itl, const Type* type)
{
    auto& rtti = itl.rtti_cache;

    switch(type->kind)
    {
        case type_class::array_t:
        {
            ArrayType* array_type = (ArrayType*)type;
            TypeTrieNode& root = make_rtti_internal(itl,array_type->contained_type);
            
            // try find an existing node
            for(u32 i = 0; i < count(root.nodes); i++)
            {
                auto& node = root.nodes[i];

                // if we find a complete match return
                if(is_array(node.type))
                {
                    ArrayType* node_type = (ArrayType*)node.type;
                    
                    if(node_type->size == array_type->size)
                    {
                        return node;
                    }
                }
            }

            // there is no node that meets the requirements we will have to dump in another

            // reserve room for array type
            const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.array_struct_size);
            auto& section = pool_section_from_slot(itl.const_pool,slot);

            // push in base type
            write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::array_t));   

            // write in array type
            write_const_pool_pointer(itl.const_pool,section,rtti.array_contained_offset,root.slot);
            write_const_pool(itl.const_pool,section,rtti.array_size_offset,array_type->size);
            write_const_pool(itl.const_pool,section,rtti.array_sub_size_offset,array_type->sub_size);

            rtti.type_data_size += section.size;

            TypeTrieNode node;

            node.slot = slot;
            node.type = type;

            push_var(root.nodes,node);

            //return newly inserted node
            return root.nodes[count(root.nodes) - 1];
        }

        case type_class::pointer_t:
        {
            // get contained type
            const PointerType* pointer_type = (PointerType*)type;
            TypeTrieNode& root  = make_rtti_internal(itl,pointer_type->contained_type);

            // try find an existing node
            for(u32 i = 0; i < count(root.nodes); i++)
            {
                auto& node = root.nodes[i];

                if(is_pointer(node.type))
                {
                    return node;
                }
            }

            // no matching pointer make one

            // reserve room for pointer type
            const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.pointer_struct_size);
            auto& section = pool_section_from_slot(itl.const_pool,slot);

            // push in base type
            write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::pointer_t));            

            // push in pointer type
            write_const_pool_pointer(itl.const_pool,section,rtti.pointer_contained_offset,root.slot);

            TypeTrieNode node;

            node.slot = slot;
            node.type = type;

            push_var(root.nodes,node);

            // return newly inserted node
            return root.nodes[count(root.nodes) - 1];
        }

        case type_class::builtin_t:
        {
            const builtin_type builtin = cast_builtin(type);
            auto& root = rtti.builtin_type_cache[u32(builtin)];

            // base type is not yet in the pool
            if(root.slot.handle == INVALID_HANDLE)
            {
                // allocate a slot in the pool for us
                const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.builtin_type_struct_size);
                auto& section = pool_section_from_slot(itl.const_pool,slot);

                // write in base type struct
                write_const_pool(itl.const_pool,section,rtti.type_class_offset,u32(rtti_type_class::builtin_t));
                write_const_pool(itl.const_pool,section,rtti.builtin_type_offset,u32(builtin));


                root.slot = slot;

                rtti.type_data_size += section.size;
            }

            return root;
        }

        default:
        {
            assert(false);
            break;
        }
    }

    assert(false);
    return rtti.builtin_type_cache[0];
}


PoolSlot make_rtti(Interloper& itl, const Type* type)
{
    const auto node = make_rtti_internal(itl,type);
    return node.slot;
}

RegSlot acquire_rtti(Interloper& itl, Function& func, const Type* type)
{
    const PoolSlot pool_slot = make_rtti(itl,type);

    return pool_addr_res(itl,func,pool_slot,0);
}

u32 promote_size(u32 size)
{
    if(size < GPR_SIZE)
    {
        return GPR_SIZE;
    }

    return size;
}

// TODO: we need to pass in a slot + offset for storing data copies...
void make_any(Interloper& itl,Function& func, RegSlot any_ptr, u32 offset, const TypedReg& reg)
{
    auto& rtti = itl.rtti_cache;

    // aquire a copy of the typing information from the const pool
    const RegSlot rtti_ptr = acquire_rtti(itl,func,reg.type); 

    // goes directly in the pointer
    if(is_trivial_copy(reg.type))
    {
        // store type struct
        store_ptr(itl,func,rtti_ptr,any_ptr,offset + rtti.any_type_offset,GPR_SIZE,false);  

        const b32 fp = is_float(reg.type);

        // store data
        store_ptr(itl,func,reg.slot,any_ptr,offset + rtti.any_data_offset,GPR_SIZE,fp);              
    } 

    // allready in memory just store a the pointer to it
    else if(is_vla(reg.type))
    {
        // store type struct
        store_ptr(itl,func,rtti_ptr,any_ptr,offset + rtti.any_type_offset,GPR_SIZE,false); 

        const StructAddr struct_addr = {make_addr(reg.slot,0)};
        const auto arr_ptr = addrof_res(itl,func,struct_addr);

        // store data
        store_ptr(itl,func,arr_ptr,any_ptr,offset + rtti.any_data_offset,GPR_SIZE,false);
    }

    else if(is_struct(reg.type))
    {
        assert(false);
    }
    
    // should never happen
    else
    {
        assert(false);
    }
}


Option<itl_error> compile_any_internal(Interloper& itl, Function& func, AstNode* arg_node, RegSlot any_ptr, u32 offset)
{
    const auto& rtti = itl.rtti_cache;
    const b32 handle_storage = is_special_reg(any_ptr,spec_reg::null);

    // push const str as fixed size array
    if(arg_node->type == ast_type::string)
    {
        LiteralNode* lit_node = (LiteralNode*)arg_node;

        const u32 size = lit_node->literal.size;
        auto rtype = make_array(itl,make_builtin(itl,builtin_type::c8_t,true),size);

        // push the data offset
        const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,lit_node->literal);
        const RegSlot addr_slot = pool_addr_res(itl,func,pool_slot,0);

        const TypedReg reg = {addr_slot,rtype};

        if(handle_storage)
        {
            // alloc the struct size for our copy
            alloc_stack(itl,func,rtti.any_struct_size);

            const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);
            make_any(itl,func,SP_SLOT,0,reg);
        }

        else
        {
            make_any(itl,func,any_ptr,offset,reg);
        }
    }

    else
    {
        // compile our arg and figure out what we have
        auto res = compile_oper(itl,func,arg_node);

        if(!res)
        {
            return res.error();
        }

        auto arg_reg = *res;

        // is allready an any just copy the struct
        if(is_any(itl,arg_reg.type))
        {
            const u32 stack_size = rtti.any_struct_size;


            if(handle_storage)
            {
                // alloc the struct size for our copy
                alloc_stack(itl,func,stack_size);

                const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);

                // need to save SP as it will get pushed last
                const RegSlot dst_ptr = copy_reg(itl,func,SP_SLOT);
                const auto dst_addr = make_pointer_addr(dst_ptr,0);

                const auto src_addr = make_struct_addr(arg_reg.slot,0);

                const auto memcpy_err = ir_memcpy(itl,func,dst_addr,src_addr,stack_size);
                if(!!memcpy_err)
                {
                    return memcpy_err;
                }
            }

            else
            {
                assert(false);
            }
        }

        else
        {
            const u32 stack_size = rtti.any_struct_size;

            if(handle_storage)
            {
                // alloc the struct size for our copy
                alloc_stack(itl,func,stack_size);

                const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);
                make_any(itl,func,SP_SLOT,0,arg_reg);
            }

            else
            {
                make_any(itl,func,any_ptr,offset,arg_reg);
            }
        }  
    }

    return option::none;
}

// return total size including data
Result<u32,itl_error> compile_any(Interloper& itl, Function& func, AstNode* arg_node)
{
    // for now this just allways takes size of the any struct
    const u32 size = align_val(itl.rtti_cache.any_struct_size,GPR_SIZE);

    const auto NULL_SLOT = make_spec_reg_slot(spec_reg::null);

    // Handle stack alloc and store itself caller will handle deallocation of stack
    const auto any_err = compile_any_internal(itl,func,arg_node,NULL_SLOT,0);
    if(!!any_err)
    {
        return *any_err;
    }

    return size;
}


Option<itl_error> compile_any_arr(Interloper& itl, Function& func, AstNode* arg_node, RegSlot any_ptr, u32 offset)
{
    // storage allocation handled by caller
    return compile_any_internal(itl,func,arg_node,any_ptr,offset);
}
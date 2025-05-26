
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

Option<u32> cache_struct(Interloper& itl, NameSpace* name_space, const String& name)
{
    auto type_opt = lookup_type_scoped(itl,name_space,name);
    if(!type_opt)
    {
        return option::none;
    }

    TypeDecl* type_decl = *type_opt; 

    if(!type_decl)
    {
        compile_error(itl,itl_error::struct_error,"could not find struct %s for rtti\n",name.buf);
        return option::none;
    }

    if(type_decl->kind != type_kind::struct_t)
    {
        compile_error(itl,itl_error::struct_error,"%s is a %s and not a struct for rtti\n",name.buf,TYPE_KIND_NAMES[u32(type_decl->kind)]);
        return option::none;
    }

    return type_decl->type_idx;
}

Option<u32> cache_offset(Interloper& itl,Struct& structure, const String& member_name)
{
    auto offset_opt = member_offset(structure,member_name);

    if(!offset_opt)
    {
        compile_error(itl,itl_error::rtti_error,"could not find offset for %s.%s\n",structure.name.buf,member_name.buf);
        return option::none;
    }

    return *offset_opt;
}


dtr_res cache_rtti_structs(Interloper& itl)
{
    auto& rtti = itl.rtti_cache;
    
    NameSpace* rtti_name_space = find_name_space(itl,"rtti");

    // cache Any struct info
    auto any_idx_opt = cache_struct(itl,rtti_name_space,"Any");
    if(invalid_type_idx(rtti.any_idx))
    {
        return dtr_res::err;
    }

    rtti.any_idx = *any_idx_opt;




    auto& any_struct = itl.struct_table[rtti.any_idx];
    auto any_data_offset_opt = cache_offset(itl,any_struct,"data");
    auto any_type_offset_opt = cache_offset(itl,any_struct,"type");

    if(!any_data_offset_opt || !any_type_offset_opt)
    {
        return dtr_res::err;
    }

    rtti.any_data_offset = *any_data_offset_opt;
    rtti.any_type_offset = *any_type_offset_opt;
    rtti.any_struct_size = any_struct.size;


    // cache Type struct info
    auto type_struct_idx_opt = cache_struct(itl,rtti_name_space,"Type");

    if(!type_struct_idx_opt)
    {
        return dtr_res::err;
    }

    const u32 type_struct_idx = *type_struct_idx_opt;
    auto& type_struct = itl.struct_table[type_struct_idx];

    auto is_const_offset_opt = cache_offset(itl,type_struct,"is_const");
    auto type_class_offset_opt = cache_offset(itl,type_struct,"kind");
    if(!is_const_offset_opt || !type_class_offset_opt)
    {
        return dtr_res::err;
    }

    rtti.is_const_offset = *is_const_offset_opt;
    rtti.type_class_offset = *type_class_offset_opt;
    rtti.type_struct_size = type_struct.size;


    // Builtin type cache
    // cache Type struct info
    auto buitlin_type_struct_idx_opt =  cache_struct(itl,rtti_name_space,"BuiltinType");
    if(!buitlin_type_struct_idx_opt)
    {
        return dtr_res::err;
    }

    const u32 builtin_type_struct_idx = *buitlin_type_struct_idx_opt;
    auto& builtin_type_struct = itl.struct_table[builtin_type_struct_idx];

    auto builtin_type_offset_opt = cache_offset(itl,builtin_type_struct,"builtin");
    if(!builtin_type_offset_opt)
    {
        return dtr_res::err;
    }

    rtti.builtin_type_offset = *builtin_type_offset_opt;
    rtti.builtin_type_struct_size = builtin_type_struct.size;

    // Pointer type cache
    auto pointer_struct_idx_opt = cache_struct(itl,rtti_name_space,"PointerType");
    
    if(!pointer_struct_idx_opt)
    {
        return dtr_res::err;
    }

    const u32 pointer_struct_idx = *pointer_struct_idx_opt;
    auto& pointer_struct = itl.struct_table[pointer_struct_idx];

    auto pointer_contained_offset_opt = cache_offset(itl,pointer_struct,"contained_type");
    if(!pointer_contained_offset_opt)
    {
        return dtr_res::err;
    }

    rtti.pointer_contained_offset = *pointer_contained_offset_opt; 
    rtti.pointer_struct_size = pointer_struct.size;


    // Array type cache
    const auto array_struct_idx_opt = cache_struct(itl,rtti_name_space,"ArrayType");
    if(!array_struct_idx_opt)
    {
        return dtr_res::err;
    }

    const u32 array_struct_idx = *array_struct_idx_opt;
    auto& array_struct = itl.struct_table[array_struct_idx];
    
    auto array_contained_offset_opt = cache_offset(itl,array_struct,"contained_type");
    auto array_size_offset_opt = cache_offset(itl,array_struct,"size");
    auto array_sub_size_offset_opt = cache_offset(itl,array_struct,"sub_size");

    if(!array_contained_offset_opt || !array_size_offset_opt || !array_sub_size_offset_opt)
    {
        return dtr_res::err;
    }

    rtti.array_contained_offset = *array_contained_offset_opt; 
    rtti.array_size_offset = *array_size_offset_opt;
    rtti.array_sub_size_offset = *array_sub_size_offset_opt;
    rtti.array_struct_size = array_struct.size;

/*
    rtti.enum_idx = cache_struct(itl,"EnumType");



    rtti.struct_idx = cache_struct(itl,"StructType");
*/
    return dtr_res::ok;
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
    for(u32 i = 0; i < TYPE_ATTR; i++)
    {
        for(u32 j = 0; j < BUILTIN_TYPE_SIZE; j++)
        {
            destroy_trie(cache.builtin_type_cache[i][j]);

            cache.builtin_type_cache[i][j] = {};
        }
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
                    
                    if(node_type->size == array_type->size && node_type->type.is_const == array_type->type.is_const)
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
            write_const_pool(itl.const_pool,section,rtti.is_const_offset,type->is_const);
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
                    PointerType* node_type = (PointerType*)node.type;

                    if(node_type->type.is_const == pointer_type->type.is_const)
                    {
                        return node;
                    }
                }
            }

            // no matching pointer make one

            // reserve room for pointer type
            const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.pointer_struct_size);
            auto& section = pool_section_from_slot(itl.const_pool,slot);

            // push in base type
            write_const_pool(itl.const_pool,section,rtti.is_const_offset,type->is_const);
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
            auto& root = rtti.builtin_type_cache[type->is_const][u32(builtin)];

            // base type is not yet in the pool
            if(root.slot.handle == INVALID_HANDLE)
            {
                // allocate a slot in the pool for us
                const auto slot = reserve_const_pool_section(itl.const_pool,pool_type::var,rtti.builtin_type_struct_size);
                auto& section = pool_section_from_slot(itl.const_pool,slot);

                // write in base type struct
                write_const_pool(itl.const_pool,section,rtti.is_const_offset,type->is_const);
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
    return rtti.builtin_type_cache[0][0];
}


PoolSlot make_rtti(Interloper& itl, const Type* type)
{
    const auto node = make_rtti_internal(itl,type);
    return node.slot;
}

RegSlot aquire_rtti(Interloper& itl, Function& func, const Type* type)
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
void make_any(Interloper& itl,Function& func, RegSlot any_ptr, u32 offset, const RegSlot src, const Type* type)
{
    auto& rtti = itl.rtti_cache;

    // aquire a copy of the typing information from the const pool
    const RegSlot rtti_ptr = aquire_rtti(itl,func,type); 

    // goes directly in the pointer
    if(is_trivial_copy(type))
    {
        // store type struct
        store_ptr(itl,func,rtti_ptr,any_ptr,offset + rtti.any_type_offset,GPR_SIZE,false);  

        const b32 fp = is_float(type);

        // store data
        store_ptr(itl,func,src,any_ptr,offset + rtti.any_data_offset,GPR_SIZE,fp);              
    } 

    // allready in memory just store a the pointer to it
    else if(is_array(type))
    {
        // store type struct
        store_ptr(itl,func,rtti_ptr,any_ptr,offset + rtti.any_type_offset,GPR_SIZE,false); 

        // directly store array pointer into the data pointer
        if(is_fixed_array(type))
        {
            const auto arr_data_slot = load_arr_data(itl,func,src,type);

            // store data
            store_ptr(itl,func,arr_data_slot,any_ptr,offset + rtti.any_data_offset,GPR_SIZE,false);
        }

        // runtime size
        else
        {
            const auto arr_ptr = addrof_res(itl,func,src);

            // store data
            store_ptr(itl,func,arr_ptr,any_ptr,offset + rtti.any_data_offset,GPR_SIZE,false);
        }   
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


dtr_res compile_any_internal(Interloper& itl, Function& func, AstNode* arg_node, RegSlot any_ptr, u32 offset)
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

        if(handle_storage)
        {
            // alloc the struct size for our copy
            alloc_stack(itl,func,rtti.any_struct_size);

            const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);
            make_any(itl,func,SP_SLOT,0,addr_slot,rtype);
        }

        else
        {
            make_any(itl,func,any_ptr,offset,addr_slot,rtype);
        }
    }

    else
    {
        // compile our arg and figure out what we have
        auto res = compile_oper(itl,func,arg_node);

        if(!res)
        {
            return dtr_res::err;
        }

        auto [arg_type,reg] = *res;

        // is allready an any just copy the struct
        if(is_any(itl,arg_type))
        {
            const u32 stack_size = rtti.any_struct_size;


            if(handle_storage)
            {
                // alloc the struct size for our copy
                alloc_stack(itl,func,stack_size);

                const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);

                // need to save SP as it will get pushed last
                const RegSlot dst_ptr = copy_reg(itl,func,SP_SLOT);
                const auto dst_addr = make_addr(dst_ptr,0);

                const auto src_addr = make_struct_addr(reg,0);

                if(!ir_memcpy(itl,func,dst_addr,src_addr,stack_size))
                {
                    return dtr_res::err;
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
                make_any(itl,func,SP_SLOT,0,reg,arg_type);
            }

            else
            {
                make_any(itl,func,any_ptr,offset,reg,arg_type);
            }
        }  
    }

    return dtr_res::ok;
}

// return total size including data
Option<u32> compile_any(Interloper& itl, Function& func, AstNode* arg_node)
{
    // for now this just allways takes size of the any struct
    const u32 size = align_val(itl.rtti_cache.any_struct_size,GPR_SIZE);

    const auto NULL_SLOT = make_spec_reg_slot(spec_reg::null);

    // Handle stack alloc and store itself caller will handle deallocation of stack
    if(!compile_any_internal(itl,func,arg_node,NULL_SLOT,0))
    {
        return option::none;
    }

    return size;
}


dtr_res compile_any_arr(Interloper& itl, Function& func, AstNode* arg_node, RegSlot any_ptr, u32 offset)
{
    // storage allocation handled by caller
    return compile_any_internal(itl,func,arg_node,any_ptr,offset);
}
Option<itl_error> traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, AddrSlot addr_slot, const Struct& structure);
Option<itl_error> struct_list_write(Interloper& itl, Function& func, AddrSlot addr_member, const Member& member, AstNode* node);
TypeResult assign_struct_initializer(Interloper &itl,Function &func, AddrSlot dst, StructInitializerNode* struct_initializer);
TypeResult slice_array_addr(Interloper& itl, Function& func, SliceNode* slice_node, RegSlot dst_slot, const TypedAddr& arr);



Option<itl_error> traverse_designated_initializer_list(Interloper& itl, Function& func, DesignatedListNode* node, AddrSlot addr_slot, Struct& structure)
{
    const u32 member_size = count(structure.members);
    BitSet set = make_bit_set(member_size);

    for(u32 i = 0; i < count(node->initializer); i++)
    {
        // TODO: We should attempt the acceses in offset order.
        const DesignatedInitializer init = node->initializer[i];

        const u32* member_idx = lookup(structure.member_map,init.name);
        if(!member_idx)
        {
            destroy_bit_set(set);
            return compile_error(itl,itl_error::struct_error,"No such member %s in structure %s",init.name,structure.name);
        }

        const u32 idx = *member_idx;

        // Note that we have seen this member
        set_bit_set(set,idx);
        const auto member = structure.members[idx];
    
        // generate a new offset
        // NOTE: make sure this is a copy
        auto addr_member = addr_slot;
        addr_member.addr.offset += member.offset;

        const auto err = struct_list_write(itl,func,addr_member,member,init.expr);
        if(!!err)
        {
            destroy_bit_set(set);
            return err;
        }
    } 

    if(set.count != member_size)
    {
        destroy_bit_set(set);
        return compile_error(itl,itl_error::struct_error,"struct designated initializer missing initializer expected %d got %d",member_size,set.count);
    }

    destroy_bit_set(set);
    return option::none;
}


Option<itl_error> access_array_member(Interloper& itl, const String& member_name,TypedAddr* struct_addr)
{
    ArrayType* array_type = (ArrayType*)struct_addr->type;

    if(member_name == "len")
    {
        if(!is_runtime_size(struct_addr->type))
        {
            struct_addr->addr_slot.addr.base = make_spec_reg_slot(spec_reg::access_fixed_len_reg);
            struct_addr->addr_slot.struct_addr = false;
        }

        // vla
        else
        {
            struct_addr->addr_slot.addr.offset += GPR_SIZE;
            struct_addr->type = make_builtin(itl,GPR_SIZE_TYPE);
        }

        return option::none;
    }

    else if(member_name == "data")
    {
        // fixed sized array is not a struct dont allow access
        if(is_fixed_array(struct_addr->type))
        {
            return compile_error(itl,itl_error::array_type_error,"no .data member on fixed size array");
        }

        struct_addr->type = make_reference(itl,array_type->contained_type);
        return option::none;
    }


    return compile_error(itl,itl_error::undeclared,"unknown array member %s",member_name.buf);
}

Option<itl_error> access_struct_member(Interloper& itl,const String& member_name, TypedAddr* struct_addr)
{
    // get offset for struct member
    const auto member_opt = get_member(itl.struct_table,struct_addr->type,member_name);

    if(!member_opt)
    {
        return compile_error(itl,itl_error::undeclared,"No such member %s for type %s",
            member_name.buf,type_name(itl,struct_addr->type).buf);
    }

    const auto member = member_opt.value();

    struct_addr->addr_slot.addr.offset += member.offset;  
    struct_addr->type = member.type;

    return option::none;
}


Option<u32> member_offset(Struct& structure, const String& name)
{
    auto member_opt = get_member(structure,name);
    if(!member_opt)
    {
        return option::none;
    }

    auto member = member_opt.value();

    return member.offset;
}

Option<itl_error> access_enum_struct_member(Interloper& itl,Function& func, const String& member_name, TypedAddr* struct_addr)
{
    const auto& enumeration = enum_from_type(itl.enum_table,struct_addr->type);

    if(!enumeration.underlying_type || !is_struct(enumeration.underlying_type))
    {
        return compile_error(itl,itl_error::struct_error,"member access on plain enum %s",enumeration.name.buf);                   
    }

    // pull info on enum struct member
    auto& enum_struct = struct_from_type(itl.struct_table,enumeration.underlying_type);

    const auto enum_struct_member_opt = get_member(enum_struct, member_name);

    if(!enum_struct_member_opt)
    {
        return compile_error(itl,itl_error::undeclared,"No such member %s for type %s",
            member_name.buf,type_name(itl,struct_addr->type).buf);              
    }

    const auto& enum_struct_member = enum_struct_member_opt.value();

    // get the start of the table
    const auto enum_table_slot = pool_addr_res(itl,func,enumeration.struct_slot,0);

    // get the enum index
    RegSlot enum_slot = INVALID_SYM_REG_SLOT;
    
    // we allready directly have the enum
    if(struct_addr->addr_slot.struct_addr)
    {
        assert(struct_addr->addr_slot.addr.offset == 0);
        enum_slot = struct_addr->addr_slot.addr.base;
    }

    // ordinary access on a pointer, we must deref it
    else
    {
        enum_slot = new_tmp(func,GPR_SIZE);
        load_addr_slot(itl,func,enum_slot,struct_addr->addr_slot,ENUM_SIZE,false,false);
    }

    // finally index the table
    const auto addr = generate_indexed_pointer(itl,func,enum_table_slot,enum_slot,enum_struct.size,enum_struct_member.offset);

    *struct_addr = {addr,enum_struct_member.type};
    return option::none;
}


Result<TypedAddr,itl_error> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
{
    BinNode* member_root =(BinNode*)node;

    AstNode* expr_node = member_root->left;

    TypedAddr struct_addr;

    // parse out initial expr
    switch(expr_node->type)
    {
        default: 
        {
            return compile_error(itl,itl_error::struct_error,"Unknown struct access %s",AST_NAMES[u32(expr_node->type)]);
        }
    }


    RecordNode* members = (RecordNode*)member_root->right;

    // perform each member access
    for(u32 m = 0; m < count(members->nodes); m++)
    {
        AstNode *n = members->nodes[m];

        switch(n->type)
        {
            default: 
            {
                return compile_error(itl,itl_error::undeclared,"Unknown member access %s",AST_NAMES[u32(n->type)]);
            }
        }
    }

    return struct_addr;
}

RegResult compute_member_ptr(Interloper& itl, Function& func, RegSlot dst_slot, AstNode* node)
{
    auto member_addr_res = compute_member_addr(itl,func,node);

    if(!member_addr_res)
    {
        return member_addr_res.error();
    }

    auto dst_addr = *member_addr_res;

    collapse_struct_addr(itl,func,dst_slot,dst_addr.addr_slot);
    return TypedReg{dst_slot,make_reference(itl,dst_addr.type)};
}

RegResult compute_member_ptr_res(Interloper& itl, Function& func, AstNode* node)
{
    const RegSlot tmp = new_tmp(func,GPR_SIZE);
    return compute_member_ptr(itl,func,tmp,node);
}

Option<itl_error> write_struct(Interloper& itl,Function& func, const TypedReg& src, AstNode *node)
{
    auto member_addr_res = compute_member_addr(itl,func,node);

    if(!member_addr_res)
    {
        return member_addr_res.error();
    }

    const auto dst_addr = *member_addr_res;

    const auto assign_err = check_assign(itl,dst_addr.type,src.type);
    if(!!assign_err)
    {
        return *assign_err;
    }

    return do_addr_store(itl,func,src.slot,dst_addr);
}


TypeResult read_struct(Interloper& itl,Function& func, RegSlot dst_slot, AstNode *node)
{
    auto member_addr_res =  compute_member_addr(itl,func,node);

    if(!member_addr_res)
    {
        return member_addr_res.error();
    }

    auto src_addr = *member_addr_res;

    // len access on fixed sized array
    if(is_special_reg(src_addr.addr_slot.addr.base,spec_reg::access_fixed_len_reg))
    {
        const ArrayType* array_type = (ArrayType*)src_addr.type;

        mov_imm(itl,func,dst_slot,array_type->size);
        return make_builtin(itl,builtin_type::u32_t);
    }

    // let caller handle reads via array accessors
    if(is_fixed_array(src_addr.type))
    {
        collapse_struct_addr(itl,func,dst_slot,src_addr.addr_slot);
        return src_addr.type;
    }

    const auto load_err = do_addr_load(itl,func,dst_slot,src_addr);
    if(!!load_err)
    {
        return *load_err;
    }

    return src_addr.type;
}


Option<itl_error> struct_list_write(Interloper& itl, Function& func, AddrSlot addr_member, const Member& member, AstNode* node)
{
    switch(node->type)
    {
        // plain values
        default:
        {
            assert(false);
        }
    }

    assert(false);
    return option::none;
}

Option<itl_error> traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, AddrSlot addr_slot, const Struct& structure)
{
    const u32 node_len = count(node->nodes);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        return compile_error(itl,itl_error::undeclared,"struct initializer missing initializer expected %d got %d",member_size,node_len);
    }
    
    for(u32 i = 0; i < count(structure.members); i++)
    {
        const auto member = structure.members[i];
    
        // generate a new offset
        // NOTE: make sure this is a copy
        auto addr_member = addr_slot;
        addr_member.addr.offset += member.offset;

        const auto err = struct_list_write(itl,func,addr_member,member,node->nodes[i]);
        if(!!err)
        {
            return err;
        }
    } 

    return option::none;
}

// NOTE: Caller must check assignment result.
TypeResult assign_struct_initializer(Interloper &itl,Function &func, AddrSlot dst, StructInitializerNode* struct_initializer)
{
    const auto struct_type_res = lookup_struct(itl,struct_initializer->name_space,struct_initializer->struct_name);
    if(!struct_type_res)
    {
        return struct_type_res;
    }
    
    const auto struct_type = *struct_type_res;

    // Compile a initializer list into the return type
    auto &structure = struct_from_type(itl.struct_table,struct_type);

    switch(struct_initializer->initializer->type)
    {
        default: assert(false);
    }

    return struct_type;
}

Option<itl_error> compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot)
{
    // TODO: add a opt to just memset the entire thing in one go
    // NOTE: this should apply all the way down i.e if we contain a struct
    // it needs to have initialzer_zero aswell
    // if(structure.initializer_zero)

    // default construction
    for(u32 m = 0; m < count(structure.members); m++)
    {
        const auto& member = structure.members[m];

        AddrSlot member_addr = addr_slot;
        member_addr.addr.offset += member.offset;

        if(member.expr)
        {
            auto context_guard = switch_context(itl,structure.filename,structure.name_space,member.expr);

            switch(member.expr->type)
            {
                default: 
                {
                    assert(false);
                }
            }
        }

        // (basically we need to just recurse this method)
        else if(is_struct(member.type))
        {
            const auto nested_structure = struct_from_type(itl.struct_table,member.type);
            const auto decl_err = compile_struct_decl_default(itl,func,nested_structure,member_addr);
            if(!!decl_err)
            {
                return decl_err;
            }
        }

        else if(is_array(member.type))
        {
            const auto init_err = default_construct_arr(itl,func,(ArrayType*)member.type,member_addr);
            if(!!init_err)
            {
                return init_err;
            }
        }

        else
        {
            if(is_reference(member.type))
            {
               return compile_error(itl,itl_error::pointer_type_error,"Reference member %s must have an explicit initializer: %s",
                    member.name.buf,type_name(itl,member.type).buf);
            }

            const RegSlot tmp = imm_zero(itl,func);
            const TypedAddr dst_addr = {member_addr,member.type};
            const auto store_err = do_addr_store(itl,func,tmp,dst_addr);
            if(!!store_err)
            {
                return store_err;
            }
        }
    }

    return option::none;
}

Option<itl_error> compile_struct_decl(Interloper& itl, Function& func, const DeclNode *decl_node, SymSlot slot)
{
    Type* ltype = nullptr;

    const auto reg_slot = make_sym_reg_slot(slot);

    // isolate our symbol as it may move
    {
        auto& sym = sym_from_slot(itl.symbol_table,slot);
        alloc_slot(itl,func,reg_slot,true);

        ltype = sym.type;
    }

    const TypedReg reg = {reg_slot,ltype};


    auto structure = struct_from_type(itl.struct_table,reg.type);

    
    if(decl_node->expr)
    {
        switch(decl_node->expr->type)
        {
            default:
            {
                assert(false);
            }
        }
    }

    // default init
    else
    {
        const AddrSlot addr_slot = make_struct_addr(reg.slot,0);
        return compile_struct_decl_default(itl,func,structure,addr_slot);
    }
}
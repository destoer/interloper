Option<itl_error> traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, AddrSlot addr_slot, const Struct& structure);
Option<itl_error> struct_list_write(Interloper& itl, Function& func, AddrSlot addr_member, const Member& member, AstNode* node);
TypeResult assign_struct_initializer(Interloper &itl,Function &func, AddrSlot dst, StructInitializerNode* struct_initializer);
TypeResult slice_array_addr(Interloper& itl, Function& func, SliceNode* slice_node, RegSlot dst_slot, const TypedAddr& arr);

void print_member(Interloper& itl,const Member& member)
{
    printf("\t%s -> %d : %s\n",member.name.buf,member.offset,type_name(itl,member.type).buf);
}

void print_struct(Interloper& itl, const Struct& structure)
{
    printf("struct %s\n{\n",structure.name.buf);

    for(const auto& member : structure.members)
    {
        print_member(itl,member);
    }

    printf("};\n");
    printf("size: %d\n",structure.size);
}

void add_struct(Interloper& itl, Struct& structure, TypeDecl& decl)
{
    structure.type_idx = decl.type_idx;
    itl.struct_table[structure.type_idx] = structure;
    finalise_type(decl,structure.type_idx);
}


void destroy_struct(Struct& structure)
{
    destroy_arr(structure.members);
    destroy_table(structure.member_map);  
}

void destroy_struct_table(StructTable& struct_table)
{
    // delete all struct defs
    for(auto& structure : struct_table)
    {
        destroy_struct(structure);
    }

    destroy_arr(struct_table);
}


Struct& struct_from_type(StructTable& struct_table, const Type* type)
{
    StructType* struct_type = (StructType*)type;

    return struct_table[struct_type->struct_idx];
}   


Option<Member> get_member(Struct& structure,const String& member_name)
{
    const u32* idx = lookup(structure.member_map,member_name);

    if(!idx)
    {
        return option::none;
    }

    const auto member = structure.members[*idx];
    return Option<Member>(member);    
}

Option<Member> get_member(StructTable& struct_table, const Type* type, const String& member_name)
{
    if(!is_struct(type))
    {
        return option::none;
    }

    auto& structure = struct_from_type(struct_table,type);

    return get_member(structure,member_name);
}


static constexpr u32 OFFSET_FORCED_FIRST = 0xffff'ffff;

std::pair<u32,u32> compute_member_size(Interloper& itl,const Type* type)
{
    if(is_fixed_array(type))
    {
        return calc_arr_allocation(itl,type);
    }

    const u32 size = type_memory_size(itl,type);
    return calc_alloc_size(size);
}


TypeResult lookup_struct(Interloper& itl, NameSpace* name_space,const String& name)
{
    const auto struct_decl_res = lookup_type_internal(itl,name_space,name);
    if(!struct_decl_res)
    {
        return compile_error(itl,itl_error::struct_error,"No such struct: %s",name.buf);
    }

    const auto struct_decl = *struct_decl_res;

    if(struct_decl->kind != type_kind::struct_t)
    {
        return compile_error(itl,itl_error::struct_error,"No such struct: %s",name.buf);
    }

    return make_struct(itl,struct_decl->type_idx);   
}

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


Option<itl_error> handle_recursive_type(Interloper& itl,const String& struct_name, TypeNode* type_decl, u32* type_idx_override)
{
    const auto name = type_decl->name;
    TypeDecl* decl_ptr = type_decl->name_space? lookup_incomplete_decl_scoped(type_decl->name_space,name) : lookup_incomplete_decl(itl,name);

    // no such decl exists
    if(!decl_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"%s : member type %s is not defined",struct_name.buf,type_decl->name.buf);
    }

    // Type is allways complete we don't need any further checking
    if(!(decl_ptr->flags & TYPE_DECL_DEF_FLAG))
    {
        return option::none;
    }


    TypeDef& def = *((TypeDef*)decl_ptr);

    // if we attempt to check a partial definition twice that the definition is recursive
    if(def.decl.state == type_def_state::checking)
    {
        // if its a pointer we dont need the complete information yet as they are all alike
        // so just override the type idx from the one reserved inside the def
        if(def_has_indirection(type_decl))
        {
            *type_idx_override = def.decl.type_idx;
        }

        else
        {
            // panic to prevent having our struct collapse into a black hole
            return compile_error(itl,itl_error::black_hole,"%s : is recursively defined via %s",struct_name.buf,type_decl->name.buf);
        }
    }

    else
    {
        return parse_def(itl,def);
    }

    return option::none;    
}

// returns member loc
Result<u32,itl_error> add_member(Interloper& itl,Struct& structure,DeclNode* m, u32* size_count,b32 forced_first, u32 flags)
{
    Member member;
    member.name = m->name;

    TypeNode* type_decl = m->type;

    itl.ctx.expr = (AstNode*)m; 

    // copy the init expr
    member.expr = m->expr;

    u32 type_idx_override = INVALID_TYPE;

    // TODO: function pointer currently requires in order decl
    // or deduction will fail
    if(type_decl->func_type)
    {
        auto type_res = get_type(itl,type_decl,type_idx_override,true);

        if(!type_res)
        {
            destroy_struct(structure);
            return type_res.error();
        }

        member.type = *type_res;
    }

    else if(!type_exists(itl,type_decl->name))
    {
        const auto recur_err = handle_recursive_type(itl,structure.name,type_decl,&type_idx_override);
        if(!!recur_err)
        {
            destroy_struct(structure);
            return *recur_err;
        }

        auto type_res = get_type(itl,type_decl,type_idx_override,true);

        if(!type_res)
        {
            destroy_struct(structure);
            return type_res.error();
        }

        member.type = *type_res;
    }

    else
    {
        auto type_res = get_type(itl,type_decl,type_idx_override,true);

        if(!type_res)
        {
            destroy_struct(structure);
            return type_res.error();
        }

        member.type = *type_res;
    }



    // we will deal with this later
    if(flags & ATTR_NO_REORDER)
    {
        member.offset = count(structure.members);
    }

    else if(forced_first)
    {
        member.offset = OFFSET_FORCED_FIRST;
    }

    // normal member decl
    else
    {
        const auto [size,count] = compute_member_size(itl,member.type);

        member.offset = size_count[log2(size)];

        // translate larger items, into several allocations on the final section
        size_count[log2(size)] += count;
    }

    const u32 loc = count(structure.members);


    if(contains(structure.member_map,member.name))
    {
        const auto res = compile_error(itl,itl_error::redeclaration,"%s : member %s redeclared",structure.name.buf,member.name.buf);
        destroy_struct(structure);
        return res;
    }

    add(structure.member_map,member.name,loc);
    push_var(structure.members,member); 

    return loc;
}

void finalise_member_offsets(Interloper& itl, Struct& structure, u32* size_count, s32 forced_first, u32 flags)
{
    // push members in order
    if(flags & ATTR_NO_REORDER)
    {
        u32 offset = 0;

        // iter back over every member and give its offset
        for(u32 m = 0; m < count(structure.members); m++)
        {
            auto& member = structure.members[m];

            const auto [size,count] = compute_member_size(itl,member.type);

            // align on size but actually add count  
            offset = align_val(offset,size);

            member.offset = offset;

            offset += size * count;
        }

        structure.data_size = offset;
        structure.size = align_val(structure.data_size,GPR_SIZE);
    }

    // default: reorder the struct for size
    else
    {
        // handle alginment & get starting zonnes + total size
        u32 alloc_start[4];

        u32 byte_start = 0;

        // insert this as the first set of data in the byte section
        if(forced_first != -1)
        {
            auto& member = structure.members[forced_first];
            const auto [size,count] = compute_member_size(itl,member.type);
            
            const u32 bytes = size * count;

            // include allocation for this member
            size_count[0] += bytes;

            // usual byte start offset by our insertion at front
            byte_start = bytes;
        }

        // finalise the offsets
        structure.size = calc_alloc_sections(alloc_start,size_count,byte_start);

        structure.data_size = structure.size;

        // iter back over every member and give its offset
        for(u32 m = 0; m < count(structure.members); m++)
        {
            auto& member = structure.members[m];

            const auto [size,count] = compute_member_size(itl,member.type);

            if(member.offset == OFFSET_FORCED_FIRST)
            {
                member.offset = 0;
            }

            else 
            {
                const u32 zone_offset = member.offset;
                member.offset = alloc_start[log2(size)] + (zone_offset * size);
            }
        }
    }
}

Option<itl_error> parse_struct_def(Interloper& itl, TypeDef& def)
{
    StructNode* node = (StructNode*)def.root;

    // NOTE: we expect the caller to save this
    trash_context(itl,node->filename,def.decl.name_space,def.root);

    Struct structure;
    
    // allocate a reserved slot for the struct
    def.decl.type_idx = count(itl.struct_table);
    resize(itl.struct_table,count(itl.struct_table) + 1);


    structure.name = node->name;
    structure.filename = node->filename;
    structure.name_space = def.decl.name_space;
    structure.member_map = make_table<String,u32>();

    // we want to get how many sizes of each we have
    // and then we can go back through and align the struct with them
    u32 size_count[4] = {0};

    s32 forced_first_loc = -1;

    const u32 flags = node->attr_flags;

    // force this to be at the first location in mem
    if(node->forced_first)
    {
        auto forced_first_loc_res = add_member(itl,structure,node->forced_first,size_count,true,flags);

        if(!forced_first_loc_res)
        {
            return forced_first_loc_res.error();
        }

        forced_first_loc = *forced_first_loc_res;
    }

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        const auto member_res = add_member(itl,structure,node->members[i],size_count,false,flags);
        if(!member_res)
        {
            return member_res.error();
        }
    }

    finalise_member_offsets(itl,structure,size_count,forced_first_loc,flags);
    

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }

    add_struct(itl,structure,def.decl);
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
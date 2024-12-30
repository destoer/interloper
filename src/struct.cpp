void print_member(Interloper& itl,const Member& member)
{
    printf("\t%s -> %d : %s\n",member.name.buf,member.offset,type_name(itl,member.type).buf);
}

void print_struct(Interloper& itl, const Struct& structure)
{
    printf("struct %s\n{\n",structure.name.buf);

    for(u32 m = 0; m < count(structure.members); m++)
    {
        print_member(itl,structure.members[m]);
    }

    printf("};\n");
    printf("size: %d\n",structure.size);
}

void add_struct(Interloper& itl, Struct& structure, TypeDecl& decl)
{
    structure.type_idx = decl.type_idx;
    itl.struct_table[structure.type_idx] = structure;
    finalise_type(decl,structure.type_idx,type_kind::struct_t);
}


void destroy_struct(Struct& structure)
{
    destroy_arr(structure.members);
    destroy_table(structure.member_map);  
}

void destroy_struct_table(StructTable& struct_table)
{
    // delete all struct defs
    for(u32 s = 0; s < count(struct_table); s++)
    {   
        auto& structure = struct_table[s];
        destroy_struct(structure);
    }

    destroy_arr(struct_table);
}


Struct& struct_from_type(StructTable& struct_table, const Type* type)
{
    StructType* struct_type = (StructType*)type;

    return struct_table[struct_type->struct_idx];
}   


std::optional<Member> get_member(Struct& structure,const String& member_name)
{
    const u32* idx = lookup(structure.member_map,member_name);

    if(!idx)
    {
        return std::nullopt;
    }

    const auto member = structure.members[*idx];
    return std::optional<Member>(member);    
}

std::optional<Member> get_member(StructTable& struct_table, const Type* type, const String& member_name)
{
    if(!is_struct(type))
    {
        return std::nullopt;
    }

    auto& structure = struct_from_type(struct_table,type);

    return get_member(structure,member_name);
}


static constexpr u32 OFFSET_FORCED_FIRST = 0xffff'ffff;

std::pair<u32,u32> compute_member_size(Interloper& itl,const Type* type)
{
    if(is_fixed_array(type))
    {
        ArrayType* array_type = (ArrayType*)type;

        u32 count = array_type->size;
        u32 size = array_type->sub_size;

        // size > GPR_SIZE align on gpr_size
        // otherwhise on its own boundary
        if(array_type->sub_size > GPR_SIZE)
        {
            count += array_type->sub_size / GPR_SIZE;
            size = GPR_SIZE;
        }

        return std::pair{size,count};
    }

    else
    {
        const u32 size = type_size(itl,type);
        return calc_alloc_size(size);
    } 
}

b32 handle_recursive_type(Interloper& itl,const String& struct_name, TypeNode* type_decl, u32* type_idx_override)
{
    TypeDecl *decl_ptr = lookup_incomplete_decl(itl,type_decl->name);

    // no such decl exists
    if(!decl_ptr)
    {
        panic(itl,itl_error::undeclared,"%s : member type %s is not defined\n",struct_name.buf,type_decl->name.buf);
        return false;
    }

    // Type is allways complete we don't need any further checking
    if(!(decl_ptr->flags & TYPE_DECL_DEF_FLAG))
    {
        return true;
    }


    TypeDef& def = *((TypeDef*)decl_ptr);

    // if we attempt to check a partial defintion twice that the definition is recursive
    if(def.decl.state == type_def_state::checking)
    {
        // if its a pointer we dont need the complete inormation yet as they are all alike
        // so just override the type idx from the one reserved inside the def
        if(def_has_indirection(type_decl))
        {
            *type_idx_override = def.decl.type_idx;
        }

        else
        {
            // panic to prevent having our struct collpase into a black hole
            panic(itl,itl_error::black_hole,"%s : is recursively defined via %s\n",struct_name.buf,type_decl->name.buf);
            return false;
        }
    }

    else
    {
        parse_def(itl,def);

        if(itl.error)
        {
            return false;
        }
    }

    return true;    
}

// returns member loc
u32 add_member(Interloper& itl,Struct& structure,DeclNode* m, u32* size_count,b32 forced_first, u32 flags)
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
        member.type = get_type(itl,type_decl,type_idx_override,true);

        if(itl.error)
        {
            destroy_struct(structure);
            return 0;            
        }
    }

    else if(!type_exists(itl,type_decl->name))
    {
        if(!handle_recursive_type(itl,structure.name,type_decl,&type_idx_override))
        {
            destroy_struct(structure);
            return 0;
        }

        member.type = get_type(itl,type_decl,type_idx_override,true);
    }

    else
    {
        member.type = get_type(itl,type_decl,type_idx_override,true);
    }


    // TODO: ensure array type cant use a deduced type size


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
        panic(itl,itl_error::redeclaration,"%s : member %s redeclared\n",structure.name.buf,member.name.buf);
        destroy_struct(structure);
        return 0;
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

void parse_struct_def(Interloper& itl, TypeDef& def)
{
    StructNode* node = (StructNode*)def.root;

    // NOTE: we expect the caller to save this
    trash_context(itl,node->filename,def.decl.name_space,def.root);

    Struct structure;
    
    // allocate a reserved slot for the struct
    def.decl.type_idx = count(itl.struct_table);
    resize(itl.struct_table,count(itl.struct_table) + 1);


    structure.name = node->name;
    structure.member_map = make_table<String,u32>();

    // we want to get how many sizes of each we have
    // and then we can go back through and align the struct with them
    u32 size_count[4] = {0};

    s32 forced_first_loc = -1;

    const u32 flags = node->attr_flags;

    // force this to be at the first location in mem
    if(node->forced_first)
    {
        forced_first_loc = add_member(itl,structure,node->forced_first,size_count,true,flags);
    }

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        if(itl.error)
        {
            return;
        }

        add_member(itl,structure,node->members[i],size_count,false,flags);
    }


    finalise_member_offsets(itl,structure,size_count,forced_first_loc,flags);
    

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }


    add_struct(itl,structure,def.decl);
}


Type* access_array_member(Interloper& itl, Type* type, const String& member_name,AddrSlot* struct_slot)
{
    ArrayType* array_type = (ArrayType*)type;

    if(member_name == "len")
    {
        if(!is_runtime_size(type))
        {
            struct_slot->slot = ACCESS_FIXED_LEN_REG_SLOT;
            struct_slot->struct_addr = false;
            return type;
        }

        // vla
        else
        {
            struct_slot->offset += GPR_SIZE;
            return make_builtin(itl,GPR_SIZE_TYPE);
        }
    }

    else if(member_name == "data")
    {
        // fixed sized array is not a struct dont allow access
        if(is_fixed_array(type))
        {
            panic(itl,itl_error::array_type_error,"no .data member on fixed size array");
            make_builtin(itl,builtin_type::void_t);
        }

        struct_slot->offset += 0;

        return make_pointer(itl,array_type->contained_type);
    }


    else
    {
        panic(itl,itl_error::undeclared,"unknown array member %s\n",member_name.buf);
        return make_builtin(itl,builtin_type::void_t);
    }
}

Type* access_struct_member(Interloper& itl, Type* type, const String& member_name, AddrSlot* struct_slot)
{
    // get offset for struct member
    const auto member_opt = get_member(itl.struct_table,type,member_name);

    if(!member_opt)
    {
        panic(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,type).buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    const auto member = member_opt.value();

    struct_slot->offset += member.offset;  

    return member.type;
}


std::optional<u32> member_offset(Struct& structure, const String& name)
{
    auto member_opt = get_member(structure,name);
    if(!member_opt)
    {
        return std::nullopt;
    }

    auto member = member_opt.value();

    return std::optional{member.offset};
}

Type* access_enum_struct_member(Interloper& itl,Function& func,Type* struct_type,
    const String& member_name, AddrSlot* struct_slot)
{
    const auto& enumeration = enum_from_type(itl.enum_table,struct_type);

    if(enumeration.kind != enum_type::struct_t)
    {
        panic(itl,itl_error::struct_error,"member access on plain enum %s\n",enumeration.name.buf);
        return make_builtin(itl,builtin_type::void_t);                    
    }

    // pull info on enum struct member
    auto& enum_struct = itl.struct_table[enumeration.underlying_type_idx];

    const auto enum_struct_member_opt = get_member(enum_struct, member_name);

    if(!enum_struct_member_opt)
    {
        panic(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,struct_type).buf);
        return make_builtin(itl,builtin_type::void_t);                
    }

    const auto& enum_struct_member = enum_struct_member_opt.value();

    // get the start of the table
    const auto enum_table_slot = pool_addr_res(itl,func,enumeration.struct_slot,0);

    // get the enum index
    SymSlot enum_slot = sym_from_idx(SYMBOL_NO_SLOT);
    
    // we allready directly have the enum
    if(struct_slot->struct_addr)
    {
        assert(struct_slot->offset == 0);

        enum_slot = struct_slot->slot;
    }

    // ordinary access on a pointer, we must deref it
    else
    {
        enum_slot = new_tmp(func,GPR_SIZE);
        load_ptr(itl,func,enum_slot,struct_slot->slot,struct_slot->offset,ENUM_SIZE,false,false);
    }

    // update for new offset
    struct_slot->offset = enum_struct_member.offset;
    struct_slot->struct_addr = false;

    // finally index the table
    
    // scale index
    const SymSlot table_offset = mul_imm_res(itl,func,enum_slot,enum_struct.size);

    // compute final addr
    const auto addr_slot = add_res(itl,func,enum_table_slot,table_offset);

    // update the struct addr
    struct_slot->slot = addr_slot;

    return enum_struct_member.type;
}


// return type, slot, offset
std::tuple<Type*,AddrSlot> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
{
    BinNode* member_root =(BinNode*)node;

    AstNode* expr_node = member_root->left;

    // Type is allways the accessed type of the current pointer
    Type* struct_type = nullptr;

    AddrSlot struct_slot;

    // parse out initail expr
    switch(expr_node->type)
    {
        case ast_type::symbol:
        {
            LiteralNode* sym_node = (LiteralNode*)expr_node;

            const auto name = sym_node->literal;
            const auto sym_ptr = get_sym(itl.symbol_table,name);

            if(!sym_ptr)
            {
                panic(itl,itl_error::undeclared,"symbol %s used before declaration\n",name.buf);
                return std::tuple{make_builtin(itl,builtin_type::void_t),struct_slot};
            }            

            const auto &sym = *sym_ptr;

            // allready a pointer so just return the slot
            // along with the derefed type
            if(is_pointer(sym.type))
            {
                struct_type = deref_pointer(sym.type);
                struct_slot = make_addr(sym.reg.slot,0);
            }

            else
            {
                // if base type is a fixed array
                // then we just directly return operations
                if(is_fixed_array(sym.type))
                {
                    struct_slot = make_addr(sym.reg.slot,0);
                }

                // if this is an enum we will do a direct index with it
                else if(is_enum(sym.type))
                {
                    struct_slot = make_struct_addr(sym.reg.slot,0);
                }

                else
                {
                    struct_slot = make_struct_addr(sym.reg.slot,0);
                }

                struct_type = sym.type;
            }

            break;        
        }

        case ast_type::index:
        {
            SymSlot addr_slot;
            std::tie(struct_type, addr_slot) = index_arr(itl,func,expr_node,new_tmp_ptr(func));

            struct_slot = make_addr(addr_slot,0);

            // we return types in here as the accessed type
            struct_type = deref_pointer(struct_type);
            break;
        }


        default: 
        {
            panic(itl,itl_error::struct_error,"Unknown struct access %s\n",AST_NAMES[u32(expr_node->type)]);
            return std::tuple{make_builtin(itl,builtin_type::void_t),struct_slot};
        }
    }


    RecordNode* members = (RecordNode*)member_root->right;

    // perform each member access
    for(u32 m = 0; m < count(members->nodes); m++)
    {
        AstNode *n = members->nodes[m];

        switch(n->type)
        {
            case ast_type::access_member:
            {
                LiteralNode* member_node = (LiteralNode*)n;
                const auto member_name = member_node->literal;

                // auto deferef pointers first
                if(is_pointer(struct_type))
                {
                    SymSlot addr_slot = new_tmp_ptr(func);
                    do_addr_load(itl,func,addr_slot,struct_slot,struct_type);

                    struct_slot = make_addr(addr_slot,0);

                    // now we are back to a straight pointer
                    struct_type = deref_pointer(struct_type);
                }



                if(is_array(struct_type))
                {
                    struct_type = access_array_member(itl,struct_type,member_name,&struct_slot);
                }

                // do enum member access
                else if(is_enum(struct_type))
                {
                    struct_type = access_enum_struct_member(itl,func,struct_type,member_name,&struct_slot);

                    if(itl.error)
                    {
                        return std::tuple{make_builtin(itl,builtin_type::void_t),struct_slot}; 
                    }
                }

                // actual struct member
                else
                {
                    struct_type = access_struct_member(itl,struct_type,member_name,&struct_slot);
                }   
                break;
            }

            case ast_type::index:
            {
                IndexNode* index_node = (IndexNode*)n;

                struct_type = access_struct_member(itl,struct_type,index_node->name,&struct_slot);
                
                if(is_runtime_size(struct_type))
                {
                    const SymSlot vla_ptr = new_tmp_ptr(func);
                    // TODO: This can be better typed to a pointer
                    do_addr_load(itl,func,vla_ptr,struct_slot,make_builtin(itl,GPR_SIZE_TYPE));
                    struct_slot = make_addr(vla_ptr,0);
                }

                // fixed size collpase the offset
                else
                {
                    collapse_struct_offset(itl,func,&struct_slot);
                }

                SymSlot addr_slot;
                std::tie(struct_type,addr_slot) = index_arr_internal(itl,func,index_node,index_node->name,struct_type,struct_slot.slot,new_tmp_ptr(func));

                struct_slot = make_addr(addr_slot,0);

                // deref of pointer
                struct_type = deref_pointer(struct_type);
                break;
            }

            default: 
            {
                panic(itl,itl_error::undeclared,"Unknown member access %s\n",AST_NAMES[u32(n->type)]);
                return std::tuple{make_builtin(itl,builtin_type::void_t),struct_slot};
            }
        }
    }

    return std::tuple{struct_type,struct_slot};
}

std::pair<Type*,SymSlot> compute_member_ptr(Interloper& itl, Function& func, AstNode* node)
{
    auto [type,addr_slot] = compute_member_addr(itl,func,node);

    collapse_struct_offset(itl,func,&addr_slot);

    return std::pair{make_pointer(itl,type),addr_slot.slot};
}

void write_struct(Interloper& itl,Function& func, SymSlot src_slot, Type* rtype, AstNode *node)
{
    const auto [accessed_type, addr_slot] = compute_member_addr(itl,func,node);

    if(itl.error)
    {
        return;
    }

    check_assign(itl,accessed_type,rtype);
    do_addr_store(itl,func,src_slot,addr_slot,accessed_type);
}


Type* read_struct(Interloper& itl,Function& func, SymSlot dst_slot, AstNode *node)
{
    const List list_old = get_cur_list(func.emitter);

    auto [accessed_type, addr_slot] = compute_member_addr(itl,func,node);

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }

    // len access on fixed sized array
    if(addr_slot.slot.handle == ACCESS_FIXED_LEN_REG)
    {
        // dont need any of the new instrs for this
        // get rid of them
        get_cur_list(func.emitter) = list_old;

        const ArrayType* array_type = (ArrayType*)accessed_type;

        mov_imm(itl,func,dst_slot,array_type->size);
        return make_builtin(itl,builtin_type::u32_t);
    }

    // let caller handle reads via array accessors
    if(is_fixed_array(accessed_type))
    {
        collapse_struct_offset(itl,func,&addr_slot);
        mov_reg(itl,func,dst_slot,addr_slot.slot);
        return accessed_type;
    }

    do_addr_load(itl,func,dst_slot,addr_slot,accessed_type);
    return accessed_type;
}


void traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, AddrSlot addr_slot, const Struct& structure)
{
    const u32 node_len = count(node->nodes);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        panic(itl,itl_error::undeclared,"struct initlizier missing initlizer expected %d got %d\n",member_size,node_len);
        return;
    }
    
    for(u32 i = 0; i < count(structure.members); i++)
    {
        const auto member = structure.members[i];
    
        // generate a new offset
        // NOTE: make sure this is a copy
        auto addr_member = addr_slot;
        addr_member.offset += member.offset;

        // either sub struct OR array member initializer
        if(node->nodes[i]->type == ast_type::initializer_list)
        {
            if(is_array(member.type))
            {
                traverse_arr_initializer_internal(itl,func,(RecordNode*)node->nodes[i],&addr_member,(ArrayType*)member.type);
            }

            else if(is_struct(member.type))
            {
                const Struct& sub_struct = struct_from_type(itl.struct_table,member.type);
                traverse_struct_initializer(itl,func,(RecordNode*)node->nodes[i],addr_member,sub_struct);
            }

            else
            {
                panic(itl,itl_error::struct_error,"nested struct initalizer for basic type %s : %s\n",member.name.buf,type_name(itl,member.type).buf);
                return;
            }
        }

        // we have a list of plain values we can actually initialize
        else
        {
            // get the operand and type check it
            const auto [rtype,slot] = compile_oper(itl,func,node->nodes[i]);
            check_assign(itl,member.type,rtype);

            do_addr_store(itl,func,slot,addr_member,member.type);
        }
    } 
}

void compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot)
{
    // TODO: add a opt to just memset the entire thing in one go
    // NOTE: this should apply all the way down i.e if we contain a struct
    // it needs to have initialzer_zero aswell
    // if(structure.initializer_zero)

    // default construction
    for(u32 m = 0; m < count(structure.members); m++)
    {
        const auto& member = structure.members[m];

        auto member_addr = addr_slot;
        member_addr.offset += member.offset;

        if(member.expr)
        {
            switch(member.expr->type)
            {
                case ast_type::initializer_list:
                {
                    compile_init_list(itl,func,member.type,member_addr,member.expr);
                    break;
                }

                // dont default init
                case ast_type::no_init:
                {
                    break;
                }

                default: 
                {
                    const auto [rtype,slot] = compile_oper(itl,func,member.expr);
                    check_assign_init(itl,member.type,rtype); 

                    do_addr_store(itl,func,slot,member_addr,member.type);
                    break;                    
                }
            }
        }

        // (basically we need to just recurse this method)
        else if(is_struct(member.type))
        {
            const auto structure = struct_from_type(itl.struct_table,member.type);
            compile_struct_decl_default(itl,func,structure,member_addr);
        }

        else if(is_array(member.type))
        {
            default_construct_arr(itl,func,(ArrayType*)member.type,member_addr);
        }

        else
        {
            const SymSlot tmp = imm_zero(itl,func);
            do_addr_store(itl,func,tmp,member_addr,member.type);
        }
    }
}

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode *decl_node, SymSlot slot)
{
    Type* ltype = nullptr;

    // isolate our symbol as it may move
    {
        auto& sym = sym_from_slot(itl.symbol_table,slot);
        alloc_slot(itl,func,slot,true);

        ltype = sym.type;
    }

    const auto structure = struct_from_type(itl.struct_table,ltype);

    
    if(decl_node->expr)
    {
        switch(decl_node->expr->type)
        {
            case ast_type::initializer_list:
            {
                const auto addr_slot = make_struct_addr(slot,0);
                traverse_struct_initializer(itl,func,(RecordNode*)decl_node->expr,addr_slot,structure);
                break;                
            }

            case ast_type::no_init:
            {
                break;
            }

            default:
            {
                const auto rtype = compile_expression(itl,func,decl_node->expr,slot);
                check_assign_init(itl,ltype,rtype);
                break;    
            }
        }
    }

    // default init
    else
    {
        const AddrSlot addr_slot = make_struct_addr(slot,0);
        compile_struct_decl_default(itl,func,structure,addr_slot);
    }
}
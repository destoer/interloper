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

void add_struct(Interloper& itl, Struct& structure, u32 slot)
{
    structure.type_idx = slot;
    itl.struct_table[slot] = structure;
    
    add_type_decl(itl,slot,structure.name,type_kind::struct_t);
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


Struct struct_from_type(StructTable& struct_table, const Type* type)
{
    StructType* struct_type = (StructType*)type;

    return struct_table[struct_type->struct_idx];
}   



std::optional<Member> get_member(StructTable& struct_table, const Type* type, const String& member_name)
{
    if(!is_struct(type))
    {
        return std::nullopt;
    }

    auto structure = struct_from_type(struct_table,type);

    const u32* idx = lookup(structure.member_map,member_name);

    if(!idx)
    {
        return std::nullopt;
    }

    const auto member = structure.members[*idx];
    return std::optional<Member>(member);
}


void parse_struct_decl(Interloper& itl, TypeDef& def);

void parse_struct_def(Interloper& itl, TypeDef& def)
{
    // mark struct as being parsed so we can check for recursion
    def.state = def_state::checking;
    parse_struct_decl(itl,def);

    // mark as checked so that we know we dont have to recheck the decl
    def.state = def_state::checked;
}


void add_member(Interloper& itl,Struct& structure,DeclNode* m, u32* size_count, const String& filename)
{
    Member member;
    member.name = m->name;

    TypeNode* type_decl = m->type;

    // copy the init expr
    member.expr = m->expr;


    u32 type_idx_override = INVALID_TYPE;

    // member is struct that has not had its defintion parsed yet
    if(!type_exists(itl,type_decl->name))
    {
        TypeDef *def_ptr = lookup(itl.type_def,type_decl->name);

        // no such definiton exists
        if(!def_ptr)
        {
            panic(itl,"%s : member type %s is not defined\n",structure.name.buf,type_decl->name.buf);
            destroy_struct(structure);
            return;
        }

        TypeDef& def = *def_ptr;

        // if we attempt to check a partial defintion twice that the definition is recursive
        if(def.state == def_state::checking)
        {
            // if its a pointer we dont need the complete inormation yet as they are all alike
            // so just override the type idx from the one reserved inside the def
            if(def_has_indirection(type_decl))
            {
                type_idx_override = def.slot;
            }

            else
            {
                // panic to prevent having our struct collpase into a black hole
                panic(itl,"%s : is recursively defined via %s\n",structure.name.buf,type_decl->name.buf);
                destroy_struct(structure);
                return;
            }
        }

        else
        {
            parse_def(itl,def);

            if(itl.error)
            {
                destroy_struct(structure);
                return;
            }
        }
    }

    itl.cur_file = filename;

    member.type = get_type(itl,type_decl,type_idx_override,true);

    // TODO: ensure array type cant use a deduced type size


    u32 size;

    if(is_fixed_array(member.type))
    {
        size = arr_size(member.type);
    }

    else
    {
        size = type_size(itl,member.type);
    }

    // TODO: handle fixed sized arrays

    // translate larger items, into several allocations on the final section
    if(size > GPR_SIZE)
    {
        member.offset = size_count[GPR_SIZE >> 1];

        size_count[GPR_SIZE >> 1] += gpr_count(size);
    }

    else
    {
        // cache the offset into its section
        member.offset = size_count[size >> 1];

        size_count[size >> 1] += 1;
    }

    const u32 loc = count(structure.members);


    if(contains(structure.member_map,member.name))
    {
        panic(itl,"%s : member %s redeclared\n",structure.name.buf,member.name.buf);
        destroy_struct(structure);
        return;
    }

    add(structure.member_map,member.name,loc);
    push_var(structure.members,member);    
}

void finalise_member_offsets(Interloper& itl, Struct& structure, u32* size_count)
{
    // TODO: handle not reordering the struct upon request

    // handle alginment & get starting zonnes + total size
    u32 alloc_start[3];

    // bytes just start at offset zero (and being bytes dont need aligment)
    alloc_start[0] = 0;

    // get u16 start pos and align it on its own boudary
    alloc_start[1] = size_count[0] * sizeof(u8);
    align(alloc_start,sizeof(u16));

    // get u32 start pos and align it on its own boudary
    alloc_start[2] = alloc_start[1] + (size_count[1] * sizeof(u16));
    align(alloc_start,sizeof(u32));


    // iter back over every member and give its offset
    for(u32 m = 0; m < count(structure.members); m++)
    {
        auto& member = structure.members[m];

        const u32 zone_offset = member.offset;

        u32 size = type_size(itl,member.type);
        size = size > GPR_SIZE? GPR_SIZE : size;

        member.offset = alloc_start[size >> 1] + (zone_offset * size);
    }

    // get the total structure size
    structure.size = alloc_start[2] + (size_count[2] * sizeof(u32));
}

void parse_struct_decl(Interloper& itl, TypeDef& def)
{
    StructNode* node = (StructNode*)def.root;

    TypeDecl* user_type = lookup(itl.type_table,node->name);
    if(user_type)
    {
        panic(itl,"%s %s redeclared as struct\n",KIND_NAMES[u32(user_type->kind)],node->name.buf);
        return;
    }

    Struct structure;
    
    // allocate a reserved slot for the struct
    const u32 slot = count(itl.struct_table);
    def.slot = slot;

    resize(itl.struct_table,count(itl.struct_table) + 1);


    structure.name = node->name;
    structure.member_map = make_table<String,u32>();

    // we want to get how many sizes of each we have
    // and then we can go back through and align the struct with them

    u32 size_count[3] = {0};

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        add_member(itl,structure,node->members[i],size_count,node->filename);
    }


    finalise_member_offsets(itl,structure,size_count);

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }


    add_struct(itl,structure,def.slot);

}

void parse_struct_declarations(Interloper& itl)
{
    auto &struct_def = itl.type_def;

    for(u32 b = 0; b < count(struct_def.buf); b++)
    {
        auto& bucket = struct_def.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto &def = bucket[i].v;

            if(def.state == def_state::not_checked)
            {
                parse_def(itl,def);
            }
        }
    }
}










std::pair<Type*,SymSlot> access_array_member(Interloper& itl, Function& func, SymSlot slot, Type* type, const String& member_name,u32* offset)
{
    UNUSED(func);

    const bool is_ptr = is_pointer(type);

    if(member_name == "len")
    {
        if(!is_runtime_size(type))
        {
            return std::pair{type,ACCESS_FIXED_LEN_REG_SLOT};
        }

        // vla
        else
        {
            if(!is_ptr)
            {
                *offset += GPR_SIZE;
                return std::pair{make_builtin(itl,builtin_type::u32_t),slot};
            }

            else
            {
                unimplemented("vla len by ptr");
            }
        }
    }

    else if(member_name == "data")
    {
        if(!is_ptr)
        {
            // this should probably be better typed
            return std::pair{make_builtin(itl,GPR_SIZE_TYPE),slot};
        }

        else
        {
            unimplemented("data by ptr");
        }
    }


    else
    {
        panic(itl,"unknown array member %s\n",member_name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }
}

// returns the member + offset
std::pair<Type*,SymSlot> access_struct_member(Interloper& itl, Function& func, SymSlot slot, Type* type, const String& member_name, u32* offset)
{
    // auto deref pointer
    if(is_pointer(type))
    {
        const SymSlot ptr_slot = slot;
        slot = new_tmp(func,GPR_SIZE);

        do_ptr_load(itl,func,slot,ptr_slot,type,*offset);
        *offset = 0;

        // now we are back to a straight pointer
        type = deref_pointer(type);
    }

    // get offset for struct member
    const auto member_opt = get_member(itl.struct_table,type,member_name);

    if(!member_opt)
    {
        panic(itl,"No such member %s for type %s\n",member_name.buf,type_name(itl,type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }

    const auto member = member_opt.value();

    *offset += member.offset;

    return std::pair{member.type,slot};    
}


// return type, slot, offset
std::tuple<Type*,SymSlot,u32> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
{
    BinNode* member_root =(BinNode*)node;

    AstNode* expr_node = member_root->left;

    // Type is allways the accessed type of the current pointer
    SymSlot struct_slot = sym_from_idx(SYMBOL_NO_SLOT);
    Type* struct_type = nullptr;

    // parse out initail expr
    switch(expr_node->type)
    {
        case ast_type::symbol:
        {
            LiteralNode* sym_node = (LiteralNode*)expr_node;

            const auto name = sym_node->literal;
            const auto sym_opt = get_sym(itl.symbol_table,name);

            if(!sym_opt)
            {
                panic(itl,"symbol %s used before declaration\n",name.buf);
                return std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0};
            }            

            const auto sym = sym_opt.value();

            // allready a pointer so just return the slot
            // along with the derefed type
            if(is_pointer(sym.type))
            {
                struct_type = deref_pointer(sym.type);
                struct_slot = sym.reg.slot;
            }

            else
            {
                struct_slot = addrof_res(itl.symbol_table,func,sym.reg.slot);
                struct_type = sym.type;
            }

            break;        
        }

        case ast_type::index:
        {
            std::tie(struct_type, struct_slot) = index_arr(itl,func,expr_node,new_tmp_ptr(func));

            // we return types in here as the accessed type
            struct_type = deref_pointer(struct_type);
            break;
        }


        default: 
        {
            panic(itl,"Unknown struct access %s\n",AST_NAMES[u32(expr_node->type)]);
            return std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0};
        }
    }


    RecordNode* members = (RecordNode*)member_root->right;

    u32 member_offset = 0;

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

                if(is_pointer(struct_type))
                {
                    PointerType* pointer_type = (PointerType*)struct_type;

                    // pointer to array
                    if(is_array(pointer_type->contained_type))
                    {
                       std::tie(struct_type,struct_slot) = access_array_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                    }

                    else
                    {
                        std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                    }
                    
                }

                else if(is_array(struct_type))
                {
                    std::tie(struct_type,struct_slot) = access_array_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                }

                // actual struct member
                else
                {
                    std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                }   
                break;
            }

            case ast_type::index:
            {
                IndexNode* index_node = (IndexNode*)n;

                std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,index_node->name,&member_offset);
                
                struct_slot = collapse_offset(func,struct_slot,&member_offset);

                if(is_runtime_size(struct_type))
                {
                    const SymSlot vla_ptr = new_tmp_ptr(func);
                    // TODO: This can be better typed to a pointer
                    do_ptr_load(itl,func,vla_ptr,struct_slot,make_builtin(itl,builtin_type::u32_t),0);
                    struct_slot = vla_ptr;
                }

                std::tie(struct_type,struct_slot) = index_arr_internal(itl,func,index_node,index_node->name,struct_type,struct_slot,new_tmp_ptr(func));

                // deref of pointer
                struct_type = deref_pointer(struct_type);
                break;
            }

            default: 
            {
                panic(itl,"Unknown member access %s\n",AST_NAMES[u32(n->type)]);
                return std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0};
            }
        }
    }

    return std::tuple{struct_type,struct_slot,member_offset};
}


void do_ptr_store(Interloper &itl,Function &func,SymSlot dst_slot,SymSlot addr_slot, const Type* type, u32 offset = 0);
void do_ptr_load(Interloper &itl,Function &func,SymSlot dst_slot,SymSlot addr_slot, const Type* type, u32 offset = 0);

void write_struct(Interloper& itl,Function& func, SymSlot src_slot, Type* rtype, AstNode *node)
{
    const auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);
    check_assign(itl,accessed_type,rtype);
    do_ptr_store(itl,func,src_slot,ptr_slot,accessed_type, offset);
}


Type* read_struct(Interloper& itl,Function& func, SymSlot dst_slot, AstNode *node)
{
    const auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);

    // len access on fixed sized array
    if(ptr_slot.handle == ACCESS_FIXED_LEN_REG)
    {
        const ArrayType* array_type = (ArrayType*)accessed_type;

        emit(func,op_type::mov_imm,dst_slot,array_type->size);
        return make_builtin(itl,builtin_type::u32_t);
    }

    do_ptr_load(itl,func,dst_slot,ptr_slot,accessed_type,offset);
    return accessed_type;
}


void traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, const SymSlot addr_slot, const Struct& structure, u32 offset = 0)
{
    const u32 node_len = count(node->nodes);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        panic(itl,"arr initlizier missing initlizer expected %d got %d\n",member_size,node_len);
        return;
    }
    
    for(u32 i = 0; i < count(structure.members); i++)
    {
        const auto member = structure.members[i];
    
        // either sub struct OR array member initializer
        if(node->nodes[i]->type == ast_type::initializer_list)
        {
            if(is_array(member.type))
            {
               u32 arr_offset = offset + member.offset;
               auto type = member.type;

               traverse_arr_initializer_internal(itl,func,(RecordNode*)node->nodes[i],addr_slot,(ArrayType*)type,&arr_offset);
            }

            else if(is_struct(member.type))
            {
                const Struct& sub_struct = struct_from_type(itl.struct_table,member.type);
                traverse_struct_initializer(itl,func,(RecordNode*)node->nodes[i],addr_slot,sub_struct,offset + member.offset);
            }

            else
            {
                panic(itl,"nested struct initalizer for basic type %s : %s\n",member.name.buf,type_name(itl,member.type).buf);
                return;
            }
        }

        // we have a list of plain values we can actually initialize
        else
        {
            // get the operand and type check it
            const auto [rtype,slot] = compile_oper(itl,func,node->nodes[i]);
            check_assign(itl,member.type,rtype);

            do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset + offset);
        }
    } 
}

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode *decl_node, Symbol& sym)
{
    const auto structure = struct_from_type(itl.struct_table,sym.type);

    alloc_slot(func,sym.reg,true);

    if(decl_node->expr)
    {
        if(decl_node->expr->type == ast_type::initializer_list)
        {
            const SymSlot addr_slot = addrof_res(itl.symbol_table,func,sym.reg.slot);

            traverse_struct_initializer(itl,func,(RecordNode*)decl_node->expr,addr_slot,structure);
        }

        else
        {
            const auto rtype = compile_expression(itl,func,decl_node->expr,sym.reg.slot);
            check_assign(itl,sym.type,rtype,false,true);        
        }
    }

    // default construction
    else
    {
        const SymSlot addr_slot = addrof_res(itl.symbol_table,func,sym.reg.slot);

        for(u32 m = 0; m < count(structure.members); m++)
        {
            const auto& member = structure.members[m];

            if(member.expr)
            {
                if(member.expr->type == ast_type::initializer_list)
                {
                    unimplemented("initializer list");
                }

                else
                {
                    const auto [rtype,slot] = compile_oper(itl,func,member.expr);
                    check_assign(itl,member.type,rtype,false,true); 

                    do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset);
                }
            }

            // TODO: handle nested struct membmer
            // (basically we need to just recurse this method)
            else if(is_struct(member.type))
            {

            }

            // TODO: handle arrays
            else if(is_array(member.type))
            {

            }

            else
            {
                const SymSlot tmp = mov_imm(func,default_value(member.type));

                do_ptr_store(itl,func,tmp,addr_slot,member.type,member.offset);
            }
        }
    }

    if(itl.error)
    {
        return;
    }
}
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
    // member is struct that has not had its defintion parsed yet
    TypeDef *def_ptr = lookup(itl.type_def,type_decl->name);

    // no such definiton exists
    if(!def_ptr)
    {
        panic(itl,itl_error::undeclared,"%s : member type %s is not defined\n",struct_name.buf,type_decl->name.buf);
        return false;
    }

    TypeDef& def = *def_ptr;

    // if we attempt to check a partial defintion twice that the definition is recursive
    if(def.state == def_state::checking)
    {
        // if its a pointer we dont need the complete inormation yet as they are all alike
        // so just override the type idx from the one reserved inside the def
        if(def_has_indirection(type_decl))
        {
            *type_idx_override = def.slot;
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
u32 add_member(Interloper& itl,Struct& structure,DeclNode* m, u32* size_count, const String& filename, b32 forced_first)
{
    Member member;
    member.name = m->name;

    TypeNode* type_decl = m->type;

    // copy the init expr
    member.expr = m->expr;


    u32 type_idx_override = INVALID_TYPE;

    itl.cur_file = filename;

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


    if(forced_first)
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

void finalise_member_offsets(Interloper& itl, Struct& structure, u32* size_count, s32 forced_first)
{
    // TODO: handle not reordering the struct upon request

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



    // iter back over every member and give its offset
    for(u32 m = 0; m < count(structure.members); m++)
    {
        auto& member = structure.members[m];

        const u32 zone_offset = member.offset;
        const auto [size,count] = compute_member_size(itl,member.type);

        if(member.offset == OFFSET_FORCED_FIRST)
        {
            member.offset = 0;
        }

        else 
        {
            member.offset = alloc_start[log2(size)] + (zone_offset * size);
        }
    }
}

void parse_struct_def(Interloper& itl, TypeDef& def)
{
    StructNode* node = (StructNode*)def.root;

    TypeDecl* user_type = lookup(itl.type_table,node->name);
    if(user_type)
    {
        panic(itl,itl_error::redeclaration,"%s %s redeclared as struct\n",KIND_NAMES[u32(user_type->kind)],node->name.buf);
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
    u32 size_count[4] = {0};

    s32 forced_first_loc = -1;

    // force this to be at the first location in mem
    if(node->forced_first)
    {
        forced_first_loc = add_member(itl,structure,node->forced_first,size_count,node->filename,true);
    }

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        if(itl.error)
        {
            return;
        }

        add_member(itl,structure,node->members[i],size_count,node->filename,false);
    }

    finalise_member_offsets(itl,structure,size_count,forced_first_loc);

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }


    add_struct(itl,structure,def.slot);

}


std::pair<Type*,SymSlot> access_array_member(Interloper& itl, Function& func, SymSlot slot, Type* type, const String& member_name,u32* offset)
{
    UNUSED(func);

    const bool is_ptr = is_pointer(type);

    ArrayType* array_type = (ArrayType*)type;

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
                return std::pair{make_builtin(itl,builtin_type::u64_t),slot};
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
            // fixed sized array is not a struct dont allow access
            if(is_fixed_array(type))
            {
                panic(itl,itl_error::array_type_error,"no .data member on fixed size array");
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
            }

            *offset += 0;

            return std::pair{make_pointer(itl,array_type->contained_type),slot};
        }

        else
        {
            unimplemented("data by ptr");
        }
    }


    else
    {
        panic(itl,itl_error::undeclared,"unknown array member %s\n",member_name.buf);
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
        panic(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }

    const auto member = member_opt.value();

    *offset += member.offset;

    return std::pair{member.type,slot};    
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

std::pair<Type*, SymSlot> access_enum_struct_member(Interloper& itl,Function& func, SymSlot struct_slot,Type* struct_type,b32 enum_base_access,
    const String& member_name, u32* member_offset)
{
    const auto& enumeration = enum_from_type(itl.enum_table,struct_type);

    if(enumeration.struct_idx == INVALID_TYPE_IDX)
    {
        panic(itl,itl_error::struct_error,"member access on plain enum %s\n",enumeration.name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};                      
    }

    // pull info on enum struct member
    auto& enum_struct = itl.struct_table[enumeration.struct_idx];

    const auto enum_struct_member_opt = get_member(enum_struct, member_name);

    if(!enum_struct_member_opt)
    {
        panic(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,struct_type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};                
    }

    const auto& enum_struct_member = enum_struct_member_opt.value();

    // get the start of the table
    const auto enum_table_slot = pool_addr_res(itl,func,enumeration.struct_slot,0);

    // get the enum index
    SymSlot enum_slot = sym_from_idx(SYMBOL_NO_SLOT);
    
    // we allready directly have the enum
    if(enum_base_access)
    {
        enum_slot = struct_slot;
    }

    // ordinary access on a pointer, we must deref it
    else
    {
        enum_slot = new_tmp(func,GPR_SIZE);
        load_ptr(itl,func,enum_slot,struct_slot,*member_offset,ENUM_SIZE,false);
    }

    // update for new offset
    *member_offset = enum_struct_member.offset;

    // finally index the table
    
    // scale index
    const SymSlot table_offset = mul_imm_res(itl,func,enum_slot,enum_struct.size);

    // compute final addr
    struct_slot = add_res(itl,func,enum_table_slot,table_offset);

    return std::pair{enum_struct_member.type,struct_slot};
}

// return type, slot, offset
std::tuple<Type*,SymSlot,u32> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
{
    BinNode* member_root =(BinNode*)node;

    AstNode* expr_node = member_root->left;

    // Type is allways the accessed type of the current pointer
    SymSlot struct_slot = sym_from_idx(SYMBOL_NO_SLOT);
    Type* struct_type = nullptr;

    // this indicates a member access on a enum that we directly have access to
    // (i.e we dont waste time taking a pointer to it)
    b32 enum_base_access = false;

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
                return std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0};
            }            

            const auto &sym = *sym_ptr;

            // allready a pointer so just return the slot
            // along with the derefed type
            if(is_pointer(sym.type))
            {
                struct_type = deref_pointer(sym.type);
                struct_slot = sym.reg.slot;
            }

            else
            {
                // if base type is a fixed array
                // then we just directly return operations
                if(is_fixed_array(sym.type))
                {
                    struct_slot = sym.reg.slot;
                }

                // if this is an enum we will do a direct index with it
                else if(is_enum(sym.type))
                {
                    struct_slot = sym.reg.slot;
                    enum_base_access = true;
                }

                else
                {
                    struct_slot = addrof_res(itl,func,sym.reg.slot);
                }

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
            panic(itl,itl_error::struct_error,"Unknown struct access %s\n",AST_NAMES[u32(expr_node->type)]);
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

                // TODO: can we simpllify this by just doing a pointer deref at the top level?
                // and repeating the switch below
                if(is_pointer(struct_type))
                {
                    PointerType* pointer_type = (PointerType*)struct_type;

                    // pointer to array
                    if(is_array(pointer_type->contained_type))
                    {
                       std::tie(struct_type,struct_slot) = access_array_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                    }

                    else if(is_enum(pointer_type->contained_type))
                    {
                        unimplemented("struct member access by enum");
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

                // do enum member access
                else if(is_enum(struct_type))
                {
                    std::tie(struct_type,struct_slot) = access_enum_struct_member(itl,func,struct_slot,struct_type,enum_base_access,member_name,&member_offset);
                    enum_base_access = false;
                    if(itl.error)
                    {
                        std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0}; 
                    }
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
                
                struct_slot = collapse_offset(itl,func,struct_slot,&member_offset);

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
                panic(itl,itl_error::undeclared,"Unknown member access %s\n",AST_NAMES[u32(n->type)]);
                return std::tuple{make_builtin(itl,builtin_type::void_t),SYM_ERROR,0};
            }
        }
    }

    return std::tuple{struct_type,struct_slot,member_offset};
}


void write_struct(Interloper& itl,Function& func, SymSlot src_slot, Type* rtype, AstNode *node)
{
    const auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);

    if(itl.error)
    {
        return;
    }

    check_assign(itl,accessed_type,rtype);
    do_ptr_store(itl,func,src_slot,ptr_slot,accessed_type, offset);
}


Type* read_struct(Interloper& itl,Function& func, SymSlot dst_slot, AstNode *node)
{
    auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }

    // len access on fixed sized array
    if(ptr_slot.handle == ACCESS_FIXED_LEN_REG)
    {
        const ArrayType* array_type = (ArrayType*)accessed_type;

        mov_imm(itl,func,dst_slot,array_type->size);
        return make_builtin(itl,builtin_type::u32_t);
    }

    // let caller handle reads via array accessors
    if(is_fixed_array(accessed_type))
    {
        const SymSlot addr = collapse_offset(itl,func,ptr_slot,&offset);
        mov_reg(itl,func,dst_slot,addr);
        return accessed_type;
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
        panic(itl,itl_error::undeclared,"struct initlizier missing initlizer expected %d got %d\n",member_size,node_len);
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

            do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset + offset);
        }
    } 
}

void compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,SymSlot addr_slot, u32 offset)
{
    // TODO: add a opt to just memset the entire thing in one go
    // NOTE: this should apply all the way down i.e if we contain a struct
    // it needs to have initialzer_zero aswell
    // if(structure.initializer_zero)

    // default construction
    for(u32 m = 0; m < count(structure.members); m++)
    {
        const auto& member = structure.members[m];

        if(member.expr)
        {
            switch(member.expr->type)
            {
                case ast_type::initializer_list:
                {
                    unimplemented("initializer list");
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

                    do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset + offset);
                    break;                    
                }
            }
        }

        // (basically we need to just recurse this method)
        else if(is_struct(member.type))
        {
            const auto structure = struct_from_type(itl.struct_table,member.type);
            compile_struct_decl_default(itl,func,structure,addr_slot,member.offset + offset);
        }

        else if(is_array(member.type))
        {
            default_construct_arr(itl,func,(ArrayType*)member.type,addr_slot,member.offset + offset);
        }

        else
        {
            const SymSlot tmp = imm_zero(itl,func);

            do_ptr_store(itl,func,tmp,addr_slot,member.type,member.offset + offset);
        }
    }
}

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode *decl_node, Symbol& sym)
{
    const auto structure = struct_from_type(itl.struct_table,sym.type);

    alloc_slot(itl,func,sym.reg,true);

    if(decl_node->expr)
    {
        switch(decl_node->expr->type)
        {
            case ast_type::initializer_list:
            {
                const SymSlot addr_slot = addrof_res(itl,func,sym.reg.slot);
                traverse_struct_initializer(itl,func,(RecordNode*)decl_node->expr,addr_slot,structure);
                break;                
            }

            case ast_type::no_init:
            {
                break;
            }

            default:
            {
                const auto rtype = compile_expression(itl,func,decl_node->expr,sym.reg.slot);
                check_assign_init(itl,sym.type,rtype);
                break;    
            }
        }
    }

    // default init
    else
    {
        const SymSlot addr_slot = addrof_res(itl,func,sym.reg.slot);
        compile_struct_decl_default(itl,func,structure,addr_slot,0);
    }
}
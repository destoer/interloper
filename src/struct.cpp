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

dtr_res handle_recursive_type(Interloper& itl,const String& struct_name, TypeNode* type_decl, u32* type_idx_override)
{
    const auto name = type_decl->name;
    TypeDecl* decl_ptr = type_decl->name_space? lookup_incomplete_decl_scoped(type_decl->name_space,name) : lookup_incomplete_decl(itl,name);

    // no such decl exists
    if(!decl_ptr)
    {
        compile_error(itl,itl_error::undeclared,"%s : member type %s is not defined\n",struct_name.buf,type_decl->name.buf);
        return dtr_res::err;
    }

    // Type is allways complete we don't need any further checking
    if(!(decl_ptr->flags & TYPE_DECL_DEF_FLAG))
    {
        return dtr_res::ok;
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
            compile_error(itl,itl_error::black_hole,"%s : is recursively defined via %s\n",struct_name.buf,type_decl->name.buf);
            return dtr_res::err;
        }
    }

    else
    {
        if(!parse_def(itl,def))
        {
            return dtr_res::err;
        }
    }

    return dtr_res::ok;    
}

// returns member loc
Option<u32> add_member(Interloper& itl,Struct& structure,DeclNode* m, u32* size_count,b32 forced_first, u32 flags)
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
        auto type_opt = get_type(itl,type_decl,type_idx_override,true);

        if(!type_opt)
        {
            destroy_struct(structure);
            return option::none;
        }

        member.type = *type_opt;
    }

    else if(!type_exists(itl,type_decl->name))
    {
        if(!handle_recursive_type(itl,structure.name,type_decl,&type_idx_override))
        {
            destroy_struct(structure);
            return option::none;
        }

        auto type_opt = get_type(itl,type_decl,type_idx_override,true);

        if(!type_opt)
        {
            destroy_struct(structure);
            return option::none;
        }

        member.type = *type_opt;
    }

    else
    {
        auto type_opt = get_type(itl,type_decl,type_idx_override,true);

        if(!type_opt)
        {
            destroy_struct(structure);
            return option::none;
        }

        member.type = *type_opt;
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
        compile_error(itl,itl_error::redeclaration,"%s : member %s redeclared\n",structure.name.buf,member.name.buf);
        destroy_struct(structure);
        return option::none;
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

dtr_res parse_struct_def(Interloper& itl, TypeDef& def)
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
        auto forced_first_loc_opt = add_member(itl,structure,node->forced_first,size_count,true,flags);

        if(!forced_first_loc_opt)
        {
            return dtr_res::err;
        }

        forced_first_loc = *forced_first_loc_opt;
    }

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        if(!add_member(itl,structure,node->members[i],size_count,false,flags))
        {
            return dtr_res::err;
        }
    }

    finalise_member_offsets(itl,structure,size_count,forced_first_loc,flags);
    

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }

    add_struct(itl,structure,def.decl);
    return dtr_res::ok;
}


Option<Type*> access_array_member(Interloper& itl, Type* type, const String& member_name,AddrSlot* struct_slot)
{
    ArrayType* array_type = (ArrayType*)type;

    if(member_name == "len")
    {
        if(!is_runtime_size(type))
        {
            struct_slot->slot = make_spec_reg_slot(spec_reg::access_fixed_len_reg);
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
            compile_error(itl,itl_error::array_type_error,"no .data member on fixed size array");
            return option::none;
        }

        struct_slot->offset += 0;

        return make_reference(itl,array_type->contained_type);
    }


    else
    {
        compile_error(itl,itl_error::undeclared,"unknown array member %s\n",member_name.buf);
        return option::none;
    }
}

Option<Type*> access_struct_member(Interloper& itl, Type* type, const String& member_name, AddrSlot* struct_slot)
{
    // get offset for struct member
    const auto member_opt = get_member(itl.struct_table,type,member_name);

    if(!member_opt)
    {
        compile_error(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,type).buf);
        return option::none;
    }

    const auto member = member_opt.value();

    struct_slot->offset += member.offset;  

    return member.type;
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

Option<Type*> access_enum_struct_member(Interloper& itl,Function& func,Type* struct_type,
    const String& member_name, AddrSlot* struct_slot)
{
    const auto& enumeration = enum_from_type(itl.enum_table,struct_type);

    if(!enumeration.underlying_type || !is_struct(enumeration.underlying_type))
    {
        compile_error(itl,itl_error::struct_error,"member access on plain enum %s\n",enumeration.name.buf);
        return option::none;                    
    }

    // pull info on enum struct member
    auto& enum_struct = struct_from_type(itl.struct_table,enumeration.underlying_type);

    const auto enum_struct_member_opt = get_member(enum_struct, member_name);

    if(!enum_struct_member_opt)
    {
        compile_error(itl,itl_error::undeclared,"No such member %s for type %s\n",member_name.buf,type_name(itl,struct_type).buf);
        return option::none;                
    }

    const auto& enum_struct_member = enum_struct_member_opt.value();

    // get the start of the table
    const auto enum_table_slot = pool_addr_res(itl,func,enumeration.struct_slot,0);

    // get the enum index
    RegSlot enum_slot = INVALID_SYM_REG_SLOT;
    
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
    const RegSlot table_offset = mul_imm_res(itl,func,enum_slot,enum_struct.size);

    // compute final addr
    const auto addr_slot = add_res(itl,func,enum_table_slot,table_offset);

    // update the struct addr
    struct_slot->slot = addr_slot;

    return enum_struct_member.type;
}


// return type, slot, offset
Option<std::tuple<Type*,AddrSlot>> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
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
                compile_error(itl,itl_error::undeclared,"symbol %s used before declaration\n",name.buf);
                return option::none;
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
            auto index_opt = index_arr(itl,func,expr_node,new_tmp_ptr(func));
            if(!index_opt)
            {
                return option::none;
            }

            auto [struct_type, addr_slot] = *index_opt;

            struct_slot = make_addr(addr_slot,0);

            // we return types in here as the accessed type
            struct_type = deref_pointer(struct_type);
            break;
        }


        default: 
        {
            compile_error(itl,itl_error::struct_error,"Unknown struct access %s\n",AST_NAMES[u32(expr_node->type)]);
            return option::none;
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
                    RegSlot addr_slot = new_tmp_ptr(func);
                    if(!do_addr_load(itl,func,addr_slot,struct_slot,struct_type))
                    {
                        return option::none;
                    }

                    struct_slot = make_addr(addr_slot,0);

                    PointerType* ptr_type = (PointerType*)struct_type;

                    if(ptr_type->pointer_kind == pointer_type::nullable)
                    {
                        compile_error(itl,itl_error::pointer_type_error,"Cannot dereference a nullable pointer %s\n",
                            type_name(itl,(Type*)ptr_type).buf);
                        return option::none;
                    }

                    // now we are back to a straight pointer
                    struct_type = deref_pointer(struct_type);
                }

                switch(struct_type->kind)
                {
                    case type_class::array_t:
                    {
                        auto type_opt = access_array_member(itl,struct_type,member_name,&struct_slot);
                        if(!type_opt)
                        {
                            return option::none;
                        }

                        struct_type = *type_opt; 
                        break;
                    }

                    // do enum member access
                    case type_class::enum_t:
                    {
                        auto struct_type_opt = access_enum_struct_member(itl,func,struct_type,member_name,&struct_slot);
                        
                        if(!struct_type_opt)
                        {
                            return option::none;
                        }
                        
                        struct_type = *struct_type_opt;
                        break;
                    }

                    // actual struct member
                    default:
                    {
                        auto struct_type_opt = access_struct_member(itl,struct_type,member_name,&struct_slot);
                        if(!struct_type_opt)
                        {
                            return option::none;
                        }

                        struct_type = *struct_type_opt;
                        break;
                    }
                }   
                break;
            }

            case ast_type::index:
            {
                IndexNode* index_node = (IndexNode*)n;

                auto struct_type_opt = access_struct_member(itl,struct_type,index_node->name,&struct_slot);
                if(!struct_type_opt)
                {
                    return option::none;
                }

                struct_type = *struct_type_opt; 
                
                if(is_runtime_size(struct_type))
                {
                    const RegSlot vla_ptr = new_tmp_ptr(func);
                    // TODO: This can be better typed to a pointer
                    if(!do_addr_load(itl,func,vla_ptr,struct_slot,make_builtin(itl,GPR_SIZE_TYPE)))
                    {
                        return option::none;
                    }
                    struct_slot = make_addr(vla_ptr,0);
                }

                // fixed size collpase the offset
                else
                {
                    collapse_struct_offset(itl,func,&struct_slot);
                }

                auto index_opt = index_arr_internal(itl,func,index_node,index_node->name,struct_type,struct_slot.slot,new_tmp_ptr(func));
                if(!index_opt)
                {
                    return option::none;
                }

                auto [struct_type, addr_slot] = *index_opt;

                struct_slot = make_addr(addr_slot,0);

                // deref of pointer
                struct_type = deref_pointer(struct_type);
                break;
            }

            default: 
            {
                compile_error(itl,itl_error::undeclared,"Unknown member access %s\n",AST_NAMES[u32(n->type)]);
                return option::none;
            }
        }
    }

    return std::tuple{struct_type,struct_slot};
}

Option<std::pair<Type*,RegSlot>> compute_member_ptr(Interloper& itl, Function& func, AstNode* node)
{
    auto member_addr_opt = compute_member_addr(itl,func,node);

    if(!member_addr_opt)
    {
        return option::none;
    }

    auto [type,addr_slot] = *member_addr_opt;

    collapse_struct_offset(itl,func,&addr_slot);

    return std::pair{make_reference(itl,type),addr_slot.slot};
}

dtr_res write_struct(Interloper& itl,Function& func, RegSlot src_slot, Type* rtype, AstNode *node)
{
    auto member_addr_opt = compute_member_addr(itl,func,node);

    if(!member_addr_opt)
    {
        return dtr_res::err;
    }

    const auto [accessed_type, addr_slot] = *member_addr_opt;

    if(!check_assign(itl,accessed_type,rtype))
    {
        return dtr_res::err;
    }

    return do_addr_store(itl,func,src_slot,addr_slot,accessed_type);
}


Option<Type*> read_struct(Interloper& itl,Function& func, RegSlot dst_slot, AstNode *node)
{
    const List list_old = get_cur_list(func.emitter);

    auto member_addr_opt =  compute_member_addr(itl,func,node);

    if(!member_addr_opt)
    {
        return option::none;
    }

    auto [accessed_type, addr_slot] = *member_addr_opt;

    // len access on fixed sized array
    if(is_special_reg(addr_slot.slot,spec_reg::access_fixed_len_reg))
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

    if(!do_addr_load(itl,func,dst_slot,addr_slot,accessed_type))
    {
        return option::none;
    }

    return accessed_type;
}


dtr_res traverse_struct_initializer(Interloper& itl, Function& func, RecordNode* node, AddrSlot addr_slot, const Struct& structure)
{
    const u32 node_len = count(node->nodes);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        compile_error(itl,itl_error::undeclared,"struct initlizier missing initlizer expected %d got %d\n",member_size,node_len);
        return dtr_res::err;
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
                if(!traverse_arr_initializer_internal(itl,func,(RecordNode*)node->nodes[i],&addr_member,(ArrayType*)member.type))
                {
                    return dtr_res::err;
                }
            }

            else if(is_struct(member.type))
            {
                const Struct& sub_struct = struct_from_type(itl.struct_table,member.type);
                if(!traverse_struct_initializer(itl,func,(RecordNode*)node->nodes[i],addr_member,sub_struct))
                {
                    return dtr_res::err;
                }
            }

            else
            {
                compile_error(itl,itl_error::struct_error,"nested struct initalizer for basic type %s : %s\n",
                    member.name.buf,type_name(itl,member.type).buf);
                return dtr_res::err;
            }
        }

        // we have a list of plain values we can actually initialize
        else
        {
            // get the operand and type check it
            const auto res = compile_oper(itl,func,node->nodes[i]);
            if(!res)
            {
                return dtr_res::err;
            }

            const auto [rtype,slot] = *res;

            if(!check_assign(itl,member.type,rtype))
            {
                return dtr_res::err;
            }

            if(!do_addr_store(itl,func,slot,addr_member,member.type))
            {
                return dtr_res::err;
            }
        }
    } 

    return dtr_res::ok;
}

dtr_res compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot)
{
    // TODO: add a opt to just memset the entire thing in one go
    // NOTE: this should apply all the way down i.e if we contain a struct
    // it needs to have initialzer_zero aswell
    // if(structure.initializer_zero)
    push_context(itl);

    // default construction
    for(u32 m = 0; m < count(structure.members); m++)
    {
        const auto& member = structure.members[m];

        AddrSlot member_addr = addr_slot;
        member_addr.offset += member.offset;

        if(member.expr)
        {
            trash_context(itl,structure.filename,structure.name_space,member.expr);

            switch(member.expr->type)
            {
                case ast_type::initializer_list:
                {
                    if(!compile_init_list(itl,func,member.type,member_addr,member.expr))
                    {
                        return dtr_res::err;
                    }
                    break;
                }

                // dont default init
                case ast_type::no_init:
                {
                    break;
                }

                default: 
                {
                    const auto res = compile_oper(itl,func,member.expr);
                    if(!res)
                    {
                        return dtr_res::err;
                    }

                    const auto [rtype,slot] = *res;

                    if(!check_assign_init(itl,member.type,rtype))
                    {
                        pop_context(itl);
                        return dtr_res::err;
                    } 

                    if(!do_addr_store(itl,func,slot,member_addr,member.type))
                    {
                        return dtr_res::err;
                    }
                    break;                    
                }
            }
        }

        // (basically we need to just recurse this method)
        else if(is_struct(member.type))
        {
            const auto nested_structure = struct_from_type(itl.struct_table,member.type);
            if(!compile_struct_decl_default(itl,func,nested_structure,member_addr))
            {
                pop_context(itl);
                return dtr_res::err;
            }
        }

        else if(is_array(member.type))
        {
            if(!default_construct_arr(itl,func,(ArrayType*)member.type,member_addr))
            {
                pop_context(itl);
                return dtr_res::err;
            }
        }

        else
        {
            if(is_reference(member.type))
            {
                compile_error(itl,itl_error::pointer_type_error,"Reference member %s must have an explicit initializer: %s\n",member.name.buf,type_name(itl,member.type).buf);
                pop_context(itl);
                return dtr_res::err;
            }

            const RegSlot tmp = imm_zero(itl,func);
            if(!do_addr_store(itl,func,tmp,member_addr,member.type))
            {
                pop_context(itl);
                return dtr_res::err;
            }
        }
    }

    pop_context(itl);
    return dtr_res::ok;
}

dtr_res compile_struct_decl(Interloper& itl, Function& func, const DeclNode *decl_node, SymSlot slot)
{
    Type* ltype = nullptr;

    const auto reg_slot = make_sym_reg_slot(slot);

    // isolate our symbol as it may move
    {
        auto& sym = sym_from_slot(itl.symbol_table,slot);
        alloc_slot(itl,func,reg_slot,true);

        ltype = sym.type;
    }

    const auto structure = struct_from_type(itl.struct_table,ltype);

    
    if(decl_node->expr)
    {
        switch(decl_node->expr->type)
        {
            case ast_type::initializer_list:
            {
                const auto addr_slot = make_struct_addr(reg_slot,0);
                return traverse_struct_initializer(itl,func,(RecordNode*)decl_node->expr,addr_slot,structure);
            }

            case ast_type::no_init:
            {
                return dtr_res::ok;
            }

            default:
            {
                const auto rtype_opt = compile_expression(itl,func,decl_node->expr,reg_slot);
                if(!rtype_opt)
                {
                    return dtr_res::err;
                }

                return check_assign_init(itl,ltype,*rtype_opt); 
            }
        }
    }

    // default init
    else
    {
        const AddrSlot addr_slot = make_struct_addr(reg_slot,0);
        return compile_struct_decl_default(itl,func,structure,addr_slot);
    }
}
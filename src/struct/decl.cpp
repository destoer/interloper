
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


Struct& struct_from_type(StructTable& struct_table, const StructType* struct_type)
{
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

    auto& structure = struct_from_type(struct_table,(StructType*)type);

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


Result<StructType*,itl_error> lookup_struct(Interloper& itl, NameSpace* name_space,const String& name)
{
    const auto struct_decl_res = lookup_type_internal(itl,name_space,name);
    if(!struct_decl_res)
    {
        return compile_error(itl,itl_error::struct_error,"No such struct: %S",name);
    }

    const auto struct_decl = *struct_decl_res;

    if(struct_decl->kind != type_kind::struct_t)
    {
        return compile_error(itl,itl_error::struct_error,"No such struct: %S",name);
    }

    return (StructType*)make_struct(itl,struct_decl->type_idx);   
}

Option<itl_error> handle_recursive_type(Interloper& itl,const String& struct_name, TypeNode* type_decl, u32* type_idx_override)
{
    const auto name = type_decl->name;
    TypeDecl* decl_ptr = type_decl->name_space? lookup_incomplete_decl_scoped(type_decl->name_space,name) : lookup_incomplete_decl(itl,name);

    // no such decl exists
    if(!decl_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"%S : member type %S is not defined",struct_name,type_decl->name);
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
            return compile_error(itl,itl_error::black_hole,"%S : is recursively defined via %S",struct_name,type_decl->name);
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
    member.name = m->sym.name;

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
        if(recur_err)
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
    member.index = loc;

    if(contains(structure.member_map,member.name))
    {
        const auto res = compile_error(itl,itl_error::redeclaration,"%S : member %S redeclared",structure.name,member.name);
        destroy_struct(structure);
        return res;
    }

    // If this has a index tag as index_t
    if(member.expr)
    {
        const auto err = type_check_init_expr(itl,member.type,member.expr);
        if(err)
        {
            return *err;
        }
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


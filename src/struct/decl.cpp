
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


Result<StructType*,itl_error> lookup_struct(Interloper& itl, const TypeLookupInfo& info)
{
    const auto struct_decl_res = lookup_type(itl,info);
    if(!struct_decl_res)
    {
        return struct_decl_res.error();
    }

    const auto type_res = get_base_user_type(itl,nullptr,*struct_decl_res,0);
    if(!type_res)
    {
        return type_res.error();
    }

    auto type = *type_res;

    if(!is_struct(type))
    {
        return compile_error(itl,itl_error::struct_error,"Type %t is not a struct",type);
    }

    return (StructType*)type;
}

Option<itl_error> handle_recursive_type(Interloper& itl,const String& struct_name, TypeNode* type_decl, u32* type_idx_override)
{
    const auto info = type_node_to_lookup(type_decl,type_lookup_kind::any_t);
    const auto res = lookup_incomplete_decl(itl,info);

    if(!res)
    {
        return res.error();
    }

    TypeDecl* decl_ptr = *res;

    // Type is allways complete we don't need any further checking
    if(!(decl_ptr->flags & TYPE_DECL_DEF_FLAG))
    {
        return option::none;
    }

    // if we attempt to check a partial definition twice that the definition is recursive
    if(decl_ptr->state == type_def_state::checking)
    {
        // if its a pointer we dont need the complete information yet as they are all alike
        // so just override the type idx from the one reserved inside the def
        if(def_has_indirection(type_decl))
        {
            *type_idx_override = decl_ptr->type_idx;
        }

        else
        {
            // panic to prevent having our struct collapse into a black hole
            return compile_error(itl,itl_error::black_hole,"%S : is recursively defined via %S",struct_name,type_decl->name);
        }
    }

    else
    {
        return parse_def(itl,decl_ptr).remap_to_err();
    }

    return option::none;    
}

// returns member loc
Result<u32,itl_error> add_member(Interloper& itl,Struct& structure,DeclNode* member_decl, u32* size_count, u32 flags)
{
    Member member;
    member.name = member_decl->sym.name;

    TypeNode* type_decl = member_decl->type;

    itl.ctx.expr = (AstNode*)member_decl; 

    // copy the init expr
    member.expr = member_decl->expr;

    u32 type_idx_override = INVALID_TYPE;

    // If this type could we recursive we may need to override the idx if its held by a reference.
    if(type_decl->kind == type_node_kind::user)
    {
        const auto checked_res = is_type_checked(itl,type_node_to_lookup(type_decl,type_lookup_kind::any_t));
        if(!checked_res)
        {
            return checked_res.error();
        }

        const auto checked = *checked_res;

        if(!checked)
        {
            const auto recur_err = handle_recursive_type(itl,structure.name,type_decl,&type_idx_override);
            if(recur_err)
            {
                destroy_struct(structure);
                return *recur_err;
            }
        }
    }

    // Now grab the type
    auto type_res = get_type(itl,type_decl,type_idx_override,true);

    if(!type_res)
    {
        destroy_struct(structure);
        return type_res.error();
    }

    member.type = *type_res;

    structure.holds_refs = structure.holds_refs || is_reference(member.type);
    if(is_struct(member.type))
    {
        auto& member_struct = struct_from_type(itl.struct_table,(StructType*)member.type);
        structure.holds_refs = structure.holds_refs || member_struct.holds_refs;
    }


    // we will deal with this later
    if(flags & ATTR_NO_REORDER)
    {
        member.offset = count(structure.members);
    }

    else if(member_decl->flags & FORCED_FIRST_FLAG)
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

void finalise_member_offsets(Interloper& itl, Struct& structure, u32* size_count, u32 flags)
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

        return;
    }

    // default: reorder the struct for size
    // handle alignment & get starting zones + total size
    u32 alloc_start[4];
    u32 byte_start = 0;

    u32 member_start = 0;

    // insert this as the first set of data in the byte section
    if(structure.members[0].offset == OFFSET_FORCED_FIRST)
    {
        auto& member = structure.members[0];
        member.offset = 0;
        const auto [size,count] = compute_member_size(itl,member.type);
        
        const u32 bytes = size * count;

        // include allocation for this member
        size_count[0] += bytes;

        // usual byte start offset by our insertion at front
        byte_start = bytes;

        member_start = 1;
    }

    // finalise the offsets
    structure.size = calc_alloc_sections(alloc_start,size_count,byte_start);
    structure.data_size = structure.size;

    // iter back over every member and give its offset
    for(u32 m = member_start; m < count(structure.members); m++)
    {
        auto& member = structure.members[m];

        const auto [size,count] = compute_member_size(itl,member.type);

        const u32 zone_offset = member.offset;
        member.offset = alloc_start[log2(size)] + (zone_offset * size);
    }
}

Result<TypeDecl*, itl_error> parse_struct_def(Interloper& itl, TypeDecl& decl)
{
    // TODO: Handle adding generic for reference
    StructNode* node = (StructNode*)decl.root;

    // NOTE: we expect the caller to save this
    trash_context(itl,node->filename,decl.name_space,decl.root);
    const auto generic_guard = switch_generic_context(itl,decl.overload);

    Struct structure;
    
    // allocate a reserved slot for the struct
    decl.type_idx = count(itl.struct_table);
    resize(itl.struct_table,count(itl.struct_table) + 1);


    structure.name = node->name;
    structure.filename = node->filename;
    structure.name_space = decl.name_space;
    structure.member_map = make_table<String,u32>();
    structure.overload = decl.overload;
    structure.base = decl.base;

    // we want to get how many sizes of each we have
    // and then we can go back through and align the struct with them
    u32 size_count[4] = {0};

    const u32 flags = node->attr_flags;

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        const auto member_res = add_member(itl,structure,node->members[i],size_count,flags);
        if(!member_res)
        {
            return member_res.error();
        }
    }

    finalise_member_offsets(itl,structure,size_count,flags);
    
    if(itl.print_types)
    {
        print_struct(itl,structure);
    }

    add_struct(itl,structure,decl);
    return &decl;
}


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
    structure.type_idx = STRUCT_START + slot;
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

bool struct_exists(Interloper& itl, const String& name)
{
    TypeDecl* type_decl = lookup(itl.type_table,name);

    if(!type_decl)
    {
        return false;
    }

    return type_decl->kind == type_kind::struct_t;
}

void parse_struct_decl(Interloper& itl, StructDef& def);

void parse_def(Interloper& itl, StructDef& def)
{
    // mark struct as being parsed so we can check for recursion
    def.state = struct_state::checking;
    parse_struct_decl(itl,def);

    // mark as checked so thatt we know we dont have to recheck the decl
    def.state = struct_state::checked;
}

void parse_struct_decl(Interloper& itl, StructDef& def)
{
    StructNode* node = def.root;

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


    itl.cur_file = node->filename;

    structure.name = node->name;
    structure.member_map = make_table<String,u32>();

    // we want to get how many sizes of each we have
    // and then we can go back through and align the struct with them

    u32 size_count[3] = {0};

    // parse out members
    for(u32 i = 0; i < count(node->members); i++)
    {
        DeclNode* m = node->members[i];

        Member member;
        member.name = m->name;

        TypeNode* type_decl = m->type;

        // copy the init expr
        member.expr = m->expr;


        u32 type_idx_override = INVALID_TYPE;

        // member is struct that has nott had its defintion parsed yet
        if(type_decl->type_idx == USER_TYPE && !struct_exists(itl,type_decl->name))
        {
            StructDef *def_ptr = lookup(itl.struct_def,type_decl->name);

            // no such definiton exists
            if(!def_ptr)
            {
                panic(itl,"%s : member type %s is not defined\n",structure.name.buf,type_decl->name.buf);
                destroy_struct(structure);
                return;
            }

            StructDef& def = *def_ptr;

            // if we attempt to check a partial defintion twice that the definition is recursive
            if(def.state == struct_state::checking)
            {
                // if its a pointer we dont need the complete inormation yet as they are all alike
                // so just override the type idx from the one reserved inside the def
                if(def_has_indirection(type_decl))
                {
                    type_idx_override = BUILTIN_TYPE_SIZE + def.slot;
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

        itl.cur_file = node->filename;

        member.type = get_type(itl,type_decl,type_idx_override);

        // TODO: ensure array type cant use a deduced type size


        u32 size;

        if(is_fixed_array(member.type))
        {
            const auto [contained_size, count] = arr_size(itl,member.type);
            size = contained_size * count;
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

    if(itl.print_types)
    {
        print_struct(itl,structure);
    }


    add_struct(itl,structure,def.slot);

}

void parse_struct_declarations(Interloper& itl)
{
    auto &struct_def = itl.struct_def;

    for(u32 b = 0; b < count(struct_def.buf); b++)
    {
        auto& bucket = struct_def.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto &def = bucket[i].v;

            if(def.state == struct_state::not_checked)
            {
                parse_def(itl,def);
            }
        }
    }
}

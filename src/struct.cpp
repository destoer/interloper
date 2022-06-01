void print_member(Interloper& itl,const Member& member)
{
    printf("\t%s -> %d : %s\n",member.name.c_str(),member.offset,type_name(itl,member.type).c_str());
}

void print_struct(Interloper& itl, const Struct& structure)
{
    printf("struct %s\n{\n",structure.name.c_str());
    for(const auto &[key,member] : structure.members)
    {
        UNUSED(key);
        print_member(itl,member);
    }
    printf("};\n");
    printf("size: %d\n",structure.size);
}

void parse_struct_declarations(Interloper& itl)
{
    for(const auto n : itl.struct_root->nodes)
    {
        Struct structure;

        structure.name = n->literal;

        // we want to get how many sizes of each we have
        // and then we can go back through and align the struct...

        u32 size_count[3] = {0};

        // parse out members
        for(const auto m : n->nodes)
        {
            Member member;
            member.name = m->literal;
            member.type = get_type(itl,m->nodes[0]);

            // TODO: we dont handle the type being another struct here

            const u32 size = type_size(itl,member.type);


            // translate larger items, into several allocations on the final section
            if(size > GPR_SIZE)
            {
                unimplemented("large size");
            }

            else
            {
                // cache the offset into its section
                member.offset = size_count[size >> 1];

                size_count[size >> 1] += 1;
            }

            // TODO: handle redefinitions
            structure.members[member.name] = member;
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
        for(auto &[key,member] : structure.members)
        {
            UNUSED(key);
            const u32 zone_offset = member.offset;
            const u32 size = type_size(itl,member.type);

            member.offset = alloc_start[size >> 1] + (zone_offset * size);
        }

        // get the total structure size
        structure.size = alloc_start[2] + (size_count[2] * sizeof(u32));

        if(itl.print_types)
        {
            print_struct(itl,structure);
        }
    }
}

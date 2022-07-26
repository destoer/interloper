void destroy_enum(Enum& enumeration)
{
    destroy_table(enumeration.member_map);
}

void destroy_enum_table(HashTable<String,Enum> enum_table)
{
    for(u32 b = 0; b < count(enum_table.buf); b++)
    {
        auto &bucket = enum_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& enumeration = bucket[i].v;
            destroy_enum(enumeration);
        }
    } 

    destroy_table(enum_table);   
}


void print_enum(Enum& enumeration)
{
    printf("enum %s\n{\n",enumeration.name.buf);
    
    for(u32 b = 0; b < count(enumeration.member_map.buf); b++)
    {
        auto &bucket = enumeration.member_map.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& member = bucket[i].v;
            printf("\t%s = %d\n",member.name.buf,member.value);
        }
    } 

    printf("}\n");       
}

void enum_decl(Interloper& itl,Parser& parser, const String& filename)
{
    const auto name_tok = next_token(parser);

    if(name_tok.type != token_type::symbol)
    {
        panic(itl,"Expected symbol for enum name got %s\n",tok_name(name_tok.type));
        return;
    }

    consume(parser,token_type::left_c_brace);

    // NOTE: at present there doesn't seem to be  a good reason to put this into a ast and parse it
    // it makes more sense just to immediatly pull the definition
    Enum enumeration;

    enumeration.name = name_tok.literal;
    enumeration.filename = filename;
    enumeration.member_map = make_table<String,EnumMember>();

    if(contains(itl.enum_table,enumeration.name))
    {
        panic(itl,"Enum %s redefined %s\n",enumeration.name.buf);
        destroy_enum(enumeration);
        return;
    }

    u32 count = 0;

    while(!match(parser,token_type::right_c_brace))
    {
        const auto member_tok = next_token(parser);

        if(member_tok.type != token_type::symbol)
        {
            panic(itl,"Expected symbol for enum %s member got %s\n",enumeration.name.buf,tok_name(member_tok.type));
            destroy_enum(enumeration);
            return;
        }

        // create member and give it a internal value representation
        EnumMember member;

        member.name = member_tok.literal;
        member.value = count++;

        if(contains(enumeration.member_map,member.name))
        {
            panic(itl,"Enum %s member %s redefined!\n",enumeration.name.buf,member.name.buf);
            destroy_enum(enumeration);
            return;
        }


        // add member to enum
        add(enumeration.member_map,member.name,member);

        consume(parser,token_type::comma);
    }

    consume(parser,token_type::right_c_brace);

    add(itl.enum_table,enumeration.name,enumeration);

    if(itl.print_types)
    {
        print_enum(enumeration);
    }
}
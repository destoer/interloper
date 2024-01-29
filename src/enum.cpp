void destroy_enum(Enum& enumeration)
{
    destroy_table(enumeration.member_map);
}

void destroy_enum_table(EnumTable &enum_table)
{
    for(u32 e = 0; e < count(enum_table); e++)
    {
        destroy_enum(enum_table[e]); 
    } 

    destroy_arr(enum_table);  
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

void compile_const_struct_list_internal(Interloper& itl,RecordNode* list, const Struct& structure, PoolSlot slot, u32 offset);

void parse_enum_def(Interloper& itl, TypeDef& def, Set<u64>& set)
{
    EnumNode* node = (EnumNode*)def.root;    

    itl.cur_file = node->filename;

    Enum enumeration;

    enumeration.kind = node->kind; 

    switch(enumeration.kind)
    {
        case enum_type::struct_t:
        {
            // check we have a valid struct
            TypeDecl* type_decl = lookup_type(itl,node->struct_name);

            if(!type_decl)
            {
                panic(itl,itl_error::struct_error,"no such struct %s for enum struct %s\n",node->struct_name.buf,node->name.buf);
                return; 
            }

            if(type_decl->kind != type_kind::struct_t)
            {
                panic(itl,itl_error::struct_error,"type %s is not a struct for enum struct %s\n",node->struct_name.buf,node->name.buf);
                return;        
            }

            // mark struct idx
            enumeration.underlying_type_idx = type_decl->type_idx;

            // reserve space for it inside the const pool
            const u32 data_size = itl.struct_table[enumeration.underlying_type_idx].size * count(node->member);
            enumeration.struct_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);
        
            break;
        }

        case enum_type::int_t:
        {
            if(!is_integer(node->type))
            {
                panic(itl,itl_error::enum_type_error,"Only integers can be used an builtin underlying enum type");
                return;
            }

            enumeration.underlying_type_idx = u32(node->type);

            break;
        }

        // dont care
        case enum_type::plain_t:
        {
            break;
        }
    }

    

    // reset filename incase struct clobbers it
    itl.cur_file = node->filename;

    // setup enum info
    enumeration.name = node->name;
    enumeration.filename = node->filename;
    enumeration.member_map = make_table<String,EnumMember>();

    u32 member_count = 0;

    b32 value_used = false;

    // parse each enum member
    for(u32 m = 0; m < count(node->member); m++)
    {
        const EnumMemberDecl& member_decl = node->member[m];

        // create member and give it a internal value representation
        EnumMember member;

        member.name = member_decl.name;

        // check for duplicate members
        if(contains(enumeration.member_map,member.name))
        {
            panic(itl,itl_error::redeclaration,"Enum %s member %s redefined!\n",enumeration.name.buf,member.name.buf);
            destroy_enum(enumeration);
            return;
        }

        switch(enumeration.kind)
        {
            case enum_type::struct_t:
            {
                member.value = member_count++;

                // compile in member access
                auto& structure = itl.struct_table[enumeration.underlying_type_idx];

                switch(member_decl.initializer->type)
                {
                    case ast_type::initializer_list:
                    {
                        itl.cur_expr = (AstNode*)member_decl.initializer;
                        const u32 offset = m * structure.size;

                        compile_const_struct_list_internal(itl,(RecordNode*)member_decl.initializer,structure,enumeration.struct_slot, offset);
                        break;
                    }

                    default:
                    {
                        unimplemented("enum struct expr init");
                        break;
                    }
                }
                break;
            }

            // specify values by hand
            case enum_type::int_t:
            {
                if(member_decl.initializer)
                {
                    value_used = true;

                    const auto [value,type] = compile_const_int_expression(itl,member_decl.initializer);

                    if(!is_integer(type))
                    {
                        return;
                    }

                    member.value = value;

                    // TODO: check for overlapping values
                    if(contains(set,value))
                    {
                        panic(itl,itl_error::enum_type_error,"Duplicate enum value: %s",member.name.buf);
                        return;
                    }

                    else
                    {
                        add(set,value);
                    }
                }

                else
                {
                    // TODO: maybe we should relax this to pick a valid int
                    if(value_used)
                    {
                        panic(itl,itl_error::enum_type_error,"Integer enums must assign all values manually when used");
                        return;
                    }

                    // we have flags automatically assign the next value
                    if(node->attr_flags & ATTR_FLAG)
                    {
                        member.value = (1 << member_count);
                    }
                }

                member_count++;
                break;
            }

            case enum_type::plain_t:
            {
                member.value = member_count++;

                if(member_decl.initializer)
                {
                    panic(itl,itl_error::enum_type_error,"Plain enum's cannot have initializers");
                    return;
                }

                break;
            }
        }


        // add member to enum
        add(enumeration.member_map,member.name,member);
    }

    // finally add enum into type table
    const u32 slot = count(itl.enum_table);
    enumeration.type_idx = slot;

    push_var(itl.enum_table,enumeration);
    add_type_decl(itl,slot,enumeration.name,type_kind::enum_t);

    if(itl.print_types)
    {
        print_enum(enumeration);
    }
}



Enum enum_from_type(EnumTable& enum_table, const Type* type)
{
    EnumType* enum_type = (EnumType*)type;

    return enum_table[enum_type->enum_idx];
}   


Type* make_enum_type(Interloper& itl,Enum& enumeration)
{
    return make_enum(itl,enumeration.type_idx);
}
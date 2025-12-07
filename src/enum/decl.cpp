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

Option<itl_error> parse_enum_def(Interloper& itl, TypeDef& def, Set<u64>& set)
{
    EnumNode* node = (EnumNode*)def.root;    

    trash_context(itl,node->filename,def.decl.name_space,def.root);

    Enum enumeration;

    if(node->type)
    {
        auto type_res = get_type(itl,node->type);

        if(!type_res)
        {
            return type_res.error();
        }

        enumeration.underlying_type = *type_res;

        if(is_struct(enumeration.underlying_type))
        {
            // reserve space for it inside the const pool
            const auto& structure = struct_from_type(itl.struct_table,enumeration.underlying_type);
            
            const u32 data_size = structure.size * count(node->member);
            enumeration.struct_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);
        }
    }

    if((node->attr_flags & ATTR_FLAG) && (!enumeration.underlying_type || !is_integer(enumeration.underlying_type)))
    {
        return compile_error(itl,itl_error::enum_type_error,"Flag enum must specify underlying integer type");
    }

    if(node->attr_flags & ATTR_USE_RESULT)
    {
        enumeration.use_result = true;
    }


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
            const auto res = compile_error(itl,itl_error::redeclaration,"Enum %S member %S redefined!",enumeration.name,member.name);
            destroy_enum(enumeration);
            return res;
        }

        if(!enumeration.underlying_type)
        {
            member.value = member_count++;

            if(member_decl.initializer)
            {
                return compile_error(itl,itl_error::enum_type_error,"Plain enum's cannot have initializers");
            }
        }

        else if(is_struct(enumeration.underlying_type))
        {
            member.value = member_count++;

            // compile in member access
            auto& structure = struct_from_type(itl.struct_table,enumeration.underlying_type);
            UNUSED(structure);


            switch(member_decl.initializer->type)
            {
                default:
                {
                    unimplemented("enum struct expr init");
                    break;
                }
            }
        }

        else if(is_integer(enumeration.underlying_type))
        {
            if(member_decl.initializer)
            {
                assert(false);
                UNUSED(set);
                // value_used = true;

                // auto expr_res = compile_const_int_expression(itl,member_decl.initializer);

                // if(!expr_res)
                // {
                //     return expr_res.error();
                // }

                // const auto const_int = *expr_res;

                // member.value = const_int.value;

                // // TODO: check for overlapping values
                // if(contains(set,const_int.value))
                // {
                //     return compile_error(itl,itl_error::enum_type_error,"Duplicate enum value: %s",member.name.buf);
                // }

                // else
                // {
                //     add(set,const_int.value);
                // }
            }

            else
            {
                // TODO: maybe we should relax this to pick a valid int
                if(value_used)
                {
                    return compile_error(itl,itl_error::enum_type_error,"Integer enums must assign all values manually when used");
                }

                // we have flags automatically assign the next value
                if(node->attr_flags & ATTR_FLAG)
                {
                    member.value = (1 << member_count);
                }

                else
                {
                    member.value = member_count;
                }
            }

            member_count++;
        }

        // add member to enum
        add(enumeration.member_map,member.name,member);
    }

    // finally add enum into type table
    const u32 slot = count(itl.enum_table);
    enumeration.type_idx = slot;

    push_var(itl.enum_table,enumeration);
    finalise_type(def.decl,enumeration.type_idx);

    if(itl.print_types)
    {
        print_enum(enumeration);
    }

    return option::none;
}



Enum enum_from_type(EnumTable& enum_table, const Type* type)
{
    EnumType* enum_type = (EnumType*)type;

    return enum_table[enum_type->enum_idx];
}   


Type* make_enum_type(Interloper& itl,Enum& enumeration)
{
    const u32 flags = enumeration.use_result? TYPE_USE_RESULT : 0;
    return make_enum(itl,enumeration.type_idx,flags);
}

Option<itl_error> compile_const_struct_list_internal(Interloper& itl,InitializerListNode* init_list, const Struct& structure, PoolSlot slot, u32* offset);

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
            const auto& structure = struct_from_type(itl.struct_table,(StructType*)enumeration.underlying_type);
            
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
            auto& structure = struct_from_type(itl.struct_table,(StructType*)enumeration.underlying_type);


            AstNode* init_expr = member_decl.initializer;
            itl.ctx.expr = init_expr;

            const auto type_err = type_check_init_expr(itl,enumeration.underlying_type,init_expr);
            if(type_err)
            {
                return *type_err;
            }

            switch(init_expr->type)
            {
                case ast_type::initializer_list:
                {
                    u32 offset = m * structure.size;

                    const auto struct_err = compile_const_struct_list_internal(itl,(InitializerListNode*)init_expr,structure,
                        enumeration.struct_slot, &offset);
                    if(struct_err)
                    {
                        return *struct_err;
                    }
                    break;
                }

                default:
                {
                    return compile_error(itl,itl_error::invalid_statement,"Unknown enum struct initializer: %s",AST_INFO[u32(init_expr->type)]);
                }
            }
        }

        else if(is_integer(enumeration.underlying_type))
        {
            if(member_decl.initializer)
            {
                value_used = true;

                auto expr_res = type_check_const_int_expression(itl,member_decl.initializer);

                if(!expr_res)
                {
                    return expr_res.error();
                }

                const auto const_int = *expr_res;

                member.value = const_int.value;

                if(contains(set,const_int.value))
                {
                    return compile_error(itl,itl_error::enum_type_error,"Duplicate enum value: %S",member.name);
                }

                else
                {
                    add(set,const_int.value);
                }
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



Enum enum_from_type(EnumTable& enum_table, const EnumType* enum_type)
{
    return enum_table[enum_type->enum_idx];
}   


Type* make_enum_type(Interloper& itl,Enum& enumeration)
{
    const u32 flags = enumeration.use_result? TYPE_USE_RESULT : 0;
    return make_enum(itl,enumeration.type_idx,flags);
}


Result<EnumType*,itl_error> lookup_enum(Interloper& itl, NameSpace* name_space,const String& name)
{
    const auto enum_decl_res = lookup_type_internal(itl,name_space,name);
    if(!enum_decl_res)
    {
        return compile_error(itl,itl_error::enum_type_error,"No such enum: %S",name);
    }

    const auto enum_decl = *enum_decl_res;

    if(enum_decl->kind != type_kind::enum_t)
    {
        return compile_error(itl,itl_error::enum_type_error,"No such enum: %S",name);
    }

    return (EnumType*)make_enum(itl,enum_decl->type_idx);   
}
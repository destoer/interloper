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

    trash_context(itl,node->filename,def.decl.name_space,def.root);

    Enum enumeration;

    if(node->type)
    {
        enumeration.underlying_type = get_type(itl,node->type);

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
        panic(itl,itl_error::enum_type_error,"Flag enum must specify underlying integer type\n");
        return;
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
            panic(itl,itl_error::redeclaration,"Enum %s member %s redefined!\n",enumeration.name.buf,member.name.buf);
            destroy_enum(enumeration);
            return;
        }

        if(!enumeration.underlying_type)
        {
            member.value = member_count++;

            if(member_decl.initializer)
            {
                panic(itl,itl_error::enum_type_error,"Plain enum's cannot have initializers");
                return;
            }
        }

        else if(is_struct(enumeration.underlying_type))
        {
            member.value = member_count++;

            // compile in member access
            auto& structure = struct_from_type(itl.struct_table,enumeration.underlying_type);

            switch(member_decl.initializer->type)
            {
                case ast_type::initializer_list:
                {
                    itl.ctx.expr = (AstNode*)member_decl.initializer;
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
        }

        else if(is_integer(enumeration.underlying_type))
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


enum class enum_decode_res
{
    ok,
    invalid_namespace,
    invalid_enum,
    invalid_member,
};

static const char* ENUM_DECODE_RES_MESSAGE[] = 
{
    "ok",
    "invalid namespace",
    "undefined enum",
    "invalid member",
};

const char* enum_decode_msg(enum_decode_res res)
{
    return ENUM_DECODE_RES_MESSAGE[u32(res)];
}

enum_decode_res decode_enum(Interloper& itl,ScopeNode* scope_node, Enum** enumeration, EnumMember** member)
{
    TypeDecl* type_decl = nullptr;

    if(count(scope_node->scope) == 1)
    {
        type_decl = lookup_type(itl,scope_node->scope[0]);
    }

    else
    {
        const u32 count_minus_one = count(scope_node->scope) - 1;
        NameSpace* name_space = scan_namespace(itl.global_namespace,clip_array(scope_node->scope,count_minus_one));

        if(!name_space)
        {
            return enum_decode_res::invalid_namespace;
        }

        type_decl = lookup_type_scoped(itl,name_space,scope_node->scope[count_minus_one]);
    }

    if(!type_decl || type_decl->kind != type_kind::enum_t)
    {
        return enum_decode_res::invalid_enum;
    }

    *enumeration = &itl.enum_table[type_decl->type_idx];

    if(scope_node->expr->type != ast_type::symbol)
    {
        return enum_decode_res::invalid_member;
    }

    LiteralNode *member_node = (LiteralNode*)scope_node->expr;
    *member = lookup((*enumeration)->member_map,member_node->literal);

    if(!*member)
    {
        return enum_decode_res::invalid_member;
    }

    return enum_decode_res::ok;
}

// Note: if this can't find an enum it will fail without a panic
Type* compile_enum(Interloper& itl, Function& func,ScopeNode* scope_node, RegSlot dst_slot)
{
    Enum* enumeration = nullptr;
    EnumMember* enum_member = nullptr;

    auto decode_res = decode_enum(itl,scope_node,&enumeration,&enum_member);


    switch(decode_res)
    {
        case enum_decode_res::ok:
        {
            // emit mov on the enum value
            mov_imm(itl,func,dst_slot,enum_member->value);

            // implictly type to underlying integer value
            if(enumeration->underlying_type && is_integer(enumeration->underlying_type))
            {
                return copy_type(itl,enumeration->underlying_type);
            }

            // normal enum type
            return make_enum_type(itl,*enumeration);
        }

        case enum_decode_res::invalid_enum:
        case enum_decode_res::invalid_namespace:
        {
            return make_builtin(itl,builtin_type::void_t);
        }
        
        case enum_decode_res::invalid_member:
        {
            panic(itl,itl_error::enum_type_error,"enum %s no such member\n",enumeration->name.buf);
            return make_builtin(itl,builtin_type::void_t);     
        }
    }

    assert(false);
    return make_builtin(itl,builtin_type::void_t);
}
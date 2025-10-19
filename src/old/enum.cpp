

enum class [[nodiscard]] enum_decode_res
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
        auto type_opt = lookup_type(itl,scope_node->scope[0]);
        if(!type_opt)
        {
            return enum_decode_res::invalid_enum;
        }

        type_decl = *type_opt;
    }

    else
    {
        const u32 count_minus_one = count(scope_node->scope) - 1;
        NameSpace* name_space = scan_namespace(itl.global_namespace,clip_array(scope_node->scope,count_minus_one));

        if(!name_space)
        {
            return enum_decode_res::invalid_namespace;
        }


        auto type_opt = lookup_type_scoped(itl,name_space,scope_node->scope[count_minus_one]);

        if(!type_opt)
        {
            return enum_decode_res::invalid_enum;
        }

        type_decl = *type_opt;
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
TypeResult compile_enum(Interloper& itl, Function& func,ScopeNode* scope_node, RegSlot dst_slot)
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
            return compile_error(itl,itl_error::enum_type_error,"enum %s no such member",enumeration->name.buf); 
        }
    }

    assert(false);
}


Option<itl_error> check_redeclaration(Interloper& itl, NameSpace* root, const String& name, const String& checked_def_type)
{
    const DefInfo* existing_def = lookup_definition(root,name);

    if(existing_def)
    {
        return compile_error(itl,itl_error::redeclaration,"%S (%s) has been redeclared as a %S!",
            name,definition_type_name(existing_def),checked_def_type);
    }

    return option::none;
}

Option<parse_error> type_alias(Interloper& itl, Parser &parser)
{
    // type_alias literal '=' type ';'
    const auto token = next_token(parser);

    
    if(token.type == token_type::symbol)
    {
        const auto eq_res = consume(parser,token_type::equal);
        if(eq_res)
        {
            return *eq_res;
        }

        auto rtype_res = parse_type(parser);

        if(!rtype_res)
        {
            return rtype_res.error();
        }

        TypeNode* rtype = *rtype_res;

        const String& name = token.literal;

        if(check_redeclaration(itl,parser.cur_namespace,name,"type alias"))
        {
            return parse_error::itl_error;
        }

        AstNode* alias_node = ast_alias(parser,rtype,name,parser.cur_file,token);
    
        const auto term_err = consume(parser,token_type::semi_colon);
        if(term_err)
        {
            return *term_err;
        }

        add_type_definition(itl, type_def_kind::alias_t,alias_node, name, parser.cur_file,parser.cur_namespace);
    }

    else 
    {
        return parser_error(parser,parse_error::unexpected_token,token,"expected symbol for type alias name got %s\n",tok_name(token.type));
    }

    return option::none;
}



Option<parse_error> struct_decl(Interloper& itl,Parser& parser, u32 flags = 0)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,name,"expected name after struct decl got %s\n",tok_name(name.type));
    }

    if(check_redeclaration(itl,parser.cur_namespace,name.literal,"struct"))
    {
        return parse_error::itl_error;
    }

    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,parser.cur_file,name);

    struct_node->attr_flags = flags;

    // Does this struct have a forced first member?
    if(match(parser,token_type::left_paren))
    {
        (void)consume(parser,token_type::left_paren);

        auto decl_res = declaration(parser,token_type::right_paren);
        if(!decl_res)
        {
            return decl_res.error();
        }
        struct_node->forced_first = (DeclNode*)decl_res.value();
    }

    const auto left_c_brace_err = consume(parser,token_type::left_c_brace);
    if(left_c_brace_err)
    {
        return *left_c_brace_err;
    }

    while(!match(parser,token_type::right_c_brace))
    {
        auto decl_res = declaration(parser,token_type::semi_colon);

        if(!decl_res)
        {
            return decl_res.error();
        }

        DeclNode* decl = (DeclNode*)*decl_res;

        push_var(struct_node->members,decl);
    }

    (void)consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        (void)consume(parser,token_type::semi_colon);
    }

    add_type_definition(itl, type_def_kind::struct_t,(AstNode*)struct_node, struct_node->name, parser.cur_file,parser.cur_namespace);
    return option::none;
}

Option<parse_error> enum_decl(Interloper& itl,Parser& parser, u32 flags)
{
    const auto name_tok = next_token(parser);

    if(name_tok.type != token_type::symbol)
    {
        (void)compile_error(itl,itl_error::missing_name,"Expected symbol for enum name got %s",tok_name(name_tok.type));
        return parse_error::itl_error;
    }

    const auto redecl_err = check_redeclaration(itl,parser.cur_namespace,name_tok.literal,"enum");
    if(redecl_err)
    {
        return parse_error::itl_error;
    }

    EnumNode* enum_node = (EnumNode*)ast_enum(parser,name_tok.literal,parser.cur_file,name_tok);


    if(match(parser,token_type::colon))
    {
        (void)consume(parser,token_type::colon);
        auto type_res = parse_type(parser);
        if(!type_res)
        {
            return type_res.error();
        }

        enum_node->type = *type_res;
    }

    if((flags & ATTR_FLAG) && !enum_node->type)
    {
        return parser_error(parser,parse_error::malformed_stmt,next_token(parser),"Flag enum must specify underlying intergeral type");       
    }

    const auto left_c_brace_err = consume(parser,token_type::left_c_brace);
    if(left_c_brace_err)
    {
        return *left_c_brace_err;
    }

    enum_node->attr_flags = flags;

    // push each member till we hit the terminating brace
    while(!match(parser,token_type::right_c_brace))
    {
        const auto member_tok = next_token(parser);

        if(member_tok.type != token_type::symbol)
        {
            return parser_error(parser,parse_error::unexpected_token,member_tok,"Expected symbol for enum %s member got %s\n",
                name_tok.literal.buf,tok_name(member_tok.type));
        }

        EnumMemberDecl member;
        member.name = member_tok.literal;

        // see if we have an initlizer
        if(match(parser,token_type::equal))
        {
            (void)consume(parser,token_type::equal);

            token_type term;

            auto initializer_res = expr_terminate(parser,"enum struct init",token_type::comma,term);
            if(!initializer_res)
            {
                return initializer_res.error();
            }

            member.initializer = *initializer_res;
        }

        else
        {
            const auto comma_err = consume(parser,token_type::comma);
            if(comma_err)
            {
                return *comma_err;
            }
        }

        push_var(enum_node->member,member);        
    }

    (void)consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        (void)consume(parser,token_type::semi_colon);
    }

    // add the type decl
    add_type_definition(itl, type_def_kind::enum_t,(AstNode*)enum_node, enum_node->name, parser.cur_file,parser.cur_namespace);
    return option::none;
}
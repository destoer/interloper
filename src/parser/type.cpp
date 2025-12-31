

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
    const auto start_span = make_span(parser.tokens,parser.tok_idx);
    const auto token = next_token(parser);

    const String& name = token.literal;

    if(token.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,token,"expected symbol for type alias name got %s\n",tok_name(token.type));
    }

    if(check_redeclaration(itl,parser.context.cur_namespace,name,"type alias"))
    {
        return parse_error::itl_error;
    }

    const auto span_res = scan_colon_stmt(parser,"Alias",name,start_span);
    if(!span_res)
    {
        return span_res.error();
    }

    const auto alias_def = make_top_level_def(parser,*span_res,0);
    add_type_definition(itl, type_def_kind::alias_t, alias_def, name, parser.context.cur_file,parser.context.cur_namespace);
    return option::none;
}

Result<AliasNode*, parse_error> parse_alias_decl(Parser &parser)
{
    // type_alias literal '=' type ';'
    const auto token = next_token(parser);
    const String& name = token.literal;
    
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


    AliasNode* alias_node = (AliasNode*)ast_alias(parser,rtype,name,parser.context.cur_file,token);

    const auto term_err = consume(parser,token_type::semi_colon);
    if(term_err)
    {
        return *term_err;
    }

    return alias_node;
}

Option<parse_error> struct_decl(Interloper& itl,Parser& parser, u32 flags = 0)
{
    const auto start_span = make_span(parser.tokens,parser.tok_idx);
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,name,"expected name after struct decl got %s\n",tok_name(name.type));
    }

    if(check_redeclaration(itl,parser.context.cur_namespace,name.literal,"struct"))
    {
        return parse_error::itl_error;
    }

    const auto res = scan_brace_stmt(parser,"Struct",name.literal,start_span);
    if(!res)
    {
        return res.error();
    }

    const auto struct_def = *res;

    const auto def = make_top_level_def(parser,struct_def,flags);
    add_type_definition(itl, type_def_kind::struct_t,def, name.literal, parser.context.cur_file,parser.context.cur_namespace);

    return option::none;
}

Result<StructNode*, parse_error> parse_struct_decl(Parser& parser, u32 flags = 0)
{
    const auto name = next_token(parser);
    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,parser.context.cur_file,name);

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

    return struct_node;
}

Option<parse_error> enum_decl(Interloper& itl,Parser& parser, u32 flags)
{
    const auto start_span = make_span(parser.tokens,parser.tok_idx);

    const auto name_tok = next_token(parser);

    if(name_tok.type != token_type::symbol)
    {
        (void)compile_error(itl,itl_error::missing_name,"Expected symbol for enum name got %s",tok_name(name_tok.type));
        return parse_error::itl_error;
    }

    const auto redecl_err = check_redeclaration(itl,parser.context.cur_namespace,name_tok.literal,"enum");
    if(redecl_err)
    {
        return parse_error::itl_error;
    }

    const auto res = scan_brace_stmt(parser,"Enum",name_tok.literal,start_span);
    if(!res)
    {
        return res.error();
    }

    const auto enum_def = *res;

    const auto def = make_top_level_def(parser,enum_def,flags);
    add_type_definition(itl, type_def_kind::enum_t,def, name_tok.literal, parser.context.cur_file,parser.context.cur_namespace);

    return option::none;
}

Result<EnumNode*,parse_error> parse_enum_decl(Parser& parser, u32 flags)
{
    const auto name_tok = next_token(parser);

    EnumNode* enum_node = (EnumNode*)ast_enum(parser,name_tok.literal,parser.context.cur_file,name_tok);

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

    return enum_node;
}
ParserResult parse_for_iter(Parser& parser, const Token& t, b32 term_paren)
{
    // e.g for(i := 0; i < size; i += 1)

    ForIterNode* for_node = ast_for_iter(parser,t);

    // handle first stmt
    // decl 
    if(peek(parser,1).type == token_type::colon)
    {
        auto decl_res = declaration(parser,token_type::semi_colon);
        if(!decl_res)
        {
            return decl_res;
        }

        for_node->initializer = *decl_res;
    }

    // auto decl
    else if(peek(parser,1).type == token_type::decl)
    {
        auto decl_res = auto_decl(parser);
        if(!decl_res)
        {
            return decl_res;
        }

        for_node->initializer = *decl_res;  
    }

    // standard stmt
    else
    {
        auto stmt_res = statement_terminate(parser,"for initializer statement");
        if(!stmt_res)
        {
            return stmt_res;
        }

        for_node->initializer = *stmt_res;
    }
    
    auto cond_res = statement_terminate(parser,"for condition"); 
    if(!cond_res)
    {
        return cond_res;
    }

    for_node->cond = *cond_res;

    // allow paren terminator followed by a '{'
    if(term_paren)
    {
        auto post_res = expr_terminate(parser,"for post statement",token_type::right_paren);
        if(!post_res)
        {
            return post_res;
        }

        for_node->post = *post_res;

        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            return parser_error(parser,parse_error::invalid_terminator,next,"invalid iter for statement terminator: %s expected {\n",
                tok_name(next.type));                      
        }
    }

    // statement was not wrapped by parens
    // expect brace to end it
    else
    {
        auto post_res = expr_terminate(parser,"for post statement",token_type::left_c_brace);
        if(!post_res)
        {
            return post_res;
        }

        for_node->post = *post_res;
        prev_token(parser);
    }  
    
    auto block_err = block_ast(parser,&for_node->block);
    if(block_err)
    {
        return *block_err;
    }

    return (AstNode*)for_node;
}

ParserResult parse_for_range(Parser& parser,const Token& t, b32 term_paren, b32 take_index, b32 take_pointer)
{
    // e.g
    // for(i in 0 <= size)
    // for([v, i] in arr)
    // for(v in arr)
    // for(@v in arr)
    ForRangeNode* for_node = ast_for_range(parser,t);

    if(take_index)
    {
        const auto sl_err =consume(parser,token_type::sl_brace);
        if(sl_err)
        {
            return *sl_err;
        }
    }

    if(take_pointer)
    {
        const auto deref_err = consume(parser,token_type::deref);
        if(deref_err)
        {
            return *deref_err;
        }

        for_node->flags = RANGE_FOR_TAKE_POINTER;
    }


    const auto name_one = next_token(parser);

    if(name_one.type != token_type::symbol)
    { 
        return parser_error(parser,parse_error::missing_expr,name_one,"Expected name for range for statement");
    }


    for_node->name_one = name_one.literal;

    // get the 2nd name
    if(take_index)
    {
        const auto comma_err = consume(parser,token_type::comma);
        if(comma_err)
        {
            return *comma_err;
        }

        const auto name_two = next_token(parser);

        if(name_two.type != token_type::symbol)
        { 
            return parser_error(parser,parse_error::missing_expr,name_two,"Expected name for range for statement");
        }

        for_node->name_two = name_two.literal;

        const auto sr_err = consume(parser,token_type::sr_brace);
        if(sr_err)
        {
            return *sr_err;
        }
    }

    const auto in_err = consume(parser,token_type::in_t);
    if(in_err)
    {
        return *in_err;
    }

    if(term_paren)
    { 
        auto cond_res = expr_terminate(parser,"for range cond",token_type::right_paren);
        if(!cond_res)
        {
            return cond_res;
        }

        for_node->cond = *cond_res;
        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            return parser_error(parser,parse_error::malformed_stmt,next,"invalid range for statement terminator: %s expected {\n",tok_name(next.type));                      
        }
    }

    else
    {
        auto cond_res = expr_terminate(parser,"for range cond statement",token_type::left_c_brace);
        if(!cond_res)
        {
            return cond_res;
        }

        for_node->cond = *cond_res;
        prev_token(parser);
    }

    auto block_err = block_ast(parser,&for_node->block);
    if(block_err)
    {
        return *block_err;
    }

    return (AstNode*)for_node;
}

ParserResult parse_for(Parser& parser, const Token& t)
{
    // allow statement to wrapped a in a set of parens
    const bool term_paren = peek(parser,0).type == token_type::left_paren;

    // ignore the first paren
    if(term_paren)
    {
        const auto left_paren_err = consume(parser,token_type::left_paren);
        if(left_paren_err)
        {
            return *left_paren_err;
        }
    }

    // check for range for
    // first look for tuple
    // [v, i] in arr
    if(peek(parser,0).type == token_type::sl_brace)
    {
        // pointer in tuple
        // [@v, i] in arr
        if(peek(parser,1).type == token_type::deref)
        {
            return parse_for_range(parser,t,term_paren,true,true);
        }

        // [v, i] in arr
        else
        {
            return parse_for_range(parser,t,term_paren,true,false);
        }
    }

    // potential pointer for array iter
    // just @v in arr
    else if(peek(parser,0).type == token_type::deref)
    {
        return parse_for_range(parser,t,term_paren,false,true);
    }

    // check for idx range
    // i in 0 < size
    else if(peek(parser,1).type == token_type::in_t)
    {
        return parse_for_range(parser,t,term_paren,false,false);
    }

    // must be iter for
    else
    {
        return parse_for_iter(parser,t,term_paren);
    }
}



Result<Case,parse_error> make_case(Parser& parser, AstNode* statement)
{
    Case case_stmt;

    case_stmt.statement = statement;
    auto block_res = block_ast_unpinned(parser);
    if(!block_res)
    {
        return block_res.error();
    }

    case_stmt.block = *block_res;
    return case_stmt;
}

ParserResult parse_switch(Parser& parser, const Token& t)
{
    auto expr_res = expr_terminate(parser,"switch statement",token_type::left_c_brace);
    if(!expr_res)
    {
        return expr_res;
    }

    SwitchNode* switch_node = ast_switch(parser,*expr_res,t);

    // while we haven't exhausted every case
    while(!match(parser,token_type::right_c_brace))
    {
        const auto case_tok = peek(parser,0);


        if(case_tok.type == token_type::default_t)
        {
            if(switch_node->default_statement)
            {
                return parser_error(parser,parse_error::malformed_stmt,case_tok,"Cannot have two default statements in switch statement\n");
            }

            (void)consume(parser,token_type::default_t);
            const auto colon_err = consume(parser,token_type::colon);
            if(colon_err)
            {
                return *colon_err;
            }


            const auto case_res = make_case(parser,nullptr);
            if(case_res)
            {
                return case_res.error();
            }

            switch_node->default_statement = *case_res;
        }

        else
        {
            // read out the case
            const auto case_err = consume(parser,token_type::case_t);
            if(case_err)
            {
                return *case_err;
            }

            auto case_expr_res = expr_terminate(parser,"switch case",token_type::colon);
            if(!case_expr_res)
            {
                return case_expr_res;
            }

            AstNode* case_expr = *case_expr_res; 
            auto case_res = make_case(parser,case_expr);
            if(!case_res)
            {
                return case_res.error();
            }

            push_var(switch_node->statements,*case_res);
        }
    }

    const auto c_brace_err = consume(parser,token_type::right_c_brace);
    if(c_brace_err)
    {
        return *c_brace_err;
    }
    return (AstNode*)switch_node;
}

IfStmt make_if_stmt(AstNode* expr, AstBlock* block)
{
    return IfStmt {expr,block};
}

ParserResult parse_if(Parser& parser, const Token& t)
{
    IfNode* if_node = ast_if(parser,t);

    auto expr_res = expr_terminate(parser,"if condition statement",token_type::left_c_brace); prev_token(parser); 
    if(!expr_res)
    {
        return expr_res;
    }

    auto body_res = block_ast_unpinned(parser);
    if(!body_res)
    {
        return body_res.error();
    }

    if_node->if_stmt = make_if_stmt(*expr_res,*body_res);

    bool done = false;
    
    while(!done)
    {
        if(peek(parser,0).type == token_type::else_t)
        {
            (void)consume(parser,token_type::else_t);

            // we have an else if
            if(peek(parser,0).type == token_type::if_t)
            {
                (void)consume(parser,token_type::if_t);

                auto expr_res = expr_terminate(parser,"else if condition statement",token_type::left_c_brace); prev_token(parser);
                if(!expr_res)
                {
                    return expr_res;
                }

                auto body_res = block_ast_unpinned(parser);
                if(!body_res)
                {
                    return body_res.error();
                }

                push_var(if_node->else_if_stmt,make_if_stmt(*expr_res,*body_res));
            }

            // just a plain else
            else
            {
                auto block_err = block_ast(parser,&if_node->else_stmt);
                if(block_err)
                {
                    return *block_err;
                }

                done = true;
            }
        }

        // this chain is done we have another token
        else
        {
            done = true;
        }
    }
    return (AstNode*)if_node;
}

ParserResult parse_while(Parser& parser,const Token& t)
{
    auto expr_res = expr_terminate(parser,"while condition statement",token_type::left_c_brace); prev_token(parser); 
    if(!expr_res)
    {
        return expr_res;
    }

    WhileNode* while_node = ast_while(parser,*expr_res,t);

    auto body_err = block_ast(parser,&while_node->block);
    if(body_err)
    {
        return *body_err;
    }

    return (AstNode*)while_node;
}


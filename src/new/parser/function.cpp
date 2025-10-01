void add_func(Interloper& itl, const String& name, NameSpace* name_space, FuncNode* root);

ParserResult func_call(Parser& parser,AstNode *expr, const Token& t)
{
    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(!!left_paren_err)
    {
        return *left_paren_err;
    }

    FuncCallNode* func_call = (FuncCallNode*)ast_call(parser,expr,t);

    // keep reading args till we run out of commas
    b32 done = false;

    // empty call we are done
    if(match(parser,token_type::right_paren))
    {
        (void)consume(parser,token_type::right_paren);
        done = true;
    }

    while(!done)
    {
        auto list_res = expr_list(parser,"function call",token_type::right_paren,&done);
        if(!list_res)
        {
            return list_res.error();
        }

        push_var(func_call->args,*list_res);
    }

    return (AstNode*)func_call;
}


Result<AstBlock,parse_error> block_ast(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    const auto lc_brace_err = consume(parser,token_type::left_c_brace);
    if(!!lc_brace_err)
    {
        return *lc_brace_err;
    }

    AstBlock block = make_block_ast(parser);

    // parse out all our statements
    while(!match(parser,token_type::right_c_brace))
    {
        if(match(parser,token_type::eof))
        {
            return parser_error(parser,parse_error::malformed_stmt,tok,"unterminated block!\n");
        }

        auto stmt_res = statement(parser);

        if(!stmt_res)
        {
            return stmt_res.error();
        }

        push_var(block.statement,*stmt_res);
    }
    
    const auto rc_brace_err = consume(parser,token_type::right_c_brace);
    if(!!rc_brace_err)
    {
        return *rc_brace_err;
    }
    return block;
}


// parse just the function signature
// NOTE: this is used to parse signatures for function pointers
Result<FuncNode*,parse_error> parse_func_sig(Parser& parser,const String& func_name, const Token& token)
{
    FuncNode *f = (FuncNode*)ast_func(parser,func_name,parser.cur_file,token);

    const auto paren = peek(parser,0);
    const auto l_paren_err = consume(parser,token_type::left_paren);
    if(!!l_paren_err)
    {
        return *l_paren_err;
    }

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            return parser_error(parser,parse_error::invalid_terminator,paren,"unterminated function declaration!\n");
        }

        // for each arg pull type, name
        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            return parser_error(parser,parse_error::unexpected_token,lit_tok,"expected name for function arg got %s\n",
                tok_name(lit_tok.type));
        }
        

        const auto colon_err = consume(parser,token_type::colon);
        if(!!colon_err)
        {
            return *colon_err;
        }

        // va_args
        if(match(parser,token_type::va_args))
        {
            (void)consume(parser,token_type::va_args);

            f->va_args = true;
            f->args_name = lit_tok.literal;

            // by definiton this must be the last arg!
            if(!match(parser,token_type::right_paren))
            {
                return parser_error(parser,parse_error::malformed_stmt,lit_tok,"va_args can only be placed as the last arg : got %s\n",
                    tok_name(peek(parser,0).type));
            }
        }

        else
        {
            auto type_res = parse_type(parser);

            if(!type_res)
            {
                return type_res.error();
            }

            // add each declartion
            DeclNode* decl = (DeclNode*)ast_decl(parser,lit_tok.literal,*type_res,false,lit_tok);
            
            push_var(f->args,decl);

            // if the declaration isnt closed get the next arg
            if(!match(parser,token_type::right_paren))
            {
                const auto comma_err = consume(parser,token_type::comma);
                if(!!comma_err)
                {
                    return *comma_err;
                }
            }
        }
    }

    const auto right_paren_err = consume(parser,token_type::right_paren);
    if(!!right_paren_err)
    {
        return *right_paren_err;
    }

    // tuple type
    if(match(parser,token_type::sl_brace))
    {
        (void)consume(parser,token_type::sl_brace);

        while(!match(parser,token_type::sr_brace))
        {
            if(match(parser,token_type::eof))
            {
                return parser_error(parser,parse_error::invalid_terminator,paren,"unterminated function declaration!\n");
            }
            
            auto return_type_res = parse_type(parser);

            if(!return_type_res)
            {
                return return_type_res.error();
            }

            push_var(f->return_type,*return_type_res);

            if(!match(parser,token_type::sr_brace))
            {
                const auto sr_brace_err = consume(parser,token_type::comma);
                if(!!sr_brace_err)
                {
                    return *sr_brace_err;
                }
            }
        }

        const auto sr_brace_err = consume(parser,token_type::sr_brace);
        if(!!sr_brace_err)
        {
            return *sr_brace_err;
        }
    }

    // single type
    else if(!match(parser,token_type::left_c_brace) && !match(parser,token_type::semi_colon))
    {
        auto return_type_res = parse_type(parser);

        if(!return_type_res)
        {
            return return_type_res.error();
        }

        push_var(f->return_type,*return_type_res);
    }

    // void
    else
    {
        TypeNode* return_type = (TypeNode*)ast_type_decl(parser,nullptr,"void",token); 
        return_type->builtin = builtin_type::void_t;
        return_type->kind = type_node_kind::builtin;

        push_var(f->return_type,return_type);
    }

    return f;
}

Option<parse_error> func_decl(Interloper& itl, Parser &parser, u32 flags)
{
    // func_dec = func ident(arg...) return_type 
    // arg = ident : type,

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
    }

    if(!!check_redeclaration(itl,parser.cur_namespace,func_name.literal,"function"))
    {
        return parse_error::itl_error;
    }

    auto func_res = parse_func_sig(parser,func_name.literal,func_name);

    if(!func_res)
    {
        return func_res.error();
    }

    FuncNode* func = *func_res;
    func->attr_flags = flags;

    auto block_res = block_ast(parser);
    if(!block_res)
    {
        return block_res.error();
    }

    func->block = *block_res; 

    // finally add the function def
    add_func(itl,func_name.literal,parser.cur_namespace,func);
    return option::none;
}

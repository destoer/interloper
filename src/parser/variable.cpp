Result<NameSpace*,parse_error> parse_name_space(Parser& parser)
{
    // See if this type is name spaced
    NameSpace* name_space = nullptr;

    // We have a namespace to parse
    if(peek(parser,1).type == token_type::scope)
    {
        auto strings_res = split_namespace(parser,peek(parser,0));

        if(!strings_res)
        {
            return strings_res.error();
        }

        auto strings = *strings_res;

        name_space = scan_namespace(parser,strings);

        destroy_arr(strings);
    }

    return name_space;
}

Result<TypeNode*,parse_error> parse_type(Parser &parser, b32 allow_fail)
{
    // parse out any specifiers
    auto specifier = peek(parser,0);

    // const on base type
    b32 is_const = false;

    // const all the way down e.g both pointer and pointed type are read only
    b32 is_constant = false;

    switch(specifier.type)
    {
        case token_type::const_t:
        {
            next_token(parser);
            is_const = true;
            break;
        }

        case token_type::constant_t:
        {
            next_token(parser);
            is_constant = true;
            break;
        }


        // no specifier
        default: break;
    }

    auto name_space_res = parse_name_space(parser);

    if(!name_space_res)
    {
        return name_space_res.error();
    }

    NameSpace* name_space = *name_space_res;

    // read out the plain type

    auto plain_tok = next_token(parser);

    TypeNode* type = (TypeNode*)ast_type_decl(parser,name_space,"",plain_tok);

    type->is_const = is_const;
    type->is_constant = is_constant;

    // plain type
    if(is_builtin_type_tok(plain_tok))
    {
        builtin_type builtin = builtin_type_from_tok(plain_tok);

        if(!allow_fail && builtin == builtin_type::null_t)
        {
            return parser_error(parser,parse_error::unexpected_token,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        }

        type->name = TYPE_NAMES[u32(builtin)];
        type->kind = type_node_kind::builtin;
        type->builtin = builtin;
    }

    // function pointer
    else if(plain_tok.type == token_type::func)
    {
        TypeNode* type = (TypeNode*)ast_type_decl(parser,nullptr,"func_pointer",plain_tok);
        type->kind = type_node_kind::func_pointer;

        auto sig_res = parse_func_sig(parser,"func_pointer",plain_tok);
        if(!sig_res)
        {
            return sig_res.error();
        }

        type->func_type = *sig_res; 
        return type;
    }

    // we might not know what this is yet so we will resolve the idx properly later...
    else if(plain_tok.type == token_type::symbol)
    {
        type->kind = type_node_kind::user;
        type->name = plain_tok.literal;
    }

    else
    {
        return parser_error(parser,parse_error::unexpected_token,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
    }    

    b32 quit = false;

    while(!quit)
    {
        // parse out other types, arrays, pointers
        switch(peek(parser,0).type)
        {
            // pointer decl
            case token_type::deref:
            {
                const auto err = consume(parser,token_type::deref);
                if(err)
                {
                    return *err;
                }

                push_var(type->compound,make_compound_type(compound_type::ptr));
                break;
            }

            // Nullable pointer decl
            case token_type::qmark:
            {
                const auto err = consume(parser,token_type::qmark);
                if(err)
                {
                    return *err;
                }

                push_var(type->compound,make_compound_type(compound_type::nullable_ptr));
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                while(peek(parser,0).type == token_type::sl_brace)
                {
                    const auto err = consume(parser,token_type::sl_brace);
                    if(err)
                    {
                        return *err;
                    }

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        push_var(type->compound,make_compound_type(compound_type::arr_var_size));
                        const auto err = consume(parser,token_type::sr_brace);
                        if(err)
                        {
                            return *err;
                        }
                    }

                    else 
                    {
                        // figure out this size later
                        if(peek(parser,0).type == token_type::qmark)
                        {
                            const auto qmark_err = consume(parser,token_type::qmark);
                            if(qmark_err)
                            {
                                return *qmark_err;
                            }

                            push_var(type->compound,make_compound_type(compound_type::arr_deduce_size));
                        
                            const auto sr_err = consume(parser,token_type::sr_brace);
                            if(sr_err)
                            {
                                return *sr_err;
                            }
                        }

                        else
                        {
                            auto e_res = expr_terminate(parser,"array declaration",token_type::sr_brace);

                            if(!e_res)
                            {
                                return e_res.error();
                            }

                            push_var(type->compound,make_compound_type_fixed(*e_res));
                        }
                    }
                }

                break;
            }

            default: 
            {
                quit = true; 
                break;
            }
        }
    }

    return type;
}

ParserResult parse_struct_initializer(Parser &parser)
{
    const auto struct_name = next_token(parser);

    const auto list_res = statement_terminate(parser,"struct assign initializer list");

    if(!list_res)
    {
        return list_res;
    }

    auto list = *list_res;

    if(list->type != ast_type::initializer_list && list->type != ast_type::designated_initializer_list)
    {
        return parser_error(parser,parse_error::malformed_stmt,struct_name,"Expected initializer list for struct initializer\n");
    }

    return ast_struct_initializer(parser,struct_name.literal,list,nullptr,struct_name); 
}

ParserResult declaration(Parser &parser, token_type terminator, b32 is_const_decl = false)
{
    // declaration
    // symbol ':' type ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,s,"declaration expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
    }

    const auto colon_err = consume(parser,token_type::colon);
    if(colon_err)
    {
        return *colon_err;
    }

    auto type_res = parse_type(parser);

    if(!type_res)
    {
        return type_res.error();
    }

    TypeNode* type = *type_res;

    //    [declare:name]
    // [type]   optional([equals])

    DeclNode* decl = (DeclNode*)ast_decl(parser,s.literal,type,is_const_decl,s);

    const auto eq = peek(parser,0);

    switch(eq.type)
    {
        // declaration with assignment
        case token_type::equal:
        {
            const auto err = consume(parser,token_type::equal);
            if(err)
            {
                return *err;
            }

            // Struct assign
            if(match(parser,token_type::symbol) && peek(parser,1).type == token_type::left_c_brace)
            {
                auto initializer_res = parse_struct_initializer(parser);

                if(!initializer_res)
                {
                    return initializer_res;
                }

                decl->expr = *initializer_res;
            }
            
            else
            {
                auto stmt_res = statement_terminate(parser,"declaration expression");

                if(!stmt_res)
                {
                    return stmt_res;
                }

                decl->expr = *stmt_res;
            }
            break;
        }

        default:
        {
            if(eq.type == terminator)
            {
                (void)consume(parser,terminator);
            }

            else
            {
                return parser_error(parser,parse_error::invalid_terminator,eq,"malformed declaration: got %s expected terminator %s\n",
                    tok_name(eq.type),tok_name(terminator));
            }
            break;
        }
    }

    return (AstNode*)decl;
}

ParserResult auto_decl(Parser &parser)
{
    // symbol := expr;
    const auto sym = next_token(parser);

    if(sym.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,sym,"declaration expected symbol got: %s:%zd\n",tok_name(sym.type),parser.tok_idx);
    }

    const auto err = consume(parser,token_type::decl);
    if(err)
    {
        return *err;
    }

    // Struct assign
    if(match(parser,token_type::symbol) && peek(parser,1).type == token_type::left_c_brace)
    {
        auto initializer_res = parse_struct_initializer(parser);

        if(!initializer_res)
        {
            return initializer_res;
        }

        return ast_auto_decl(parser,sym.literal,*initializer_res,sym);
    }


    auto expr_res = statement_terminate(parser,"auto declaration");

    if(!expr_res)
    {
        return expr_res;
    }

    return ast_auto_decl(parser,sym.literal,*expr_res,sym);
}


ParserResult tuple_assign(Parser& parser, const Token& t)
{
    TupleAssignNode* tuple_node = (TupleAssignNode*)ast_tuple_assign(parser,t);

    // generalise this so we can pick up on a ',' being a "terminator"
    // in other expressions
    while(!match(parser,token_type::eof))
    {
        const auto sym_tok = next_token(parser);

        AstNode* sym_node = nullptr;

        switch(sym_tok.type)
        {
            case token_type::symbol:
            {
                auto var_res = var(parser,nullptr,sym_tok);

                if(!var_res)
                {
                    return var_res;
                }

                sym_node = *var_res;
                break;
            }

            case token_type::ignore:
            {
                sym_node = ast_plain(parser,ast_type::ignore,sym_tok);
                break;
            }

            case token_type::deref:
            {
                const Token deref_tok = next_token(parser);

                auto unary_res = ast_deref(parser,var(parser,nullptr,deref_tok),sym_tok);
                
                if(!unary_res)
                {
                    return unary_res;
                }

                sym_node = *unary_res;
                break;
            }

            default:
            {
                return parser_error(parser,parse_error::unexpected_token,t,"tuple assignment attempted on non symbol: %s\n",
                    tok_name(sym_tok.type));
            }
        }

        const TupleAssignSymbol sym = {sym_node,{INVALID_HANDLE}};
        push_var(tuple_node->symbols,sym);

        const auto delim = next_token(parser);

        switch(delim.type)
        {
            // keep going
            case token_type::comma:
            {
                break;
            }

            // end of tuple
            case token_type::sr_brace:
            {
                if(match(parser,token_type::decl))
                {
                    const auto err = consume(parser,token_type::decl);
                    if(err)
                    {
                        return *err;
                    }
                    tuple_node->auto_decl = true;
                }

                else
                {
                    const auto err = consume(parser,token_type::equal);
                    if(err)
                    {
                        return *err;
                    }
                }

                auto next = next_token(parser);

                NameSpace* name_space = nullptr;

                // handle scope
                if(match(parser,token_type::scope))
                {
                    prev_token(parser);
                    const auto namespace_res = split_namespace(parser,next);
                    
                    if(!namespace_res)
                    {
                        return namespace_res.error();
                    }

                    name_space = scan_namespace(parser,*namespace_res);

                    next = next_token(parser);
                }


                auto func_call_res = func_call(parser,ast_symbol(parser,name_space,next.literal,next),next);
                if(!func_call_res)
                {
                    return func_call_res;
                }

                tuple_node->func_call = (FuncCallNode*)*func_call_res;
                const auto err = consume(parser,token_type::semi_colon);
                if(err)
                {
                    return *err;
                }

                return (AstNode*)tuple_node;
            }

            // something has gone wrong
            default: 
            {
                return parser_error(parser,parse_error::malformed_stmt,t,"malformed tuple statement"); 
            }
        }
    }

    return (AstNode*)tuple_node;    
}

AccessMember make_access_member_expr(AstNode* expr)
{
    AccessMember member;
    member.expr = expr;
    member.type = member_access_type::index_t;

    return member;
}

AccessMember make_access_member_name(const String& name)
{
    AccessMember member;
    member.name = name;
    member.type = member_access_type::struct_t;


    return member;
}

ParserResult parse_user_type_info(Parser& parser, NameSpace* name_space, const String& type_name, const Token& token)
{
    const auto dot_err = consume(parser,token_type::dot);
    if(dot_err)
    {
        return *dot_err;
    }

    if(!match(parser,token_type::symbol))
    {
        return parser_error(parser,parse_error::malformed_stmt,token,"Expected symbol for user type info got %s\n",tok_name(peek(parser,0).type));
    }

    if(match(parser,token_type::dot))
    {
        return parser_error(parser,parse_error::malformed_stmt,token,"User type access should only have one member %S",type_name);
    }

    auto member_name = next_token(parser);
    return ast_user_type_info_access(parser,name_space,type_name,member_name.literal,token);
}   

ParserResult struct_access(Parser& parser, AstNode* expr_node,const Token& t)
{
    StructAccessNode* struct_access = ast_struct_access(parser,expr_node,t);

    while(match(parser,token_type::dot))
    {
        (void)consume(parser,token_type::dot);

        const auto member_tok = next_token(parser);

        if(member_tok.type == token_type::symbol)
        {
            // perform peeking for modifiers
            if(match(parser,token_type::sl_brace))
            {
                const auto slice_opt = try_parse_slice(parser,member_tok);

                if(slice_opt)
                {
                    const auto slice = *slice_opt;

                    if(!slice)
                    {
                        return slice;
                    }

                    push_var(struct_access->members,make_access_member_expr(*slice));
                }

                else
                {
                    auto index_res = array_index(parser,member_tok);
                    if(!index_res)
                    {
                        return index_res;
                    }

                    push_var(struct_access->members,make_access_member_expr(*index_res));
                }
            }

            // plain old member
            else
            {
                push_var(struct_access->members,make_access_member_name(member_tok.literal));
            }
        }

        else
        {
            return parser_error(parser,parse_error::unexpected_token,member_tok,"expected struct member got %S(%s)\n",
                member_tok.literal,tok_name(member_tok.type));           
        }
    }

    return (AstNode*)struct_access;
}


ParserResult array_index(Parser& parser,const Token& t)
{
    IndexNode* arr_access = (IndexNode*)ast_index(parser,t.literal,t);

    while(match(parser,token_type::sl_brace))
    {
        (void)consume(parser,token_type::sl_brace);

        auto expr_res = expr_terminate(parser,"array indexing",token_type::sr_brace);

        if(!expr_res)
        {
            return expr_res;
        }

        push_var(arr_access->indexes,*expr_res);
    }

    return (AstNode*)arr_access;
}

ParserResult arr_slice(Parser& parser,const Token& t)
{
    const auto sl_err =consume(parser,token_type::sl_brace);
    if(sl_err)
    {
        return *sl_err;
    }

    SliceNode* slice_node = (SliceNode*)ast_slice(parser,t.literal,t);

    // check if left side is empty.
    if(match(parser,token_type::colon))
    {
        (void)consume(parser,token_type::colon);
    }

    else
    {
        auto lower_res = expr_terminate(parser,"slice lower",token_type::colon);
        if(!lower_res)
        {
            return lower_res;
        }

        slice_node->lower = *lower_res;
    }

    // check if right side is empty
    if(match(parser,token_type::sr_brace))
    {
        if(!slice_node->lower)
        {
            return parser_error(parser,parse_error::missing_expr,t,"Array slice with empty lower and upper bounds!");
        }

        (void)consume(parser,token_type::sr_brace);
    }

    else
    {
        auto upper_res = expr_terminate(parser,"slice upper",token_type::sr_brace);
        if(!upper_res)
        {
            return upper_res;
        }

        slice_node->upper = *upper_res;
    }

    return (AstNode*)slice_node;
}

Option<ParserResult> try_parse_slice(Parser& parser, const Token& t)
{
    // Check for a ':' to detect slicing.
    // For now only support it on the first index.
    u32 peek_offset = 0;
    while(peek(parser,peek_offset).type != token_type::sr_brace)
    {
        if(peek(parser,peek_offset).type == token_type::colon)
        {
            return arr_slice(parser,t);
        }
        peek_offset += 1;
    }

    return option::none;
}

ParserResult arr_access(Parser& parser, const Token& t)
{
    const auto slice_opt = try_parse_slice(parser,t);

    if(slice_opt)
    {
        return *slice_opt;
    }

    // Standard array access.
    auto index_res = array_index(parser,t);

    if(!index_res)
    {
        return index_res;
    }

    AstNode* arr_access = *index_res;

    if(match(parser,token_type::dot))
    {
        return struct_access(parser,arr_access,t);
    }

    else
    {
        return arr_access;
    }
}

ParserResult var(Parser& parser, NameSpace* name_space, const Token& sym_tok, b32 allow_call)
{
    const Token next = peek(parser,0);

    AstNode* node = nullptr;

    switch(next.type)
    {
        case token_type::dot:
        {   
            if(parser_type_exists(parser,name_space,sym_tok.literal))
            {
                return parse_user_type_info(parser,name_space,sym_tok.literal,sym_tok);
            }

            auto access_res = struct_access(parser,ast_symbol(parser,name_space,sym_tok.literal,sym_tok),sym_tok);
            if(!access_res)
            {
                return access_res;
            }

            node = *access_res;
            break;
        }

        case token_type::sl_brace:
        {
            assert(!name_space);
            
            auto access_res = arr_access(parser,sym_tok);
            if(!access_res)
            {
                return access_res;
            }

            node = *access_res;
            break;
        }


        default:
        {
            node = ast_symbol(parser,name_space,sym_tok.literal,sym_tok);
            break;
        }
    }

    // function pointer call
    if(match(parser,token_type::left_paren) && allow_call)
    {
        return func_call(parser,node,sym_tok);
    }

    return node;
}

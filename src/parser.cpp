#include <interloper.h>
#include <unistd.h>
#include "expression.cpp"

Result<BlockNode*,parse_error> block(Parser &parser);
ParserResult block_ast(Parser &parser);

Result<FuncNode*,parse_error> parse_func_sig(Parser& parser, const String& func_name,const Token& token);

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);
static constexpr u32 ATTR_USE_RESULT = (1 << 2);

const u32 AST_ALLOC_DEFAULT_SIZE = 8 * 1024;

Parser make_parser(const String& cur_file,NameSpace* root, ArenaAllocator* namespace_allocator,
    ArenaAllocator *global_string_allocator,ArenaAllocator* ast_allocator,ArenaAllocator* string_allocator, AstPointers* ast_arrays)
{
    Parser parser;
    parser.ast_allocator = ast_allocator;
    parser.string_allocator = string_allocator;
    parser.global_string_allocator = global_string_allocator;
    parser.namespace_allocator = namespace_allocator;

    // NOTE: this relies on get_program_name to allocate the string correctly
    parser.cur_file = cur_file;
    parser.cur_path = extract_path(parser.cur_file);
    parser.ast_arrays = ast_arrays;
    parser.global_namespace = root;
    parser.cur_namespace = parser.global_namespace;

    return parser;
}

void add_ast_pointer(Parser& parser, void* pointer)
{
    push_raw_var(*parser.ast_arrays,pointer);
}

Token next_token(Parser &parser)
{
    if(parser.tok_idx >= count(parser.tokens))
    {
        // TODO: make this return the actual file end
        // for row and col
        return token_plain(token_type::eof,0);
    }

    return parser.tokens[parser.tok_idx++];  
}

void prev_token(Parser &parser)
{
    if(parser.tok_idx != 0)
    {
        parser.tok_idx -= 1;
    }
}


Token peek(Parser &parser,u32 v)
{
    const auto idx = parser.tok_idx + v;
    if(idx >= count(parser.tokens))
    {
        return token_plain(token_type::eof,0);
    }

    return parser.tokens[idx];
}



Option<parse_error> consume(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        return parser_error(parser,parse_error::unexpected_token,tok,"expected '%s' got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
    return option::none;
}

bool match(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    return t == type;
}

bool is_builtin_type_tok(const Token &tok)
{
   return tok.type >= token_type::u8 && tok.type <= token_type::f64_t; 
}

builtin_type builtin_type_from_tok(const Token& tok)
{
    // compute builtin type idx 
    return builtin_type(s32(tok.type) - s32(token_type::u8));
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

        name_space = scan_namespace(parser.global_namespace,strings);

        // Namespace does not allready exist create it!
        if(!name_space)
        {
            name_space = new_named_scope(*parser.namespace_allocator,*parser.global_string_allocator,parser.global_namespace,strings);
        }

        destroy_arr(strings);
    }

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
                if(!!err)
                {
                    return *err;
                }

                push_var(type->compound_type,ast_plain(parser,ast_type::ptr_indirection,plain_tok));
                break;
            }

            // Nullable pointer decl
            case token_type::qmark:
            {
                const auto err = consume(parser,token_type::qmark);
                if(!!err)
                {
                    return *err;
                }

                push_var(type->compound_type,ast_plain(parser,ast_type::nullable_ptr_indirection,plain_tok));
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                while(peek(parser,0).type == token_type::sl_brace)
                {
                    const auto err = consume(parser,token_type::sl_brace);
                    if(!!err)
                    {
                        return *err;
                    }

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        push_var(type->compound_type,ast_plain(parser,ast_type::arr_var_size,plain_tok));
                        const auto err = consume(parser,token_type::sr_brace);
                        if(!!err)
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
                            if(!!qmark_err)
                            {
                                return *qmark_err;
                            }

                            const auto e = ast_plain(parser,ast_type::arr_deduce_size,plain_tok);
                            push_var(type->compound_type,e);
                        
                            const auto sr_err = consume(parser,token_type::sr_brace);
                            if(!!sr_err)
                            {
                                return *sr_err;
                            }
                        }

                        else
                        {
                            auto e_res = ast_unary(parser,expr_terminate(parser,"array declaration",token_type::sr_brace), ast_type::arr_fixed, plain_tok);

                            if(!e_res)
                            {
                                return e_res.error();
                            }

                            push_var(type->compound_type,*e_res);
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

    if(list->type != ast_type::initializer_list)
    {
        return parser_error(parser,parse_error::malformed_stmt,struct_name,"Expected initializer list for struct initializer");
    }

    return ast_struct_initializer(parser,struct_name.literal,(RecordNode*)list,struct_name); 
}

ParserResult declaration(Parser &parser, token_type terminator, b32 is_const_decl = false)
{
    // declartion
    // symbol ':' type ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,s,"declartion expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
    }

    const auto colon_err = consume(parser,token_type::colon);
    if(!!colon_err)
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
    // [type]   optional([eqauls])

    DeclNode* decl = (DeclNode*)ast_decl(parser,s.literal,type,is_const_decl,s);

    const auto eq = peek(parser,0);

    switch(eq.type)
    {
        // declartion with assingment
        case token_type::equal:
        {
            const auto err = consume(parser,token_type::equal);
            if(!!err)
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
                return parser_error(parser,parse_error::invalid_terminator,eq,"malformed declartion: got %s expected terminator %s\n",
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
        return parser_error(parser,parse_error::unexpected_token,sym,"declartion expected symbol got: %s:%zd\n",tok_name(sym.type),parser.tok_idx);
    }

    const auto err = consume(parser,token_type::decl);
    if(!!err)
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
                auto var_res = var(parser,sym_tok);

                if(!var_res)
                {
                    return var_res;
                }

                sym_node = *var_res;
                break;
            }

            case token_type::deref:
            {
                const Token deref_tok = next_token(parser);

                auto unary_res = ast_unary(parser,var(parser,deref_tok),ast_type::deref,sym_tok);
                
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

        push_var(tuple_node->symbols,sym_node);

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
                    if(!!err)
                    {
                        return *err;
                    }
                    tuple_node->auto_decl = true;
                }

                else
                {
                    const auto err = consume(parser,token_type::equal);
                    if(!!err)
                    {
                        return *err;
                    }
                }

                const auto next = next_token(parser);

                // handle scope
                if(match(parser,token_type::scope))
                {
                    prev_token(parser);
                    const auto namespace_res = split_namespace(parser,next);
                    
                    if(!namespace_res)
                    {
                        return namespace_res.error();
                    }

                    const auto name_space = *namespace_res;

                    const auto sym_tok = next_token(parser);

                    auto func_call_res = func_call(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);

                    if(!func_call_res)
                    {
                        return func_call_res;
                    }

                    tuple_node->func_call = (FuncCallNode*)func_call_res.value();

                    const auto err = consume(parser,token_type::semi_colon);
                    if(!!err)
                    {
                        return *err;
                    }

                    return ast_scope(parser,(AstNode*)tuple_node,name_space,next);
                }

                else
                {
                    auto func_call_res = func_call(parser,ast_literal(parser,ast_type::symbol,next.literal,next),next);
                    if(!func_call_res)
                    {
                        return func_call_res;
                    }

                    tuple_node->func_call = (FuncCallNode*)*func_call_res;
                    const auto err = consume(parser,token_type::semi_colon);
                    if(!!err)
                    {
                        return *err;
                    }

                    return (AstNode*)tuple_node;
                }

                break;
            }

            // something has gone wrong
            default: 
            {
                return parser_error(parser,parse_error::malformed_stmt,t,"malformed tuple statement "); 
            }
        }
    }

    return (AstNode*)tuple_node;    
}

ParserResult struct_access(Parser& parser, AstNode* expr_node,const Token& t)
{
    RecordNode* member_root = (RecordNode*)ast_record(parser,ast_type::access_members,t);

    auto bin_res = ast_binary(parser,expr_node,(AstNode*)member_root,ast_type::access_struct,t);

    if(!bin_res)
    {
        return bin_res;
    }

    BinNode* root = (BinNode*)bin_res.value();

    while(match(parser,token_type::dot))
    {
        (void)consume(parser,token_type::dot);

        const auto member_tok = next_token(parser);

        if(member_tok.type == token_type::symbol)
        {
            // perform peeking for modifers
            if(match(parser,token_type::sl_brace))
            {
                auto index_res = array_index(parser,member_tok);
                if(!index_res)
                {
                    return index_res;
                }

                push_var(member_root->nodes,*index_res);
            }

            // plain old member
            else
            {
                AstNode* member_node = ast_literal(parser,ast_type::access_member, member_tok.literal,member_tok);

                push_var(member_root->nodes,member_node);
            }
        }

        else
        {
            return parser_error(parser,parse_error::unexpected_token,member_tok,"expected struct member got %s(%s)\n",
                member_tok.literal.buf,tok_name(member_tok.type));           
        }
    }

    return (AstNode*)root;
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
    if(!!sl_err)
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

ParserResult arr_access(Parser& parser, const Token& t)
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

ParserResult var(Parser& parser, const Token& sym_tok, b32 allow_call)
{
    const Token next = peek(parser,0);

    AstNode* node = nullptr;

    switch(next.type)
    {
        case token_type::dot:
        {   
            auto access_res = struct_access(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);
            if(!access_res)
            {
                return access_res;
            }

            node = *access_res;
            break;
        }

        case token_type::sl_brace:
        {
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
           node = ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok);
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
        auto list_res = expr_list(parser,"function call",token_type::right_paren);
        if(!list_res)
        {
            return list_res.error();
        }

        auto [node,term_seen] = *list_res;

        push_var(func_call->args,node);

        // no more args terminate the call
        done = term_seen;
    }

    return (AstNode*)func_call;
}

ParserResult const_assert(Parser& parser,const Token& t)
{
    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(!!left_paren_err)
    {
        return *left_paren_err;
    }

    const auto expr = expr_terminate(parser,"const_assert",token_type::right_paren);
    const auto right_paren_err = consume(parser,token_type::semi_colon);
    if(!!right_paren_err)
    {
        return *right_paren_err;
    }

    return ast_unary(parser,expr,ast_type::const_assert,t);       
}

ParserResult parse_for_iter(Parser& parser, const Token& t, b32 term_paren)
{
    // e.g for(i := 0; i < size; i += 1)

    ForIterNode* for_node = (ForIterNode*)ast_for_iter(parser,t);

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
    
    auto block_res = block(parser);
    if(!block_res)
    {
        return block_res.error();
    }

    // for stmt parsed now compile the actual block
    for_node->block = *block_res;

    return (AstNode*)for_node;
}

ParserResult parse_for_range(Parser& parser,const Token& t, b32 term_paren, b32 take_index, b32 take_pointer)
{
    // e.g
    // for(i in 0 <= size)
    // for([v, i] in arr)
    // for(v in arr)
    // for(@v in arr)
    ForRangeNode* for_node = (ForRangeNode*)ast_for_range(parser,t);

    if(take_index)
    {
        const auto sl_err =consume(parser,token_type::sl_brace);
        if(!!sl_err)
        {
            return *sl_err;
        }
    }

    if(take_pointer)
    {
        const auto deref_err = consume(parser,token_type::deref);
        if(!!deref_err)
        {
            return *deref_err;
        }
    }

    for_node->take_pointer = take_pointer;

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
        if(!!comma_err)
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
        if(!!sr_err)
        {
            return *sr_err;
        }
    }

    const auto in_err = consume(parser,token_type::in_t);
    if(!!in_err)
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

    auto block_res = block(parser);
    if(!block_res)
    {
        return block_res.error();
    }

    // for stmt parsed now compile the actual block
    for_node->block = *block_res;

    return (AstNode*)for_node;
}

ParserResult statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        case token_type::ret:
        {
            // return value is optional
            if(!match(parser,token_type::semi_colon))
            {
                if(peek(parser,1).type == token_type::left_c_brace)
                {
                    auto initializer_res = parse_struct_initializer(parser);

                    if(!initializer_res)
                    {
                        return initializer_res;
                    }

                    AstNode* initializer = *initializer_res;
                    initializer->type = ast_type::struct_return;

                    return initializer;
                }

                RecordNode* record = (RecordNode*)ast_record(parser,ast_type::ret,t);
                b32 done = false;

                // can be more than one expr (comma seperated)
                while(!done)
                {
                    auto list_res = expr_list(parser,"return",token_type::semi_colon);
                    if(!list_res)
                    {
                        return list_res.error();
                    }

                    auto [e,term_seen] = *list_res;

                    done = term_seen;
                    push_var(record->nodes,e);
                }

                return (AstNode*)record;
            }

            else
            {
                const auto term_err = consume(parser,token_type::semi_colon);
                if(!!term_err)
                {
                    return *term_err;
                }
                return ast_plain(parser,ast_type::ret,t);
            }
        }

        case token_type::const_assert:
        {
            return const_assert(parser,t); 
        }            


        case token_type::deref:
        {
            prev_token(parser);
            return statement_terminate(parser,"pointer deference");
        }


        // tuple assign
        case token_type::sl_brace:
        {
            return tuple_assign(parser, t);
        }

        case token_type::constant_t:
        {
            return declaration(parser,token_type::semi_colon,true);
        }

        case token_type::symbol:
        {
            const auto t2 = peek(parser,0);

            switch(t2.type)
            {
                // declaration with specified type
                case token_type::colon:
                {
                    prev_token(parser);
                    return declaration(parser,token_type::semi_colon);    
                }

                case token_type::decl:
                {
                    prev_token(parser);
                    return auto_decl(parser);
                }

                // assignment expr
                case token_type::plus_eq:
                case token_type::minus_eq:
                case token_type::divide_eq:
                case token_type::times_eq:
                case token_type::bitwise_or_eq:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"assignment");
                }

                case token_type::equal:
                {
                    const auto sym_tok = t;

                    // Struct assign
                    if(peek(parser,1).type == token_type::symbol && peek(parser,2).type == token_type::left_c_brace)
                    {
                        (void)consume(parser,token_type::equal);
                        AstNode* left = ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok);

                        return ast_binary(parser,left,parse_struct_initializer(parser),ast_type::equal,sym_tok);  
                    }

                    prev_token(parser);
                    return statement_terminate(parser,"assignment");
                }


                // check for brackets
                // array indexes etc here 

                // function call
                case token_type::left_paren:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"function call");
                }

                // array access
                case token_type::sl_brace:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"array access");
                }

                // expr for member access?
                case token_type::dot:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"struct access");
                }

                case token_type::scope:
                {
                    prev_token(parser);
                    return statement_terminate(parser,"scope stmt");
                }


                default:
                {
                    return parser_error(parser,parse_error::unexpected_token,t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                }
            }
            break;
        }

        case token_type::left_c_brace:
        {
            // block expects to see the left c brace
            prev_token(parser);

            return block_ast(parser);
        }

        // assume one cond for now
        case token_type::for_t:
        {

            // allow statment to wrapped a in a set of parens
            const bool term_paren = peek(parser,0).type == token_type::left_paren;

            // ignore the first paren
            if(term_paren)
            {
                const auto left_paren_err = consume(parser,token_type::left_paren);
                if(!!left_paren_err)
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

        case token_type::while_t:
        {
            auto expr_opt = expr_terminate(parser,"while condition statement",token_type::left_c_brace); prev_token(parser); 
            auto body_opt = block_ast(parser);

            return ast_binary(parser,expr_opt,body_opt,ast_type::while_block,t);
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            BlockNode* if_block = (BlockNode*)ast_if_block(parser,t);


            auto expr_opt = expr_terminate(parser,"if condtion statement",token_type::left_c_brace); prev_token(parser); 
            auto body_opt = block_ast(parser);

            auto if_res = ast_binary(parser,expr_opt,body_opt,ast_type::if_t,t);
            if(!if_res)
            {
                return if_res;
            }

            BinNode *if_stmt = (BinNode*)if_res.value();

            push_var(if_block->statements,(AstNode*)if_stmt);
            
            bool done = false;
            
            while(!done)
            {
                if(peek(parser,0).type == token_type::else_t)
                {
                    auto else_tok = next_token(parser);

                    // we have an else if
                    if(peek(parser,0).type == token_type::if_t)
                    {
                        (void)consume(parser,token_type::if_t);

                        auto expr_opt = expr_terminate(parser,"else if condition statement",token_type::left_c_brace); prev_token(parser);
                        auto body_opt = block_ast(parser);

                        auto else_if_res = ast_binary(parser,expr_opt,body_opt,ast_type::else_if_t,else_tok);
                        if(!else_if_res)
                        {
                            return else_if_res;
                        }

                        BinNode* else_if_stmt = (BinNode*)else_if_res.value();
                        push_var(if_block->statements,(AstNode*)else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        auto block_opt = block_ast(parser);
                        auto else_res = ast_unary(parser,block_opt,ast_type::else_t,else_tok);
                        if(!else_res)
                        {
                            return else_res;
                        }

                        AstNode *else_stmt = *else_res;

                        push_var(if_block->statements,else_stmt);

                        done = true;
                    }
                }

                // this chain is done we have another token
                else
                {
                    done = true;
                }
            }
            return (AstNode*)if_block;
        }

        case token_type::switch_t:
        {
            auto expr_res = expr_terminate(parser,"switch statement",token_type::left_c_brace);
            if(!expr_res)
            {
                return expr_res;
            }

            SwitchNode* switch_node = (SwitchNode*)ast_switch(parser,*expr_res,t);

            // while we havent exhaused every case
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
                    if(!!colon_err)
                    {
                        return *colon_err;
                    }

                    auto block_res = block_ast(parser);
                    auto unary_res = ast_unary(parser,block_res,ast_type::default_t,case_tok);    
                    if(!unary_res)
                    {
                        return unary_res;
                    }

                    switch_node->default_statement = (UnaryNode*)*unary_res;  
                }

                else
                {
                    // read out the case
                    const auto case_err = consume(parser,token_type::case_t);
                    if(!!case_err)
                    {
                        return *case_err;
                    }

                    auto case_res = expr_terminate(parser,"switch case",token_type::colon);
                    if(!case_res)
                    {
                        return case_res;
                    }

                    AstNode* case_node = *case_res; 
                    auto block_res = block(parser);
                    if(!block_res)
                    {
                        return block_res.error();
                    }

                    CaseNode* case_statement = (CaseNode*)ast_case(parser,case_node,*block_res,case_tok);
                    push_var(switch_node->statements,case_statement);
                }
            }

            const auto c_brace_err = consume(parser,token_type::right_c_brace);
            if(!!c_brace_err)
            {
                return *c_brace_err;
            }
            return (AstNode*)switch_node;
        }

        // dont care
        case token_type::semi_colon:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"Lone semi colon");
        }

        case token_type::ignore:
        {
            prev_token(parser);
            return statement_terminate(parser,"Ignored assign");
        }

        default:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"statement: unexpected token '%s' : %d\n",tok_name(t.type),u32(t.type));
        }
    }

    assert(false);
    return parse_error::malformed_stmt;
}

Result<BlockNode*,parse_error> block(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    const auto lc_brace_err = consume(parser,token_type::left_c_brace);
    if(!!lc_brace_err)
    {
        return *lc_brace_err;
    }

    BlockNode* b = (BlockNode*)ast_block(parser,tok);

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

        push_var(b->statements,*stmt_res);
    }
    
    const auto rc_brace_err = consume(parser,token_type::right_c_brace);
    if(!!rc_brace_err)
    {
        return *rc_brace_err;
    }
    return b;
}

ParserResult block_ast(Parser &parser)
{
    auto block_res = block(parser);

    if(!block_res)
    {
        return block_res.error();
    }

    return (AstNode*)block_res.value();
}

Option<itl_error> check_redeclaration(Interloper& itl, NameSpace* root, const String& name, const String& checked_def_type)
{
    const DefInfo* existing_def = lookup_definition(root,name);

    if(existing_def)
    {
        return compile_error(itl,itl_error::redeclaration,"%s (%s) has been redeclared as a %s!\n",
            name.buf,definition_type_name(existing_def),checked_def_type.buf);
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
        if(!!eq_res)
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

        if(!!check_redeclaration(itl,parser.cur_namespace,name,"type alias"))
        {
            return parse_error::itl_error;
        }

        AstNode* alias_node = ast_alias(parser,rtype,name,parser.cur_file,token);
    
        const auto term_err = consume(parser,token_type::semi_colon);
        if(!!term_err)
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

    auto block_res = block(parser);
    if(!block_res)
    {
        return block_res.error();
    }

    func->block = *block_res; 

    // finally add the function def
    add_func(itl,func_name.literal,parser.cur_namespace,func);
    return option::none;
}

Option<parse_error> struct_decl(Interloper& itl,Parser& parser, u32 flags = 0)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,name,"expected name after struct decl got %s\n",tok_name(name.type));
    }

    if(!!check_redeclaration(itl,parser.cur_namespace,name.literal,"struct"))
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
    if(!!left_c_brace_err)
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
        (void)compile_error(itl,itl_error::missing_name,"Expected symbol for enum name got %s\n",tok_name(name_tok.type));
        return parse_error::itl_error;
    }

    const auto redecl_err = check_redeclaration(itl,parser.cur_namespace,name_tok.literal,"enum");
    if(!!redecl_err)
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
    if(!!left_c_brace_err)
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
            if(!!comma_err)
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

StringBuffer read_source_file(const String& filename)
{
    auto [file,err] = read_str_buf(filename);
    if(err)
    {
        printf("no such file: %s\n",filename.buf);
        exit(0);
    }    

    return file;
}


struct FileQueue
{
    Set<String> set;
    Array<String> stack;
};

void add_file(FileQueue& queue, const String& filename)
{
    if(!contains(queue.set,filename))
    {
        add(queue.set,filename);
        push_var(queue.stack,filename);
    }
}

String get_program_name(ArenaAllocator& allocator,const String& filename)
{
    if(!contains_ext(filename))
    {
        return cat_string(allocator,filename,".itl");
    }

    return filename;
}


void destroy_parser(Parser& parser)
{
    destroy_arr(parser.tokens);
}

Result<u32,parse_error> parse_attr(Parser& parser, const Token& tok)
{
    u32 flags = 0;

    const auto left_paren_err = consume(parser,token_type::left_paren);
    if(!!left_paren_err)
    {
        return *left_paren_err;
    }

    const auto attr = next_token(parser);

    if(attr.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::unexpected_token,tok,"Expected name for attr got %s\n",tok_name(attr.type));
    }

    const auto attr_name = attr.literal;

    // TODO: change this to a pregenned hashtable if it starts getting big
    if(attr_name == "no_reorder")
    {
        flags |= ATTR_NO_REORDER;
    }

    else if(attr_name == "flag")
    {
        flags |= ATTR_FLAG;
    }

    else if(attr_name == "use_result")
    {
        flags |= ATTR_USE_RESULT;
    }

    else
    {
        return parser_error(parser,parse_error::malformed_stmt,tok,"Unknown attr %s\n",attr_name.buf);
    }

    const auto right_paren_err = consume(parser,token_type::right_paren);
    if(!!right_paren_err)
    {
        return *right_paren_err;
    }

    return flags;
}

Option<parse_error> parse_directive(Interloper& itl,Parser& parser)
{
    const auto next = next_token(parser);

    if(next.type != token_type::symbol)
    {
        return parser_error(parser,parse_error::missing_expr,next,"Expected name for directive got %s\n",tok_name(next.type));
    }

    // TODO: move this lookup to a hashtable if it starts getting large
    const auto name = next.literal;

    if(name == "attr")
    {
        const auto flags_res = parse_attr(parser,next);

        if(!flags_res)
        {
            return flags_res.error();
        }

        const u32 flags = *flags_res;

        const auto stmt = peek(parser,0);

        // look at which declaration is next?
        switch(stmt.type)
        {
            case token_type::struct_t:
            {
                (void)consume(parser,token_type::struct_t);
                
                const auto struct_decl_err = struct_decl(itl,parser,flags);
                if(!!struct_decl_err)
                {
                    return struct_decl_err;
                }
                break;
            }

            case token_type::enum_t:
            {
                (void)consume(parser,token_type::enum_t);

                const auto enum_decl_err = enum_decl(itl,parser,flags);
                if(!!enum_decl_err)
                {
                    return enum_decl_err;
                } 
                break; 
            }

            case token_type::func:
            {
                (void)consume(parser,token_type::func);

                const auto func_err = func_decl(itl,parser,flags);
                if(!!func_err)
                {
                    return func_err;
                }
                break;        
            }


            default:
            {
                return parser_error(parser,parse_error::malformed_stmt,stmt,"Attribute is not legal on stmt: %s\n",tok_name(stmt.type));
            }
        }
    }

    else
    {
        return parser_error(parser,parse_error::unexpected_token,next,"Unknown directive %s\n",name.buf);
    }

    return option::none;
}

Result<Array<String>,parse_error> split_namespace_internal(Parser& parser, const Token& start, bool full_namespace)
{
    Array<String> name_space;

    while(!match(parser,token_type::eof))
    {
        const auto name = next_token(parser);

        if(name.type != token_type::symbol)
        {
            const auto res = parser_error(parser,parse_error::unexpected_token,name,"Expected name for namespace got: %s\n",tok_name(name.type));
            destroy_arr(name_space);
            return res;
        }   

        push_var(name_space,name.literal);

        if(match(parser,token_type::scope))
        {
            const auto scope_err = consume(parser,token_type::scope);
            if(!!scope_err)
            {
                destroy_arr(name_space);
                return *scope_err;
            }

            // Last token after :: is not to be treated as part of the namespace
            if(peek(parser,1).type != token_type::scope && !full_namespace)
            {
                goto done;
            }
        }

        else
        {
            goto done;
        }
    }

done:
    if(count(name_space) == 0)
    {
        destroy_arr(name_space);
        return parser_error(parser,parse_error::missing_expr,start,"Namespace is empty");
    }

    return name_space;
}

Result<Array<String>,parse_error> split_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,false);
}

Result<Array<String>,parse_error>  split_full_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,true);
}



Option<parse_error> parse_top_level_token(Interloper& itl, Parser& parser, FileQueue& queue)
{
    const auto &t = next_token(parser);

    switch(t.type)
    {
        case token_type::import:
        {
            // stl path: import <name>
            if(match(parser,token_type::logical_lt))
            {
                const auto lt_err = consume(parser,token_type::logical_lt);
                if(!!lt_err)
                {
                    return lt_err;
                }

                if(!match(parser,token_type::symbol))
                {
                    const auto err = next_token(parser);
                    return parser_error(parser,parse_error::missing_expr,next_token(parser),"expected string for import got %s : %s\n",
                        tok_name(err.type),err.literal.buf);
                }

                const auto name_tok = next_token(parser);

                const auto gt_err = consume(parser,token_type::logical_gt);
                if(!!gt_err)
                {
                    return gt_err;
                }

                const auto full_path = cat_string(itl.string_allocator,itl.stl_path,get_program_name(itl.string_allocator,name_tok.literal)); 

                add_file(queue, full_path);
            }

            // relative path: import "boop"
            else if(match(parser,token_type::string))
            {
                const auto name_tok = next_token(parser);

                const auto full_path = cat_string(itl.string_allocator,parser.cur_path,get_program_name(itl.string_allocator,name_tok.literal));

                add_file(queue,full_path);
            }

            // unk
            else
            {
                return parser_error(parser,parse_error::missing_expr,next_token(parser),"expected string for import got %s : %s\n",
                    tok_name(t.type),t.literal.buf);
            }
            break;
        }

        // function declartion
        case token_type::func:
        {
            const auto func_err = func_decl(itl,parser,0);
            if(!!func_err)
            {
                return func_err;
            }
            break;
        }

        case token_type::struct_t:
        {
            const auto struct_err = struct_decl(itl,parser);
            if(!!struct_err)
            {
                return struct_err;
            }
            break;
        }

        case token_type::enum_t:
        {
            const auto enum_err = enum_decl(itl,parser,0);
            if(!!enum_err)
            {
                return enum_err;
            }
            break;
        }

        case token_type::type_alias:
        {
            const auto type_err = type_alias(itl,parser);
            if(!!type_err)
            {
                return type_err;
            }
            break;
        }

        // global constant
        case token_type::constant_t:
        {
            auto decl_res = declaration(parser,token_type::semi_colon,true);
            if(!decl_res)
            {
                return decl_res.error();
            }

            DeclNode* decl = (DeclNode*)decl_res.value();

            GlobalDeclNode* const_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.constant_decl,const_decl);
            break; 
        }

        // global mut
        case token_type::global_t:
        {
            auto decl_res = declaration(parser,token_type::semi_colon);
            if(!decl_res)
            {
                return decl_res.error();
            }

            DeclNode* decl = (DeclNode*)decl_res.value();

            GlobalDeclNode* global_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.global_decl,global_decl);
            break; 
        }

        case token_type::namespace_t:
        {
            auto name_space_res = split_full_namespace(parser,t);
            if(!name_space_res)
            {
                return name_space_res.error();
            }

            auto name_space = *name_space_res;

            parser.cur_namespace = scan_namespace(itl.global_namespace,name_space);

            // Namespace does not allready exist create it!
            if(!parser.cur_namespace)
            {
                parser.cur_namespace = new_named_scope(*parser.namespace_allocator,*parser.global_string_allocator,parser.global_namespace,name_space);
            }

            destroy_arr(name_space);

            if(match(parser,token_type::semi_colon))
            {
                (void)consume(parser,token_type::semi_colon);
            }
            break;
        }


        default:
        {
            if(t.type == token_type::symbol)
            {
                return parser_error(parser,parse_error::unexpected_token,t,"unexpected top level symbol '%s'\n",t.literal.buf);
            }

            else
            {
                return parser_error(parser,parse_error::unexpected_token,t,"unexpected top level token '%s' : (%d)\n",tok_name(t.type),u32(t.type));
            }

            break;
        }
    }

    return option::none;
}

Option<parse_error> parse_file(Interloper& itl,const String& file, const String& filename,FileQueue& queue)
{
    // Parse out the file
    Parser parser = make_parser(filename,itl.global_namespace,&itl.namespace_allocator,&itl.string_allocator,&itl.ast_allocator,&itl.ast_string_allocator,&itl.ast_arrays);

    if(tokenize(file,filename,parser.string_allocator,parser.tokens))
    {
        destroy_arr(parser.tokens);
        itl.first_error_code = itl_error::lexer_error;
        return parse_error::lexer_error;
    }
    
    if(itl.print_tokens)
    {
        printf("tokens for file: %s\n",filename.buf);
        print_tokens(parser.tokens);
    }
    
    const auto size = count(parser.tokens);

    while(parser.tok_idx < size)
    {
        // check for a directive
        if(match(parser,token_type::hash))
        {
            const auto hash_err = consume(parser,token_type::hash);
            if(!!hash_err)
            {
                return hash_err;
            }

            const auto directive_err = parse_directive(itl,parser);
            if(!!directive_err)
            {
                return directive_err;
            }
        }

        // plain decl
        else
        {
            const auto parse_err = parse_top_level_token(itl,parser,queue);
            if(!!parse_err)
            {
                return parse_err;
            }
        }
    }

    destroy_arr(parser.tokens);
    return option::none;
}

Option<parse_error> parse(Interloper& itl, const String& initial_filename)
{
    const char *itl_path = nullptr;

    if(file_exists("interloper"))
    {
        itl_path = ".";
    }

    else
    {
        itl_path = getenv("INTERLOPER_INSTALL_DIR");

        if(!itl_path)
        {
            fprintf(stderr,"Could not find install dir env var INTERLOPER_INSTALL_DIR\n");
            return parse_error::itl_error;
        }
    }

    itl.stl_path = cat_string(itl.string_allocator,itl_path,"/stl/");

    FileQueue queue;
    queue.set = make_set<String>();

    
    // add the initial file
    add_file(queue,get_program_name(itl.string_allocator,initial_filename));


    // import basic by default
    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"basic.itl"));

    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"internal.itl"));

    Option<parse_error> err = option::none;

    while(count(queue.stack))
    {
        // get the next filename to parse
        const String filename = pop(queue.stack);

        auto [file,file_err] = read_str_buf(filename);

        if(file_err)
        {
            printf("file %s does not exist\n",filename.buf);
            err = parse_error::itl_error;
            break;
        }

        err = parse_file(itl,make_string(file),filename,queue);

        destroy_arr(file);

        if(!!err)
        {
            break;
        }
    }

    destroy_arr(queue.stack);
    destroy_set(queue.set);

    return err;
}


void print_depth(int depth)
{
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d ",depth);    
}

void print(const AstNode *root, b32 override_seperator)
{
    static int depth = 0;

    if(!root)
    {
        print_depth(depth + 1);
        puts("EMPTY");
        return;
    }


    if(!override_seperator && (root->type == ast_type::function || root->type == ast_type::struct_t || root->type == ast_type::enum_t))
    {
        printf("\n\n\n");
    }


    depth += 1;

    print_depth(depth);

    switch(root->fmt)
    {
        case ast_fmt::plain:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);
            break;
        }

        case ast_fmt::binary:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);

            BinNode* bin_node = (BinNode*)root;

            print(bin_node->left);
            print(bin_node->right);

            break;
        }

        case ast_fmt::unary:
        {
            printf(" %s\n",AST_NAMES[static_cast<size_t>(root->type)]);

            UnaryNode* unary_node = (UnaryNode*)root;

            print(unary_node->next);
            break;
        }

        case ast_fmt::literal:
        {
            LiteralNode* literal_node = (LiteralNode*)root;

            printf(" %s : %s\n",AST_NAMES[static_cast<size_t>(root->type)],literal_node->literal.buf);
            break;
        }

        case ast_fmt::value:
        {
            ValueNode* value_node = (ValueNode*)root;

            if(value_node->value.sign)
            {
                printf("value: %ld\n",s64(value_node->value.v));
            }

            else
            {
                printf("value: %lu\n",value_node->value.v);
            }
            break;
        }

        case ast_fmt::float_t:
        {
            FloatNode* float_node = (FloatNode*)root;

            printf("float: %lf\n",float_node->value);
            break;
        }


        case ast_fmt::function:
        {
            FuncNode* func_node = (FuncNode*)root;

            printf("function %s:%s\n",func_node->filename.buf,func_node->name.buf);

            for(u32 a = 0; a < count(func_node->args); a++)
            {
                print((AstNode*)func_node->args[a]);
            }            

            if(func_node->block)
            {
                print((AstNode*)func_node->block);
            }

            for(u32 r = 0; r < count(func_node->return_type); r++)
            {
                print((AstNode*)func_node->return_type[r]);
            }
            break;
        }

        case ast_fmt::struct_t:
        {
            StructNode* struct_node = (StructNode*) root;

            printf("struct %s:%s\n",struct_node->filename.buf,struct_node->name.buf);

            for(u32 m = 0; m < count(struct_node->members); m++)
            {
                print((AstNode*)struct_node->members[m]);
            }                        

            break;
        }

        case ast_fmt::enum_t:
        {
            EnumNode* enum_node = (EnumNode*) root;

            print((AstNode*)enum_node->type);            
            for(u32 m = 0; m < count(enum_node->member); m++)
            {
                // we store this directly as its not used anywher else
                // fake the depth print
                depth += 1;
                print_depth(depth);

                printf("member %s: \n",enum_node->member[m].name.buf);
                print(enum_node->member[m].initializer);
                depth -= 1;
            }
            break;           
        }

        case ast_fmt::char_t:
        {
            CharNode* char_node = (CharNode*)root;
            
            printf("char: '%c' : %d\n",char_node->character,char_node->character);

            break;
        }

        case ast_fmt::type:
        {
            TypeNode* type_decl = (TypeNode*)root;
            // does double duty with sizeof_type
            printf(" %s: %s %s\n", AST_NAMES[static_cast<size_t>(root->type)],type_decl->is_const? "const" : "",type_decl->name.buf);

            if(type_decl->func_type)
            {
                print((AstNode*)type_decl->func_type,true);
            }

            for(u32 c = 0; c < count(type_decl->compound_type); c++)
            {
                print(type_decl->compound_type[c]);
            }
            break;
        }

        case ast_fmt::declaration:
        {
            DeclNode* decl_node = (DeclNode*)root;

            printf("%s : %s\n",decl_node->node.type == ast_type::const_decl? "const decl" : "decl",decl_node->name.buf);

            print((AstNode*)decl_node->type);
            print(decl_node->expr);
            break;
        }

        case ast_fmt::auto_decl:
        {
            AutoDeclNode* auto_decl = (AutoDeclNode*)root;
            printf("auto decl : %s\n",auto_decl->name.buf);
            print(auto_decl->expr);
            break;
        }

        case ast_fmt::global_declaration:
        {
            GlobalDeclNode* global_node = (GlobalDeclNode*)root;
            printf("global %s decl:\n",global_node->decl->node.type == ast_type::const_decl ? "const" : "");
            print((AstNode*)global_node->decl);
            break;
        }

        case ast_fmt::block:
        {
            printf("block\n");
            BlockNode* block_node = (BlockNode*)root;

            for(u32 s = 0; s < count(block_node->statements); s++)
            {
                print((AstNode*)block_node->statements[s]);
            }                        

            break;
        }

        case ast_fmt::function_call:
        {
            FuncCallNode* func_call = (FuncCallNode*)root;

            printf("function call\n");

            print(func_call->expr);

            for(u32 a = 0; a < count(func_call->args); a++)
            {
                print(func_call->args[a]);
            }                                    

            break;
        }

        case ast_fmt::for_iter:
        {
            printf("for\n");

            ForIterNode* for_node = (ForIterNode*)root;

            print(for_node->initializer);
            print(for_node->cond);
            print(for_node->post);

            print((AstNode*)for_node->block);

            break;
        }

        case ast_fmt::for_range:
        {
            ForRangeNode* for_node = (ForRangeNode*)root;

            printf("for [%s, %s] in\n",for_node->name_one.buf, for_node->name_two.buf);
            print(for_node->cond);

            break;
        }

        case ast_fmt::record:
        {
            printf("%s\n",AST_NAMES[static_cast<size_t>(root->type)]);


            RecordNode* record_node = (RecordNode*)root;

            for(u32 n = 0; n < count(record_node->nodes); n++)
            {
                print(record_node->nodes[n]);
            }                 

            break;
        }

        case ast_fmt::struct_initializer:
        {
            StructInitializerNode* struct_initializer_node = (StructInitializerNode*)root;

            printf("%s %s\n",AST_NAMES[u32(struct_initializer_node->node.type)],struct_initializer_node->struct_name.buf);

            for(u32 n = 0; n < count(struct_initializer_node->record->nodes); n++)
            {
                print(struct_initializer_node->record->nodes[n]);
            }                 

            break;
        }

        case ast_fmt::index:
        {
            IndexNode* index_node = (IndexNode*)root;

            printf("index: %s\n",index_node->name.buf);

            for(u32 i = 0; i < count(index_node->indexes); i++)
            {
                print(index_node->indexes[i]);
            }

            break;
        }

        case ast_fmt::slice:
        {
            SliceNode* slice_node = (SliceNode*)root;

            printf("slice: %s\n",slice_node->name.buf);

            if(slice_node->lower)
            {
                print(slice_node->lower);
            }

            if(slice_node->upper)
            {
                print(slice_node->upper);
            }
            break;
        }

        case ast_fmt::switch_t:
        {
            printf("switch\n");

            SwitchNode* switch_node = (SwitchNode*)root;

            print((AstNode*)switch_node->expr);

            for(u32 s = 0; s < count(switch_node->statements); s++)
            {
                print((AstNode*)switch_node->statements[s]);
            }

            print((AstNode*)switch_node->default_statement);

            break;
        }

        case ast_fmt::case_t:
        {
            printf("case\n");

            CaseNode* case_node = (CaseNode*)root;

            print((AstNode*)case_node->statement);
            print((AstNode*)case_node->block);

            break;
        }

        case ast_fmt::scope:
        {
            printf("scope: ");

            ScopeNode* scope_node = (ScopeNode*)root;

            for(size_t i = 0; i < count(scope_node->scope); i++)
            {
                printf("%s::",scope_node->scope[i].buf);
            }
            putchar('\n');

            print(scope_node->expr);

            break;
        }

        case ast_fmt::tuple_assign:
        {
            puts("tuple assign");

            TupleAssignNode* tuple_node = (TupleAssignNode*)root;

            for(u32 s = 0; s < count(tuple_node->symbols); s++)
            {
                print(tuple_node->symbols[s]);
            }

            print((AstNode*)tuple_node->func_call);
            break;
        }

        case ast_fmt::type_alias:
        {
            AliasNode* alias_node = (AliasNode*)root;

            printf("type alias %s\n",alias_node->name.buf);

            print((AstNode*)alias_node->type);
            break;
        }

        case ast_fmt::builtin_type_info:
        {
            BuiltinAccessNode* builtin_node = (BuiltinAccessNode*)root;

            printf("builtin type access: %s.%s\n",TYPE_NAMES[u32(builtin_node->type)],builtin_node->field.buf);
            break;
        }
    }

    depth -= 1;
}

std::pair<u32,u32> get_line_info(const String& filename, u32 idx)
{
    auto [file,err] = read_str_buf(filename);

    if(err)
    {
        printf("file %s does not exist\n",filename.buf);
        return std::pair{1,1};
    }

    // 1 indexed
    u32 col = 1;
    u32 row = 1;

    for(u32 i = 0; i < file.size && i != idx; i++)
    {
        if(file[i] == '\n')
        {
            row += 1;
            col = 0;
        }

        else
        {
            col += 1;
        }
    }

    destroy_arr(file);

    return std::pair{row,col};
}

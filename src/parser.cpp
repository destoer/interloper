#include <interloper.h>
#include <unistd.h>
#include "expression.cpp"

Option<BlockNode*> block(Parser &parser);
Option<AstNode*> block_ast(Parser &parser);

Option<FuncNode*> parse_func_sig(Parser& parser, const String& func_name,const Token& token);

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);

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



void consume(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        parser_error(parser,tok,"expected '%s' got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
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

Option<TypeNode*> parse_type(Parser &parser, b32 allow_fail)
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
        auto strings_opt = split_namespace(parser,peek(parser,0));

        if(!strings_opt)
        {
            return option::none;
        }

        auto strings = *strings_opt;

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
            parser_error(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
            return option::none;
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

        auto sig_opt = parse_func_sig(parser,"func_pointer",plain_tok);
        if(!sig_opt)
        {
            return option::none;
        }

        type->func_type = *sig_opt; 
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
        parser_error(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        return option::none;
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
                consume(parser,token_type::deref);

                push_var(type->compound_type,ast_plain(parser,ast_type::ptr_indirection,plain_tok));
                break;
            }

            // Nullable pointer decl
            case token_type::qmark:
            {
                consume(parser,token_type::qmark);

                push_var(type->compound_type,ast_plain(parser,ast_type::nullable_ptr_indirection,plain_tok));
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                while(peek(parser,0).type == token_type::sl_brace)
                {
                    consume(parser,token_type::sl_brace);

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        push_var(type->compound_type,ast_plain(parser,ast_type::arr_var_size,plain_tok));
                        consume(parser,token_type::sr_brace);
                    }

                    else 
                    {
                        // figure out this size later
                        if(peek(parser,0).type == token_type::qmark)
                        {
                            consume(parser,token_type::qmark);

                            const auto e = ast_plain(parser,ast_type::arr_deduce_size,plain_tok);
                            push_var(type->compound_type,e);
                        
                            consume(parser,token_type::sr_brace);
                        }

                        else
                        {
                            auto e_opt = ast_unary(parser,expr_terminate(parser,"array declaration",token_type::sr_brace), ast_type::arr_fixed, plain_tok);

                            if(!e_opt)
                            {
                                return option::none;
                            }

                            push_var(type->compound_type,*e_opt);
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




Option<AstNode*> declaration(Parser &parser, token_type terminator, b32 is_const_decl = false)
{
    // declartion
    // symbol ':' type ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        parser_error(parser,s,"declartion expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
        return option::none;
    }

    consume(parser,token_type::colon);


    auto type_opt = parse_type(parser);

    if(!type_opt)
    {
        return option::none;
    }

    TypeNode* type = *type_opt;

    //    [declare:name]
    // [type]   optional([eqauls])

    DeclNode* decl = (DeclNode*)ast_decl(parser,s.literal,type,is_const_decl,s);

    const auto eq = peek(parser,0);

    switch(eq.type)
    {
        // declartion with assingment
        case token_type::equal:
        {
            consume(parser,token_type::equal);
            auto stmt_opt = statement_terminate(parser,"declaration");

            if(!stmt_opt)
            {
                return option::none;
            }

            decl->expr = *stmt_opt;
            break;
        }

        default:
        {
            if(eq.type == terminator)
            {
                consume(parser,terminator);
            }

            else
            {
                parser_error(parser,eq,"malformed declartion: got %s expected terminator %s\n",tok_name(eq.type),tok_name(terminator));
                return option::none;
            }
            break;
        }
    }

    return (AstNode*)decl;
}

Option<AstNode*> auto_decl(Parser &parser)
{
    // symbol := expr;
    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        parser_error(parser,s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),parser.tok_idx);
        return option::none;
    }

    consume(parser,token_type::decl);

    auto expr_opt = statement_terminate(parser,"auto declaration");

    if(!expr_opt)
    {
        return option::none;
    }

    AstNode* decl = (AstNode*)ast_auto_decl(parser,s.literal,*expr_opt,s);

    return decl;    
}


Option<AstNode*> tuple_assign(Parser& parser, const Token& t)
{
    b32 done = false;


    TupleAssignNode* tuple_node = (TupleAssignNode*)ast_tuple_assign(parser,t);

    // generalise this so we can pick up on a ',' being a "terminator"
    // in other expressions
    while(!done)
    {
        const auto sym_tok = next_token(parser);

        AstNode* sym_node = nullptr;

        switch(sym_tok.type)
        {
            case token_type::symbol:
            {
                auto var_opt = var(parser,sym_tok);

                if(!var_opt)
                {
                    assert(false);
                    return option::none;
                }

                sym_node = *var_opt;
                break;
            }

            case token_type::deref:
            {
                const Token deref_tok = next_token(parser);

                auto unary_opt = ast_unary(parser,var(parser,deref_tok),ast_type::deref,sym_tok);
                
                if(!unary_opt)
                {
                    assert(false);
                    return option::none;
                }

                sym_node = *unary_opt;
                break;
            }

            default:
            {
                parser_error(parser,t,"tuple assignment attempted on non symbol: %s\n",tok_name(sym_tok.type));
                return option::none;
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
                    consume(parser,token_type::decl);
                    tuple_node->auto_decl = true;
                }

                else
                {
                    consume(parser,token_type::equal);
                }

                const auto next = next_token(parser);

                // handle scope
                if(match(parser,token_type::scope))
                {
                    prev_token(parser);
                    const auto namespace_opt = split_namespace(parser,next);
                    
                    if(!namespace_opt)
                    {
                        assert(false);
                        return option::none;
                    }

                    const auto name_space = *namespace_opt;

                    const auto sym_tok = next_token(parser);

                    auto func_call_opt = func_call(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);

                    if(!func_call_opt)
                    {
                        assert(false);
                        return option::none;
                    }

                    tuple_node->func_call = (FuncCallNode*)*func_call_opt;

                    return ast_scope(parser,(AstNode*)tuple_node,name_space,next);
                }

                else
                {
                    auto func_call_opt = func_call(parser,ast_literal(parser,ast_type::symbol,next.literal,next),next);
                    if(!func_call_opt)
                    {
                        assert(false);
                        return option::none;
                    }

                    tuple_node->func_call = (FuncCallNode*)*func_call_opt;
                    consume(parser,token_type::semi_colon);
                    done = true;
                }

                break;
            }

            // something has gone wrong
            default: 
            {
                parser_error(parser,t,"malformed tuple statement ");
                return option::none;
            }
        }
    }

    return (AstNode*)tuple_node;    
}

Option<AstNode*> struct_access(Parser& parser, AstNode* expr_node,const Token& t)
{
    RecordNode* member_root = (RecordNode*)ast_record(parser,ast_type::access_members,t);

    auto bin_opt = ast_binary(parser,expr_node,(AstNode*)member_root,ast_type::access_struct,t);

    if(!bin_opt)
    {
        return option::none;
    }

    BinNode* root = (BinNode*)*bin_opt;

    while(match(parser,token_type::dot))
    {
        consume(parser,token_type::dot);

        const auto member_tok = next_token(parser);

        if(member_tok.type == token_type::symbol)
        {
            // perform peeking for modifers
            if(match(parser,token_type::sl_brace))
            {
                auto index_opt = array_index(parser,member_tok);
                if(!index_opt)
                {
                    return option::none;
                }

                push_var(member_root->nodes,*index_opt);
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
            parser_error(parser,member_tok,"expected struct member got %s(%s)\n",member_tok.literal.buf,tok_name(member_tok.type));
            return option::none;            
        }
    }

    return (AstNode*)root;
}


Option<AstNode*> array_index(Parser& parser,const Token& t)
{
    IndexNode* arr_access = (IndexNode*)ast_index(parser,t.literal,t);

    while(match(parser,token_type::sl_brace))
    {
        consume(parser,token_type::sl_brace);

        auto expr_opt = expr_terminate(parser,"array indexing",token_type::sr_brace);

        if(!expr_opt)
        {
            return option::none;
        }

        push_var(arr_access->indexes,*expr_opt);
    }

    return (AstNode*)arr_access;
}

Option<AstNode*> arr_slice(Parser& parser,const Token& t)
{
    consume(parser,token_type::sl_brace);

    SliceNode* slice_node = (SliceNode*)ast_slice(parser,t.literal,t);

    // check if left side is empty.
    if(match(parser,token_type::colon))
    {
        consume(parser,token_type::colon);
    }

    else
    {
        auto lower_opt = expr_terminate(parser,"slice lower",token_type::colon);
        if(!lower_opt)
        {
            return option::none;
        }

        slice_node->lower = *lower_opt;
    }

    // check if right side is empty
    if(match(parser,token_type::sr_brace))
    {
        if(!slice_node->lower)
        {
            parser_error(parser,t,"Array slice with empty lower and upper bounds!");
            return option::none;
        }

        consume(parser,token_type::sr_brace);
    }

    else
    {
        auto upper_opt = expr_terminate(parser,"slice upper",token_type::sr_brace);
        if(!upper_opt)
        {
            return option::none;
        }

        slice_node->upper = *upper_opt;
    }

    return (AstNode*)slice_node;
}

Option<AstNode*> arr_access(Parser& parser, const Token& t)
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
    auto index_opt = array_index(parser,t);

    if(!index_opt)
    {
        return option::none;
    }

    AstNode* arr_access = *index_opt;

    if(match(parser,token_type::dot))
    {
        return struct_access(parser,arr_access,t);
    }

    else
    {
        return arr_access;
    }
}

Option<AstNode*> var(Parser& parser, const Token& sym_tok, b32 allow_call)
{
    const Token next = peek(parser,0);

    AstNode* node = nullptr;

    switch(next.type)
    {
        case token_type::dot:
        {   
            auto access_opt = struct_access(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);
            if(!access_opt)
            {
                return option::none;
            }

            node = *access_opt;
            break;
        }

        case token_type::sl_brace:
        {
            auto access_opt = arr_access(parser,sym_tok);
            if(!access_opt)
            {
                return option::none;
            }

            node = *access_opt;
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

Option<AstNode*> func_call(Parser& parser,AstNode *expr, const Token& t)
{
    consume(parser,token_type::left_paren);

    FuncCallNode* func_call = (FuncCallNode*)ast_call(parser,expr,t);

    // keep reading args till we run out of commas
    b32 done = false;

    // empty call we are done
    if(match(parser,token_type::right_paren))
    {
        consume(parser,token_type::right_paren);
        done = true;
    }

    while(!done)
    {
        auto list_opt = expr_list(parser,"function call",token_type::right_paren);
        if(!list_opt)
        {
            assert(false);
            return option::none;
        }

        auto [node,term_seen] = *list_opt;

        push_var(func_call->args,node);

        // no more args terminate the call
        done = term_seen;
    }

    return (AstNode*)func_call;
}

Option<AstNode*> const_assert(Parser& parser,const Token& t)
{
    consume(parser,token_type::left_paren);

    const auto expr = expr_terminate(parser,"const_assert",token_type::right_paren);
    consume(parser,token_type::semi_colon);

    return ast_unary(parser,expr,ast_type::const_assert,t);       
}

Option<AstNode*> parse_for_iter(Parser& parser, const Token& t, b32 term_paren)
{
    // e.g for(i := 0; i < size; i += 1)

    ForIterNode* for_node = (ForIterNode*)ast_for_iter(parser,t);

    // handle first stmt
    // decl 
    if(peek(parser,1).type == token_type::colon)
    {
        auto decl_opt = declaration(parser,token_type::semi_colon);
        if(!decl_opt)
        {
            return option::none;
        }

        for_node->initializer = *decl_opt;
    }

    // auto decl
    else if(peek(parser,1).type == token_type::decl)
    {
        auto decl_opt = auto_decl(parser);
        if(!decl_opt)
        {
            return option::none;
        }

        for_node->initializer = *decl_opt;  
    }

    // standard stmt
    else
    {
        auto stmt_opt = statement_terminate(parser,"for initializer statement");
        if(!stmt_opt)
        {
            return option::none;
        }

        for_node->initializer = *stmt_opt;
    }
    
    auto cond_opt = statement_terminate(parser,"for condition"); 
    if(!cond_opt)
    {
        return option::none;
    }

    for_node->cond = *cond_opt;

    // allow paren terminator followed by a '{'
    if(term_paren)
    {
        auto post_opt = expr_terminate(parser,"for post statement",token_type::right_paren);
        if(!post_opt)
        {
            return option::none;
        }

        for_node->post = *post_opt;

        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            parser_error(parser,next,"invalid iter for statement terminator: %s expected {\n",tok_name(next.type));
            return option::none;                        
        }
    }

    // statement was not wrapped by parens
    // expect brace to end it
    else
    {
        auto post_opt = expr_terminate(parser,"for post statement",token_type::left_c_brace);
        if(!post_opt)
        {
            return option::none;
        }

        for_node->post = *post_opt;
        prev_token(parser);
    }  
    
    auto block_opt = block(parser);
    if(!block_opt)
    {
        return option::none;
    }

    // for stmt parsed now compile the actual block
    for_node->block = *block_opt;

    return (AstNode*)for_node;
}

Option<AstNode*> parse_for_range(Parser& parser,const Token& t, b32 term_paren, b32 take_index, b32 take_pointer)
{
    // e.g
    // for(i in 0 <= size)
    // for([v, i] in arr)
    // for(v in arr)
    // for(@v in arr)
    ForRangeNode* for_node = (ForRangeNode*)ast_for_range(parser,t);

    if(take_index)
    {
        consume(parser,token_type::sl_brace);
    }

    if(take_pointer)
    {
        consume(parser,token_type::deref);
    }

    for_node->take_pointer = take_pointer;

    const auto name_one = next_token(parser);

    if(name_one.type != token_type::symbol)
    { 
        parser_error(parser,name_one,"Expected name for range for statement");
        return option::none;
    }


    for_node->name_one = name_one.literal;

    // get the 2nd name
    if(take_index)
    {
        consume(parser,token_type::comma);

        const auto name_two = next_token(parser);

        if(name_two.type != token_type::symbol)
        { 
            parser_error(parser,name_two,"Expected name for range for statement");
            return option::none;
        }

        for_node->name_two = name_two.literal;

        consume(parser,token_type::sr_brace);
    }

    consume(parser,token_type::in_t);

    if(term_paren)
    { 
        auto cond_opt = expr_terminate(parser,"for range cond",token_type::right_paren);
        if(!cond_opt)
        {
            return option::none;
        }

        for_node->cond = *cond_opt;
        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            parser_error(parser,next,"invalid range for statement terminator: %s expected {\n",tok_name(next.type));
            return option::none;                        
        }
    }

    else
    {
        auto cond_opt = expr_terminate(parser,"for range cond statement",token_type::left_c_brace);
        if(!cond_opt)
        {
            return option::none;
        }

        for_node->cond = *cond_opt;
        prev_token(parser);
    }

    auto block_opt = block(parser);
    if(!block_opt)
    {
        return option::none;
    }

    // for stmt parsed now compile the actual block
    for_node->block = *block_opt;

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
                    if(!match(parser,token_type::symbol))
                    {
                        parser_error(parser,t,"Expected struct name for struct return");
                        return option::none;
                    }
                    
                    const auto name_tok = next_token(parser);


                    const auto list_opt = expr_terminate(parser,"struct return intializer",token_type::semi_colon);

                    if(!list_opt)
                    {
                        return option::none;
                    }

                    auto list = *list_opt;

                    if(list->type != ast_type::initializer_list)
                    {
                        parser_error(parser,t,"Expected initializer list for struct return");
                        return option::none;
                    }

                    return ast_struct_return(parser,name_tok.literal,(RecordNode*)list,t);
                }

                RecordNode* record = (RecordNode*)ast_record(parser,ast_type::ret,t);

                b32 done = false;

                // can be more than one expr (comma seperated)
                while(!done)
                {
                    auto list_opt = expr_list(parser,"return",token_type::semi_colon);
                    if(!list_opt)
                    {
                        return option::none;
                    }

                    auto [e,term_seen] = *list_opt;

                    done = term_seen;
                    push_var(record->nodes,e);
                }

                return (AstNode*)record;
            }

            else
            {
                consume(parser,token_type::semi_colon);
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
                case token_type::equal:
                {
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
                    parser_error(parser,t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    return option::none;
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
                consume(parser,token_type::left_paren);
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

            auto if_opt = ast_binary(parser,expr_opt,body_opt,ast_type::if_t,t);
            if(!if_opt)
            {
                return option::none;
            }

            BinNode *if_stmt = (BinNode*)*if_opt;

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
                        consume(parser,token_type::if_t);

                        auto expr_opt = expr_terminate(parser,"else if condition statement",token_type::left_c_brace); prev_token(parser);
                        auto body_opt = block_ast(parser);

                        auto else_if_opt = ast_binary(parser,expr_opt,body_opt,ast_type::else_if_t,else_tok);
                        if(!else_if_opt)
                        {
                            return option::none;
                        }

                        BinNode* else_if_stmt = (BinNode*)*else_if_opt;
                        push_var(if_block->statements,(AstNode*)else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        auto block_opt = block_ast(parser);
                        auto else_opt = ast_unary(parser,block_opt,ast_type::else_t,else_tok);
                        if(!else_opt)
                        {
                            return option::none;
                        }

                        AstNode *else_stmt = *else_opt;

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
            auto expr_opt = expr_terminate(parser,"switch statement",token_type::left_c_brace);
            if(!expr_opt)
            {
                return option::none;
            }

            SwitchNode* switch_node = (SwitchNode*)ast_switch(parser,*expr_opt,t);

            // while we havent exhaused every case
            while(!match(parser,token_type::right_c_brace))
            {
                const auto case_tok = peek(parser,0);


                if(case_tok.type == token_type::default_t)
                {
                    if(switch_node->default_statement)
                    {
                        parser_error(parser,case_tok,"Cannot have two default statements in switch statement\n");
                        return option::none;
                    }

                    consume(parser,token_type::default_t);
                    consume(parser,token_type::colon);

                    auto block_opt = block_ast(parser);
                    auto unary_opt = ast_unary(parser,block_opt,ast_type::default_t,case_tok);    
                    if(!unary_opt)
                    {
                        return option::none;
                    }

                    switch_node->default_statement = (UnaryNode*)*unary_opt;  
                }

                else
                {
                    // read out the case
                    consume(parser,token_type::case_t);
                    auto case_opt = expr_terminate(parser,"switch case",token_type::colon);
                    if(!case_opt)
                    {
                        return option::none;
                    }

                    AstNode* case_node = *case_opt; 
                    auto block_opt = block(parser);
                    if(!block_opt)
                    {
                        return option::none;
                    }

                    CaseNode* case_statement = (CaseNode*)ast_case(parser,case_node,*block_opt,case_tok);
                    push_var(switch_node->statements,case_statement);
                }
            }

            consume(parser,token_type::right_c_brace);
            return (AstNode*)switch_node;
        }

        // dont care
        case token_type::semi_colon:
        {
            break;
        }

        default:
        {
            parser_error(parser,t,"statement: unexpected token '%s' : %d\n",tok_name(t.type),u32(t.type));
            return option::none;
        }
    }

    return option::none;
}

Option<BlockNode*> block(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    consume(parser,token_type::left_c_brace);

    BlockNode* b = (BlockNode*)ast_block(parser,tok);

    // parse out all our statements
    while(!match(parser,token_type::right_c_brace))
    {
        if(match(parser,token_type::eof))
        {
            parser_error(parser,tok,"unterminated block!\n");
            return option::none;
        }

        auto stmt_opt = statement(parser);

        if(!stmt_opt)
        {
            print_token(peek(parser,0));
            assert(false);
            return option::none;
        }

        push_var(b->statements,*stmt_opt);
    }
    
    consume(parser,token_type::right_c_brace);
    return b;
}

Option<AstNode*> block_ast(Parser &parser)
{
    auto block_opt = block(parser);

    if(!block_opt)
    {
        return option::none;
    }

    return (AstNode*)block_opt.value();
}

dtr_res check_redeclaration(Interloper& itl, NameSpace* root, const String& name, const String& checked_def_type)
{
    const DefInfo* existing_def = lookup_definition(root,name);

    if(existing_def)
    {
        compile_error(itl,itl_error::redeclaration,"%s (%s) has been redeclared as a %s!\n",
            name.buf,definition_type_name(existing_def),checked_def_type.buf);
        return dtr_res::err;
    }

    return dtr_res::ok;  
}

dtr_res type_alias(Interloper& itl, Parser &parser)
{
    // type_alias literal '=' type ';'
    const auto token = next_token(parser);

    
    if(token.type == token_type::symbol)
    {
        consume(parser,token_type::equal);
        auto rtype_opt = parse_type(parser);

        if(!rtype_opt)
        {
            return dtr_res::err;
        }

        TypeNode* rtype = *rtype_opt;

        const String& name = token.literal;

        if(!check_redeclaration(itl,parser.cur_namespace,name,"type alias"))
        {
            return dtr_res::err;
        }

        AstNode* alias_node = ast_alias(parser,rtype,name,parser.cur_file,token);
    
        consume(parser,token_type::semi_colon);

        add_type_definition(itl, type_def_kind::alias_t,alias_node, name, parser.cur_file,parser.cur_namespace);
    }

    else 
    {
        parser_error(parser,token,"expected symbol for type alias name got %s\n",tok_name(token.type));
        return dtr_res::err;
    }

    return dtr_res::ok;
}

// parse just the function signature
// NOTE: this is used to parse signatures for function pointers
Option<FuncNode*> parse_func_sig(Parser& parser,const String& func_name, const Token& token)
{
    FuncNode *f = (FuncNode*)ast_func(parser,func_name,parser.cur_file,token);

    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);


    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            parser_error(parser,paren,"unterminated function declaration!\n");
            return option::none;
        }

        // for each arg pull type, name
        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            parser_error(parser,lit_tok,"expected name for function arg got %s\n",tok_name(lit_tok.type));
            return option::none;
        }
        

        consume(parser,token_type::colon);

        // va_args
        if(match(parser,token_type::va_args))
        {
            consume(parser,token_type::va_args);

            f->va_args = true;
            f->args_name = lit_tok.literal;

            // by definiton this must be the last arg!
            if(!match(parser,token_type::right_paren))
            {
                parser_error(parser,lit_tok,"va_args can only be placed as the last arg : got %s\n",tok_name(peek(parser,0).type));
                return option::none;
            }
        }

        else
        {
            auto type_opt = parse_type(parser);

            if(!type_opt)
            {
                return option::none;
            }


            // add each declartion
            DeclNode* decl = (DeclNode*)ast_decl(parser,lit_tok.literal,*type_opt,false,lit_tok);
            
            push_var(f->args,decl);

            // if the declaration isnt closed get the next arg
            if(!match(parser,token_type::right_paren))
            {
                consume(parser,token_type::comma);
            }
        }
    }

    consume(parser,token_type::right_paren);

    // tuple type
    if(match(parser,token_type::sl_brace))
    {
        consume(parser,token_type::sl_brace);

        while(!match(parser,token_type::sr_brace))
        {
            if(match(parser,token_type::eof))
            {
                parser_error(parser,paren,"unterminated function declaration!\n");
                return option::none;
            }
            
            auto return_type_opt = parse_type(parser);

            if(!return_type_opt)
            {
                return option::none;
            }

            push_var(f->return_type,*return_type_opt);

            if(!match(parser,token_type::sr_brace))
            {
                consume(parser,token_type::comma);
            }
        }

        consume(parser,token_type::sr_brace);
    }

    // single type
    else if(!match(parser,token_type::left_c_brace) && !match(parser,token_type::semi_colon))
    {
        auto return_type_opt = parse_type(parser);

        if(!return_type_opt)
        {
            return option::none;
        }

        push_var(f->return_type,*return_type_opt);
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

dtr_res func_decl(Interloper& itl, Parser &parser)
{
    // func_dec = func ident(arg...) return_type 
    // arg = ident : type,

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        parser_error(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return dtr_res::err;
    }

    if(!check_redeclaration(itl,parser.cur_namespace,func_name.literal,"function"))
    {
        return dtr_res::err;
    }

    auto func_opt = parse_func_sig(parser,func_name.literal,func_name);

    if(!func_opt)
    {
        return dtr_res::err;
    }

    FuncNode* func = *func_opt;

    auto block_opt = block(parser);
    if(!block_opt)
    {
        return dtr_res::err;
    }

    func->block = *block_opt; 

    // finally add the function def
    add_func(itl,func_name.literal,parser.cur_namespace,func);
    return dtr_res::ok;
}

dtr_res struct_decl(Interloper& itl,Parser& parser, u32 flags = 0)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        parser_error(parser,name,"expected name after struct decl got %s\n",tok_name(name.type));
        return dtr_res::err;
    }

    if(!check_redeclaration(itl,parser.cur_namespace,name.literal,"struct"))
    {
        return dtr_res::err;
    }

    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,parser.cur_file,name);

    struct_node->attr_flags = flags;

    // Does this struct have a forced first member?
    if(match(parser,token_type::left_paren))
    {
        consume(parser,token_type::left_paren);

        auto decl_opt = declaration(parser,token_type::right_paren);
        if(!decl_opt)
        {
            return dtr_res::err;
        }
        struct_node->forced_first = (DeclNode*)*decl_opt;
    }

    consume(parser,token_type::left_c_brace);

    while(!match(parser,token_type::right_c_brace))
    {
        auto decl_opt = declaration(parser,token_type::semi_colon);

        if(!decl_opt)
        {
            return dtr_res::err;
        }

        DeclNode* decl = (DeclNode*)*decl_opt;

        push_var(struct_node->members,decl);
    }

    consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        consume(parser,token_type::semi_colon);
    }

    add_type_definition(itl, type_def_kind::struct_t,(AstNode*)struct_node, struct_node->name, parser.cur_file,parser.cur_namespace);
    return dtr_res::ok;
}

dtr_res enum_decl(Interloper& itl,Parser& parser, u32 flags)
{
    const auto name_tok = next_token(parser);

    if(name_tok.type != token_type::symbol)
    {
        compile_error(itl,itl_error::missing_name,"Expected symbol for enum name got %s\n",tok_name(name_tok.type));
        return dtr_res::err;
    }

    if(!check_redeclaration(itl,parser.cur_namespace,name_tok.literal,"enum"))
    {
        return dtr_res::err;
    }

    EnumNode* enum_node = (EnumNode*)ast_enum(parser,name_tok.literal,parser.cur_file,name_tok);


    if(match(parser,token_type::colon))
    {
        consume(parser,token_type::colon);
        auto type_opt = parse_type(parser);
        if(!type_opt)
        {
            return dtr_res::err;
        }

        enum_node->type = *type_opt;
    }

    if((flags & ATTR_FLAG) && !enum_node->type)
    {
        parser_error(parser,next_token(parser),"Flag enum must specify underlying intergeral type");
        return dtr_res::err;        
    }

    consume(parser,token_type::left_c_brace);

    enum_node->attr_flags = flags;

    // push each member till we hit the terminating brace
    while(!match(parser,token_type::right_c_brace))
    {
        const auto member_tok = next_token(parser);

        if(member_tok.type != token_type::symbol)
        {
            parser_error(parser,member_tok,"Expected symbol for enum %s member got %s\n",name_tok.literal.buf,tok_name(member_tok.type));
            return dtr_res::err;
        }

        EnumMemberDecl member;
        member.name = member_tok.literal;

        // see if we have an initlizer
        if(match(parser,token_type::equal))
        {
            consume(parser,token_type::equal);

            token_type term;

            auto initializer_opt = expr_terminate(parser,"enum struct init",token_type::comma,term);
            if(!initializer_opt)
            {
                return dtr_res::err;
            }

            member.initializer = *initializer_opt;
        }

        else
        {
            consume(parser,token_type::comma);
        }

        push_var(enum_node->member,member);        
    }

    consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        consume(parser,token_type::semi_colon);
    }

    // add the type decl
    add_type_definition(itl, type_def_kind::enum_t,(AstNode*)enum_node, enum_node->name, parser.cur_file,parser.cur_namespace);
    return dtr_res::ok;
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

Option<u32> parse_attr(Parser& parser, const Token& tok)
{
    u32 flags = 0;

    consume(parser,token_type::left_paren);

    const auto attr = next_token(parser);

    if(attr.type != token_type::symbol)
    {
        parser_error(parser,tok,"Expected name for attr got %s\n",tok_name(attr.type));
        return option::none;
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

    else
    {
        parser_error(parser,tok,"Unknown attr %s\n",attr_name.buf);
        return option::none;
    }

    consume(parser,token_type::right_paren);

    return flags;
}

dtr_res parse_directive(Interloper& itl,Parser& parser)
{
    const auto next = next_token(parser);

    if(next.type != token_type::symbol)
    {
        parser_error(parser,next,"Expected name for directive got %s\n",tok_name(next.type));
        return dtr_res::err;
    }

    // TODO: move this lookup to a hashtable if it starts getting large
    const auto name = next.literal;

    if(name == "attr")
    {
        const auto flags_opt = parse_attr(parser,next);

        if(!flags_opt)
        {
            return dtr_res::err;
        }

        const u32 flags = *flags_opt;

        const auto stmt = peek(parser,0);

        // look at which declaration is next?
        switch(stmt.type)
        {
            case token_type::struct_t:
            {
                consume(parser,token_type::struct_t);
                
                if(!struct_decl(itl,parser,flags))
                {
                    return dtr_res::err;
                }
                break;
            }

            case token_type::enum_t:
            {
                consume(parser,token_type::enum_t);
                
                if(!enum_decl(itl,parser,flags))
                {
                    return dtr_res::err;
                } 
                break; 
            }

            default:
            {
                parser_error(parser,stmt,"Attribute is not legal on stmt: %s\n",tok_name(stmt.type));
                return dtr_res::err;    
            }
        }
    }

    else
    {
        parser_error(parser,next,"Unknown directive %s\n",name.buf);
        return dtr_res::err;
    }

    return dtr_res::ok;
}

Option<Array<String>> split_namespace_internal(Parser& parser, const Token& start, bool full_namespace)
{
    Array<String> name_space;

    while(!match(parser,token_type::eof))
    {
        const auto name = next_token(parser);

        if(name.type != token_type::symbol)
        {
            parser_error(parser,name,"Expected name for namespace got: %s\n",tok_name(name.type));
            destroy_arr(name_space);
            return option::none;
        }   

        push_var(name_space,name.literal);

        if(match(parser,token_type::scope))
        {
            consume(parser,token_type::scope);

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
        parser_error(parser,start,"Namespace is empty");
        return option::none;
    }

    return name_space;
}

Option<Array<String>> split_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,false);
}

Option<Array<String>> split_full_namespace(Parser& parser, const Token& start)
{
    return split_namespace_internal(parser,start,true);
}



dtr_res parse_top_level_token(Interloper& itl, Parser& parser, FileQueue& queue)
{
    const auto &t = next_token(parser);

    switch(t.type)
    {
        case token_type::import:
        {
            // stl path: import <name>
            if(match(parser,token_type::logical_lt))
            {
                consume(parser,token_type::logical_lt);

                if(!match(parser,token_type::symbol))
                {
                    const auto err = next_token(parser);
                    parser_error(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(err.type),err.literal.buf);
                    return dtr_res::err;
                }

                const auto name_tok = next_token(parser);

                consume(parser,token_type::logical_gt);

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
                parser_error(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(t.type),t.literal.buf);
                return dtr_res::err;
            }
            break;
        }

        // function declartion
        case token_type::func:
        {
            if(!func_decl(itl,parser))
            {
                return dtr_res::err;
            }
            break;
        }

        case token_type::struct_t:
        {
            if(!struct_decl(itl,parser))
            {
                return dtr_res::err;
            }
            break;
        }

        case token_type::enum_t:
        {
            if(!enum_decl(itl,parser,0))
            {
                return dtr_res::err;
            }
            break;
        }

        case token_type::type_alias:
        {
            if(!type_alias(itl,parser))
            {
                return dtr_res::err;
            }
            break;
        }

        // global constant
        case token_type::constant_t:
        {
            auto decl_opt = declaration(parser,token_type::semi_colon,true);
            if(!decl_opt)
            {
                return dtr_res::err;
            }

            DeclNode* decl = (DeclNode*)*decl_opt;

            GlobalDeclNode* const_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.constant_decl,const_decl);
            break; 
        }

        // global mut
        case token_type::global_t:
        {
            auto decl_opt = declaration(parser,token_type::semi_colon);
            if(!decl_opt)
            {
                return dtr_res::err;
            }

            DeclNode* decl = (DeclNode*)*decl_opt;

            GlobalDeclNode* global_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_namespace,t);

            push_var(itl.global_decl,global_decl);
            break; 
        }

        case token_type::namespace_t:
        {
            auto name_space_opt = split_full_namespace(parser,t);
            if(!name_space_opt)
            {
                return dtr_res::err;
            }

            auto name_space = *name_space_opt;

            parser.cur_namespace = scan_namespace(itl.global_namespace,name_space);

            // Namespace does not allready exist create it!
            if(!parser.cur_namespace)
            {
                parser.cur_namespace = new_named_scope(*parser.namespace_allocator,*parser.global_string_allocator,parser.global_namespace,name_space);
            }

            destroy_arr(name_space);

            if(match(parser,token_type::semi_colon))
            {
                consume(parser,token_type::semi_colon);
            }
            break;
        }


        default:
        {
            if(t.type == token_type::symbol)
            {
                parser_error(parser,t,"unexpected top level symbol '%s'\n",t.literal.buf);
                return dtr_res::err;
            }

            else
            {
                parser_error(parser,t,"unexpected top level token '%s' : (%d)\n",tok_name(t.type),u32(t.type));
                return dtr_res::err;
            }

            break;
        }
    }

    return dtr_res::ok;
}

dtr_res parse_file(Interloper& itl,const String& file, const String& filename,FileQueue& queue)
{
    // Parse out the file
    Parser parser = make_parser(filename,itl.global_namespace,&itl.namespace_allocator,&itl.string_allocator,&itl.ast_allocator,&itl.ast_string_allocator,&itl.ast_arrays);

    if(tokenize(file,filename,parser.string_allocator,parser.tokens))
    {
        destroy_arr(parser.tokens);
        itl.first_error_code = itl_error::lexer_error;
        return dtr_res::err;
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
            consume(parser,token_type::hash);
            if(!parse_directive(itl,parser))
            {
                return dtr_res::err;
            }
        }

        // plain decl
        else
        {
            if(!parse_top_level_token(itl,parser,queue))
            {
                return dtr_res::err;
            }
        }
    }

    destroy_arr(parser.tokens);
    return dtr_res::ok;
}

dtr_res parse(Interloper& itl, const String& initial_filename)
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
            return dtr_res::err;
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

    dtr_res res = dtr_res::ok;

    while(count(queue.stack))
    {
        // get the next filename to parse
        const String filename = pop(queue.stack);

        auto [file,err] = read_str_buf(filename);

        if(err)
        {
            printf("file %s does not exist\n",filename.buf);
            res = dtr_res::err;
            break;
        }

        res = parse_file(itl,make_string(file),filename,queue);

        destroy_arr(file);

        if(!res)
        {
            break;
        }
    }

    destroy_arr(queue.stack);
    destroy_set(queue.set);

    return res;
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

        case ast_fmt::struct_return:
        {
            StructReturnNode* struct_return_node = (StructReturnNode*)root;

            printf("struct return %s\n",struct_return_node->struct_name.buf);

            for(u32 n = 0; n < count(struct_return_node->record->nodes); n++)
            {
                print(struct_return_node->record->nodes[n]);
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

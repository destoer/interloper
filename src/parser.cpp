#include <interloper.h>
#include "expression.cpp"

void type_panic(Parser &parser);
BlockNode *block(Parser &parser);

FuncNode* parse_func_sig(Parser& parser, const String& func_name,const Token& token);

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);

const u32 AST_ALLOC_DEFAULT_SIZE = 8 * 1024;

Parser make_parser(const String& cur_file,ArenaAllocator* ast_allocator,ArenaAllocator* string_allocator, AstPointers* ast_arrays)
{
    Parser parser;
    parser.allocator = ast_allocator;
    parser.string_allocator = string_allocator;

    // NOTE: this relies on get_program_name to allocate the string correctly
    parser.cur_file = cur_file;
    parser.cur_path = extract_path(parser.cur_file);
    parser.ast_arrays = ast_arrays;

    return parser;
}

void add_ast_pointer(Parser& parser, void* pointer)
{
    push_var(*parser.ast_arrays,pointer);
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
        panic(parser,tok,"expected '%s' got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
}

bool match(Parser &parser,token_type type)
{
    const auto t = parser.tok_idx >= count(parser.tokens)? token_type::eof : parser.tokens[parser.tok_idx].type;

    return t == type;
}


void type_panic(Parser &parser)
{
    const auto tok = peek(parser,1);
    panic(parser,tok,"expected type declaration got: %s\n",tok_name(tok.type));
}


bool is_builtin_type_tok(const Token &tok)
{
   return tok.type >= token_type::u8 && tok.type <= token_type::bool_t; 
}

builtin_type builtin_type_from_tok(const Token& tok)
{
    // compute builtin type idx 
    return builtin_type(s32(tok.type) - s32(token_type::u8));
}

u32 plain_type_idx(const Token &tok)
{
    // within the plain type range
    if(is_builtin_type_tok(tok))
    {
        return u32(builtin_type_from_tok(tok));
    }

    else if(tok.type == token_type::func)
    {
        return FUNC_POINTER;
    }

    // we might not know what this is yet so we will resolve the idx properly later...
    else if(tok.type == token_type::symbol)
    {
        return USER_TYPE;
    }

    else
    {
        return INVALID_TYPE;
    }    
}


TypeNode *parse_type(Parser &parser, b32 allow_fail)
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

    // read out the plain type

    auto plain_tok = next_token(parser);

    u32 type_idx = plain_type_idx(plain_tok);


    if(type_idx == INVALID_TYPE || type_idx == u32(builtin_type::null_t))
    {
        if(!allow_fail)
        {
            panic(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        }
        return nullptr;
    }

    String type_literal;
    
    switch(type_idx)
    {
        case USER_TYPE:
        {
            type_literal = plain_tok.literal;
            break;
        }

        case FUNC_POINTER:
        {
            TypeNode* type = (TypeNode*)ast_type_decl(parser,"func_pointer",plain_tok);
            type->type_idx = FUNC_POINTER;
            type->func_type = parse_func_sig(parser,"func_pointer",plain_tok);
            return type;
        }

        default:
        {
            type_literal = TYPE_NAMES[type_idx];
            break;
        }
    }

    TypeNode* type = (TypeNode*)ast_type_decl(parser,type_literal,plain_tok);
    type->type_idx = type_idx;
    type->is_const = is_const;
    type->is_constant = is_constant;


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
                            AstNode* e = ast_unary(parser,expr_terminate(parser,"array declaration",token_type::sr_brace), ast_type::arr_fixed, plain_tok);

                            push_var(type->compound_type,e);
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




AstNode *declaration(Parser &parser, token_type terminator, b32 is_const_decl = false)
{
    // declartion
    // symbol ':' type ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    consume(parser,token_type::colon);


    TypeNode* type = parse_type(parser);

    if(!type)
    {
        type_panic(parser);
        return nullptr;
    }

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
            
            decl->expr = statement_terminate(parser,"declaration");
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
                panic(parser,eq,"malformed declartion: got %s expected terminator %s\n",tok_name(eq.type),tok_name(terminator));
            }
            break;
        }
    }

    return (AstNode*)decl;
}

AstNode *auto_decl(Parser &parser)
{
    // symbol := expr;
    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    consume(parser,token_type::decl);

    AstNode* e = statement_terminate(parser,"auto declaration");
    AstNode* decl = (AstNode*)ast_auto_decl(parser,s.literal,e,s);

    return decl;    
}


AstNode* tuple_assign(Parser& parser, const Token& t)
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
                sym_node = var(parser,sym_tok);
                break;
            }

            case token_type::deref:
            {
                const Token deref_tok = next_token(parser);

                sym_node = ast_unary(parser,var(parser,deref_tok),ast_type::deref,sym_tok);
                break;
            }

            default:
            {
                panic(parser,t,"tuple assignment attempted on non symbol: %s\n",tok_name(sym_tok.type));
                return nullptr;
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
                    consume(parser,token_type::scope);
                    
                    const auto sym_tok = next_token(parser);

                    tuple_node->func_call = (FuncCallNode*)func_call(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);

                    return ast_scope(parser,(AstNode*)tuple_node,next.literal,next);
                }

                else
                {
                    tuple_node->func_call = (FuncCallNode*)func_call(parser,ast_literal(parser,ast_type::symbol,next.literal,next),next);
                    consume(parser,token_type::semi_colon);
                    done = true;
                }

                break;
            }

            // something has gone wrong
            default: 
            {
                panic(parser,t,"malformed tuple statement ");
                return nullptr;
            }
        }
    }

    return (AstNode*)tuple_node;    
}

AstNode* opt_block(Parser& parser)
{
    BlockNode* block_node = nullptr;
    
    // block is optional
    if(match(parser,token_type::left_c_brace))
    {
        // read out the block
        block_node = block(parser);
    }

    return (AstNode*)block_node;
}


AstNode *struct_access(Parser& parser, AstNode* expr_node,const Token& t)
{
    RecordNode* member_root = (RecordNode*)ast_record(parser,ast_type::access_members,t);

    BinNode* root = (BinNode*)ast_binary(parser,expr_node,(AstNode*)member_root,ast_type::access_struct,t);

    while(match(parser,token_type::dot))
    {
        consume(parser,token_type::dot);

        const auto member_tok = next_token(parser);

        if(member_tok.type == token_type::symbol)
        {
            // perform peeking for modifers
            if(match(parser,token_type::sl_brace))
            {
                push_var(member_root->nodes,array_index(parser,member_tok));
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
            panic(parser,member_tok,"expected struct member got %s(%s)\n",member_tok.literal.buf,tok_name(member_tok.type));
            return nullptr;            
        }
    }

    return (AstNode*)root;
}


AstNode* array_index(Parser& parser,const Token& t)
{
    IndexNode* arr_access = (IndexNode*)ast_index(parser,t.literal,t);

    while(match(parser,token_type::sl_brace))
    {
        consume(parser,token_type::sl_brace);

        AstNode* e = expr_terminate(parser,"array indexing",token_type::sr_brace);
        push_var(arr_access->indexes,e);
    }

    return (AstNode*)arr_access;
}

AstNode* arr_access(Parser& parser, const Token& t)
{
    AstNode* arr_access = array_index(parser,t);

    if(match(parser,token_type::dot))
    {
        return struct_access(parser,arr_access,t);
    }

    else
    {
        return arr_access;
    }
}

AstNode* var(Parser& parser, const Token& sym_tok, b32 allow_call)
{
    const Token next = peek(parser,0);

    AstNode* node = nullptr;

    switch(next.type)
    {
        case token_type::dot:
        {   
            node = struct_access(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);
            break;
        }

        case token_type::sl_brace:
        {
            node = arr_access(parser,sym_tok);
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

AstNode* func_call(Parser& parser,AstNode *expr, const Token& t, Array<TypeNode*>* generic)
{
    consume(parser,token_type::left_paren);

    FuncCallNode* func_call = (FuncCallNode*)ast_call(parser,expr,generic,t);


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
        auto [node,term_seen] = expr_list(parser,"function call",token_type::right_paren);

        push_var(func_call->args,node);

        // no more args terminate the call
        done = term_seen;
    }

    return (AstNode*)func_call;
}

AstNode* const_assert(Parser& parser,const Token& t)
{
    consume(parser,token_type::left_paren);

    const auto expr = expr_terminate(parser,"const_assert",token_type::right_paren);
    consume(parser,token_type::semi_colon);

    return ast_unary(parser,expr,ast_type::const_assert,t);       
}

AstNode* parse_for_iter(Parser& parser, const Token& t, b32 term_paren)
{
    // e.g for(i := 0; i < size; i += 1)

    ForIterNode* for_node = (ForIterNode*)ast_for_iter(parser,t);

    // handle first stmt
    // decl 
    if(peek(parser,1).type == token_type::colon)
    {
        for_node->initializer = declaration(parser,token_type::semi_colon);
    }

    // auto decl
    else if(peek(parser,1).type == token_type::decl)
    {
        for_node->initializer = auto_decl(parser);  
    }

    // standard stmt
    else
    {
        for_node->initializer = statement_terminate(parser,"for initializer statement");
    }
    
    for_node->cond = statement_terminate(parser,"for condition"); 

    // allow paren terminator followed by a '{'
    if(term_paren)
    {
        for_node->post = expr_terminate(parser,"for post statement",token_type::right_paren);
        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            panic(parser,next,"invalid iter for statement terminator: %s expected {\n",tok_name(next.type));
            return nullptr;                        
        }
    }

    // statement was not wrapped by parens
    // expect brace to end it
    else
    {
        for_node->post = expr_terminate(parser,"for post statement",token_type::left_c_brace);
        prev_token(parser);
    }  
    
    // for stmt parsed now compile the actual block
    for_node->block = block(parser);

    return (AstNode*)for_node;
}

AstNode* parse_for_range(Parser& parser,const Token& t, b32 term_paren, b32 take_index, b32 take_pointer)
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
        panic(parser,name_one,"Expected name for range for statement");
        return nullptr;
    }


    for_node->name_one = name_one.literal;

    // get the 2nd name
    if(take_index)
    {
        consume(parser,token_type::comma);

        const auto name_two = next_token(parser);

        if(name_two.type != token_type::symbol)
        { 
            panic(parser,name_two,"Expected name for range for statement");
            return nullptr;
        }

        for_node->name_two = name_two.literal;

        consume(parser,token_type::sr_brace);
    }

    consume(parser,token_type::in_t);

    if(term_paren)
    { 
        for_node->cond = expr_terminate(parser,"for range cond",token_type::right_paren);
        auto next = peek(parser,0);
        if(next.type != token_type::left_c_brace)
        {
            panic(parser,next,"invalid range for statement terminator: %s expected {\n",tok_name(next.type));
            return nullptr;                        
        }
    }

    else
    {
        for_node->cond = expr_terminate(parser,"for range cond statement",token_type::left_c_brace);
        prev_token(parser);
    }

    // for stmt parsed now compile the actual block
    for_node->block = block(parser);

    return (AstNode*)for_node;
}

AstNode* parse_template(Parser& parser, const Token& t)
{
    Array<TypeNode*> generic;

    b32 done = false;

    while(!done)
    {
        TypeNode* type = parse_type(parser,true);

        if(parser.error)
        {
            destroy_arr(generic);
            return nullptr;
        }

        push_var(generic,type);

        // more args
        if(match(parser,token_type::comma))
        {
            consume(parser,token_type::comma);
        }

        // end
        else if(match(parser,token_type::logical_gt))
        {
            consume(parser,token_type::logical_gt);
            done = true;
        }

        // we have a problem!
        else
        {
            destroy_arr(generic);
            return nullptr;  
        }
    }

    return func_call(parser,ast_literal(parser,ast_type::symbol,t.literal,t),t,&generic);
}

AstNode* template_or_var(Parser& parser, const Token& t)
{
    const auto old = parser.tok_idx;

    AstNode* call = parse_template(parser,t);

    if(call)
    {
        return call;
    }

    // did not find function template
    // walk back the parser behind <
    parser.tok_idx = old - 1;

    return var(parser,t,true);
}

AstNode *statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        case token_type::ret:
        {
            // return value is optional
            if(!match(parser,token_type::semi_colon))
            {
                RecordNode* record = (RecordNode*)ast_record(parser,ast_type::ret,t);

                b32 done = false;

                // can be more than one expr (comma seperated)
                while(!done)
                {
                    auto [e,term_seen] = expr_list(parser,"return",token_type::semi_colon);
                    done = term_seen;

                    if(!e)
                    {
                        panic(parser,t,"malformed tuple return");
                        return nullptr;
                    }

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

                // template usage
                case token_type::logical_lt:
                {
                    consume(parser,token_type::logical_lt);
                    return template_or_var(parser,t);
                }

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
                    panic(parser,t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    break;
                }
            }
            break;
        }

        case token_type::left_c_brace:
        {
            // block expects to see the left c brace
            prev_token(parser);

            return (AstNode*)block(parser);
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
            AstNode* while_expr = expr_terminate(parser,"while condition statement",token_type::left_c_brace); prev_token(parser); 
            AstNode* while_body = (AstNode*)block(parser);

            AstNode* while_stmt = ast_binary(parser,while_expr,while_body,ast_type::while_block,t);

            return while_stmt;
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            BlockNode* if_block = (BlockNode*)ast_if_block(parser,t);


            AstNode* if_expr = expr_terminate(parser,"if condtion statement",token_type::left_c_brace); prev_token(parser); 
            AstNode* if_body = (AstNode*)block(parser);

            BinNode *if_stmt = (BinNode*)ast_binary(parser,if_expr,if_body,ast_type::if_t,t);

            push_var(if_block->statements,if_stmt);
            
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

                        AstNode* else_if_expr = expr_terminate(parser,"else if condition statement",token_type::left_c_brace); prev_token(parser); 
                        AstNode* else_if_body = (AstNode*)block(parser);

                        BinNode* else_if_stmt = (BinNode*)ast_binary(parser,else_if_expr,else_if_body,ast_type::else_if_t,else_tok);

                        push_var(if_block->statements,else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        AstNode* else_body = (AstNode*)block(parser);
                        AstNode *else_stmt = ast_unary(parser,else_body,ast_type::else_t,else_tok);

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
            AstNode* e = expr_terminate(parser,"switch statement",token_type::left_c_brace);
            SwitchNode* switch_node = (SwitchNode*)ast_switch(parser,e,t);

            // while we havent exhaused every case
            while(!match(parser,token_type::right_c_brace))
            {
                const auto case_tok = peek(parser,0);


                if(case_tok.type == token_type::default_t)
                {
                    if(switch_node->default_statement)
                    {
                        panic(parser,case_tok,"Cannot have two default statements in switch statement\n");
                    }

                    consume(parser,token_type::default_t);
                    consume(parser,token_type::colon);

                    AstNode* block_node = (AstNode*)block(parser);
                    switch_node->default_statement = (UnaryNode*)ast_unary(parser,block_node,ast_type::default_t,case_tok);      
                }

                else
                {
                    // read out the case
                    consume(parser,token_type::case_t);
                    AstNode* case_node = expr_terminate(parser,"switch case",token_type::colon);

                    BlockNode* block_node = (BlockNode*)block(parser);

                    CaseNode* case_statement = (CaseNode*)ast_case(parser,case_node,block_node,case_tok);
                    push_var(switch_node->statements,case_statement);
                }

                
                if(parser.error)
                {
                    return nullptr;
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
            panic(parser,t,"statement: unexpected token '%s' : %d\n",tok_name(t.type),u32(t.type));
            break;
        }
    }

    return nullptr;
}

BlockNode *block(Parser &parser)
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
            panic(parser,tok,"unterminated block!\n");
            return nullptr;
        }

        auto stmt = statement(parser);

        if(stmt)
        {
            push_var(b->statements,stmt);
        }

        if(parser.error)
        {
            return b;
        }
    }
    

    consume(parser,token_type::right_c_brace);

    return b;
}


void type_alias(Interloper& itl, Parser &parser)
{
    // type_alias literal '=' type ';'
    const auto token = next_token(parser);

    
    if(token.type == token_type::symbol)
    {
        consume(parser,token_type::equal);
        TypeNode* rtype = parse_type(parser);

        const String& name = token.literal;

        if(contains(itl.type_def,name))
        {
            panic(itl,itl_error::redeclaration,"type %s redeclared as alias\n",name.buf);
            return;
        }

        AstNode* alias_node = ast_alias(parser,rtype,name,parser.cur_file,parser.cur_name_space,token);
    
        consume(parser,token_type::semi_colon);

        add_type_def(itl, def_kind::alias_t,alias_node, name, parser.cur_file,parser.cur_name_space);
    }

    else 
    {
        panic(parser,token,"expected symbol for type alias name got %s\n",tok_name(token.type));
    }
}

// parse just the function signature
// NOTE: this is used to parse signatures for function pointers
FuncNode* parse_func_sig(Parser& parser,const String& func_name, const Token& token)
{
    FuncNode *f = (FuncNode*)ast_func(parser,func_name,parser.cur_file,parser.cur_name_space,token);

    // generic decl
    if(match(parser,token_type::logical_lt))
    {
        consume(parser,token_type::logical_lt);

        if(match(parser,token_type::symbol))
        {
            const auto name = next_token(parser);
            push_var(f->generic_name,name.literal);
        }

        else
        {
            panic(parser,token,"unexecpted token in generic decl");
            return nullptr;
        }

        consume(parser,token_type::logical_gt);
    }

    if(parser.error)
    {
        return nullptr;
    }

    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);


    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            panic(parser,paren,"unterminated function declaration!\n");
            return nullptr;
        }

        // for each arg pull type, name
       

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            panic(parser,lit_tok,"expected name for function arg got %s\n",tok_name(lit_tok.type));
            return nullptr;
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
                panic(parser,lit_tok,"va_args can only be placed as the last arg : got %s\n",tok_name(peek(parser,0).type));
                return nullptr;
            }
        }

        else
        {
            TypeNode* type = parse_type(parser);

            if(!type)
            {
                type_panic(parser);
                return nullptr;
            }


            // add each declartion
            DeclNode* decl = (DeclNode*)ast_decl(parser,lit_tok.literal,type,false,lit_tok);
            
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
                panic(parser,paren,"unterminated function declaration!\n");
                return nullptr;
            }
            
            TypeNode* return_type = parse_type(parser);

            if(!return_type)
            {
                type_panic(parser);
                return nullptr;
            }

            push_var(f->return_type,return_type);

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
        TypeNode* return_type = parse_type(parser);

        if(!return_type)
        {
            type_panic(parser);
        }

        push_var(f->return_type,return_type);
    }

    // void
    else
    {
        TypeNode* return_type = (TypeNode*)ast_type_decl(parser,"void",token); 
        return_type->type_idx = u32(builtin_type::void_t);

        push_var(f->return_type,return_type);
    }

    return f;
}

void func_decl(Interloper& itl, Parser &parser)
{

    // func_dec = func ident(arg...) return_type 
    // arg = ident : type,

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        panic(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return;
    }

    if(func_exists(itl,func_name.literal,parser.cur_name_space))
    {
        panic(itl,itl_error::redeclaration,"function %s has been declared twice!\n",func_name.literal.buf);
        return;
    }

    FuncNode* f = parse_func_sig(parser,func_name.literal,func_name);

    if(!f)
    {
        return;
    }

    f->block = block(parser); 

    // finally add the function def
    add_func(itl,func_name.literal,parser.cur_name_space,f);
}

void struct_decl(Interloper& itl,Parser& parser, u32 flags = 0)
{
    const auto name = next_token(parser);

    if(name.type != token_type::symbol)
    {
        panic(parser,name,"expected name after struct decl got %s\n",tok_name(name.type));
        return;
    }

    if(contains(itl.type_def,name.literal))
    {
        panic(itl,itl_error::redeclaration,"type %s redeclared as struct\n",name.literal.buf);
        return;
    }

    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,parser.cur_file,parser.cur_name_space,name);

    struct_node->attr_flags = flags;

    // Does this struct have a forced first member?
    if(match(parser,token_type::left_paren))
    {
        consume(parser,token_type::left_paren);

        struct_node->forced_first = (DeclNode*)declaration(parser,token_type::right_paren);
    }

    consume(parser,token_type::left_c_brace);

    while(!match(parser,token_type::right_c_brace))
    {
        DeclNode* decl = (DeclNode*)declaration(parser,token_type::semi_colon);

        if(!decl)
        {
            panic(parser,name,"malformed struct member decl\n");
            return;
        }

        push_var(struct_node->members,decl);
    }

    consume(parser,token_type::right_c_brace);

    // semi colon after decl is optional
    if(match(parser,token_type::semi_colon))
    {
        consume(parser,token_type::semi_colon);
    }


    add_type_def(itl, def_kind::struct_t,(AstNode*)struct_node, struct_node->name, parser.cur_file,parser.cur_name_space);
}

void enum_decl(Interloper& itl,Parser& parser, u32 flags)
{
    const auto name_tok = next_token(parser);

    if(name_tok.type != token_type::symbol)
    {
        panic(itl,itl_error::missing_name,"Expected symbol for enum name got %s\n",tok_name(name_tok.type));
        return;
    }


    // check redeclaration
    TypeDef* type_def = lookup(itl.type_def,name_tok.literal);

    if(type_def)
    {
        panic(itl,itl_error::redeclaration,"%s %s redeclared as enum\n",KIND_NAMES[u32(type_def->kind)],name_tok.literal.buf);
        return;
    }

    EnumNode* enum_node = (EnumNode*)ast_enum(parser,name_tok.literal,parser.cur_file,parser.cur_name_space,name_tok);


    if(match(parser,token_type::colon))
    {
        consume(parser,token_type::colon);

        const auto type_tok = next_token(parser);

        if(type_tok.type == token_type::symbol)
        {
            enum_node->struct_name = type_tok.literal;
            enum_node->kind = enum_type::struct_t;
        }

        else if(is_builtin_type_tok(type_tok))
        {
            enum_node->kind = enum_type::int_t;
            enum_node->type = builtin_type_from_tok(type_tok);
        }

        else
        {
            panic(parser,next_token(parser),"Expected type name got %s\n",type_tok.type);
            return;            
        }
    }


    if((flags & ATTR_FLAG) && enum_node->kind != enum_type::int_t)
    {
        panic(parser,next_token(parser),"Flag enum must specify underlying intergeral type");
        return;        
    }

    consume(parser,token_type::left_c_brace);

    enum_node->attr_flags = flags;

    // push each member till we hit the terminating brace
    while(!match(parser,token_type::right_c_brace))
    {
        const auto member_tok = next_token(parser);

        if(member_tok.type != token_type::symbol)
        {
            panic(parser,member_tok,"Expected symbol for enum %s member got %s\n",name_tok.literal.buf,tok_name(member_tok.type));
            return;
        }

        EnumMemberDecl member;
        member.name = member_tok.literal;

        // see if we have an initlizer
        if(match(parser,token_type::equal))
        {
            consume(parser,token_type::equal);

            token_type term;

            AstNode* initializer = expr_terminate(parser,"enum struct init",token_type::comma,term);

            member.initializer = initializer;
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
    add_type_def(itl, def_kind::enum_t,(AstNode*)enum_node, enum_node->name, parser.cur_file,parser.cur_name_space);
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

u32 parse_attr(Parser& parser, const Token& tok)
{
    u32 flags = 0;

    consume(parser,token_type::left_paren);

    const auto attr = next_token(parser);

    if(attr.type != token_type::symbol)
    {
        panic(parser,tok,"Expected name for attr got %s\n",tok_name(attr.type));
        return 0;
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
        panic(parser,tok,"Unknown attr %s\n",attr_name.buf);
        return 0;
    }

    consume(parser,token_type::right_paren);

    return flags;
}

void parse_directive(Interloper& itl,Parser& parser)
{
    const auto next = next_token(parser);

    if(next.type != token_type::symbol)
    {
        panic(parser,next,"Expected name for directive got %s\n",tok_name(next.type));
        return;
    }

    // TODO: move this lookup to a hashtable if it starts getting large
    const auto name = next.literal;

    if(name == "attr")
    {
        const u32 flags = parse_attr(parser,next);

        if(parser.error)
        {
            return;
        }

        const auto stmt = peek(parser,0);

        // look at which declaration is next?
        switch(stmt.type)
        {
            case token_type::struct_t:
            {
                consume(parser,token_type::struct_t);
                
                struct_decl(itl,parser,flags);
                break;
            }

            case token_type::enum_t:
            {
                consume(parser,token_type::enum_t);
                
                enum_decl(itl,parser,flags); 
                break; 
            }

            default:
            {
                panic(parser,stmt,"Attribute is not legal on stmt: %s\n",tok_name(stmt.type));
                return;    
            }
        }
    }

    else
    {
        panic(parser,next,"Unknown directive %s\n",name.buf);
        return;
    }
}

void parse_top_level_token(Interloper& itl, Parser& parser, FileQueue& queue)
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
                    panic(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(err.type),err.literal.buf);
                }

                const auto name_tok = next_token(parser);

                consume(parser,token_type::logical_gt);

                if(parser.error)
                {
                    return;
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
                panic(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(t.type),t.literal.buf);
                return;
            }
            break;
        }

        // function declartion
        case token_type::func:
        {
            func_decl(itl,parser);
            break;
        }

        case token_type::struct_t:
        {
            struct_decl(itl,parser);
            break;
        }

        case token_type::enum_t:
        {
            enum_decl(itl,parser,0);
            break;
        }

        case token_type::type_alias:
        {
            type_alias(itl,parser);
            break;
        }

        // global constant
        case token_type::constant_t:
        {
            DeclNode* decl = (DeclNode*)declaration(parser,token_type::semi_colon,true);

            GlobalDeclNode* const_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_name_space,t);

            push_var(itl.constant_decl,const_decl);
            break; 
        }

        // global mut
        case token_type::global_t:
        {
            DeclNode* decl = (DeclNode*)declaration(parser,token_type::semi_colon);

            GlobalDeclNode* global_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,parser.cur_file,parser.cur_name_space,t);

            push_var(itl.global_decl,global_decl);
            break; 
        }

        case token_type::namespace_t:
        {
            const auto name = next_token(parser);

            if(name.type != token_type::symbol)
            {
                panic(parser,name,"Expected name for namespace got: %s",tok_name(name.type));
            }   

            // read out the name space, and put it on string allocator
            // so we can freely pass it
            parser.cur_name_space = copy_string(*parser.string_allocator,name.literal);


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
                panic(parser,t,"unexpected top level symbol '%s'\n",t.literal.buf);
            }

            else
            {
                panic(parser,t,"unexpected top level token '%s' : (%d)\n",tok_name(t.type),u32(t.type));
            }

            return;
        }
    }
}

bool parse_file(Interloper& itl,const String& file, const String& filename,FileQueue& queue)
{
    // Parse out the file
    Parser parser = make_parser(filename,&itl.ast_allocator,&itl.ast_string_allocator,&itl.ast_arrays);

    if(tokenize(file,filename,parser.string_allocator,parser.tokens))
    {
        destroy_arr(parser.tokens);
        itl.error = true;
        itl.error_code = itl_error::lexer_error;
        return true;
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

            parse_directive(itl,parser);
        }

        // plain decl
        else
        {
            parse_top_level_token(itl,parser,queue);
        }

 
        if(itl.error)
        {
            destroy_arr(parser.tokens);
            return true;
        }

        if(parser.error)
        {
            // print line number
            print_line(filename,parser.line);
            destroy_arr(parser.tokens);
            return true;
        }
    }

    destroy_arr(parser.tokens);
    return false;
}

bool parse(Interloper& itl, const String& initial_filename)
{
    FileQueue queue;
    queue.set = make_set<String>();

    
    // add the initial file
    add_file(queue,get_program_name(itl.string_allocator,initial_filename));


    // TODO: this should probably be a SHELL VAR but just hard code it for now
    itl.stl_path = make_static_string("stl/",strlen("stl/"));

    // import basic by default
    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"basic.itl"));

    add_file(queue,cat_string(itl.string_allocator,itl.stl_path,"internal.itl"));

    b32 error = false;

    while(count(queue.stack))
    {
        // get the next filename to parse
        const String filename = pop(queue.stack);

        auto [file,err] = read_str_buf(filename);

        if(err)
        {
            printf("file %s does not exist\n",filename.buf);
            error = true;
            break;
        }

        error = parse_file(itl,make_string(file),filename,queue);

        destroy_arr(file);

        if(error)
        {
            break;
        }
    }

    destroy_arr(queue.stack);
    destroy_set(queue.set);

    return error;
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


        case ast_fmt::function:
        {
            FuncNode* func_node = (FuncNode*)root;

            printf("function %s:%s",func_node->filename.buf,func_node->name.buf);

            if(count(func_node->generic_name))
            {
                putchar('<');

                for(u32 t = 0; t < count(func_node->generic_name); t++)
                {
                    printf("%s,",func_node->generic_name[t].buf);
                }

                putchar('>');  
            }

            putchar('\n');

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

            if(enum_node->kind == enum_type::struct_t)
            {
                printf("enum %s:%s : %s\n",enum_node->filename.buf,enum_node->name.buf,enum_node->struct_name.buf);
            }

            else
            {
                printf("enum %s:%s : type idx %d\n",enum_node->filename.buf,enum_node->name.buf,u32(enum_node->type));
            }

            
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

            for(u32 g = 0; g < count(func_call->generic); g++)
            {
                print((AstNode*)func_call->generic[g]);
            }

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

            printf("%s\n",scope_node->scope.buf);
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

    for(u32 i = 0; i < file.size; i++)
    {
        if(i == idx)
        {
            break;
        }

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

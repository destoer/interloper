#include <interloper.h>
#include "expression.cpp"
#include "enum.cpp"

void type_panic(Parser &parser);
BlockNode *block(Parser &parser);


const u32 AST_ALLOC_DEFAULT_SIZE = 8 * 1024;

Parser make_parser(const String& cur_file,ArenaAllocator* ast_allocator,ArenaAllocator* string_allocator, AstPointers* ast_arrays)
{
    Parser parser;
    parser.allocator = ast_allocator;
    parser.string_allocator = string_allocator;
    parser.cur_file = cur_file;
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
        return token_plain(token_type::eof,0,0);
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
        return token_plain(token_type::eof,0,0);
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

u32 plain_type_idx(const Token &tok)
{
    // within the plain type range
    if(is_builtin_type_tok(tok))
    {
        // compute builtin type idx 
        return s32(tok.type) - s32(token_type::u8);
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


TypeNode *parse_type(Parser &parser)
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
        panic(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        return nullptr;
    }

    String type_literal;
    
    if(type_idx == USER_TYPE)
    {   
        type_literal = plain_tok.literal;
    }

    else
    {
        type_literal = TYPE_NAMES[type_idx];
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




AstNode *declaration(Parser &parser, token_type terminator)
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

    DeclNode* decl = (DeclNode*)ast_decl(parser,s.literal,type,s);

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
                consume(parser,token_type::equal);

                tuple_node->func_call = (FuncCallNode*)func_call(parser,next_token(parser));
                consume(parser,token_type::semi_colon);
                done = true;
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

AstNode* var(Parser& parser, const Token& sym_tok)
{
    const Token next = peek(parser,0);

    switch(next.type)
    {
        case token_type::dot:
        {   
            return struct_access(parser,ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok),sym_tok);
        }

        case token_type::sl_brace:
        {
            return arr_access(parser,sym_tok);
        }


        default:
        {
           return ast_literal(parser,ast_type::symbol,sym_tok.literal,sym_tok);
        }
    }
}

AstNode* func_call(Parser& parser,const Token& t)
{
    consume(parser,token_type::left_paren);

    FuncCallNode* func_call = (FuncCallNode*)ast_call(parser,t.literal,t);


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


        case token_type::deref:
        {
            const auto t2 = peek(parser,0);

            if(t2.type != token_type::symbol)
            {
                panic(parser,t2,"statement: expected symbol for deref %s\n");
                break;
            }

            prev_token(parser);
            return statement_terminate(parser,"pointer deference");
        }


        // tuple assign
        case token_type::sl_brace:
        {
            return tuple_assign(parser, t);
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
            ForNode* for_node = (ForNode*)ast_for(parser,t);

            // allow statment to wrapped a in a set of parens
            const bool term_paren = peek(parser,0).type == token_type::left_paren;

            // ignore the first paren
            if(term_paren)
            {
                consume(parser,token_type::left_paren);
            }

            
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
            

            // for(s32 x = 5; x > 0; x -= 1) (multiple statement)

            for_node->cond = statement_terminate(parser,"for condition"); 

            // allow paren terminator followed by a '{'
            if(term_paren)
            {
                for_node->post = expr_terminate(parser,"for post statement",token_type::right_paren);
                auto next = peek(parser,0);
                if(next.type != token_type::left_c_brace)
                {
                    panic(parser,next,"invalid single for statement terminator: %s expected {\n",tok_name(next.type));
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

        push_var(b->statements,statement(parser));

        if(parser.error)
        {
            return b;
        }
    }
    

    consume(parser,token_type::right_c_brace);

    return b;
}


void type_alias(Interloper& itl, Parser &parser, const String& filename)
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

        AstNode* alias_node = ast_alias(parser,rtype,name,filename,token);
    
        consume(parser,token_type::semi_colon);

        add_type_def(itl, def_kind::alias_t,alias_node, name, filename);
    }

    else 
    {
        panic(parser,token,"expected symbol for type alias name got %s\n",tok_name(token.type));
    }
}

void func_decl(Interloper& itl, Parser &parser, const String& filename)
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

    if(contains(itl.function_table,func_name.literal))
    {
        panic(itl,itl_error::redeclaration,"function %s has been declared twice!\n",func_name.literal.buf);
        return;
    }

    FuncNode *f = (FuncNode*)ast_func(parser,func_name.literal,filename,func_name);

    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);


    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            panic(parser,paren,"unterminated function declaration!\n");
            return;
        }

        // for each arg pull type, name
       

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            panic(parser,lit_tok,"expected name for function arg\n");
            return;
        }
        

        consume(parser,token_type::colon);

        TypeNode* type = parse_type(parser);

        if(!type)
        {
            type_panic(parser);
            return;
        }


        // add each declartion
        DeclNode* decl = (DeclNode*)ast_decl(parser,lit_tok.literal,type,lit_tok);
        
        push_var(f->args,decl);

        // if the declaration isnt closed get the next arg
        if(peek(parser,0).type != token_type::right_paren)
        {
            consume(parser,token_type::comma);
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
                return;
            }
            
            TypeNode* return_type = parse_type(parser);

            if(!return_type)
            {
                type_panic(parser);
                return;
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
    else if(!match(parser,token_type::left_c_brace))
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
        TypeNode* return_type = (TypeNode*)ast_type_decl(parser,"void",func_name); 
        return_type->type_idx = u32(builtin_type::void_t);

        push_var(f->return_type,return_type);
    }

    f->block = block(parser); 

    // finally add the function def
    add_func(itl,func_name.literal,f);
}

void struct_decl(Interloper& itl,Parser& parser, const String& filename)
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

    StructNode* struct_node = (StructNode*)ast_struct(parser,name.literal,filename,name);

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


    add_type_def(itl, def_kind::struct_t,(AstNode*)struct_node, struct_node->name, filename);
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


void add_file(HashTable<String,u32> &file_set, Array<String>& stack, const String& filename)
{
    if(!contains(file_set,filename))
    {
        add(file_set,filename,u32(0));
        push_var(stack,filename);
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

bool parse_file(Interloper& itl,const String& file, const String& filename,const String& stl_path, HashTable<String,u32>& file_set, Array<String> &file_stack)
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
        print_tokens(parser.tokens);
    }
    
    const auto size = count(parser.tokens);

    // TODO: move this to a seperate loop to make freeing up crap ez
    // TODO: put an extra string in the top level decl of the ast
    // so we know what file it came from
    while(parser.tok_idx < size)
    {
        const auto &t = next_token(parser);

        // okay what is our "top level" token
        switch(t.type)
        { 
            case token_type::import:
            {
                if(!match(parser,token_type::string))
                {
                    panic(parser,next_token(parser),"expected string for import got %s : %s\n",tok_name(t.type),t.literal.buf);
                    destroy_arr(parser.tokens);
                    return true;
                }

                const auto name_tok = next_token(parser);

                // stl file
                if(!contains_ext(name_tok.literal))
                {
                    add_file(file_set,file_stack, cat_string(itl.string_allocator,stl_path,get_program_name(itl.string_allocator,name_tok.literal)));
                }

                else
                {
                    add_file(file_set,file_stack,name_tok.literal);
                }
                break;
            }

            // function declartion
            case token_type::func:
            {
                func_decl(itl,parser,filename);
                break;
            }

            case token_type::struct_t:
            {
                struct_decl(itl,parser,filename);
                break;
            }

            case token_type::enum_t:
            {
                enum_decl(itl,parser,filename);
                break;
            }

            case token_type::type_alias:
            {
                type_alias(itl,parser,filename);
                break;
            }

            // global constant
            case token_type::constant_t:
            {
                DeclNode* decl = (DeclNode*)declaration(parser,token_type::semi_colon);

                GlobalDeclNode* const_decl = (GlobalDeclNode*)ast_global_decl(parser,decl,true,filename,t);

                push_var(itl.constant_decl,const_decl);
                break; 
            }

            default:
            {
                panic(parser,t,"unexpected top level token %s(%d)'\n",tok_name(t.type),u32(t.type));
                destroy_arr(parser.tokens);
                return true;
            }
        }

        if(itl.error)
        {
            destroy_arr(parser.tokens);
            return true;
        }

        if(parser.error)
        {
            // print line number
            print_line(filename,parser.line + 1);
            destroy_arr(parser.tokens);
            return true;
        }
    }

    destroy_arr(parser.tokens);
    return false;
}

bool parse(Interloper& itl, const String& initial_filename)
{
    // TODO: destruct these
    Array<String> file_stack;
    HashTable<String,u32> file_set = make_table<String,u32>();

    // add the initial file
    add_file(file_set,file_stack,get_program_name(itl.string_allocator,initial_filename));


    // TODO: this should probably be a SHELL VAR but just hard code it for now
    const String stl_path = make_static_string("stl/",strlen("stl/"));

    // import basic by default
    add_file(file_set,file_stack,cat_string(itl.string_allocator,stl_path,"basic.itl"));

    add_file(file_set,file_stack,cat_string(itl.string_allocator,stl_path,"internal.itl"));


    b32 error = false;

    while(count(file_stack))
    {
        // get the next filename to parse
        const String filename = pop(file_stack);

        auto [file,err] = read_str_buf(filename);

        if(err)
        {
            printf("file %s does not exist\n",filename.buf);
            error = true;
            break;
        }

        error = parse_file(itl,make_string(file),filename,stl_path,file_set,file_stack);

        destroy_arr(file);

        if(error)
        {
            break;
        }
    }

    destroy_arr(file_stack);
    destroy_table(file_set);

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

void print(const AstNode *root)
{
    static int depth = 0;

    if(!root)
    {
        print_depth(depth + 1);
        puts("EMPTY");
        return;
    }


    if(root->type == ast_type::function || root->type == ast_type::struct_t)
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

            printf("value: %s%d\n",value_node->value.sign? "-" : "",value_node->value.v);
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

            print((AstNode*)func_node->block);

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

        case ast_fmt::char_t:
        {
            CharNode* char_node = (CharNode*)root;
            
            printf("char: '%c' : %d\n",char_node->character,char_node->character);

            break;
        }

        case ast_fmt::type:
        {
            TypeNode* type_decl = (TypeNode*)root;
            printf("type: %s %s\n", type_decl->is_const? "const" : "",type_decl->name.buf);

            for(u32 c = 0; c < count(type_decl->compound_type); c++)
            {
                print(type_decl->compound_type[c]);
            }
            break;
        }

        case ast_fmt::declaration:
        {
            DeclNode* decl_node = (DeclNode*)root;

            printf("decl : %s\n",decl_node->name.buf);

            print((AstNode*)decl_node->type);
            print(decl_node->expr);
            break;
        }

        case ast_fmt::auto_decl:
        {
            assert(false);
            break;
        }

        case ast_fmt::global_declaration:
        {
            GlobalDeclNode* global_node = (GlobalDeclNode*)root;
            printf("global %s decl:\n",global_node->is_const? "const" : "");
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

            printf("function call : %s\n",func_call->name.buf);

            for(u32 a = 0; a < count(func_call->args); a++)
            {
                print(func_call->args[a]);
            }                                    

            break;
        }

        case ast_fmt::for_block:
        {
            printf("for\n");

            ForNode* for_node = (ForNode*)root;

            print(for_node->initializer);
            print(for_node->cond);
            print(for_node->post);

            print((AstNode*)for_node->block);

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
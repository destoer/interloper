#include <interloper.h>


void type_panic(Parser &parser);
AstNode *block(Parser &parser);


Token next_token(Parser &parser)
{
    const auto &vt = *parser.tokens;
    if(parser.tok_idx >= vt.size())
    {
        // TODO: make this return the actual file end
        // for row and col
        return Token(token_type::eof,"",0,0);
    }

    return vt[parser.tok_idx++];  
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
    const auto &vt = *parser.tokens;
    if(idx >= vt.size())
    {
        return Token(token_type::eof,"",0,0);
    }

    return vt[idx];
}



Value read_value(const Token &t)
{
    const u32 v = convert_imm(t.literal); 

    // is this literal a -ve?
    const bool sign = t.literal[0] == '-';

    return Value(v,sign);
}

void consume(Parser &parser,token_type type)
{
    const auto &vt = *parser.tokens;
    const auto t = parser.tok_idx >= vt.size()? token_type::eof : vt[parser.tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token(parser);
        panic(parser,tok,"expected %s got %s\n", tok_name(type),tok_name(t));
    }
    parser.tok_idx += 1;
}

bool match(Parser &parser,token_type type)
{
    const auto &vt = *parser.tokens;
    const auto t = parser.tok_idx >= vt.size()? token_type::eof : vt[parser.tok_idx].type;

    return t == type;
}


AstNode *copy_node(const AstNode *node)
{
    if(!node)
    {
        return nullptr;
    }

    auto copy  = new AstNode();

    copy->type = node->type;
    copy->literal = node->literal;

    switch(copy->type)
    {
        case ast_type::value:
        {
            copy->value = node->value;
            break;
        }

        case ast_type::ptr_indirection:
        case ast_type::type:
        {
            copy->type_idx = node->type_idx;
        }

        default: break;
    }

    for(const AstNode* n : node->nodes)
    {
        if(n)
        {
            copy->nodes.push_back(copy_node(n));
        }
    }

    return copy;
}


void type_panic(Parser &parser)
{
    const auto tok = peek(parser,1);
    panic(parser,tok,"expected type declaration got: %s\n",tok_name(tok.type));
}


static constexpr s32 INVALID_TYPE = -1;

bool is_builtin_type_tok(const Token &tok)
{
   return tok.type >= token_type::u8 && tok.type <= token_type::bool_t; 
}

s32 plain_type_idx(const Token &tok)
{
    // within the plain type range
    if(is_builtin_type_tok(tok))
    {
        // compute builtin type idx 
        return s32(tok.type) - s32(token_type::u8);
    }


    else if(tok.type == token_type::symbol)
    {
        unimplemented("user defined type");
    }

    else
    {
        return INVALID_TYPE;
    }    
}


AstNode *parse_type(Parser &parser)
{
    // read out the plain type

    auto plain_tok = next_token(parser);

    s32 type_idx = plain_type_idx(plain_tok);

    if(type_idx == INVALID_TYPE)
    {
        panic(parser,plain_tok,"expected plain type got : '%s'\n",tok_name(plain_tok.type));
        return nullptr;
    }

    std::string type_literal = TYPE_NAMES[type_idx];

    // TODO: need to mark the idx for when have user defined types
    auto type = new AstNode(ast_type::type);
    type->type_idx = type_idx;
    

    b32 quit = false;

    while(!quit)
    {
        // parse out other types, arrays, pointers
        switch(peek(parser,0).type)
        {
            // pointer decl
            case token_type::deref:
            {
                u32 ptr_indirection = 0;
                while(peek(parser,0).type == token_type::deref)
                {
                    next_token(parser);
                    ptr_indirection++;
                    type_literal = type_literal + '@';
                }

                auto ptr_node = new AstNode(ast_type::ptr_indirection);
                ptr_node->type_idx = ptr_indirection;

                ptr_node->literal = std::to_string(ptr_indirection);

                type->nodes.push_back(ptr_node);
                break;
            }


            // array decl
            case token_type::sl_brace:
            {
                auto arr_decl = new AstNode(ast_type::arr_dimensions);

                while(peek(parser,0).type == token_type::sl_brace)
                {
                    consume(parser,token_type::sl_brace);

                    // var size
                    if(peek(parser,0).type == token_type::sr_brace)
                    {
                        arr_decl->nodes.push_back(new AstNode(ast_type::arr_var_size));
                        consume(parser,token_type::sr_brace);
                    }

                    else 
                    {
                        arr_decl->nodes.push_back(expr_terminate(parser,token_type::sr_brace));
                    }
                }

                type->nodes.push_back(arr_decl);

                break;
            }

            default: quit = true; break;
        }
    }

    type->literal = type_literal;

    return type;
}




AstNode *declaration(Parser &parser, AstNode *type)
{
    // declartion
    // type symbol ( ';' | '=' expression ';')

    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: '%s'  (%zd)\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    //    [declare:name]
    // [type]   optional([eqauls])

    auto d = new AstNode(ast_type::declaration,s.literal);
    d->nodes.push_back(type);

    const auto eq = peek(parser,0);

    switch(eq.type)
    {
        // declartion without assigment
        case token_type::semi_colon:
        {
            consume(parser,token_type::semi_colon);
            break;
        }

        // declartion with assingment
        case token_type::equal:
        {
            consume(parser,token_type::equal);
            
            const auto e = expr(parser,next_token(parser));
            
            // declartion with initalizer skip over initial symbol
            if(e && e->nodes.size() >= 1)
            {
                d->nodes.push_back(e);
            }
            break;
        }

        default:
        {
            delete_tree(d);
            panic(parser,eq,"malformed declartion: %s\n",tok_name(eq.type));
            break;
        }
    }

    return d;
}

AstNode *auto_decl(Parser &parser)
{
    // decl symbol = expr;
    const auto s = next_token(parser);

    if(s.type != token_type::symbol)
    {
        panic(parser,s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),parser.tok_idx);
        return nullptr;
    }

    
    // okay here we require an expression on the right side
    consume(parser,token_type::equal);

    const auto d = new AstNode(ast_type::auto_decl,s.literal);

    d->nodes.push_back(expr(parser,next_token(parser)));

    return d;    
}

AstNode *statement(Parser &parser)
{
    const auto t = next_token(parser);

    switch(t.type)
    {
        // handle builtin types
        case token_type::u8:
        case token_type::u16:
        case token_type::u32:
        case token_type::s8:
        case token_type::s16:
        case token_type::s32:
        case token_type::bool_t:
        {
            prev_token(parser);

            auto type = parse_type(parser);

            if(!type)
            {
                type_panic(parser);
                return nullptr;
            }

            return declaration(parser,type);
        }

    
        case token_type::decl:
        {
            return auto_decl(parser);
        }

        case token_type::ret:
        {
            auto r = new AstNode(ast_type::ret);

            // return value is optional
            if(peek(parser,0) != token_type::semi_colon)
            {
                r->nodes.push_back(expr(parser,next_token(parser)));
            }
            else
            {
                consume(parser,token_type::semi_colon);
            }

            return r;
        }


        case token_type::deref:
        {
            const auto t2 = peek(parser,0);

            if(t2.type != token_type::symbol)
            {
                panic(parser,t2,"statement: expected symbol for deref %s\n");
                break;
            }

            return expr(parser,t);
        }

        // TODO: detect a declartion with a user defined type
        case token_type::symbol:
        {
            const auto t2 = peek(parser,0);

            switch(t2.type)
            {
                // assignment expr
                case token_type::plus_eq:
                case token_type::minus_eq:
                case token_type::divide_eq:
                case token_type::times_eq:
                case token_type::equal:
                {
                    return expr(parser,t);
                }

                // check for brackets
                // array indexes etc here 

                // function call
                case token_type::left_paren:
                {
                    return expr(parser,t);
                }

                // array access
                case token_type::sl_brace:
                {
                    return expr(parser,t);
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
            parser.tok_idx--;

            return block(parser);
        }

        // assume one cond for now
        case token_type::for_t:
        {
            const auto for_block = new AstNode(ast_type::for_block);

            // allow statment to wrapped a in a set of parens
            const bool term_paren = peek(parser,0).type == token_type::left_paren;
            const token_type first_term = term_paren? token_type::right_paren : token_type::left_c_brace;
            token_type terminator;

            // ignore the first paren
            if(term_paren)
            {
                consume(parser,token_type::left_paren);
            }

            // auto decl
            if(peek(parser,0).type == token_type::decl)
            {
                consume(parser,token_type::decl);
                for_block->nodes.push_back(auto_decl(parser));
                terminator = token_type::semi_colon;                 
            }

            // standard decl
            else
            {

                // decl for builtin type
                if(is_builtin_type_tok(peek(parser,0)))
                {
                    auto type = parse_type(parser);
                    for_block->nodes.push_back(declaration(parser,type));
                    terminator = token_type::semi_colon; 
                }

                // standard stmt
                else
                {
                    for_block->nodes.push_back(expr_terminate(parser,first_term,terminator)); 
                }
            }


            // for(s32 x = 5; x > 0; x -= 1) (multiple statement)
            if(terminator == token_type::semi_colon)
            {
                for_block->nodes.push_back(expr_terminate(parser,token_type::semi_colon)); 

                // allow paren terminator followed by a '{'
                if(term_paren)
                {
                    for_block->nodes.push_back(expr_terminate(parser,token_type::right_paren));
                    auto next = peek(parser,0);
                    if(next.type != token_type::left_c_brace)
                    {
                        panic(parser,next,"invalid single for statement terminator: %s\n",tok_name(next.type));
                        return nullptr;                        
                    }
                }

                // statement was not wrapped by parens
                // expect brace to end it
                else
                {
                    for_block->nodes.push_back(expr_terminate(parser,token_type::left_c_brace));
                    prev_token(parser);
                }  
            }

             // for(x > 0) (single statement)
            else
            {
                // allow paren terminator
                if(terminator == token_type::right_paren && term_paren)
                {
                    terminator = peek(parser,0).type;
                }

                if(terminator != token_type::left_c_brace)
                {
                    panic(parser,peek(parser,0),"invalid single if statement terminator: %s\n",tok_name(terminator));
                    return nullptr;                        
                }
            }


            // for stmt parsed now compile the actual block
            for_block->nodes.push_back(block(parser));

            return for_block;
        }

        // else_if and else parsed out here
        case token_type::if_t:
        {
            const auto if_block = new AstNode(ast_type::if_block);

            const auto if_stmt = new AstNode(ast_type::if_t);

            // read out if expr and block
            if_stmt->nodes.push_back(expr_terminate(parser,token_type::left_c_brace)); prev_token(parser); 
            if_stmt->nodes.push_back(block(parser));

           
            if_block->nodes.push_back(if_stmt);
            
            bool done = false;
            
            while(!done)
            {
                if(peek(parser,0).type == token_type::else_t)
                {
                    consume(parser,token_type::else_t);

                    // we have an else if
                    if(peek(parser,0).type == token_type::if_t)
                    {
                        consume(parser,token_type::if_t);

                        const auto else_if_stmt = new AstNode(ast_type::else_if_t);

                        else_if_stmt->nodes.push_back(expr_terminate(parser,token_type::left_c_brace)); prev_token(parser); 
                        else_if_stmt->nodes.push_back(block(parser));

                        if_block->nodes.push_back(else_if_stmt);
                    }

                    // just a plain else
                    else
                    {
                        const auto else_stmt = new AstNode(ast_type::else_t);

                        // no expr for else
                        else_stmt->nodes.push_back(block(parser));

                        if_block->nodes.push_back(else_stmt);

                        done = true;
                    }
                }

                // this chain is done we have another token
                else
                {
                    done = true;
                }
            }
            return if_block;
        }

        default:
        {
            panic(parser,t,"statement: unexpected token %s\n",tok_name(t.type));
            break;
        }
    }

    return nullptr;
}

AstNode *block(Parser &parser)
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(parser,0);
    consume(parser,token_type::left_c_brace);

    auto b = new AstNode(ast_type::block);

    
    // parse out all our statements
    while(!match(parser,token_type::right_c_brace))
    {
        if(match(parser,token_type::eof))
        {
            delete_tree(b);
            panic(parser,tok,"unterminated block!");
            return nullptr;
        }

        b->nodes.push_back(statement(parser));

        if(parser.error)
        {
            return b;
        }
    }
    

    consume(parser,token_type::right_c_brace);

    return b;
}

AstNode *func(Parser &parser)
{

    // first check this is a valid function definiton and consume it 
    // func_dec = func return_type ident(arg...)
    // arg = type ident

    // can be void (i.e we have no return type)
    AstNode *return_type = nullptr;
    
    // void
    if(peek(parser,0).type == token_type::symbol && peek(parser,1).type == token_type::left_paren)
    {
        return_type = new AstNode(ast_type::type);
        return_type->type_idx = u32(builtin_type::void_t);
    }

    // type specified
    else
    {
        return_type = parse_type(parser);

        if(!return_type)
        {
            type_panic(parser);
            return nullptr;
        }
    }

    // what is the name of our function?
    const auto func_name = next_token(parser);

    if(func_name.type != token_type::symbol)
    {
        panic(parser,func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return nullptr;  
    }

    auto f = new AstNode(ast_type::function, func_name.literal);
    


    const auto paren = peek(parser,0);
    consume(parser,token_type::left_paren);

    auto a = new AstNode(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(parser,token_type::right_paren))
    {
        if(match(parser,token_type::eof))
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,paren,"unterminated function declaration!");
            return nullptr;
        }

        // for each arg pull type, name
        auto type = parse_type(parser);

        if(!type)
        {
            type_panic(parser);
            delete_tree(a);
            delete_tree(f);
            return nullptr;
        }

        const auto lit_tok = next_token(parser);

        if(lit_tok.type != token_type::symbol)
        {
            delete_tree(a);
            delete_tree(f);
            panic(parser,lit_tok,"expected name for function arg");
            return nullptr;
        }
        
        // add each declartion
        auto d = new AstNode(ast_type::declaration,lit_tok.literal);
        d->nodes.push_back(type);

        a->nodes.push_back(d);

        // if the declaration isnt closed get the next arg
        if(peek(parser,0).type != token_type::right_paren)
        {
            consume(parser,token_type::comma);
        }
    }

    consume(parser,token_type::right_paren);

    // no args is fine

    auto b = block(parser);


    //      [func: name]
    // [type] [block]  [args]
    f->nodes.push_back(return_type);
    f->nodes.push_back(b);
    f->nodes.push_back(a);

    return f;
}


bool parse(AstNode **root_ptr, const std::vector<Token> &tokens, const std::vector<std::string> &lines)
{
    panic(!root_ptr,"attempted to parse into null tree");

    *root_ptr = new AstNode(ast_type::root);
    const auto size = tokens.size();


    Parser parser;
    parser.tokens = &tokens;

    while(parser.tok_idx < size)
    {
        const auto &t = next_token(parser);
        // okay what is our "top level" token
        switch(t.type)
        {
            // function declartion
            case token_type::func:
            {
                (*root_ptr)->nodes.push_back(func(parser));
                break;
            }


            default:
            {
                panic(parser,t,"unexpected token %s: %s\n",tok_name(t.type),t.literal.c_str());
                delete_tree(*root_ptr); *root_ptr = nullptr;
                break;
            }
        }

        if(parser.error)
        {
            // print line number
            printf("%s\n",lines[parser.line].c_str());

            break;
        }
    }

    return parser.error;
}

void print(const AstNode *root)
{
    if(!root)
    {
        return;
    }

    if(root->type == ast_type::function)
    {
        printf("\n\n\n");
    }


    static int depth = 0;
    
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d ",depth);
    
    printf(" %s : %s\n",AST_NAMES[static_cast<size_t>(root->type)],root->literal.c_str());
    depth += 1;

    for(const auto &n: root->nodes)
    {
        if(n)
        {
            print(n);
        }
    }
    depth -= 1;
}
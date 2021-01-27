#include <interloper.h>



/*
func s32 main()
{
    s32 x = 1;
    return x;
}
*/

// how do we want to define this because say for instance in a func declartion
// this should be allowed to "fail" for i.e no return type


Token Parser::next_token()
{
    if(tok_idx >= tokens.size())
    {
        return Token(token_type::eof);
    }

    return tokens[tok_idx++];
}

Token Parser::peek(uint32_t v)
{
    const auto idx = tok_idx + v;
    if(idx >= tokens.size())
    {
        return Token(token_type::eof);
    }

    return tokens[idx];
}



void Parser::consume(token_type type)
{
    const auto t = tok_idx >= tokens.size()? token_type::eof : tokens[tok_idx].type;

    if(t != type)
    {
        printf("expected %s got %s\n", tok_name(type),tok_name(t));
        exit(1);
    }
    tok_idx += 1;
}

bool Parser::match(token_type type)
{
    const auto t = tok_idx >= tokens.size()? token_type::eof : tokens[tok_idx].type;

    return t == type;
}

// also how do we want to handle types?
// because we will also have builtin ones...
// a literal for every type is going to make our life a pain
AstNode *Parser::type()
{
    const auto tok = next_token();

    // is a builtin type
    switch(tok.type)
    {
        case token_type::s32:
        {
            return new AstNode(ast_type::type,"s32");
        }

        case token_type::symbol:
        {
            // user defined type (must be resolved in initial parser over complete tree)
            puts("user defined type!");
            exit(1);
        }

        default: break;
    }


    return nullptr;
}

AstNode *Parser::declartion(const std::string &type)
{
    // declartion
    // type symbol ( ';' | '=' expression ';')

    const auto s = next_token();

    if(s.type != token_type::symbol)
    {
        printf("declartion expected symbol got: %s:%zd\n",tok_name(s.type),tok_idx);
        exit(1);
    }

    //    [declare:name]
    // [type: name]   optional([eqauls])

    auto d = new AstNode(ast_type::declaration,s.literal);
    d->nodes.push_back(new AstNode(ast_type::type,type));

    const auto eq = peek(0);

    switch(eq.type)
    {
        // declartion without assigment
        case token_type::semi_colon:
        {
            consume(token_type::semi_colon);
            break;
        }

        // declartion with assingment
        case token_type::equal:
        {
            const auto e = expr(next_token());
            
            // declartion with initalizer skip over initial symbol
            if(e && e->nodes.size() >= 1)
            {
                d->nodes.push_back(e);
            }
            break;
        }

        default:
        {
            printf("malformed declartion: %s\n",tok_name(eq.type));
            exit(1);
        }
    }

    return d;
}

AstNode *Parser::statement()
{
    const auto t = next_token();

    switch(t.type)
    {
        case token_type::s32:
        {
            return declartion("s32");
        }

        case token_type::ret:
        {
            auto r = new AstNode(ast_type::ret);
            r->nodes.push_back(expr(next_token()));
            return r;
        }

        case token_type::symbol:
        {
            const auto t2 = peek(0);

            switch(t2.type)
            {
                // assignment expr
                case token_type::equal:
                {
                    return expr(t);
                }

                // check for brackets
                // array indexes etc here 

                default:
                {
                    printf("statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    exit(1);
                }
            }

        }


        default:
        {
            printf("statement: unexpected token %s\n",tok_name(t.type));
            exit(1);
        }
    }

    return nullptr;
}

AstNode *Parser::block()
{
    // now parse out the block

    // block = '{' statement... '}'

    consume(token_type::left_c_brace);

    auto b = new AstNode(ast_type::block);

    
    // parse out all our statements
    while(!match(token_type::right_c_brace))
    {
        if(match(token_type::eof))
        {
            printf("unterminated function block!");
            exit(1);
        }

        b->nodes.push_back(statement());
    }
    

    consume(token_type::right_c_brace);

    return b;
}

AstNode *Parser::func()
{

    // first check this is a valid function definiton and consume it 
    // func_dec = func return_type ident(arg...)
    // arg = type ident

    // can be null (i.e we have no return type)
    auto t = type();

    // assume void
    if(!t)
    {
        t = new AstNode(ast_type::type,"void");
    }

    // what is the name of our function?
    const auto func_name = next_token();

    if(func_name.type != token_type::symbol)
    {
        // todo add proper error messaging (need to include info with tokens)
        printf("expected function name got: %s!\n",tok_name(func_name.type));
        exit(1);     
    }

    auto f = new AstNode(ast_type::function, func_name.literal);
    



    consume(token_type::left_paren);

    AstNode *a = new AstNode(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(token_type::right_paren))
    {
        if(match(token_type::eof))
        {
            printf("unterminated function declaration!");
            exit(1);
        }


        const auto t = next_token();
        printf("function args, %s: %s\n",tok_name(t.type),t.literal.c_str());
        exit(1);
    }

    consume(token_type::right_paren);

    // no args (make void)
    if(!a->nodes.size())
    {
        a->nodes.push_back(new AstNode(ast_type::type,"void"));
    }


    auto b = block();


    //      [func: name]
    // [type] [block]  [args]
    f->nodes.push_back(t);
    f->nodes.push_back(b);
    f->nodes.push_back(a);

    return f;
}

AstNode *Parser::parse()
{
    auto ast = new AstNode(ast_type::root);
    const auto size = tokens.size();
    while(tok_idx < size)
    {
        const auto &t = next_token();
        // okay what is our "top level" token
        switch(t.type)
        {
            // function declartion
            case token_type::func:
            {
                ast->nodes.push_back(func());
                break;
            }


            default:
            {
                printf("unexpected token %s: %s\n",tok_name(t.type),t.literal.c_str());
                exit(1);
            }
        }
    }

    return ast;
}

void Parser::print(const AstNode *root) const
{
    if(!root)
    {
        return;
    }

    static int depth = 0;
    
    for(int i = 0; i < depth; i++)
    { 
        printf(" -");
    }
    printf(" %d-",depth);
    
    const auto data = root->data;

    printf(" %s %s\n",AST_NAMES[static_cast<size_t>(data.type)],data.literal.c_str());
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
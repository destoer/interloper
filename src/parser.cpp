#include <interloper.h>



Token Parser::next_token()
{
    const auto &vt = *tokens;
    if(tok_idx >= vt.size())
    {
        // TODO: make this return the actual file end
        // for row and col
        return Token(token_type::eof,"",0,0);
    }

    return vt[tok_idx++];
}

Token Parser::peek(uint32_t v)
{
    const auto idx = tok_idx + v;
    const auto &vt = *tokens;
    if(idx >= vt.size())
    {
        return Token(token_type::eof,"",0,0);
    }

    return vt[idx];
}



void Parser::consume(token_type type)
{
    const auto &vt = *tokens;
    const auto t = tok_idx >= vt.size()? token_type::eof : vt[tok_idx].type;

    if(t != type)
    {
        const auto tok = next_token();
        panic(tok,"expected %s got %s\n", tok_name(type),tok_name(t));
    }
    tok_idx += 1;
}

bool Parser::match(token_type type)
{
    const auto &vt = *tokens;
    const auto t = tok_idx >= vt.size()? token_type::eof : vt[tok_idx].type;

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
        panic(s,"declartion expected symbol got: %s:%zd\n",tok_name(s.type),tok_idx);
        return nullptr;
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
            delete_tree(d);
            panic(eq,"malformed declartion: %s\n",tok_name(eq.type));
            break;
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
                    panic(t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    break;
                }
            }
            break;
        }


        default:
        {
            panic(t,"statement: unexpected token %s\n",tok_name(t.type));
            break;
        }
    }

    return nullptr;
}

AstNode *Parser::block()
{
    // now parse out the block

    // block = '{' statement... '}'
    const auto tok = peek(0);
    consume(token_type::left_c_brace);

    auto b = new AstNode(ast_type::block);

    
    // parse out all our statements
    while(!match(token_type::right_c_brace))
    {
        if(match(token_type::eof))
        {
            delete_tree(b);
            panic(tok,"unterminated block!");
            return nullptr;
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
        delete_tree(t);
        panic(func_name,"expected function name got: %s!\n",tok_name(func_name.type));  
        return nullptr;  
    }

    auto f = new AstNode(ast_type::function, func_name.literal);
    


    const auto paren = peek(0);
    consume(token_type::left_paren);

    auto a = new AstNode(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(token_type::right_paren))
    {
        if(match(token_type::eof))
        {
            delete_tree(a);
            delete_tree(f);
            panic(paren,"unterminated function declaration!");
            return nullptr;
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

void Parser::parse(const std::vector<std::string> *file,const std::vector<Token> *tokens, AstNode **root_ptr)
{
    
    this->file = file;
    this->tokens = tokens;

    assert(file != nullptr);
    assert(tokens != nullptr);
    assert(root_ptr != nullptr);

    error = false;
    tok_idx = 0;
    *root_ptr = new AstNode(ast_type::root);
    const auto &vt = *tokens;
    const auto size = vt.size();
    while(tok_idx < size)
    {
        const auto &t = next_token();
        // okay what is our "top level" token
        switch(t.type)
        {
            // function declartion
            case token_type::func:
            {
                (*root_ptr)->nodes.push_back(func());
                break;
            }


            default:
            {
                panic(t,"unexpected token %s: %s\n",tok_name(t.type),t.literal.c_str());
                break;
            }
        }
    }


    this->file = nullptr;
    this->tokens = nullptr;
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
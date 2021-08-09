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

void Parser::prev_token()
{
    if(tok_idx != 0)
    {
        tok_idx -= 1;
    }
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

// assume this is a plain type for now
// i.e no array etc
Type Parser::get_type(std::string &type_literal)
{
    const auto tok = next_token();

    // is a builtin type
    switch(tok.type)
    {
        case token_type::u8:
        {
            type_literal = "u8";
            return Type(builtin_type::u8_t);
        }

        case token_type::u16:
        {
            type_literal = "u16";
            return Type(builtin_type::u16_t);
        }


        case token_type::u32:
        {
            type_literal = "u32";
            return Type(builtin_type::u32_t);
        }


        case token_type::s8:
        {
            type_literal = "s8";
            return Type(builtin_type::s8_t);
        }

        case token_type::s16:
        {
            type_literal = "s16";
            return Type(builtin_type::s16_t);
        }

        case token_type::s32:
        {
            type_literal = "s32";
            return Type(builtin_type::s32_t);
        }

        case token_type::bool_t:
        {
            type_literal = "bool";
            return Type(builtin_type::bool_t);
        }
        

        case token_type::symbol:
        {
            // user defined type (must be resolved in initial parser over complete tree)
            puts("user defined type!");
            exit(1);
        }

        default:
        {
            panic(tok,"expected type declaration got: %s\n",tok_name(tok.type));
            return Type(builtin_type::void_t);
        }
    }

    assert(false);
}

AstNode *Parser::declaration(const Type &var_type, const std::string &type_str)
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
    // [type]   optional([eqauls])

    auto d = new AstNode(ast_type::declaration,s.literal);
    d->nodes.push_back(new AstNode(var_type,type_str));

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
            consume(token_type::equal);
            
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
        // handle builtin types
        case token_type::u8:
        case token_type::u16:
        case token_type::u32:
        case token_type::s8:
        case token_type::s16:
        case token_type::s32:
        case token_type::bool_t:
        {
            prev_token();
            std::string type_literal;
            const auto var_type = get_type(type_literal);
            return declaration(var_type,type_literal);
        }


        case token_type::ret:
        {
            auto r = new AstNode(ast_type::ret);

            // return value is optional
            if(peek(0) != token_type::semi_colon)
            {
                r->nodes.push_back(expr(next_token()));
            }
            else
            {
                consume(token_type::semi_colon);
            }

            return r;
        }

        // TODO: detect a declartion with a user defined type
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

                // function call
                case token_type::left_paren:
                {
                    return expr(t);
                }


                default:
                {
                    panic(t2,"statement: unhandled symbol expr: %s\n",tok_name(t2.type));
                    break;
                }
            }
            break;
        }

        case token_type::left_c_brace:
        {
            // block expects to see the left c brace
            tok_idx--;

            return block();
        };


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

        if(error)
        {
            return b;
        }
    }
    

    consume(token_type::right_c_brace);

    return b;
}

AstNode *Parser::func()
{

    // first check this is a valid function definiton and consume it 
    // func_dec = func return_type ident(arg...)
    // arg = type ident

    // can be void (i.e we have no return type)
    std::string return_type_literal;
    Type return_type;
    
    // void
    if(peek(0).type == token_type::symbol && peek(1).type == token_type::left_paren)
    {
        return_type_literal = "void";
        return_type = Type(builtin_type::void_t);
    }

    // type specified
    else
    {
        return_type = get_type(return_type_literal);
    }

    // what is the name of our function?
    const auto func_name = next_token();

    if(func_name.type != token_type::symbol)
    {
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

        // for each arg pull type, name
        std::string type_name;
        const auto type = get_type(type_name);
        const auto lit_tok = next_token();

        if(lit_tok.type != token_type::symbol)
        {
            panic(lit_tok,"expected name for function arg");
            return nullptr;
        }
        
        // add each declartion
        auto d = new AstNode(ast_type::declaration,lit_tok.literal);
        d->nodes.push_back(new AstNode(type,type_name));

        a->nodes.push_back(d);

        // if the declaration isnt closed get the next arg
        if(peek(0).type != token_type::right_paren)
        {
            consume(token_type::comma);
        }
    }

    consume(token_type::right_paren);

    // no args is fine

    auto b = block();


    //      [func: name]
    // [type] [block]  [args]
    f->nodes.push_back(new AstNode(return_type,return_type_literal));
    f->nodes.push_back(b);
    f->nodes.push_back(a);

    return f;
}

void Parser::init(const std::vector<std::string> *file,const std::vector<Token> *tokens)
{
    this->file = file;
    this->tokens = tokens;

    assert(file != nullptr);
    assert(tokens != nullptr);
    
    error = false;
    initialized = true;
    tok_idx = 0;
}


void Parser::parse(AstNode **root_ptr)
{
    assert(initialized);
    assert(root_ptr != nullptr);

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

        if(error)
        {
            break;
        }
    }

    initialized = false;
    this->file = nullptr;
    this->tokens = nullptr;
}

void Parser::print(const AstNode *root) const
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
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


Token Parser::next_token(const std::vector<Token> &tokens)
{
    if(tok_idx >= tokens.size())
    {
        return Token(token_type::eof);
    }

    return tokens[tok_idx++];
}


void Parser::consume(const std::vector<Token> &tokens, token_type type)
{
    const auto t = tok_idx >= tokens.size()? token_type::eof : tokens[tok_idx].type;

    if(t != type)
    {
        printf("expected %s got %s\n", TOKEN_NAMES[static_cast<size_t>(type)],TOKEN_NAMES[static_cast<size_t>(t)]);
        exit(1);
    }

}

bool Parser::match(const std::vector<Token> &tokens, token_type type)
{
    const auto t = tok_idx >= tokens.size()? token_type::eof : tokens[tok_idx].type;

    return t == type;
}

// also how do we want to handle types?
// because we will also have builtin ones...
// a literal for every type is going to make our life a pain
AstNode *Parser::type(const std::vector<Token> &tokens)
{
    const auto tok = next_token(tokens);

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

AstNode *Parser::func(const std::vector<Token> &tokens)
{

    // first check this is a valid function definiton and consume it 
    // func_dec = func return_type ident(arg...)
    // arg = type ident

    // can be null (i.e we have no return type)
    auto t = type(tokens);

    // assume void
    if(t == nullptr)
    {
        t = new AstNode(ast_type::type,"void");
    }

    // what is the name of our function?
    const auto func_name = next_token(tokens);

    if(func_name.type != token_type::symbol)
    {
        // todo add proper error messaging (need to include info with tokens)
        printf("expected function name got: %s!\n",TOKEN_NAMES[static_cast<size_t>(func_name.type)]);
        exit(1);     
    }

    auto f = new AstNode(ast_type::function, func_name.literal);
    



    consume(tokens,token_type::left_paren);

    AstNode *a = new AstNode(ast_type::function_args);

    // parse out the function args
    // if  token is eof then we have a problem 
    while(!match(tokens,token_type::right_paren))
    {
        // handle this being eof...
        puts("FUNCTION ARGS!");
        exit(1);
    }

    consume(tokens,token_type::right_paren);

    // no args (make void)
    if(!a->nodes.size())
    {
        f->nodes.push_back(new AstNode(ast_type::type,"void"));
    }



    // now parse out the block

    // block = '{' statement... '}'

/*
    auto name = get_literal();
    expect( ( )
    for(...)
    {
            // allow there to be nothign
        get_dec()
        consume(,)
            // allow there to be nothign
    }
    expect( ) )

    ast (function,name)
    ast[0] = type
    ast[1] = block
    ast[2] = args
*/
    


    // ok now we have to parse out the block


    return nullptr;
}

AstNode *Parser::parse(const std::vector<Token> &tokens)
{
    auto ast = new AstNode(ast_type::root);
    const auto size = tokens.size();
    while(tok_idx < size)
    {
        const auto &t = next_token(tokens);
        // okay what is our "top level" token
        switch(t.type)
        {
            // function declartion
            case token_type::func:
            {
                ast->nodes.push_back(func(tokens));
                break;
            }


            default:
            {
                printf("unexpected token %s: %s\n",TOKEN_NAMES[static_cast<size_t>(t.type)],t.literal.c_str());
                exit(1);
            }
        }
    }

    return ast;
}
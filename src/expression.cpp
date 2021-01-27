#include <interloper.h>

int32_t Parser::lbp(const Token &t)
{
    const auto bp = TOKEN_INFO[static_cast<size_t>(t.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        printf("lbp: illegal token: %s\n",tok_name(t.type));
        exit(1);
    }

    return bp;
}

AstNode *Parser::led(Token &t,AstNode *left)
{
    UNUSED(left);

    switch(t.type)
    {
    
        case token_type::equal:
        {
            return new AstNode(left,ast_type::equal,expression(lbp(t.type)));
        }
        
        case token_type::plus:
        {
            return new AstNode(left,ast_type::plus,expression(lbp(t.type)));
        }
    
        default:
        {
            printf("led: unexpected token %s\n",tok_name(t.type));
            exit(1);
        }        
    }

    // should not be reached
    assert(false);
    return nullptr;
}

// unary operators
AstNode *Parser::nud(Token &t)
{
    switch(t.type)
    {
        case token_type::value:
        {
            return new AstNode(nullptr,AstData(ast_type::value,t.literal),nullptr);
        }

        case token_type::symbol:
        {
            return new AstNode(nullptr,AstData(ast_type::symbol,t.literal),nullptr);
        }

        // declaration
        case token_type::equal:
        {
            if(expr_tok.type == token_type::equal)
            {
                printf("expected expression");
                exit(1);
            }

            return new AstNode(expression(0),AstData(ast_type::equal,t.literal),nullptr);
        }


        default:
        {
            printf("nud: unexpected token %s\n",tok_name(t.type));
            exit(1);
        }
    }

    // should not be reached
    assert(false);
    return nullptr;
}

// pratt parser
// https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
AstNode *Parser::expression(int32_t rbp)
{
    auto cur = expr_tok;
    expr_tok = next_token();

    auto left = nud(cur);

    while(rbp < lbp(expr_tok))
    {
        cur = expr_tok;
        expr_tok = next_token();
        left = led(cur,left);
    }

    return left;
}

AstNode *Parser::expr(const Token &t)
{
    seen_eq = false;
    expr_tok = t;
    return expression(0);
}
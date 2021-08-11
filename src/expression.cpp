#include <interloper.h>

void Parser::consume_expr(token_type type)
{
    if(type != expr_tok.type)
    {
        panic(expr_tok,"expected: %s got %s\n",tok_name(type),tok_name(expr_tok.type));
    }

    expr_tok = next_token_expr();
}

Token Parser::next_token_expr()
{
    const auto tok = next_token();
    
    // easiest just to jam a state machine in here
    if(tok.type == token_type::left_paren)
    {
        brace_count += 1;
    }

    else if(tok.type == token_type::right_paren)
    {
        brace_count -= 1;
    }

    return tok;    
}

int32_t Parser::lbp(const Token &t)
{
    const auto bp = TOKEN_INFO[static_cast<size_t>(t.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        panic(t,"lbp: illegal token: %s\n",tok_name(t.type));
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
            // right precedence rbp = lbp -1 so that things on the right 
            // are sen as sub expressions
            return new AstNode(left,expression(lbp(t)-1),ast_type::equal);  
        }
    
      
        case token_type::plus:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::plus);
        }

        case token_type::minus:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::minus);
        }

        case token_type::divide:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::divide);
        }
    
        case token_type::times:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::times);
        }

        case token_type::bitwise_and:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::bitwise_and);
        }

        case token_type::bitwise_or:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::bitwise_or);
        }

        case token_type::bitwise_xor:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::bitwise_xor);
        }

        case token_type::logical_or:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_or);
        }
    
        case token_type::logical_and:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_and);
        }


        case token_type::logical_lt:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_lt);
        }

        case token_type::logical_gt:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_gt);
        }   

        case token_type::logical_le:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_le);
        }   

        case token_type::logical_ge:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_ge);
        }   

        case token_type::logical_eq:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_eq);
        }    

        case token_type::logical_ne:
        {
            return new AstNode(left,expression(lbp(t)),ast_type::logical_ne);
        }         


        default:
        {
            panic(t,"led: unexpected token %s\n",tok_name(t.type));
            return nullptr;
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
        // cast(<type>,<expr>)
        case token_type::cast:
        {

            consume_expr(token_type::left_paren);


            // get_type is inside the normal parser we need
            // to correct the tok idx
            tok_idx -= 1;

            std::string type_name;
            const auto type = get_type(type_name);

            const auto left = new AstNode(type,type_name);
           
            // correct our state machine
            expr_tok = next_token_expr();

            consume_expr(token_type::comma);

            const auto right = expression(0);

            consume_expr(token_type::right_paren);

            return new AstNode(left,right,ast_type::cast);
        }

        case token_type::value:
        {
            const u32 v = convert_imm(t.literal); 

            // is this literal a -ve?
            const bool sign = t.literal[0] == '-';


            Value value(v,sign);

            return new AstNode(nullptr,nullptr,value,t.literal);
        }

        case token_type::false_t:
        {
            return new AstNode(nullptr,nullptr,ast_type::false_t,t.literal);
        }      

        case token_type::true_t:
        {
            return new AstNode(nullptr,nullptr,ast_type::true_t,t.literal);
        }      

        case token_type::symbol:
        {
            // look ahead extra tokens that would change the meaning of this

            // function call
            if(expr_tok.type == token_type::left_paren)
            {
                consume_expr(token_type::left_paren);

                auto func_call = new AstNode(ast_type::function_call,t.literal);



                // keep reading args till we run out of commas
                bool done = false;

                // empty call we are done
                if(expr_tok.type == token_type::right_paren)
                {
                    done = true;
                    consume_expr(token_type::right_paren);
                }

                while(!done)
                {

                    auto expr = expression(0);

                    func_call->nodes.push_back(expr);

                    // no more args terminate the call
                    if(expr_tok.type != token_type::comma)
                    {
                        consume_expr(token_type::right_paren);
                        done = true;
                    }

                    else
                    {
                        consume_expr(token_type::comma);
                    }
                }

                return func_call;
            }


            // plain symbol
            return new AstNode(nullptr,nullptr,ast_type::symbol,t.literal);
        }

        case token_type::minus:
        {
            return new AstNode(expression(100),nullptr,ast_type::minus);
        }

        case token_type::plus:
        {
            return new AstNode(expression(100),nullptr,ast_type::plus);
        }

        case token_type::bitwise_not:
        {
            return new AstNode(expression(100),nullptr,ast_type::bitwise_not);
        }

        case token_type::logical_not:
        {
            return new AstNode(expression(100),nullptr,ast_type::logical_not);
        }


        case token_type::left_paren:
        {
            const auto expr = expression(0);

            consume_expr(token_type::right_paren);
            return expr;
        }

        default:
        {
            panic(t,"nud: unexpected token %s\n",tok_name(t.type));
            break;
        }
    }

    return nullptr;
}

// pratt parser
// https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
// ^ this algo is elegant as hell

AstNode *Parser::expression(int32_t rbp)
{
    auto cur = expr_tok;
    expr_tok = next_token_expr();

    auto left = nud(cur);
    if(error)
    {
        return left;
    }


    while(rbp < lbp(expr_tok))
    {
        cur = expr_tok;
        expr_tok = next_token_expr();
        left = led(cur,left);
        if(error)
        {
            return left;
        }
    }

    return left;
}

AstNode *Parser::expr(const Token &t)
{
    brace_count = 0;
    expr_tok = t;

    if(expr_tok.type == token_type::left_paren)
    {
        brace_count += 1;
    }

    else if(expr_tok.type == token_type::right_paren)
    {
        brace_count -= 1;
    }

    const auto e = expression(0);

    if(brace_count != 0)
    {
        panic(expr_tok,"unterminated bracket: ");
    }

    return e;
}
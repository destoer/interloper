#include <interloper.h>

AstNode *expression(Parser &parser,s32 rbp);
AstNode *expr(Parser &parser,const Token &t);



Token next_token(Parser &parser);
Value read_value(const Token &t);
void type_panic(Parser &parser);
AstNode *parse_type(Parser &parser);
AstNode *copy_node(const AstNode *node);

Token next_token_expr(Parser &parser)
{
    const auto tok = next_token(parser);
    
    // easiest just to jam a state machine in here
    if(tok.type == token_type::left_paren)
    {
        parser.brace_count += 1;
    }

    else if(tok.type == token_type::right_paren)
    {
        parser.brace_count -= 1;
    }

    return tok;    
}

void consume_expr(Parser &parser,token_type type)
{
    if(type != parser.expr_tok.type)
    {
        panic(parser,parser.expr_tok,"expected: %s got %s\n",tok_name(type),tok_name(parser.expr_tok.type));
    }

    parser.expr_tok = next_token_expr(parser);
}

s32 lbp(Parser &parser,const Token &t)
{
    const auto bp = TOKEN_INFO[static_cast<size_t>(t.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        panic(parser,t,"lbp: illegal token: %s\n",tok_name(t.type));
    }

    return bp;
}

// i.e +=
AstNode *oper_eq(Parser &parser,AstNode *left,Token t,ast_type oper)
{
    auto e = expression(parser,lbp(parser,t)-1);

    // sugar as <sym> = <sym> + <expr>
    auto e2 = ast_binary(copy_node(left),e,oper);

    auto n = ast_binary(left,e2,ast_type::equal);

    return n;    
}

AstNode *led(Parser &parser,Token &t,AstNode *left)
{
    switch(t.type)
    {
        case token_type::plus_eq:
        {
            return oper_eq(parser,left,t,ast_type::plus);
        }

        case token_type::minus_eq:
        {
            return oper_eq(parser,left,t,ast_type::minus);
        }

        case token_type::times_eq:
        {
            return oper_eq(parser,left,t,ast_type::times);
        }


        case token_type::divide_eq:
        {
            return oper_eq(parser,left,t,ast_type::divide);
        }

        case token_type::equal:
        { 
            // right precedence rbp = lbp -1 so that things on the right 
            // are sen as sub expressions
            return ast_binary(left,expression(parser,lbp(parser,t)-1),ast_type::equal);  
        }
    
      
        case token_type::plus:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::plus);
        }

        case token_type::minus:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::minus);
        }

        case token_type::divide:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::divide);
        }

        case token_type::mod:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::mod);
        }

        case token_type::shift_l:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::shift_l);
        }

        case token_type::shift_r:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::shift_r);
        }

        case token_type::times:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::times);
        }

        // and operator in binary context is a bitwise and
        case token_type::operator_and:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::bitwise_and);
        }

        case token_type::bitwise_or:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::bitwise_or);
        }

        case token_type::bitwise_xor:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::bitwise_xor);
        }

        case token_type::logical_or:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_or);
        }
    
        case token_type::logical_and:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_and);
        }


        case token_type::logical_lt:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_lt);
        }

        case token_type::logical_gt:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_gt);
        }   

        case token_type::logical_le:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_le);
        }   

        case token_type::logical_ge:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_ge);
        }   

        case token_type::logical_eq:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_eq);
        }    

        case token_type::logical_ne:
        {
            return ast_binary(left,expression(parser,lbp(parser,t)),ast_type::logical_ne);
        }         


        default:
        {
            panic(parser,t,"led: unexpected token %s\n",tok_name(t.type));
            return nullptr;
        }        
    }

    // should not be reached
    panic("led fell through!?");
    return nullptr;
}


AstNode *member_access(Parser &parser, AstNode* expr_node)
{
    // skip dot token
    auto member_tok = next_token_expr(parser);

    AstNode* member_node = nullptr;

    if(member_tok.type == token_type::symbol)
    {
        // perform peeking for modifers
        if(match(parser,token_type::sl_brace))
        {
            unimplemented("member array access");
        }

        // plain old member
        else
        {
            member_node = ast_literal(ast_type::access_member, member_tok.literal);
                
            // correct the state machine
            parser.expr_tok = next_token_expr(parser);
        }
    }

    else
    {

        panic(parser,member_tok,"expected struct member got %s(%s)\n",member_tok.literal.c_str(),tok_name(member_tok.type));
        return nullptr;
    }



    // add expr node only to the inital member i.e (make it the bottom node)
    // TODO: can we do better than this?
    if(expr_node)
    {
        member_node->nodes.push_back(expr_node);
        expr_node = nullptr;
    }


    if(parser.expr_tok.type == token_type::dot)
    {
        // index later nodes before we push any so that it later processed depth first
        // i.e left most is at the bottom of the tree
        AstNode* next = member_access(parser,expr_node);

        next->nodes.push_back(member_node);

        return next;
    }

    return member_node;
}


// unary operators
AstNode *nud(Parser &parser,Token &t)
{
    switch(t.type)
    {
        // cast(<type>,<expr>)
        case token_type::cast:
        {
            consume_expr(parser,token_type::left_paren);


            // get_type is inside the normal parser we need
            // to correct the tok idx
            parser.tok_idx -= 1;

            auto type = parse_type(parser);

            if(!type)
            {
                type_panic(parser);
                return nullptr;
            }

            // correct our state machine
            parser.expr_tok = next_token_expr(parser);

            consume_expr(parser,token_type::comma);

            const auto right = expression(parser,0);

            consume_expr(parser,token_type::right_paren);
            
            return ast_binary(type,right,ast_type::cast);    
        }

        // array initializer
        case token_type::left_c_brace:
        {
            auto init = ast_plain(ast_type::initializer_list);
            while(parser.expr_tok.type != token_type::right_c_brace)
            {
                init->nodes.push_back(expression(parser,0));

                if(parser.expr_tok.type != token_type::right_c_brace)
                {
                    consume_expr(parser,token_type::comma);
                }
            }

            consume_expr(parser,token_type::right_c_brace);
            
            return init;
        }

        case token_type::value:
        {
            const auto value = read_value(t);
            return ast_value(value,t.literal);
        }

        case token_type::char_t:
        {
            return ast_literal(ast_type::char_t,t.literal);
        }

        case token_type::string:
        {
            return ast_literal(ast_type::string,t.literal);
        }

        case token_type::false_t:
        {
            return ast_plain(ast_type::false_t);
        }      

        case token_type::true_t:
        {
            return ast_plain(ast_type::true_t);
        }      

        case token_type::symbol:
        {
            // look ahead extra tokens that would change the meaning of this

            switch(parser.expr_tok.type)
            {
                // function call
                case token_type::left_paren:
                {
                    consume_expr(parser,token_type::left_paren);

                    auto func_call = ast_literal(ast_type::function_call,t.literal);



                    // keep reading args till we run out of commas
                    bool done = false;

                    // empty call we are done
                    if(parser.expr_tok.type == token_type::right_paren)
                    {
                        done = true;
                        consume_expr(parser,token_type::right_paren);
                    }

                    while(!done)
                    {
                        auto expr = expression(parser,0);

                        func_call->nodes.push_back(expr);

                        // no more args terminate the call
                        if(parser.expr_tok.type != token_type::comma)
                        {
                            consume_expr(parser,token_type::right_paren);
                            done = true;
                        }

                        else
                        {
                            consume_expr(parser,token_type::comma);
                        }
                    }

                    return func_call;
                }

                case token_type::dot:
                {
                    return member_access(parser,ast_literal(ast_type::symbol,t.literal));
                }

                case token_type::sl_brace:
                {
                    auto arr_access = ast_literal(ast_type::array_access,t.literal);

                    while(parser.expr_tok.type == token_type::sl_brace)
                    {
                        consume_expr(parser,token_type::sl_brace);

                        const auto e = expression(parser,0);
                        arr_access->nodes.push_back(e);
                    
                        consume_expr(parser,token_type::sr_brace);
                    }

                    if(parser.expr_tok.type == token_type::dot)
                    {
                        return member_access(parser,arr_access);
                    }

                    else
                    {
                        return arr_access;
                    }
                }

                default:
                {
                    // plain symbol
                    return ast_literal(ast_type::symbol,t.literal);
                }
                break;
            }
        }

        case token_type::minus:
        {
            return ast_unary(expression(parser,100),ast_type::minus);
        }

        case token_type::plus:
        {
            return ast_unary(expression(parser,100),ast_type::plus);
        }

        case token_type::bitwise_not:
        {
            return ast_unary(expression(parser,100),ast_type::bitwise_not);
        }

        case token_type::logical_not:
        {
            return ast_unary(expression(parser,100),ast_type::logical_not);
        }


        case token_type::left_paren:
        {
            const auto expr = expression(parser,0);

            consume_expr(parser,token_type::right_paren);
            return expr;
        }

        case token_type::deref:
        {
            return ast_unary(expression(parser,lbp(parser,t)),ast_type::deref);
        }

        // in unary context and operator takes addr
        case token_type::operator_and:
        {
            return ast_unary(expression(parser,30),ast_type::addrof);
        }

        default:
        {
            panic(parser,t,"nud: unexpected token %s\n",tok_name(t.type));
            break;
        }
    }

    return nullptr;
}

// pratt parser
// https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
// ^ this algo is elegant as hell

AstNode *expression(Parser &parser,s32 rbp)
{
    auto cur = parser.expr_tok;
    parser.expr_tok = next_token_expr(parser);

    auto left = nud(parser,cur);

    if(parser.terminate)
    {
        return left;
    }

    if(parser.expr_tok.type == parser.termination_type)
    {
        parser.terminate = true;
        return left;
    }

    while(rbp < lbp(parser,parser.expr_tok))
    {
        cur = parser.expr_tok;
        parser.expr_tok = next_token_expr(parser);
        left = led(parser,cur,left);

        if(parser.terminate)
        {
            return left;
        }
    }

    return left;
}


AstNode *expr_terminate_internal(Parser &parser,token_type t)
{
    // make pratt parser terminate as soon as it sees
    // this token
    parser.termination_type = t;

    auto e = expr(parser,next_token(parser));

    parser.termination_type = token_type::eof;

    return e;
}


AstNode *expr_terminate(Parser &parser,token_type t, token_type &term)
{
    auto e = expr_terminate_internal(parser,t);
    parser.terminate = false;

    // what token did we terminate on?
    term = parser.expr_tok.type;

    return e;
}


// panic on failure to terminate with token
AstNode *expr_terminate(Parser &parser,token_type t)
{
    auto e = expr_terminate_internal(parser,t);
    

    // expression must terminate on this token
    if(parser.expr_tok.type != t || !parser.terminate)
    {
        panic(parser,parser.expr_tok,"invalid expr ended with '%s' should end with '%s'\n",tok_name(parser.expr_tok.type),tok_name(t));
        delete e;
        return nullptr;
    }

    parser.terminate = false;
    return e;
}

AstNode *expr(Parser &parser,const Token &t)
{
    parser.brace_count = 0;
    parser.expr_tok = t;

    if(parser.expr_tok.type == token_type::left_paren)
    {
        parser.brace_count += 1;
    }

    else if(parser.expr_tok.type == token_type::right_paren)
    {
        parser.brace_count -= 1;
    }

    const auto e = expression(parser,0);

    // non closed brace, where specified terminator is not a right_paren
    if(parser.brace_count != 0 && !(parser.terminate && parser.expr_tok.type == token_type::right_paren))
    {
        panic(parser,parser.expr_tok,"unterminated bracket: ");
    }

    return e;
}
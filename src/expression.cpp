#include <interloper.h>

AstNode *expression(Parser &parser,s32 rbp);
AstNode *expr(Parser &parser,const Token &t);



Token next_token(Parser &parser);
Value read_value(const Token &t);
void type_panic(Parser &parser);
TypeNode *parse_type(Parser &parser);

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
    auto e2 = ast_binary(parser,left,e,oper,t);

    auto n = ast_binary(parser,left,e2,ast_type::equal,t);

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
            return ast_binary(parser,left,expression(parser,lbp(parser,t)-1),ast_type::equal,t);  
        }
    
      
        case token_type::plus:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::plus,t);
        }

        case token_type::minus:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::minus,t);
        }

        case token_type::divide:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::divide,t);
        }

        case token_type::mod:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::mod,t);
        }

        case token_type::shift_l:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::shift_l,t);
        }

        case token_type::shift_r:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::shift_r,t);
        }

        case token_type::times:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::times,t);
        }

        // and operator in binary context is a bitwise and
        case token_type::operator_and:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::bitwise_and,t);
        }

        case token_type::bitwise_or:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::bitwise_or,t);
        }

        case token_type::bitwise_xor:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::bitwise_xor,t);
        }

        case token_type::logical_or:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_or,t);
        }
    
        case token_type::logical_and:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_and,t);
        }


        case token_type::logical_lt:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_lt,t);
        }

        case token_type::logical_gt:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_gt,t);
        }   

        case token_type::logical_le:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_le,t);
        }   

        case token_type::logical_ge:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_ge,t);
        }   

        case token_type::logical_eq:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_eq,t);
        }    

        case token_type::logical_ne:
        {
            return ast_binary(parser,left,expression(parser,lbp(parser,t)),ast_type::logical_ne,t);
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


AstNode* array_index(Parser& parser,const String& name)
{
    IndexNode* arr_access = (IndexNode*)ast_index(parser,name,parser.expr_tok);

    while(parser.expr_tok.type == token_type::sl_brace)
    {
        consume_expr(parser,token_type::sl_brace);

        AstNode* e = expression(parser,0);
        push_var(arr_access->indexes,e);
    
        consume_expr(parser,token_type::sr_brace);
    }

    return (AstNode*)arr_access;
}

AstNode *struct_access(Parser& parser, AstNode* expr_node)
{
    RecordNode* member_root = (RecordNode*)ast_record(parser,ast_type::access_members,parser.expr_tok);

    BinNode* root = (BinNode*)ast_binary(parser,expr_node,(AstNode*)member_root,ast_type::access_struct,parser.expr_tok);

    while(parser.expr_tok.type == token_type::dot)
    {
        const auto member_tok = next_token_expr(parser);

        if(member_tok.type == token_type::symbol)
        {
            // perform peeking for modifers
            if(match(parser,token_type::sl_brace))
            {
                parser.expr_tok = next_token_expr(parser);
                push_var(member_root->nodes,array_index(parser,member_tok.literal));
            }

            // plain old member
            else
            {
                AstNode* member_node = ast_literal(parser,ast_type::access_member, member_tok.literal,member_tok);

                push_var(member_root->nodes,member_node);
                    
                // correct the state machine
                parser.expr_tok = next_token_expr(parser);
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

AstNode* nud_sym(Parser& parser, const Token& t)
{
    // look ahead extra tokens that would change the meaning of this
    switch(parser.expr_tok.type)
    {
        // function call
        case token_type::left_paren:
        {
        
            consume_expr(parser,token_type::left_paren);

            FuncCallNode* func_call = (FuncCallNode*)ast_call(parser,t.literal,t);



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

                push_var(func_call->args,expr);

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

            return (AstNode*)func_call;
        }
    
        case token_type::dot:
        {   
            return struct_access(parser,ast_literal(parser,ast_type::symbol,t.literal,t));
        }

        case token_type::sl_brace:
        {
            AstNode* arr_access = array_index(parser,t.literal);

            if(parser.expr_tok.type == token_type::dot)
            {
                return struct_access(parser,arr_access);
            }

            else
            {
                return arr_access;
            }
        }
    

        // TODO: for now this is just for hanlding enums
        case token_type::scope:
        {
            consume_expr(parser,token_type::scope);

            if(parser.expr_tok.type != token_type::symbol)
            {
                panic(parser,parser.expr_tok,"expected name after scope, got %s\n",tok_name(parser.expr_tok.type));
                return nullptr;
            }

            const auto cur = parser.expr_tok;
            parser.expr_tok = next_token_expr(parser);

            return ast_scope(parser,nud_sym(parser,cur),t.literal,t);
        }

        default:
        {
            // plain symbol
            return ast_literal(parser,ast_type::symbol,t.literal,t);
        }
        break;
    }   
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

            AstNode* type = (AstNode*)parse_type(parser);

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
            
            return ast_binary(parser,type,right,ast_type::cast,t);    
        }
    
        // sizeof(<expr>)
        case token_type::sizeof_t:
        {
            consume_expr(parser,token_type::left_paren);

            AstNode* e = expression(parser,0);

            consume_expr(parser,token_type::right_paren);
            
            return ast_unary(parser,e,ast_type::sizeof_t,t);    
        }
    
        // array initializer
        case token_type::left_c_brace:
        {
            RecordNode* init = (RecordNode*)ast_record(parser,ast_type::initializer_list,t);
            while(parser.expr_tok.type != token_type::right_c_brace)
            {
                push_var(init->nodes,expression(parser,0));

                if(parser.expr_tok.type != token_type::right_c_brace)
                {
                    consume_expr(parser,token_type::comma);
                }
            }

            consume_expr(parser,token_type::right_c_brace);
            
            return (AstNode*)init;
        }
    
        case token_type::value:
        {
            return ast_value(parser,t.value,t);
        }

        case token_type::char_t:
        {
            return ast_char(parser,t.character,t);
        }

        case token_type::string:
        {
            return ast_literal(parser,ast_type::string,t.literal,t);
        }

        case token_type::false_t:
        {
            return ast_plain(parser,ast_type::false_t,t);
        }      

        case token_type::true_t:
        {
            return ast_plain(parser,ast_type::true_t,t);
        }      

        case token_type::null_t:
        {
            return ast_plain(parser,ast_type::null_t,t);
        }

        case token_type::symbol:
        {
            return nud_sym(parser,t);
        }

        case token_type::minus:
        {
            return ast_unary(parser,expression(parser,100),ast_type::minus,t);
        }

        case token_type::plus:
        {
            return ast_unary(parser,expression(parser,100),ast_type::plus,t);
        }

        case token_type::bitwise_not:
        {
            return ast_unary(parser,expression(parser,100),ast_type::bitwise_not,t);
        }

        case token_type::logical_not:
        {
            return ast_unary(parser,expression(parser,100),ast_type::logical_not,t);
        }


        case token_type::left_paren:
        {
            const auto expr = expression(parser,0);

            consume_expr(parser,token_type::right_paren);
            return expr;
        }

        case token_type::deref:
        {
            return ast_unary(parser,expression(parser,lbp(parser,t)),ast_type::deref,t);
        }

        // in unary context and operator takes addr
        case token_type::operator_and:
        {
            return ast_unary(parser,expression(parser,30),ast_type::addrof,t);
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
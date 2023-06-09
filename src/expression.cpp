#include <interloper.h>

AstNode *expression(Parser &parser,s32 rbp);

AstNode* expr_terminate_in_expr(Parser& parser,const String& expression_name, token_type type);
std::pair<AstNode*,b32> expr_list_in_expr(Parser& parser,const String& expression_name, token_type type);
Token next_token(Parser &parser);
Value read_value(const Token &t);
void type_panic(Parser &parser);
TypeNode *parse_type(Parser &parser);

void next_expr_token(Parser& parser)
{
    parser.expr_tok = next_token(parser);

    if(parser.expr_tok.type == parser.termination_type || (parser.expr_tok.type == token_type::comma && parser.list_terminate))
    {
        parser.terminate = true;
    }
}

void consume_expr(Parser &parser,token_type type)
{
    if(type != parser.expr_tok.type)
    {
        panic(parser,parser.expr_tok,"expected: %s got %s\n",tok_name(type),tok_name(parser.expr_tok.type));
    }

    next_expr_token(parser);
}

s32 lbp(Parser &parser,const Token &t)
{
    const auto bp = TOKEN_INFO[static_cast<size_t>(t.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        switch(t.type)
        {
            case token_type::increment:
            {
                panic(parser,t,"increment operator not supported\n");
                break;
            }

            case token_type::decrement:
            {
                panic(parser,t,"decrement operator not supported\n");
                break;
            }

            default:
            {
                panic(parser,t,"unexpected token '%s' in %s\n",tok_name(t.type),parser.expression_name.buf);
                break;
            }
        }

        
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
            panic(parser,t,"led: unexpected token '%s' in %s\n",tok_name(t.type),parser.expression_name.buf);
            return nullptr;
        }        
    }

    // should not be reached
    crash_and_burn("led fell through!?");
    return nullptr;
}



AstNode* nud_sym(Parser& parser, const Token& t)
{
    // look ahead extra tokens that would change the meaning of this
    switch(parser.expr_tok.type)
    {
        // function call
        case token_type::left_paren:
        {
            // correct the state machine
            prev_token(parser);

            AstNode* call = func_call(parser,t); 
            next_expr_token(parser);

            return call;
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
            next_expr_token(parser);

            return ast_scope(parser,nud_sym(parser,cur),t.literal,t);
        }


        default:
        {
            prev_token(parser);
            AstNode* node = var(parser,t);

            next_expr_token(parser);

            return node;
        }
        break;
    }   
}

AstNode* builtin_type_info_access(Parser& parser,builtin_type type)
{
    const Token t = parser.expr_tok;

    if(parser.expr_tok.type == token_type::dot)
    {
        next_expr_token(parser);

        if(parser.expr_tok.type == token_type::symbol)
        {
            const String literal = parser.expr_tok.literal;

            next_expr_token(parser);
            return ast_builtin_access(parser,type,literal,t);
        }
    }

    panic(parser,parser.expr_tok,"expected member access after builtin type, got %s\n",tok_name(parser.expr_tok.type));
    return nullptr;   
}

// unary operators
AstNode *nud(Parser &parser, const Token &t)
{
    switch(t.type)
    {
    
        case token_type::u32:
        {
            return builtin_type_info_access(parser,builtin_type::u32_t);
        }

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
            // NOTE: we bypass the normal function here because commas require special handling
            parser.expr_tok = next_token(parser);

            consume_expr(parser,token_type::comma);

            const auto right = expr_terminate_in_expr(parser,"cast",token_type::right_paren);

            return ast_binary(parser,type,right,ast_type::cast,t);    
        }
    
        // sizeof(<expr>)
        case token_type::sizeof_t:
        {
            consume_expr(parser,token_type::left_paren);

            AstNode* e = expr_terminate_in_expr(parser,"sizeof",token_type::right_paren);

            return ast_unary(parser,e,ast_type::sizeof_t,t);    
        }
    
        // initializer list
        case token_type::left_c_brace:
        {
            RecordNode* init = (RecordNode*)ast_record(parser,ast_type::initializer_list,t);
            b32 done = false;

            while(!done)
            {
                if(parser.error)
                {
                    return nullptr;
                }

                auto [e,term_seen] = expr_list_in_expr(parser,"initializer list",token_type::right_c_brace);
                done = term_seen; 

                push_var(init->nodes,e);
            }

            // allow trailing comma 
            if(parser.expr_tok.type == token_type::comma && match(parser,token_type::right_c_brace))
            {
                next_expr_token(parser);
            }

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
            const auto expr = expr_terminate_in_expr(parser,"brackets",token_type::right_paren);
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
            panic(parser,t,"nud: unexpected token '%s' in %s\n",tok_name(t.type),parser.expression_name.buf);
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
    next_expr_token(parser);

    auto left = nud(parser,cur);

    if(parser.terminate || parser.error)
    {
        return left;
    }

    while(rbp < lbp(parser,parser.expr_tok))
    {
        cur = parser.expr_tok;
        next_expr_token(parser);
        left = led(parser,cur,left);

        if(parser.terminate || parser.error)
        {
            return left;
        }
    }

    return left;
}



AstNode *expr_terminate_internal(Parser &parser,const String& expression_name,const Token& token,token_type t, 
    b32 must_terminate = false, b32 list_terminate = false)
{
    //printf("Expression termination on %s\n",tok_name(t));

    // backup vars on stack as this is used recursively
    const b32 terminate_old = parser.terminate;
    const token_type term_type_old = parser.termination_type;
    const b32 list_terminate_old = parser.list_terminate;
    const String expression_name_old = parser.expression_name;

    // make pratt parser terminate as soon as it sees
    // this token
    parser.termination_type = t;
    parser.list_terminate = list_terminate;
    parser.terminate = false;

    // get our expression
    parser.expr_tok = token;
    parser.expression_name = expression_name;

    const auto e = expression(parser,0);

    // expression must terminate on this token
    if(!parser.terminate && must_terminate)
    {
        panic(parser,parser.expr_tok,"%s should terminate with '%s' terminated with '%s'\n",parser.expression_name.buf,
            tok_name(t),tok_name(parser.expr_tok.type));
        return nullptr;
    }


    parser.termination_type = term_type_old;
    parser.terminate = terminate_old;
    parser.list_terminate = list_terminate_old;
    parser.expression_name = expression_name_old;

    return e;
}


// Can optionally terminate, caller must check
AstNode *expr_terminate(Parser &parser,const String& expression_name,token_type t, token_type &term)
{
    auto e = expr_terminate_internal(parser,expression_name,next_token(parser),t);

    // what token did we terminate on?
    term = parser.expr_tok.type;

    return e;
}


// panic on failure to terminate with token
AstNode *expr_terminate(Parser &parser,const String& expression_name,token_type t)
{
    return expr_terminate_internal(parser,expression_name,next_token(parser),t,true);
}


AstNode *statement_terminate(Parser& parser,const String& expression_name)
{
    return expr_terminate(parser,expression_name,token_type::semi_colon);
}

std::pair<AstNode*,b32> expr_list(Parser& parser,const String& expression_name, token_type type)
{
    AstNode* e = expr_terminate_internal(parser,expression_name,next_token(parser),type,true,true);

    const b32 seen_list_term = parser.expr_tok.type == type;

    return std::pair{e,seen_list_term};
}

// for use inside the parser so the state machine does not have to be messed with
AstNode* expr_terminate_in_expr(Parser& parser,const String& expression_name, token_type type)
{
    AstNode* e = expr_terminate_internal(parser,expression_name,parser.expr_tok,type,true);

    next_expr_token(parser);

    return e;
}

std::pair<AstNode*,b32> expr_list_in_expr(Parser& parser,const String& expression_name, token_type type)
{
    AstNode* e = expr_terminate_internal(parser,expression_name,parser.expr_tok,type,true,true);

    const b32 seen_list_term = parser.expr_tok.type == type;

    next_expr_token(parser);

    return std::pair{e,seen_list_term};    
}
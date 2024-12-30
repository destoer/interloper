#include <interloper.h>

AstNode *expression(Parser &parser,ExprCtx& ctx,s32 rbp);

AstNode* expr_terminate_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type);
std::pair<AstNode*,b32> expr_list_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type);

Token next_token(Parser &parser);
Value read_value(const Token &t);
void type_panic(Parser &parser);
TypeNode *parse_type(Parser &parser, b32 allow_fail = false);
Array<String> split_namespace(Parser& parser, const Token& start);

void next_expr_token(Parser& parser,ExprCtx& ctx)
{
    ctx.expr_tok = next_token(parser);

    b32 should_term = false;

    // what will cause early termination?
    if(ctx.expr_flags & EXPR_TERM_LIST_FLAG)
    {
        should_term = (ctx.expr_tok.type == token_type::comma) || (ctx.expr_tok.type == ctx.term);
    }

    else
    {
        should_term = (ctx.expr_tok.type == ctx.term);
    }

    ctx.expr_flags |= (should_term << EXPR_TERMINATED_FLAG_BIT);
}

void consume_expr(Parser &parser,ExprCtx& ctx,token_type type)
{
    if(type != ctx.expr_tok.type)
    {
        panic(parser,ctx.expr_tok,"expected: %s got %s\n",tok_name(type),tok_name(ctx.expr_tok.type));
    }

    next_expr_token(parser,ctx);
}

s32 lbp(Parser &parser,const ExprCtx& ctx,const Token &t)
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
                panic(parser,t,"unexpected token '%s' in %s\n",tok_name(t.type),ctx.expression_name.buf);
                break;
            }
        }

        
    }

    return bp;
}

// i.e +=
AstNode *oper_eq(Parser &parser,ExprCtx& ctx,AstNode *left,Token t,ast_type oper)
{
    auto e = expression(parser,ctx,lbp(parser,ctx,t) - 1);

    // sugar as <sym> = <sym> + <expr>
    auto e2 = ast_binary(parser,left,e,oper,t);

    auto n = ast_binary(parser,left,e2,ast_type::equal,t);

    return n;    
}

AstNode *parse_binary(Parser &parser,ExprCtx& ctx,Token &t,AstNode *left)
{
    switch(t.type)
    {
        case token_type::plus_eq:
        {
            return oper_eq(parser,ctx,left,t,ast_type::plus);
        }

        case token_type::minus_eq:
        {
            return oper_eq(parser,ctx,left,t,ast_type::minus);
        }

        case token_type::times_eq:
        {
            return oper_eq(parser,ctx,left,t,ast_type::times);
        }


        case token_type::divide_eq:
        {
            return oper_eq(parser,ctx,left,t,ast_type::divide);
        }

        case token_type::bitwise_or_eq:
        {
            return oper_eq(parser,ctx,left,t,ast_type::bitwise_or);
        }

        case token_type::equal:
        {
            // right precedence rbp = lbp -1 so that things on the right 
            // are sen as sub expressions
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t) - 1),ast_type::equal,t);  
        }
    
      
        case token_type::plus:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::plus,t);
        }

        case token_type::minus:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::minus,t);
        }

        case token_type::divide:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::divide,t);
        }

        case token_type::mod:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::mod,t);
        }

        case token_type::shift_l:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::shift_l,t);
        }

        case token_type::shift_r:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::shift_r,t);
        }

        case token_type::times:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::times,t);
        }

        // and operator in binary context is a bitwise and
        case token_type::operator_and:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::bitwise_and,t);
        }

        case token_type::bitwise_or:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::bitwise_or,t);
        }

        case token_type::bitwise_xor:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::bitwise_xor,t);
        }

        case token_type::logical_or:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_or,t);
        }
    
        case token_type::logical_and:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_and,t);
        }


        case token_type::logical_lt:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_lt,t);
        }

        case token_type::logical_gt:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_gt,t);
        }   

        case token_type::logical_le:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_le,t);
        }   

        case token_type::logical_ge:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_ge,t);
        }   

        case token_type::logical_eq:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_eq,t);
        }    

        case token_type::logical_ne:
        {
            return ast_binary(parser,left,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::logical_ne,t);
        }         


        default:
        {
            panic(parser,t,"led: unexpected token '%s' in %s\n",tok_name(t.type),ctx.expression_name.buf);
            return nullptr;
        }        
    }

    // should not be reached
    crash_and_burn("led fell through!?");
    return nullptr;
}



AstNode* parse_sym(Parser& parser,ExprCtx& ctx, const Token& t)
{
    // look ahead extra tokens that would change the meaning of this
    switch(ctx.expr_tok.type)
    {
        // function call
        case token_type::left_paren:
        {
            // correct the state machine
            prev_token(parser);

            AstNode* call = func_call(parser,ast_literal(parser,ast_type::symbol,t.literal,t),t); 
            next_expr_token(parser,ctx);

            return call;
        }

        case token_type::scope:
        {
            // correct state machine
            prev_token(parser);
            prev_token(parser);

            const auto name_space = split_namespace(parser,ctx.expr_tok);

            if(parser.error)
            {
                return nullptr;
            }

            const auto cur = next_token(parser);
            next_expr_token(parser,ctx);

            return ast_scope(parser,parse_sym(parser,ctx,cur),name_space,t);
        }


        default:
        {
            prev_token(parser);
            AstNode* node = var(parser,t,true);

            next_expr_token(parser,ctx);

            return node;
        }
        break;
    }   
}

AstNode* builtin_type_info_access(Parser& parser,ExprCtx& ctx,builtin_type type)
{
    const Token t = ctx.expr_tok;

    if(ctx.expr_tok.type == token_type::dot)
    {
        next_expr_token(parser,ctx);

        if(ctx.expr_tok.type == token_type::symbol)
        {
            const String literal = ctx.expr_tok.literal;

            next_expr_token(parser,ctx);
            return ast_builtin_access(parser,type,literal,t);
        }
    }

    panic(parser,ctx.expr_tok,"expected member access after builtin type, got %s\n",tok_name(ctx.expr_tok.type));
    return nullptr;   
}

AstNode* type_operator(Parser& parser,ExprCtx& ctx, ast_type kind)
{
    consume_expr(parser,ctx,token_type::left_paren);

    // get_type is inside the normal parser we need
    // to correct the tok idx
    prev_token(parser);

    auto type = parse_type(parser);

    if(!type)
    {
        type_panic(parser);
        return nullptr;
    }

    // correct our state machine
    ctx.expr_tok = next_token(parser);

    consume_expr(parser,ctx,token_type::right_paren);

    return ast_type_operator(type,kind);   
}

AstNode* parse_cast(Parser& parser,ExprCtx& ctx, const Token &t, ast_type cast_type)
{
    consume_expr(parser,ctx,token_type::left_paren);

    // get_type is inside the normal parser we need
    // to correct the tok idx
    prev_token(parser);

    AstNode* type = (AstNode*)parse_type(parser);

    if(!type)
    {
        type_panic(parser);
        return nullptr;
    }

    // correct our state machine
    // NOTE: we bypass the normal function here because commas require special handling
    ctx.expr_tok = next_token(parser);

    consume_expr(parser,ctx,token_type::comma);

    const auto right = expr_terminate_in_expr(parser,ctx,"cast",token_type::right_paren);

    return ast_binary(parser,type,right,cast_type,t); 
}

// unary operators
AstNode *parse_unary(Parser &parser,ExprCtx& ctx, const Token &t)
{
    switch(t.type)
    {
        case token_type::u8:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::u8_t);
        }

        case token_type::u16:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::u16_t);
        }

        case token_type::u32:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::u32_t);
        }

        case token_type::u64:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::u64_t);
        }

        case token_type::s8:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::s8_t);
        }

        case token_type::s16:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::s16_t);
        }

        case token_type::s32:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::s32_t);
        }

        case token_type::s64:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::s64_t);
        }

        case token_type::bool_t:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::bool_t);
        }

        case token_type::byte_t:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::byte_t);
        }

        case token_type::c8_t:
        {
            return builtin_type_info_access(parser,ctx,builtin_type::c8_t);
        }


        // cast(<type>,<expr>)
        case token_type::cast:
        {
            return parse_cast(parser,ctx,t,ast_type::cast);
        }
    
        // recast_arr(<type>,<expr>)
        case token_type::recast_arr:
        {
            return parse_cast(parser,ctx,t,ast_type::recast_arr);
        }

        // sizeof(<expr>)
        case token_type::sizeof_t:
        {
            consume_expr(parser,ctx,token_type::left_paren);

            AstNode* e = expr_terminate_in_expr(parser,ctx,"sizeof",token_type::right_paren);

            return ast_unary(parser,e,ast_type::sizeof_t,t);    
        }

        // sizeof_type(<type>)
        case token_type::sizeof_type_t:
        {
            return type_operator(parser,ctx,ast_type::sizeof_type_t);   
        }

        // sizeof_data(<type>)
        case token_type::sizeof_data_t:
        {
            return type_operator(parser,ctx,ast_type::sizeof_data_t);   
        }


        // initializer list
        case token_type::left_c_brace:
        {
            // check for no init 
            // NOTE: as this is is not common we resolve it here
            // rather than inside the tokenizer
            if(ctx.expr_tok.type == token_type::qmark)
            {
                consume_expr(parser,ctx,token_type::qmark);
                consume_expr(parser,ctx,token_type::right_c_brace);

                return ast_plain(parser,ast_type::no_init,t);
            }


            RecordNode* init = (RecordNode*)ast_record(parser,ast_type::initializer_list,t);
            b32 done = false;

            while(!done)
            {
                if(parser.error)
                {
                    return nullptr;
                }

                auto [e,term_seen] = expr_list_in_expr(parser,ctx,"initializer list",token_type::right_c_brace);
                done = term_seen; 

                push_var(init->nodes,e);
            }

            // allow trailing comma if
            // NOTE: if its the terminator we just ignore it so the term cond goes in properly
            if(ctx.term != token_type::comma && ctx.expr_tok.type == token_type::comma && match(parser,token_type::right_c_brace))
            {
                next_expr_token(parser,ctx);
            }

            return (AstNode*)init;
        }
    
        case token_type::value:
        {
            return ast_value(parser,t.value,t);
        }

        case token_type::float_t:
        {
            return ast_float(parser,t.fp,t);
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
            return parse_sym(parser,ctx,t);
        }

        case token_type::minus:
        {
            return ast_unary(parser,expression(parser,ctx,100),ast_type::minus,t);
        }

        case token_type::plus:
        {
            return ast_unary(parser,expression(parser,ctx,100),ast_type::plus,t);
        }

        case token_type::bitwise_not:
        {
            return ast_unary(parser,expression(parser,ctx,100),ast_type::bitwise_not,t);
        }

        case token_type::logical_not:
        {
            return ast_unary(parser,expression(parser,ctx,100),ast_type::logical_not,t);
        }


        case token_type::left_paren:
        {
            const auto expr = expr_terminate_in_expr(parser,ctx,"brackets",token_type::right_paren);
            return expr;
        }

        case token_type::deref:
        {
            return ast_unary(parser,expression(parser,ctx,lbp(parser,ctx,t)),ast_type::deref,t);
        }

        // in unary context and operator takes addr
        case token_type::operator_and:
        {
            return ast_unary(parser,expression(parser,ctx,30),ast_type::addrof,t);
        }

        default:
        {
            panic(parser,t,"nud: unexpected token '%s' in %s\n",tok_name(t.type),ctx.expression_name.buf);
            break;
        }
    }

    return nullptr;
}

// pratt parser
// https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
// ^ this algo is elegant as hell

AstNode *expression(Parser &parser,ExprCtx& ctx,s32 rbp)
{
    auto cur = ctx.expr_tok;
    next_expr_token(parser,ctx);

    auto left = parse_unary(parser,ctx,cur);

    if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || parser.error)
    {
        return left;
    }

    while(rbp < lbp(parser,ctx,ctx.expr_tok))
    {
        cur = ctx.expr_tok;
        next_expr_token(parser,ctx);
        left = parse_binary(parser,ctx,cur,left);

        if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || parser.error)
        {
            return left;
        }
    }

    return left;
}


AstNode *expr_terminate_internal(Parser &parser,ExprCtx& ctx)
{
    const auto e = expression(parser,ctx,0);

    // expression must terminate on this token
    if(!(ctx.expr_flags & EXPR_TERMINATED_FLAG) && (ctx.expr_flags & EXPR_MUST_TERMINATE_FLAG))
    {
        panic(parser,ctx.expr_tok,"%s should terminate with '%s' terminated with '%s'\n",ctx.expression_name.buf,
            tok_name(ctx.term),tok_name(ctx.expr_tok.type));
        return nullptr;
    }

    return e;
}


// Can optionally terminate, caller must check
AstNode *expr_terminate(Parser &parser,const String& expression_name,token_type t, token_type &term)
{
    ExprCtx ctx;
    ctx.term = t;
    ctx.expression_name = expression_name;
    ctx.expr_tok = next_token(parser);

    auto e = expr_terminate_internal(parser,ctx);

    // what token did we terminate on?
    term = ctx.expr_tok.type;

    return e;
}


// panic on failure to terminate with token
AstNode *expr_terminate(Parser &parser,const String& expression_name,token_type t)
{
    ExprCtx ctx;
    ctx.term = t;
    ctx.expression_name = expression_name;
    ctx.expr_tok = next_token(parser);
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG;

    return expr_terminate_internal(parser,ctx);
}


AstNode *statement_terminate(Parser& parser,const String& expression_name)
{
    return expr_terminate(parser,expression_name,token_type::semi_colon);
}

std::pair<AstNode*,b32> expr_list(Parser& parser,const String& expression_name, token_type type)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = next_token(parser);
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG | EXPR_TERM_LIST_FLAG;

    AstNode* e = expr_terminate_internal(parser,ctx);

    const b32 seen_list_term = ctx.expr_tok.type == type;

    return std::pair{e,seen_list_term};
}

// for use inside the parser so the state machine does not have to be messed with
AstNode* expr_terminate_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = old_ctx.expr_tok;
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG;

    AstNode* e = expr_terminate_internal(parser,ctx);

    next_expr_token(parser,old_ctx);

    return e;
}

std::pair<AstNode*,b32> expr_list_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = old_ctx.expr_tok;
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG | EXPR_TERM_LIST_FLAG;

    AstNode* e = expr_terminate_internal(parser,ctx);

    const b32 seen_list_term = ctx.expr_tok.type == type;

    next_expr_token(parser,old_ctx);

    return std::pair{e,seen_list_term};    
}
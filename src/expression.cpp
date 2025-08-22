#include <interloper.h>

ParserResult expression(Parser &parser,ExprCtx& ctx,Result<s32,parse_error> rbp_opt);
ParserResult expr_terminate_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type);
ParserResult expr_list_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type, b32* hit_term);
ParserResult parse_initliazer_list(Parser& parser, ExprCtx& ctx, const Token& t);

Token next_token(Parser &parser);
Value read_value(const Token &t);
Result<TypeNode*,parse_error> parse_type(Parser &parser, b32 allow_fail = false);
Result<Array<String>,parse_error> split_namespace(Parser& parser, const Token& start);

void next_expr_token(Parser& parser,ExprCtx& ctx)
{
    ctx.expr_tok = next_token(parser);

    b32 should_term = false;

    const b32 hit_terminator = (ctx.expr_tok.type == ctx.term);

    // what will cause early termination?
    if(ctx.expr_flags & EXPR_TERM_LIST_FLAG)
    {
        should_term = (ctx.expr_tok.type == token_type::comma) || hit_terminator;
    }

    else
    {
        should_term = (ctx.expr_tok.type == ctx.term);
    }

    ctx.expr_flags |= (hit_terminator << EXPR_HIT_TERMINATOR_FLAG_BIT);
    ctx.expr_flags |= (should_term << EXPR_TERMINATED_FLAG_BIT);
}

Option<parse_error> consume_expr(Parser &parser,ExprCtx& ctx,token_type type)
{
    if(type != ctx.expr_tok.type)
    {
        return parser_error(parser,parse_error::invalid_consume,ctx.expr_tok,"expected: %s got %s\n",tok_name(type),tok_name(ctx.expr_tok.type));
    }

    next_expr_token(parser,ctx);
    return option::none;
}

Result<s32,parse_error> lbp(Parser &parser,const ExprCtx& ctx,const Token &t)
{
    const auto bp = TOKEN_INFO[static_cast<size_t>(t.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        switch(t.type)
        {
            case token_type::increment:
            {
                return parser_error(parser,parse_error::invalid_lbp,t,"increment operator not supported\n");
            }

            case token_type::decrement:
            {
                return parser_error(parser,parse_error::invalid_lbp,t,"decrement operator not supported\n");
            }

            default:
            {
                return parser_error(parser,parse_error::invalid_lbp,t,"unexpected token '%s' in %s\n",tok_name(t.type),ctx.expression_name.buf);
            }
        }
    }

    return bp;
}

Result<s32,parse_error> lbp_subexpr(Parser &parser,const ExprCtx& ctx,const Token &t)
{
    auto lbp_res = lbp(parser,ctx,t);

    if(!lbp_res)
    {
        return lbp_res;
    }

    const s32 lbp = *lbp_res;
    return lbp - 1;
}

// i.e +=
ParserResult oper_eq(Parser &parser,ExprCtx& ctx,AstNode *left,Token t,ast_type oper)
{
    auto res = expression(parser,ctx,lbp_subexpr(parser,ctx,t));

    if(!res)
    {
        return res;
    }

    auto e = *res;

    // sugar as <sym> = <sym> + <expr>
    auto e2 = ast_binary(parser,left,e,oper,t);
    auto ans = ast_binary(parser,left,e2,ast_type::equal,t);

    return ans;    
}

ParserResult parse_binary(Parser &parser,ExprCtx& ctx,Token &t,AstNode *left)
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
            return ast_binary(parser,left,expression(parser,ctx,lbp_subexpr(parser,ctx,t)),ast_type::equal,t);  
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
            return parser_error(parser,parse_error::unexpected_token,t,"led: unexpected token '%s' in %s\n",tok_name(t.type),ctx.expression_name.buf);
        }        
    }

    // should not be reached
    crash_and_burn("led fell through!?");
    assert(false);
}



ParserResult parse_sym(Parser& parser,ExprCtx& ctx, const Token& t)
{
    // look ahead extra tokens that would change the meaning of this
    switch(ctx.expr_tok.type)
    {
        // function call
        case token_type::left_paren:
        {
            // correct the state machine
            prev_token(parser);

            auto call_res = func_call(parser,ast_literal(parser,ast_type::symbol,t.literal,t),t); 
            next_expr_token(parser,ctx);

            return call_res;
        }

        case token_type::scope:
        {
            // correct state machine
            prev_token(parser);
            prev_token(parser);

            auto name_space_res = split_namespace(parser,ctx.expr_tok);

            if(!name_space_res)
            {
                return name_space_res.error();
            }

            Array<String> name_space_strings = *name_space_res;

            const auto cur = next_token(parser);
            next_expr_token(parser,ctx);

            // Read out struct initializer
            if(cur.type == token_type::symbol && ctx.expr_tok.type == token_type::left_c_brace)
            {
                const auto struct_name = cur;
                const auto start = ctx.expr_tok;
                (void)consume_expr(parser,ctx,token_type::left_c_brace);

                auto list_res = parse_initliazer_list(parser,ctx,start);
                if(!list_res)
                {
                    return list_res;
                }

                NameSpace* name_space = scan_namespace(parser.global_namespace,name_space_strings); 
                auto initializer = ast_struct_initializer(parser,struct_name.literal,*list_res,name_space,struct_name);
                
                return initializer;
            }

            auto sym_res = parse_sym(parser,ctx,cur);

            if(!sym_res)
            {
                return sym_res;
            }

            return ast_scope(parser,*sym_res,name_space_strings,t);
        }

        default:
        {
            prev_token(parser);
            auto var_res = var(parser,t,true);

            next_expr_token(parser,ctx);

            return var_res;
        }
        break;
    }   
}

ParserResult builtin_type_info_access(Parser& parser,ExprCtx& ctx,builtin_type type)
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

    return parser_error(parser,parse_error::unexpected_token,ctx.expr_tok,"expected member access after builtin type, got %s\n",
        tok_name(ctx.expr_tok.type)); 
}

ParserResult type_operator(Parser& parser,ExprCtx& ctx, ast_type kind)
{
    const auto consume_left_paren_err = consume_expr(parser,ctx,token_type::left_paren);
    if(!!consume_left_paren_err)
    {
        return *consume_left_paren_err;
    }

    // get_type is inside the normal parser we need
    // to correct the tok idx
    prev_token(parser);

    auto type_res = parse_type(parser);

    if(!type_res)
    {
        return type_res.error();
    }

    // correct our state machine
    ctx.expr_tok = next_token(parser);

    const auto consume_right_paren_err = consume_expr(parser,ctx,token_type::right_paren);
    if(!!consume_right_paren_err)
    {
        return *consume_right_paren_err;
    }

    return ast_type_operator(type_res.value(),kind);   
}

ParserResult parse_cast(Parser& parser,ExprCtx& ctx, const Token &t, ast_type cast_type)
{
    auto expr_consume_err = consume_expr(parser,ctx,token_type::left_paren);
    if(!!expr_consume_err)
    {
        return *expr_consume_err;
    }

    // get_type is inside the normal parser we need
    // to correct the tok idx
    prev_token(parser);

    auto type_res = parse_type(parser);

    if(!type_res)
    {
        return type_res.error();
    }

    AstNode* type = (AstNode*)type_res.value();
    
    // correct our state machine
    // NOTE: we bypass the normal function here because commas require special handling
    ctx.expr_tok = next_token(parser);

    const auto comma_consume_err = consume_expr(parser,ctx,token_type::comma);
    if(!!comma_consume_err)
    {
        return *comma_consume_err;
    }

    const auto right = expr_terminate_in_expr(parser,ctx,"cast",token_type::right_paren);

    return ast_binary(parser,type,right,cast_type,t); 
}

ParserResult parse_value_initializer_list(Parser& parser, ExprCtx& ctx, const Token& t);
ParserResult parse_designated_initializers(Parser& parser, ExprCtx& ctx, const Token& t);

ParserResult parse_initliazer_list(Parser& parser, ExprCtx& ctx, const Token& t)
{
    const bool designated = ctx.expr_tok.type == token_type::symbol && match(parser,token_type::colon);

    const auto list_res = designated? parse_designated_initializers(parser,ctx,t) :  parse_value_initializer_list(parser,ctx,t);

    // allow trailing comma if the list is about to end and its not the terminator i.e
    // {{0,0},{0,0},}
    if(ctx.expr_tok.type == token_type::comma && match(parser,ctx.term))
    {
        next_expr_token(parser,ctx);
    }

    return list_res;
}

ParserResult parse_value_initializer_list(Parser& parser, ExprCtx& ctx, const Token& t)
{
    RecordNode* init = (RecordNode*)ast_record(parser,ast_type::initializer_list,t);

    b32 hit_term = false;
    while(!hit_term)
    {
        auto e_res = expr_list_in_expr(parser,ctx,"value initializer list",token_type::right_c_brace,&hit_term);

        if(!e_res)
        {
            return e_res.error();
        }

        push_var(init->nodes,*e_res);
    }

    return (AstNode*)init;
}



ParserResult parse_designated_initializers(Parser& parser, ExprCtx& ctx, const Token& t)
{
    DesignatedListNode* list = (DesignatedListNode*)ast_designated_initializer_list(parser,t);

    b32 hit_term = false;
    while(!hit_term)
    {
        // Want to know if we started as designated because the entire list should match!
        const bool designated = ctx.expr_tok.type == token_type::symbol && match(parser,token_type::colon);

        if(!designated)
        {
            return parser_error(parser,parse_error::malformed_stmt,ctx.expr_tok,"Designator: Hit unexpected token %s\n",tok_name(ctx.expr_tok.type));
        }

        const auto name = ctx.expr_tok.literal;

        next_expr_token(parser,ctx);
        next_expr_token(parser,ctx);

        // Nested struct initializer?
        if(ctx.expr_tok.type == token_type::symbol && match(parser,token_type::left_c_brace))
        {
            const auto struct_name = ctx.expr_tok;
            (void)consume_expr(parser,ctx,token_type::symbol);
            (void)consume_expr(parser,ctx,token_type::left_c_brace);

            auto list_res = parse_initliazer_list(parser,ctx,t);

            if(!list_res)
            {
                return list_res;
            }

            auto struct_initializer = ast_struct_initializer(parser,struct_name.literal,*list_res,nullptr,struct_name);

            const DesignatedInitializer init = {struct_initializer,name};
            push_var(list->initializer,init);

            if(ctx.expr_tok.type == token_type::comma)
            {
                (void)consume_expr(parser,ctx,token_type::comma);
            }

            else if(ctx.expr_tok.type == token_type::right_c_brace)
            {
                hit_term = true;
                (void)consume_expr(parser,ctx,token_type::right_c_brace);
            }
        }

        else
        {
            // Read out the designator
            auto e_res = expr_list_in_expr(parser,ctx,"designated initializer list",token_type::right_c_brace,&hit_term);

            if(!e_res)
            {
                return e_res.error();
            }

            const DesignatedInitializer init = {*e_res,name};
            push_var(list->initializer,init);
        }       
    }

    return (AstNode*)list;
}

// unary operators
ParserResult parse_unary(Parser &parser,ExprCtx& ctx, const Token &t)
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
    
        // sizeof(<expr>)
        case token_type::sizeof_t:
        {
            const auto consume_err = consume_expr(parser,ctx,token_type::left_paren);
            if(!!consume_err)
            {
                return *consume_err;
            }

            auto expr_res = expr_terminate_in_expr(parser,ctx,"sizeof",token_type::right_paren);

            return ast_unary(parser,expr_res,ast_type::sizeof_t,t);    
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
                const auto consume_qmark_err = consume_expr(parser,ctx,token_type::qmark);
                if(!!consume_qmark_err)
                {
                    return *consume_qmark_err;
                }

                const auto consume_c_brace_err = consume_expr(parser,ctx,token_type::right_c_brace);
                if(!!consume_c_brace_err)
                {
                    return *consume_c_brace_err;
                }

                return ast_plain(parser,ast_type::no_init,t);
            }

            return parse_initliazer_list(parser,ctx,t);
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

        case token_type::ignore:
        {
            return ast_plain(parser,ast_type::ignore,t);
        }

        default:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"unary: unexpected token '%s' in %s\n",
                tok_name(t.type),ctx.expression_name.buf);
        }
    }

    assert(false);
    return parse_error::unexpected_token;
}

// pratt parser
// https://web.archive.org/web/20151223215421/http://hall.org.ua/halls/wizzard/pdf/Vaughan.Pratt.TDOP.pdf
// ^ this algo is elegant as hell

ParserResult expression(Parser &parser,ExprCtx& ctx,Result<s32,parse_error> rbp_res)
{
    if(!rbp_res)
    {
        return rbp_res.error();
    }

    const u32 rbp = *rbp_res;

    auto cur = ctx.expr_tok;
    next_expr_token(parser,ctx);

    auto left = parse_unary(parser,ctx,cur);

    if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || !left)
    {
        return left;
    }

    auto lbp_res = lbp(parser,ctx,ctx.expr_tok);
    if(!lbp_res)
    {
        return lbp_res.error();
    }

    u32 left_binding_power = *lbp_res;

    while(rbp < left_binding_power)
    {
        cur = ctx.expr_tok;
        next_expr_token(parser,ctx);
        left = parse_binary(parser,ctx,cur,*left);

        if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || !left)
        {
            return left;
        }
        
        lbp_res = lbp(parser,ctx,ctx.expr_tok);
        if(!lbp_res)
        {
            return lbp_res.error();
        }

        left_binding_power = *lbp_res;
    }

    return left;
}


ParserResult expr_terminate_internal(Parser &parser,ExprCtx& ctx)
{
    const auto e = expression(parser,ctx,0);

    // expression must terminate on this token
    if(!(ctx.expr_flags & EXPR_TERMINATED_FLAG) && (ctx.expr_flags & EXPR_MUST_TERMINATE_FLAG))
    {
        return parser_error(parser,parse_error::invalid_terminator,ctx.expr_tok,"%s should terminate with '%s' terminated with '%s'\n",
            ctx.expression_name.buf,tok_name(ctx.term),tok_name(ctx.expr_tok.type));
    }

    return e;
}


// Can optionally terminate, caller must check
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t, token_type &term)
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
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t)
{
    ExprCtx ctx;
    ctx.term = t;
    ctx.expression_name = expression_name;
    ctx.expr_tok = next_token(parser);
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG;

    return expr_terminate_internal(parser,ctx);
}


ParserResult statement_terminate(Parser& parser,const String& expression_name)
{
    return expr_terminate(parser,expression_name,token_type::semi_colon);
}

ParserResult expr_list(Parser& parser,const String& expression_name, token_type type,b32* hit_terminator)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = next_token(parser);
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG | EXPR_TERM_LIST_FLAG;

    auto e_res = expr_terminate_internal(parser,ctx);

    *hit_terminator = ctx.expr_flags & EXPR_HIT_TERMINATOR;
    return e_res;
}

// for use inside the parser so the state machine does not have to be messed with
ParserResult expr_terminate_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = old_ctx.expr_tok;
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG;

    auto e_opt = expr_terminate_internal(parser,ctx);

    next_expr_token(parser,old_ctx);

    return e_opt;
}

ParserResult expr_list_in_expr(Parser& parser,ExprCtx& old_ctx,const String& expression_name, token_type type,b32* hit_terminator)
{
    ExprCtx ctx;
    ctx.term = type;
    ctx.expression_name = expression_name;
    ctx.expr_tok = old_ctx.expr_tok;
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG | EXPR_TERM_LIST_FLAG;

    auto e_res = expr_terminate_internal(parser,ctx);
    *hit_terminator = ctx.expr_flags & EXPR_HIT_TERMINATOR;
    next_expr_token(parser,old_ctx);

    return e_res;
}
#include <interloper.h>

ParserResult expression(Parser &parser,ExprCtx& ctx,Result<s32,parse_error> rbp_opt);
ParserResult parse_initializer_list(Parser& parser, const Token& t);
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t, token_type &term);
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t);
ParserResult expr_list(Parser& parser,const String& expression_name, token_type type,b32* hit_terminator);

Token next_token(Parser &parser);
Value read_value(const Token &t);
Result<TypeNode*,parse_error> parse_type(Parser &parser);
Result<Array<String>,parse_error> split_namespace(Parser& parser, const Token& start);

ParserResult parse_template_call(Parser& parser, NameSpace* name_space, const Token& cur);

Token next_expr_token(Parser& parser,ExprCtx& ctx)
{
    const auto token = next_token(parser);

    const b32 hit_terminator = (token.type == ctx.term);
    b32 should_term = hit_terminator;

    // what will cause early termination?
    if(ctx.expr_flags & EXPR_TERM_LIST_FLAG)
    {
        should_term |= (token.type == token_type::comma);
    }

    if(!(ctx.expr_flags & EXPR_HIT_TERMINATOR) && should_term)
    {
        ctx.term_tok = token;
    }

    ctx.expr_flags |= (hit_terminator << EXPR_HIT_TERMINATOR_FLAG_BIT);
    ctx.expr_flags |= (should_term << EXPR_TERMINATED_FLAG_BIT);

    return token;
}

Result<s32,parse_error> lbp(Parser &parser,const ExprCtx& ctx,const Token &token)
{
    const auto bp = TOKEN_INFO[u32(token.type)].lbp;

    //printf("lbp: %s -> %d\n",tok_name(t.type),bp);

    if(bp == -1)
    {
        (void)parser_error(parser,parse_error::invalid_lbp,token,"Invalid token %s",tok_name(token.type));

        switch(token.type)
        {
            case token_type::increment:
            {
                return parser_error(parser,parse_error::invalid_lbp,token,"increment operator not supported");
            }

            case token_type::decrement:
            {
                return parser_error(parser,parse_error::invalid_lbp,token,"decrement operator not supported");
            }

            default:
            {
                assert(false);
                return parser_error(parser,parse_error::invalid_lbp,token,"unexpected token '%s' in %s",tok_name(token.type),ctx.expression_name.buf);
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
ParserResult oper_eq(Parser &parser,ExprCtx& ctx,AstNode *left,Token t,arith_bin_type oper)
{
    auto res = expression(parser,ctx,lbp_subexpr(parser,ctx,t));

    if(!res)
    {
        return res;
    }

    auto e = *res;

    // sugar as <sym> = <sym> + <expr>
    auto e2 = ast_bin_arith(parser,oper,left,e,t);
    auto ans = ast_equal(parser,left,e2,t);

    return ans;    
}

ParserResult parse_binary(Parser &parser,ExprCtx& ctx,Token &t,AstNode *left)
{
    switch(t.type)
    {
        case token_type::plus_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::add_t);
        }

        case token_type::minus_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::sub_t);
        }

        case token_type::times_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::mul_t);
        }


        case token_type::divide_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::div_t);
        }

        case token_type::bitwise_or_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::or_t);
        }

        case token_type::bitwise_and_eq:
        {
            return oper_eq(parser,ctx,left,t,arith_bin_type::and_t);
        }

        case token_type::equal:
        {
            // right precedence rbp = lbp -1 so that things on the right 
            // are sen as sub expressions
            return ast_equal(parser,left,expression(parser,ctx,lbp_subexpr(parser,ctx,t)),t);  
        }
    
      
        case token_type::plus:
        {
            return ast_bin_arith(parser,arith_bin_type::add_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::minus:
        {
            return ast_bin_arith(parser,arith_bin_type::sub_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::divide:
        {
            return ast_bin_arith(parser,arith_bin_type::div_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::mod:
        {
            return ast_bin_arith(parser,arith_bin_type::mod_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::shift_l:
        {
            return ast_shift(parser,shift_type::left,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::shift_r:
        {
            return ast_shift(parser,shift_type::right,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::times:
        {
            return ast_bin_arith(parser,arith_bin_type::mul_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        // and operator in binary context is a bitwise and
        case token_type::operator_and:
        {
            return ast_bin_arith(parser,arith_bin_type::and_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::bitwise_or:
        {
            return ast_bin_arith(parser,arith_bin_type::or_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::bitwise_xor:
        {
            return ast_bin_arith(parser,arith_bin_type::xor_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::logical_or:
        {
            return ast_logic(parser,boolean_logic_op::or_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }
    
        case token_type::logical_and:
        {
            return ast_logic(parser,boolean_logic_op::and_t,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }


        case token_type::logical_lt:
        {
            return ast_comparison(parser,comparison_op::lt,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        case token_type::logical_gt:
        {
            return ast_comparison(parser,comparison_op::gt,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }   

        case token_type::logical_le:
        {
            return ast_comparison(parser,comparison_op::le,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }   

        case token_type::logical_ge:
        {
            return ast_comparison(parser,comparison_op::ge,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }   

        case token_type::logical_eq:
        {
            return ast_comparison(parser,comparison_op::eq,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }    

        case token_type::logical_ne:
        {
            return ast_comparison(parser,comparison_op::ne,left,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }         


        default:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"led: unexpected token '%s' in %s",tok_name(t.type),ctx.expression_name.buf);
        }        
    }

    // should not be reached
    crash_and_burn("led fell through!?");
    assert(false);
}

EnumMemberNode* parse_enum_member(Parser& parser, const Token& member_name, const Array<String>& name_space_strings)
{
    NameSpace* name_space = nullptr;

    const u32 count_minus_one = count(name_space_strings) - 1;
    
    // If there is only a single namespace this can only be the enum name if it exists at all.
    if(count_minus_one != 0)
    {   
        name_space = scan_namespace(parser,clip_array(name_space_strings,count_minus_one));
    }

    const auto enum_name = name_space_strings[count_minus_one];

    if(!parser_type_kind_exists(parser,name_space,enum_name,type_kind::enum_t))
    {
        return nullptr;
    }

    return ast_enum_member(parser,name_space,enum_name,member_name.literal,member_name);
}

ParserResult parse_sym(Parser& parser,NameSpace* name_space, const Token& cur)
{
    const auto next = peek(parser,0);

    // look ahead extra tokens that would change the meaning of this
    switch(next.type)
    {
        // Generic function
        case token_type::dollar:
        {
            (void)consume(parser,token_type::dollar);
            return parse_template_call(parser,name_space,cur);
        }

        // function call
        case token_type::left_paren:
        {
            return func_call(parser,ast_symbol(parser,name_space,cur.literal,cur),cur); 
        }

        case token_type::scope:
        {
            // correct state machine
            prev_token(parser);

            auto name_space_res = split_namespace(parser,next);

            if(!name_space_res)
            {
                return name_space_res.error();
            }

            Array<String> name_space_strings = *name_space_res;

            const auto cur_next = next_token(parser);

            EnumMemberNode* member = parse_enum_member(parser,cur_next,name_space_strings); 
            if(member)
            {
                destroy_arr(name_space_strings);
                return (AstNode*)member;
            }

            NameSpace* name_space = scan_namespace(parser,name_space_strings);
            destroy_arr(name_space_strings); 
            return parse_sym(parser,name_space,cur_next);
        }

        default:
        {
            // Read out struct initializer
            if(next.type == token_type::left_c_brace && parser_type_kind_exists(parser,name_space,cur.literal,type_kind::struct_t))
            {
                const auto struct_name = cur;
                const auto start = next;
                (void)consume(parser,token_type::left_c_brace);

                auto list_res = parse_initializer_list(parser,start);
                if(!list_res)
                {
                    return list_res;
                }

                return ast_struct_initializer(parser,struct_name.literal,*list_res,name_space,struct_name);
            }

            return var(parser,name_space,cur,true);
        }
        break;
    }   
}

ParserResult builtin_type_info_access(Parser& parser,const Token& start,builtin_type type)
{
    if(consume_match(parser,token_type::dot))
    {
        if(match(parser,token_type::symbol))
        {
            const auto sym = next_token(parser);
            return ast_builtin_access(parser,type,sym.literal,start);
        }
    }

    const auto cur = next_token(parser);

    return parser_error(parser,parse_error::unexpected_token,cur,"expected member access after builtin type, got %s",
        tok_name(cur.type)); 
}

ParserResult type_operator(Parser& parser, type_operator oper, const Token& token)
{
    const auto consume_left_paren_err = consume(parser,token_type::left_paren);
    if(consume_left_paren_err)
    {
        return *consume_left_paren_err;
    }

    auto type_res = parse_type(parser);

    if(!type_res)
    {
        return type_res.error();
    }

    const auto consume_right_paren_err = consume(parser,token_type::right_paren);
    if(consume_right_paren_err)
    {
        return *consume_right_paren_err;
    }

    return ast_type_operator(parser,type_res.value(),oper,token);   
}

ParserResult parse_cast(Parser& parser,const Token &t)
{
    auto expr_consume_err = consume(parser,token_type::left_paren);
    if(expr_consume_err)
    {
        return *expr_consume_err;
    }

    auto type_res = parse_type(parser);

    if(!type_res)
    {
        return type_res.error();
    }

    TypeNode* type = type_res.value();

    // Don't use the expr version is this is a subexpr
    const auto comma_consume_err = consume(parser,token_type::comma);
    if(comma_consume_err)
    {
        return *comma_consume_err;
    }

    const auto right_res = expr_terminate(parser,"cast",token_type::right_paren);
    if(!right_res)
    {
        return right_res;
    }

    return ast_cast(parser,type,right_res.value(),t); 
}

ParserResult parse_value_initializer_list(Parser& parser, const Token& t);
ParserResult parse_designated_initializers(Parser& parser, const Token& t);

ParserResult parse_initializer_list(Parser& parser, const Token& t)
{
    const bool designated = match(parser,token_type::symbol) && match(parser,token_type::colon,1);
    const auto list_res = designated? parse_designated_initializers(parser,t) :  parse_value_initializer_list(parser,t);

    // allow trailing comma if the list is about to end and its not the terminator i.e
    // {{0,0},{0,0},}
    if(match(parser,token_type::comma) && match(parser,token_type::right_c_brace,1))
    {
        next_token(parser);
    }

    return list_res;
}

ParserResult parse_value_initializer_list(Parser& parser, const Token& t)
{
    InitializerListNode* init = ast_initializer_list(parser,t);

    b32 hit_term = false;
    while(!hit_term)
    {
        auto e_res = expr_list(parser,"value initializer list",token_type::right_c_brace,&hit_term);

        if(!e_res)
        {
            return e_res.error();
        }

        push_var(init->list,*e_res);
    }

    return (AstNode*)init;
}



ParserResult parse_designated_initializers(Parser& parser, const Token& t)
{
    DesignatedListNode* list = ast_designated_initializer_list(parser,t);

    b32 hit_term = false;
    while(!hit_term)
    {
        const auto sym = next_token(parser);

        // Want to know if we started as designated because the entire list should match!
        const bool designated = sym.type == token_type::symbol && consume_match(parser,token_type::colon);

        if(!designated)
        {
            return parser_error(parser,parse_error::malformed_stmt,sym,"Designator: Hit unexpected token %s",tok_name(sym.type));
        }

        const auto name = sym.literal;

        // Nested struct initializer?
        if(match(parser,token_type::symbol) && match(parser,token_type::left_c_brace,1))
        {
            const auto struct_name = next_token(parser);
            (void)consume(parser,token_type::left_c_brace);

            auto list_res = parse_initializer_list(parser,t);

            if(!list_res)
            {
                return list_res;
            }

            auto struct_initializer = ast_struct_initializer(parser,struct_name.literal,*list_res,nullptr,struct_name);

            const DesignatedInitializer init = {struct_initializer,name};
            push_var(list->initializer,init);

            if(!consume_match(parser,token_type::comma))
            {
                hit_term = consume_match(parser,token_type::right_c_brace);
            }
        }

        else
        {
            // Read out the designator
            auto e_res = expr_list(parser,"designated initializer list",token_type::right_c_brace,&hit_term);

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
            return builtin_type_info_access(parser,t,builtin_type::u8_t);
        }

        case token_type::u16:
        {
            return builtin_type_info_access(parser,t,builtin_type::u16_t);
        }

        case token_type::u32:
        {
            return builtin_type_info_access(parser,t,builtin_type::u32_t);
        }

        case token_type::u64:
        {
            return builtin_type_info_access(parser,t,builtin_type::u64_t);
        }

        case token_type::s8:
        {
            return builtin_type_info_access(parser,t,builtin_type::s8_t);
        }

        case token_type::s16:
        {
            return builtin_type_info_access(parser,t,builtin_type::s16_t);
        }

        case token_type::s32:
        {
            return builtin_type_info_access(parser,t,builtin_type::s32_t);
        }

        case token_type::s64:
        {
            return builtin_type_info_access(parser,t,builtin_type::s64_t);
        }

        case token_type::bool_t:
        {
            return builtin_type_info_access(parser,t,builtin_type::bool_t);
        }

        case token_type::byte_t:
        {
            return builtin_type_info_access(parser,t,builtin_type::byte_t);
        }

        case token_type::c8_t:
        {
            return builtin_type_info_access(parser,t,builtin_type::c8_t);
        }


        // cast(<type>,<expr>)
        case token_type::cast:
        {
            return parse_cast(parser,t);
        }
    
        // sizeof(<expr>)
        case token_type::sizeof_t:
        {
            const auto consume_err = consume(parser,token_type::left_paren);
            if(consume_err)
            {
                return *consume_err;
            }

            auto expr_res = expr_terminate(parser,"sizeof",token_type::right_paren);

            return ast_sizeof(parser,expr_res,t);    
        }

        // cast_ref(<expr>)
        case token_type::cast_ref:
        {
            const auto consume_err = consume(parser,token_type::left_paren);
            if(consume_err)
            {
                return *consume_err;
            }

            auto expr_res = expr_terminate(parser,"cast_ref",token_type::right_paren);

            return ast_cast_ref(parser,expr_res,t);    
        }

        // sizeof_type(<type>)
        case token_type::sizeof_type_t:
        {
            return type_operator(parser,type_operator::sizeof_type_t,t);   
        }

        // sizeof_data(<type>)
        case token_type::sizeof_data_t:
        {
            return type_operator(parser,type_operator::sizeof_data_t,t);   
        }


        // initializer list
        case token_type::left_c_brace:
        {
            // check for no init 
            // NOTE: as this is is not common we resolve it here
            // rather than inside the tokenizer
            if(consume_match(parser,token_type::qmark))
            {
                const auto consume_c_brace_err = consume(parser,token_type::right_c_brace);
                if(consume_c_brace_err)
                {
                    return *consume_c_brace_err;
                }

                return ast_plain(parser,ast_type::no_init,t);
            }

            return parse_initializer_list(parser,t);
        }
    
        case token_type::value:
        {
            const builtin_type type = value_type(t.value);
            return ast_value(parser,type,t.value.v,t);
        }

        case token_type::float_t:
        {
            return ast_float(parser,t.fp,t);
        }

        case token_type::char_t:
        {
            return ast_value(parser,builtin_type::c8_t,t.character,t);
        }

        case token_type::string:
        {
            return ast_string(parser,t.literal,t);
        }

        case token_type::false_t:
        {
            return ast_value(parser,builtin_type::bool_t,false,t);
        }      

        case token_type::true_t:
        {
            return ast_value(parser,builtin_type::bool_t,true,t);
        }      

        case token_type::null_t:
        {
            return ast_plain(parser,ast_type::null_t,t);
        }

        case token_type::dollar:
        {
            const auto sym = next_token(parser);

            if(sym.type != token_type::symbol)
            {
                return parser_error(parser,parse_error::unexpected_token,t,
                    "Expected symbol after $ for generic var got %s",tok_name(sym.type));
            }
            
            return ast_generic_var(parser,sym.literal,sym); 
        }   

        case token_type::symbol:
        {
            return parse_sym(parser,nullptr,t);
        }

        case token_type::minus:
        {
            return ast_unary_arith(parser,arith_unary_op::sub_t,expression(parser,ctx,100),t);
        }

        case token_type::plus:
        {
            return ast_unary_arith(parser,arith_unary_op::add_t,expression(parser,ctx,100),t);
        }

        case token_type::bitwise_not:
        {
            return ast_unary_arith(parser,arith_unary_op::bitwise_not_t,expression(parser,ctx,100),t);
        }

        case token_type::logical_not:
        {
            return ast_unary_arith(parser,arith_unary_op::logical_not_t,expression(parser,ctx,100),t);
        }


        case token_type::left_paren:
        {
            const auto expr = expr_terminate(parser,"brackets",token_type::right_paren);
            return expr;
        }

        case token_type::deref:
        {
            return ast_deref(parser,expression(parser,ctx,lbp(parser,ctx,t)),t);
        }

        // in unary context and operator takes addr
        case token_type::operator_and:
        {
            return ast_addrof(parser,expression(parser,ctx,30),t);
        }

        case token_type::ignore:
        {
            return ast_plain(parser,ast_type::ignore,t);
        }

        default:
        {
            return parser_error(parser,parse_error::unexpected_token,t,"unary: unexpected token '%s' in %s",tok_name(t.type),ctx.expression_name.buf);
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

    auto left = parse_unary(parser,ctx,next_token(parser));
    auto cur = next_expr_token(parser,ctx);

    if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || !left)
    {
        return left;
    }

    auto lbp_res = lbp(parser,ctx,cur);
    if(!lbp_res)
    {
        return lbp_res.error();
    }

    u32 left_binding_power = *lbp_res;

    while(rbp < left_binding_power)
    {
        left = parse_binary(parser,ctx,cur,*left);        
        cur = next_expr_token(parser,ctx);

        if((ctx.expr_flags & EXPR_TERMINATED_FLAG) || !left)
        {
            return left;
        }

        lbp_res = lbp(parser,ctx,cur);
        
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
        return parser_error(parser,parse_error::invalid_terminator,ctx.term_tok,"%s should terminate with '%s' terminated with '%s'",
            ctx.expression_name.buf,tok_name(ctx.term),tok_name(ctx.term_tok.type));
    }

    return e;
}


// Can optionally terminate, caller must check
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t, token_type &term)
{
    ExprCtx ctx;
    ctx.term = t;
    ctx.expression_name = expression_name;

    auto e = expr_terminate_internal(parser,ctx);

    // what token did we terminate on?
    term = ctx.term_tok.type;

    return e;
}


// panic on failure to terminate with token
ParserResult expr_terminate(Parser &parser,const String& expression_name,token_type t)
{
    ExprCtx ctx;
    ctx.term = t;
    ctx.expression_name = expression_name;
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
    ctx.expr_flags = EXPR_MUST_TERMINATE_FLAG | EXPR_TERM_LIST_FLAG;

    auto e_res = expr_terminate_internal(parser,ctx);

    *hit_terminator = ctx.expr_flags & EXPR_HIT_TERMINATOR;
    return e_res;
}

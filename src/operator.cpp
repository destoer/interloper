


void add_operator(Interloper& itl, operator_type type, NameSpace* name_space, const TopLevelDefinition& def)
{
    FunctionDef func_def;

    // Make sure our function is not allocated on the
    // same string allocator as the AST
    func_def.name = "operator"
    func_def.parser_def = def;
    
    func_def.root = nullptr;
    func_def.func = nullptr;
    func_def.name_space = name_space;
    
    Operator operator_info;
    operator_info.type = type;
    operator_info.func_def = func_def;

    push_var(itl.operator_table.lookup[u32(type)],operator_info);
}

Option<parse_error> operator_decl(Interloper& itl,Parser& parser, const ParsedAttr& attr)
{
    const auto start_span = make_const_span(parser.ctx.tokens,parser.ctx.tok_idx, parser.ctx.tokens.size - parser.ctx.tok_idx);

    const auto operator_tok = next_token(parser);

    operator_type type = operator_type::add;

    switch(operator_tok.type)
    {
        case token_type::plus:
        {
            type = operator_type::add;
            break;
        }

        default:
        {
            return parser_error(parser,parse_error::unexpected_token,operator_tok,"Expected operator got: %s!",
                tok_name(operator_tok.type));  
        }
    }

    const auto res = scan_brace_stmt(parser,"Operator",OPERATOR_NAMES[u32(type)],start_span);
    if(!res)
    {
        return res.error();
    }

    const auto operator_def = *res;

    const auto def = make_top_level_def(parser,operator_def,attr);
    add_operator(itl,type,parser.ctx.cur_namespace,def);

    return option::none;
}

Option<parse_error> parse_operator_decl(Parser& parser, Operator& operator_def)
{
    const auto operator_tok = next_token(parser);
    auto func_res = parse_func_sig(parser,"operator",operator_tok);

    if(!func_res)
    {
        return func_res.error();
    }

    FuncNode* func = *func_res;
    func->attr = operator_def.def.attr;
    operator_def.func_def.func_node = func;

    auto block_err = block_ast(parser,&func->block);
    if(block_err)
    {
        return block_err;
    }

    // finally add the function def
    return option::none;
}

// TODO: This needs to have two values for now
Option<itl_error> type_check_operator_overload(Interloper& itl, Operator& operator_def, FuncCallNode* func_call)
{
    // TODO: This does not support generics yet.
    const auto func_err = finalise_func(itl,operator_def.func_def,func_call,false);
    if(func_err)
    {
        return func_err;
    }

    // Check atleast one is a struct and both are "values"


    // Check that this is overload is unique
    // TODO: How is this done?



    return option::none;
}

Operator* find_bin_operator(Interloper& itl,const Span<Operator>& lookup, FuncCallNode* func_call)
{

}

Operator* find_operator_overload(Interloper& itl, FuncCallNode* func_call, operator_type type)
{
    auto& table = itl.operator_table[u32(type)];
    const auto span = make_span(table,count(table));

    switch(type)
    {
        case operator_type::add:
        {
            return find_bin_operator(span,func_call);
        }
    }
}

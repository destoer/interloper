


void add_operator(Interloper& itl, operator_type type, NameSpace* name_space, const TopLevelDefinition& def)
{
    Operator operator_info;
    operator_info.type = type;
    operator_info.name_space = name_space;
    operator_info.def = def;

    push_var(itl.operator_table.lookup[u32(type)],operator_info);
}

Option<parse_error> operator_decl(Interloper& itl,Parser& parser, const ParsedAttr& attr)
{
    const auto start_span = make_const_span(parser.ctx.tokens,parser.ctx.tok_idx, parser.ctx.tokens.size - parser.ctx.tok_idx);

    // what is the name of our function?
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
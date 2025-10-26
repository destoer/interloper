ConstValueResult type_check_const_int_expression(Interloper& itl, AstNode* node)
{   
    const auto type_res = type_check_expr(itl,node);
    if(!type_res)
    {
        return type_res.error();
    }

    // not valid if this is not an int
    if(!is_integer(node->expr_type))
    {
        return compile_error(itl,itl_error::int_type_error,"expected integer for const int expr got %t",node->expr_type); 
    }

    if(!node->known_value)
    {
        return compile_error(itl,itl_error::int_type_error,"Value for const integer expression is not known");
    }

    return ConstValue{node->expr_type,*node->known_value};
}

Option<itl_error> compile_constant_initializer(Interloper& itl,Symbol& symbol, AstNode* expr)
{
    auto type_res = type_check_expr(itl,expr);

    if(!type_res)
    {
        return type_res.error();
    }

    auto known_value = expr->known_value;

    if(is_builtin(symbol.type))
    {
        if(!known_value)
        {
            return compile_error(itl,itl_error::const_type_error,"Builtin const expression is not statically known");
        }

        // Static value is allready known from semantic checking.
        symbol.known_value = known_value;
        return check_assign_init(itl,symbol.type,*type_res);
    }

    assert(false);
}

Option<itl_error> compile_constant_decl(Interloper& itl, DeclNode* decl_node, b32 global)
{
    // pull type and name so we can create a symbol
    const auto name = decl_node->name;

    if(symbol_exists(itl.symbol_table,name))
    {
        return compile_error(itl,itl_error::redeclaration,"constant symbol %S redefined",name);
    }

    // force constant
    decl_node->type->is_constant = true;

    // build the typing info
    auto type_res = get_type(itl,decl_node->type);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;

    // add into table
    auto& sym = global? add_global(itl,name,type,true) : add_symbol(itl,name,type);

    // make sure this is marked as constant
    // incase it is declared locally
    sym.reg.segment = reg_segment::constant;

    // compile the expression
    return compile_constant_initializer(itl,sym,decl_node->expr);    
}

Option<itl_error> compile_constant(Interloper& itl, GlobalDeclNode* node)
{
    auto context_guard = switch_context(itl,node->filename,node->name_space,(AstNode*)node);
    return compile_constant_decl(itl,node->decl,true);
}


Option<itl_error> compile_constants(Interloper& itl)
{
    for(GlobalDeclNode* decl : itl.constant_decl)
    {
        const auto decl_err = compile_constant(itl,decl);
        if(decl_err)
        {
            return decl_err;
        }
    }

    return option::none;
}
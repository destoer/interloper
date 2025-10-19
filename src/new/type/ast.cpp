// TODO: Split this fairly immediately.

Option<itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space);
Option<itl_error> type_check_expr(Interloper& itl, AstNode* node);

Option<itl_error> check_startup_defs(Interloper& itl)
{   
    // if(itl.rtti_enable)
    // {
    //     const auto cache_err = cache_rtti_structs(itl);
    //     if(cache_err)
    //     {
    //         return cache_err;
    //     }
    // }

    // itl.std_name_space = find_name_space(itl,"std");

    // if(!itl.std_name_space)
    // {
    //     return compile_error(itl,itl_error::undeclared,"std namespace is not declared");
    // }

    const auto main_err = check_startup_func(itl,"main",itl.global_namespace);
    if(main_err)
    {
        return main_err;
    }

    const auto start_err = check_startup_func(itl,"start",itl.global_namespace);
    if(start_err)
    {
        return start_err;
    }

    // const auto memcpy_err = check_startup_func(itl,"memcpy",itl.std_name_space);
    // if(memcpy_err)
    // {
    //     return memcpy_err;
    // }
    
    // const auto zero_err = check_startup_func(itl,"zero_mem",itl.std_name_space);
    // if(zero_err)
    // {
    //     return zero_err;
    // }

    return option::none;
}

Option<itl_error> type_check_decl(Interloper &itl, DeclNode* decl)
{
    if(decl->expr)
    {
        const auto decl_err = type_check_expr(itl,decl->expr);
        if(decl_err)
        {
            return decl_err;
        }
    }

    auto type_res = get_complete_type(itl,decl->type);
    if(!type_res)
    {
        return type_res.error();
    }

    decl->node.expr_type = *type_res;

    // TODO: check declaration mark symbol slot for decl in node.    
    // decl->slot = 


    return option::none;
}

Option<itl_error> type_check_expr(Interloper& itl, AstNode* expr)
{
    switch(expr->type)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Type checker(expr) unknown node %s",AST_NAMES[u32(expr->type)]);
        }
    }

    return option::none;
}

// Don't care so much about the legality of structures here, we just want to blindly check the type
// And do name resolution, as we are going to have to reswitch the AST when we compile anyways.

Option<itl_error> type_check_block(Interloper& itl, AstBlock& block)
{
    for(AstNode* stmt : block.statement)
    {
        switch(stmt->type)
        {
            case ast_type::decl:
            {
                return type_check_decl(itl, (DeclNode*)stmt);
            }

            default:
            {
                return compile_error(itl,itl_error::invalid_expr,"Type checker(stmt) unknown node %s",AST_NAMES[u32(stmt->type)]);
            }
        }
    }

    assert(false);
    return option::none;
}


Option<itl_error> type_check_ast(Interloper& itl)
{
    // Forcibly check startup funcs
    // This will trigger type checking of further functions
    const auto startup_err = check_startup_defs(itl);
    if(startup_err)
    {
        return startup_err;
    }

    // Type check globals

    assert(false);
}
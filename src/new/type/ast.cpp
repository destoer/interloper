// TODO: Split this fairly immediately.

Option<itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space);
TypeResult type_check_expr(Interloper& itl, AstNode* expr);

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
        const auto decl_res = type_check_expr(itl,decl->expr);
        if(!decl_res)
        {
            return decl_res.error();
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

TypeResult type_check_arith_bin(Interloper& itl, ArithBinNode* expr)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(expr->oper)];

    const auto left_res = type_check_expr(itl,expr->left);

    if(!left_res)
    {
        return left_res.error();
    }
    

    const auto right_res = type_check_expr(itl,expr->right);

    if(!right_res)
    {
        return right_res.error();
    }

    auto left = *left_res;
    auto right = *right_res;

    // pointer arith adds the size of the underlying type
    if(is_pointer(left) && is_integer(right))
    {
        if(expr->oper != arith_bin_op::add_t || expr->oper != arith_bin_op::sub_t)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for pointers");

        }

        return left;
    }

    // allow pointer subtraction
    else if(is_pointer(left) && is_pointer(right))
    {
        if(expr->oper != arith_bin_op::sub_t)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for pointers");
        }

        return make_builtin(itl,builtin_type::u64_t);
    }

    // floating point arith
    else if(is_float(left) && is_float(right))
    {
        const op_type type = arith_info.float_form;

        if (type == op_type::none)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for floats");
        }

        return make_builtin(itl,builtin_type::f64_t);
    }

    else if(is_bool(left) && is_bool(right))
    {
        switch(expr->oper)
        {
            // Treat these like integer operations
            case arith_bin_op::or_t:
            case arith_bin_op::and_t:
            {
                return make_builtin(itl,builtin_type::bool_t);
            }

            default:
            {
                return compile_error(itl,itl_error::invalid_expr,"operation is not defined for bool");
            }
        }
    }

    // integer arith
    else if(is_integer(left) && is_integer(right))
    {
        return effective_arith_type(itl,left,right,expr->oper);  
    }

    // No idea!
    return compile_error(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %t and %t",left,right);
}

TypeResult assign_expr_type(AstNode* node, TypeResult result)
{
    if(!result)
    {
        return result;
    }

    return node->expr_type = *result;
}

TypeResult type_check_expr(Interloper& itl, AstNode* expr)
{
    switch(expr->type)
    {
        case ast_type::value:
        {
            ValueNode* value_node = (ValueNode*)expr;
            return expr->expr_type = make_builtin(itl,value_node->type);
        }

        case ast_type::arith_unary:
        {
            ArithUnaryNode* arith = (ArithUnaryNode*)expr;
            return assign_expr_type(expr,type_check_expr(itl,arith->expr));
        }

        case ast_type::arith_bin:
        {
            ArithBinNode* arith = (ArithBinNode*)expr;
            return assign_expr_type(expr,type_check_arith_bin(itl,arith));
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Type checker(expr) unknown node %s",AST_NAMES[u32(expr->type)]);
        }
    }

    assert(false);
    return make_builtin(itl,builtin_type::void_t);
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
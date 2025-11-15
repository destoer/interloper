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

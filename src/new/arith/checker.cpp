TypeResult type_check_boolean_logic(Interloper& itl, AstNode* expr) {
    BooleanLogicNode* logic = (BooleanLogicNode*)expr;

    const auto left_res = type_check_expr(itl,logic->left);

    if(!left_res)
    {
        return left_res.error();
    }
    

    const auto right_res = type_check_expr(itl,logic->right);

    if(!right_res)
    {
        return right_res.error();
    }
    
    const auto ltype = *left_res;
    const auto rtype = *right_res;
   
    if(!is_bool(ltype) || !is_bool(rtype))
    {
        return compile_error(itl,itl_error::bool_type_error,"Boolean logic is only defined on bools %t %s %t",
            ltype,BOOLEAN_LOGIC_NAMES[u32(logic->oper)],rtype);
    }

    return make_builtin(itl,builtin_type::bool_t);
}


Result<b32,itl_error> check_static_cmp(Interloper& itl, const Type* value, const Type* oper, u64 v)
{
    // unsigned value against signed value
    // if one side is signed and the other unsigned
    // allow comparision if the unsigned is a static value that
    // the signed side can represent
    if(!is_signed(value) && is_signed(oper))
    {
        // value is within range of operand value
        // change value to a the signed type
        if(v <= builtin_max(cast_builtin(oper)))
        {
            return true;
        }

        else
        {
            return compile_error(itl,itl_error::out_of_bounds,"value: %x exceeds type %t",v,oper);
        }
    }

    // value is outside the range of the other type
    else if(is_signed(value) == is_signed(oper))
    {
        if(builtin_size(cast_builtin(value)) > builtin_size(cast_builtin(oper)))
        {
            return compile_error(itl,itl_error::out_of_bounds,"value: %x exceeds type %t",v,oper);
        }
    }

    return false;
}


TypeResult type_check_comparison(Interloper& itl, AstNode* expr) 
{
    CmpNode* cmp = (CmpNode*)expr;

    auto left_res = type_check_expr(itl,cmp->left);
    if(!left_res)
    {
        return left_res.error();
    }

    auto right_res = type_check_expr(itl,cmp->right);
    if(!right_res)
    {
        return right_res.error();
    }

    auto ltype = *left_res;
    auto rtype = *right_res;

    // if one side is a value do type checking
    if(is_integer(ltype) && is_integer(rtype))
    {
        // Coerce the known value to the other operands type if we have checked this is fine.
        if(cmp->left->known_value)
        {
            const auto coerce_res = check_static_cmp(itl,ltype,rtype,*cmp->left->known_value);
            if(!coerce_res)
            {
                return coerce_res.error();
            }
            
            if(*coerce_res)
            {
                ltype = rtype;
            }
        }

        else if(cmp->right->known_value)
        {
            const auto coerce_res = check_static_cmp(itl,rtype,ltype,*cmp->right->known_value);
            if(!coerce_res)
            {
                return coerce_res.error();
            }

            if(*coerce_res)
            {
                rtype = ltype;
            }
        } 
    }
    
    return check_comparison_operation(itl,ltype,rtype,cmp->oper);
}

TypeResult type_check_arith_bin(Interloper& itl, AstNode* expr)
{
    ArithBinNode* bin = (ArithBinNode*)expr;
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(bin->oper)];

    const auto left_res = type_check_expr(itl,bin->left);

    if(!left_res)
    {
        return left_res.error();
    }
    

    const auto right_res = type_check_expr(itl,bin->right);

    if(!right_res)
    {
        return right_res.error();
    }

    auto left = *left_res;
    auto right = *right_res;

    // pointer arith adds the size of the underlying type
    if(is_pointer(left) && is_integer(right))
    {
        if(bin->oper != arith_bin_op::add_t || bin->oper != arith_bin_op::sub_t)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for pointers");

        }

        return left;
    }

    // allow pointer subtraction
    else if(is_pointer(left) && is_pointer(right))
    {
        if(bin->oper != arith_bin_op::sub_t)
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
        switch(bin->oper)
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
        return effective_arith_type(itl,left,right,bin->oper);  
    }

    // No idea!
    return compile_error(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %t and %t",left,right);
}

TypeResult type_check_arith_unary(Interloper& itl, AstNode* expr)
{
    ArithUnaryNode* unary = (ArithUnaryNode*)expr;

    const auto expr_res = type_check_expr(itl,unary->expr);
    if(!expr_res)
    {
        return expr_res;
    }

    const auto rtype = *expr_res;

    switch(unary->oper)
    {
        case arith_unary_op::add_t:
        case arith_unary_op::bitwise_not_t:
        case arith_unary_op::sub_t:
        {
            if(!is_integer(rtype))
            {
                return compile_error(itl,itl_error::int_type_error,"Unary %s only defined on int got: %t",ARITH_UNARY_NAMES[u32(unary->oper)],rtype);
            }

            return rtype;
        }
        
        case arith_unary_op::logical_not_t:
        {
            // integer or pointer, eq to zero
            if(is_integer(rtype) || is_pointer(rtype) || is_array(rtype) || is_bool(rtype))
            {
                return make_builtin(itl,builtin_type::bool_t);
            }

            return compile_error(itl,itl_error::bool_type_error,"compile: logical_not expected any of(integer, array, pointer, bool)  got: %t",rtype);
        }
    }

    assert(false);
    return make_builtin(itl,builtin_type::void_t);
}
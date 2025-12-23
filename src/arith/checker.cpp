struct BinType
{
    Type* ltype;
    Type* rtype;
};

using BinTypeResult = Result<BinType,itl_error>;

template<typename T>
BinTypeResult type_check_expr_bin(Interloper& itl, ExprBinOperNode<T>* bin)
{
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
    
    return BinType {*left_res,*right_res};
}

TypeResult type_check_shift(Interloper& itl, AstNode* expr) {
    ShiftNode* shift = (ShiftNode*)expr;

    auto bin_res = type_check_expr_bin(itl,shift);
    if(!bin_res)
    {
        return bin_res.error();
    }

    const auto& bin = *bin_res;

    if(!(is_integer(bin.ltype) && is_integer(bin.rtype)))
    {
        return compile_error(itl,itl_error::int_type_error,"shifts only defined for integers, got %t and %t",bin.ltype,bin.rtype);
    }

    return bin.ltype;
}

TypeResult type_check_boolean_logic(Interloper& itl, AstNode* expr) {
    BooleanLogicNode* logic = (BooleanLogicNode*)expr;

    auto bin_res = type_check_expr_bin(itl,logic);
    if(!bin_res)
    {
        return bin_res.error();
    }

    const auto& bin = *bin_res;

    if(!is_bool(bin.ltype) || !is_bool(bin.rtype))
    {
        return compile_error(itl,itl_error::bool_type_error,"Boolean logic is only defined on bools %t %s %t",
            bin.ltype,BOOLEAN_LOGIC_NAMES[u32(logic->oper)],bin.rtype);
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

    auto bin_res = type_check_expr_bin(itl,cmp);
    if(!bin_res)
    {
        return bin_res.error();
    }

    auto bin = *bin_res;

    // if one side is a value do type checking
    if(is_integer(bin.ltype) && is_integer(bin.rtype))
    {
        // Coerce the known value to the other operands type if we have checked this is fine.
        if(cmp->left->known_value)
        {
            const auto coerce_res = check_static_cmp(itl,bin.ltype,bin.rtype,*cmp->left->known_value);
            if(!coerce_res)
            {
                return coerce_res.error();
            }
            
            if(*coerce_res)
            {
                bin.ltype = bin.rtype;
            }
        }

        else if(cmp->right->known_value)
        {
            const auto coerce_res = check_static_cmp(itl,bin.rtype,bin.ltype,*cmp->right->known_value);
            if(!coerce_res)
            {
                return coerce_res.error();
            }

            if(*coerce_res)
            {
                bin.rtype = bin.ltype;
            }
        } 
    }
    
    return check_comparison_operation(itl,bin.ltype,bin.rtype,cmp->oper);
}

Value value_from_known_expr(AstNode* expr)
{
    return make_value(*expr->known_value,is_signed(expr->expr_type));
} 


Type* make_value_type(Interloper& itl, const Value& value)
{
    const auto builtin = value_type(value);
    return make_builtin(itl,builtin);
}

Type* calc_known_value(Interloper& itl, AstNode* expr,  u64 result, bool sign)
{
    const auto ans = make_value(result,sign);
    expr->known_value = ans.v;
    return make_value_type(itl,ans);   
}

TypeResult compute_known_integer_arith(Interloper& itl, ArithBinNode* bin)
{
    const Value left = value_from_known_expr(bin->left);
    const Value right = value_from_known_expr(bin->right);
    const bool final_sign = left.sign || right.sign;

    switch(bin->oper)
    {
        case arith_bin_op::add_t:
        {
            return calc_known_value(itl,&bin->node,left.v + right.v,final_sign);
        }

        case arith_bin_op::sub_t:
        {
            return calc_known_value(itl,&bin->node,left.v - right.v,final_sign);
        }

        case arith_bin_op::mul_t:
        {
            return calc_known_value(itl,&bin->node,left.v * right.v,final_sign);
        }

        case arith_bin_op::mod_t:
        {
            if(right.v == 0)
            {
                return compile_error(itl,itl_error::int_type_error,"Modulus by zero");
            }

            if(final_sign)
            {
                return calc_known_value(itl,&bin->node,s64(left.v) % s64(right.v),final_sign);
            }

            return calc_known_value(itl,&bin->node,left.v % right.v,final_sign);
        }

        case arith_bin_op::div_t:
        {
            if(right.v == 0)
            {
                return compile_error(itl,itl_error::int_type_error,"Division by zero");
            }

            if(final_sign)
            {
                return calc_known_value(itl,&bin->node,s64(left.v) / s64(right.v),final_sign);
            }

            return calc_known_value(itl,&bin->node,left.v / right.v,final_sign);
        }

        case arith_bin_op::xor_t:
        {
            return calc_known_value(itl,&bin->node,left.v ^ right.v,final_sign);
        }

        case arith_bin_op::and_t:
        {
            const auto ans = make_value(left.v & right.v,final_sign);
            bin->node.known_value = ans.v;

            if(is_bool(bin->left->expr_type))
            {
                return make_builtin(itl,builtin_type::bool_t);
            }

            return make_value_type(itl,ans);
        }

        case arith_bin_op::or_t:
        {
            const auto ans = make_value(left.v | right.v,final_sign);
            bin->node.known_value = ans.v;

            if(is_bool(bin->left->expr_type))
            {
                return make_builtin(itl,builtin_type::bool_t);
            }

            return make_value_type(itl,ans);
        }
    }

    assert(false);
}

TypeResult type_check_arith_bin(Interloper& itl, AstNode* expr)
{
    ArithBinNode* arith = (ArithBinNode*)expr;
    const auto type = arith->oper;

    const ArithmeticInfo& arith_info = ARITH_INFO[u32(type)];

    auto bin_res = type_check_expr_bin(itl,arith);
    if(!bin_res)
    {
        return bin_res.error();
    }

    const auto& bin = *bin_res;

    const bool known_expr = arith->left->known_value && arith->right->known_value;

    // pointer arith adds the size of the underlying type
    if(is_pointer(bin.ltype) && is_integer(bin.rtype))
    {
        if(type != arith_bin_op::add_t && type != arith_bin_op::sub_t)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for pointers");

        }

        return bin.ltype;
    }

    // allow pointer subtraction
    else if(is_pointer(bin.ltype) && is_pointer(bin.rtype))
    {
        if(type != arith_bin_op::sub_t)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for pointers");
        }

        return make_builtin(itl,builtin_type::u64_t);
    }

    // floating point arith
    else if(is_float(bin.ltype) && is_float(bin.rtype))
    {
        if (arith_info.float_form == op_type::none)
        {
            return compile_error(itl,itl_error::invalid_expr,"operation is not defined for floats");
        }

        return make_builtin(itl,builtin_type::f64_t);
    }

    else if(is_bool(bin.ltype) && is_bool(bin.rtype))
    {
        switch(type)
        {
            // Treat these like integer operations
            case arith_bin_op::or_t:
            case arith_bin_op::and_t:
            {
                if(known_expr)
                {
                    return compute_known_integer_arith(itl,arith);
                }

                return make_builtin(itl,builtin_type::bool_t);
            }

            default:
            {
                return compile_error(itl,itl_error::invalid_expr,"operation is not defined for bool");
            }
        }
    }

    // integer arith
    else if(is_integer(bin.ltype) && is_integer(bin.rtype))
    {
        if(known_expr)
        {
            return compute_known_integer_arith(itl,arith);
        }

        return effective_arith_type(itl,bin.ltype,bin.rtype,type);  
    }

    // No idea!
    return compile_error(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %t and %t",bin.ltype,bin.rtype);
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
        case arith_unary_op::sub_t:
        {
            if(!is_integer(rtype) && !is_float(rtype))
            {
                return compile_error(itl,itl_error::int_type_error,"Unary arith %s only defined on ints and floats got: %t",
                    ARITH_UNARY_NAMES[u32(unary->oper)],rtype);
            }

            return rtype;
        }

        case arith_unary_op::bitwise_not_t:
        {
            if(!is_integer(rtype))
            {
                return compile_error(itl,itl_error::int_type_error,"Bitwise not is only defined on ints got: %t",rtype);
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

TypeResult type_check_addrof(Interloper& itl, AstNode* expr)
{
    AddrOfNode* addr = (AddrOfNode*)expr;

    const auto res = type_check_expr(itl,addr->expr);

    if(!res)
    {
        return res.error();
    }

    auto type = *res;

    if(is_func_pointer(type))
    {
        if(addr->expr->type == ast_type::symbol)
        {
            SymbolNode* sym_node = (SymbolNode*)addr->expr;
            // Taken directly on a function name just return the type
            if(sym_node->func)
            {
                return type;
            }
        }

        // Taken on a ordinary symbol return as a pointer
        return make_reference(itl,type);
    }

    if(is_fixed_array(type))
    {
        return compile_error(itl,itl_error::array_type_error,"Cannot take pointer to fixed sized array");
    }

    return make_reference(itl,type);
}

TypeResult type_check_deref(Interloper& itl, AstNode* expr)
{
    DerefNode* deref = (DerefNode*)expr;

    const auto res = type_check_expr(itl,deref->expr);

    if(!res)
    {
        return res.error();
    }

    auto type = *res;

    // make sure we actually have a pointer
    if(!is_pointer(type))
    {
        return compile_error(itl,itl_error::pointer_type_error,"Expected pointer got: %t",type);
    }

    PointerType* pointer = (PointerType*)type;

    if(pointer->pointer_kind == pointer_type::nullable)
    {
        return compile_error(itl,itl_error::pointer_type_error,"Cannot dereference a nullable pointer %t",type);
    }

    return pointer->contained_type;
}

TypeResult type_check_null(Interloper& itl, AstNode* expr)
{
    UNUSED(expr);
    return make_nullable_ptr(itl,make_builtin(itl,builtin_type::null_t));
}
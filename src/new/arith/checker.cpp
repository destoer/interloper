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

    // pointer arith adds the size of the underlying type
    if(is_pointer(bin.ltype) && is_integer(bin.rtype))
    {
        if(type != arith_bin_op::add_t || type != arith_bin_op::sub_t)
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
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

    if(logic->left->known_value && logic->right->known_value)
    {
        const auto left = *logic->left->known_value;
        const auto right = *logic->right->known_value;

        switch(logic->oper)
        {
            case boolean_logic_op::and_t:
            {
                logic->node.known_value = right && left;
                break;
            }

            case boolean_logic_op::or_t:
            {
                logic->node.known_value = right || left;
                break;
            }
        }
    }

    return make_builtin(itl,builtin_type::bool_t);
}


Result<b32,itl_error> check_static_cmp(Interloper& itl, const Type* value, const Type* oper, u64 v)
{
    const bool value_sign = is_signed(value);

    // allow comparision if the unsigned is a static value that
    // fits within the range (this works for both sign and unsigned opers)
    if(!value_sign)
    {
        // value is within range of operand value
        // change value to a the signed type
        if(v <= builtin_max(cast_builtin(oper)))
        {
            return true;
        }

        return compile_error(itl,itl_error::out_of_bounds,"value: %X exceeds type %t",v,oper);
    }

    
    // If signs differ on and the known value is not unsigned we cannot do checks.
    else if(is_signed(value) != is_signed(oper))
    {
        return false;
    }

    // same sign

    if(value_sign)
    {
        const auto min = s64(builtin_min(cast_builtin(oper)));
        const auto max = s64(builtin_max(cast_builtin(oper)));
        const auto signed_value = s64(v);

        if(signed_value < min || signed_value > max)
        {
            return compile_error(itl,itl_error::out_of_bounds,"value: exceeds type bounds %t: %l < %l < %l",oper,min,signed_value,max);
        }

        return true;
    }

    else 
    {
        if(v <= builtin_max(cast_builtin(oper)))
        {
            return true;
        }

        return compile_error(itl,itl_error::out_of_bounds,"value: %X exceeds type %t",v,oper);
    }

    return false;
}


TypeResult check_known_cmp(Interloper& itl, CmpNode* cmp)
{
    const auto res = check_comparison_operation(itl,cmp->left->expr_type,cmp->right->expr_type,cmp->oper);
    if(!res)
    {
        return res;
    }

    const b32 sign = is_signed(cmp->left->expr_type);

    const u64 left = *cmp->left->known_value;
    const u64 right = *cmp->right->known_value;

    switch(cmp->oper)
    {
        case comparison_op::lt: 
        {
            cmp->node.known_value = sign? s64(left) < s64(right) : left < right;
            break;
        }

        case comparison_op::le: 
        {
            cmp->node.known_value = sign? s64(left) <= s64(right) : left <= right;
            break;
        }

        case comparison_op::gt: 
        {
            cmp->node.known_value = sign? s64(left) > s64(right) : left > right;
            break;
        }

        case comparison_op::ge: 
        {
            cmp->node.known_value = sign? s64(left) >= s64(right) : left >= right;
            break;
        }

        case comparison_op::eq: 
        {
            cmp->node.known_value = left == right;
            break;
        }
        
        case comparison_op::ne: 
        {
            cmp->node.known_value = left != right;
            break;
        }
    }

    return res;
}


bool compare_decay_integer(const Type* type)
{
    return is_pointer(type) || is_enum(type) || is_integer(type) || is_bool(type);
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

    // Statically known comparison that is integer based
    if(cmp->left->known_value && cmp->right->known_value)
    {
        if(compare_decay_integer(bin.ltype) && compare_decay_integer(bin.rtype))
        {
            return check_known_cmp(itl,cmp);
        }
    }

    // if one side is a value do type checking
    else if(is_integer(bin.ltype) && is_integer(bin.rtype))
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

u64 compute_known_unary_arith(ArithUnaryNode* unary)
{
    const auto value = *unary->expr->known_value;
    switch(unary->oper)
    {
        case arith_unary_op::add_t:
        {
            return +value;
        }

        case arith_unary_op::sub_t:
        {
            return -value;
        }

        case arith_unary_op::bitwise_not_t:
        {
            return ~value;
        }

        case arith_unary_op::logical_not_t:
        {
            return !value;
        }
    }

    assert(false);
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

    if(unary->expr->known_value && is_integer(rtype))
    {
        unary->node.known_value = compute_known_unary_arith(unary);
    }

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
    if(addr->expr->type == ast_type::struct_access)
    {
        StructAccessNode* access = (StructAccessNode*)addr->expr;
        access->flags |= STRUCT_TAKE_ADDR_FLAG;
    }

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

TypeResult type_check_sizeof(Interloper& itl, AstNode* expr)
{
    SizeOfNode* size_node = (SizeOfNode*)expr;
    const auto res = type_check_expr(itl,size_node->expr);
    if(!res)
    {
        return res;
    }

    auto value = make_value(type_memory_size(itl,*res),false);

    size_node->node.known_value = value.v; 

    return make_value_type(itl,value);
}

TypeResult access_builtin_type_info(Interloper& itl, AstNode* node, builtin_type type, const String& member_name)
{
    const BuiltinTypeInfo& info = builtin_type_info[u32(type)];

    if(member_name == "size")
    {
        node->known_value = u64(info.size);
    }

    else if(member_name == "max")
    {
        node->known_value = u64(info.max);
    }

    else if(member_name == "min")
    {
        node->known_value = u64(info.min);
    }

    else
    {
        return compile_error(itl,itl_error::generic_type_error,"unknown type info for builtin type %s.%S",TYPE_NAMES[u32(type)],member_name);
    }

    return info.is_signed? itl.ssize_type : itl.usize_type;
}

TypeResult type_check_builtin_type_info(Interloper& itl, AstNode* expr)
{
    BuiltinAccessNode* builtin_info = (BuiltinAccessNode*)expr;
    return access_builtin_type_info(itl,&builtin_info->node,builtin_info->type,builtin_info->field);
}

TypeResult type_check_user_type_info(Interloper& itl, AstNode* expr)
{
    UserTypeInfoNode* user_info = (UserTypeInfoNode*)expr;

    auto type_decl_opt = lookup_type_internal(itl,user_info->name_space,user_info->type_name);
    if(!type_decl_opt)
    {
        return compile_error(itl,itl_error::undeclared,"No such type %n%S",user_info->name_space,user_info->type_name);
    }

    TypeDecl* type_decl = *type_decl_opt;
    const auto& member_name = user_info->member_name;

    switch(type_decl->kind)
    {
        case type_kind::builtin:
        {
            builtin_type type = builtin_type(type_decl->type_idx);
            return access_builtin_type_info(itl,&user_info->node,type,member_name);
        }

        case type_kind::struct_t:
        {
            if(member_name == "size")
            {
                const auto& structure = itl.struct_table[type_decl->type_idx];
                user_info->node.known_value = structure.size;

                return itl.usize_type;
            }

            return compile_error(itl,itl_error::struct_error,"Unknown type info for struct %S",type_decl->name);
        }

        case type_kind::enum_t:
        {
            if(member_name == "len")
            {
                const auto enumeration = itl.enum_table[type_decl->type_idx];

                user_info->node.known_value = enumeration.member_map.size;

                return itl.usize_type;
            }

            return compile_error(itl,itl_error::enum_type_error,"Unknown type info for enum %S",type_decl->name);
        }

        case type_kind::alias_t:
        {
            return compile_error(itl,itl_error::generic_type_error,"Cannot access type properties on alias %S",type_decl->name);
        }
    }

    assert(false);
}
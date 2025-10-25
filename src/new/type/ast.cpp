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

FuncCall call_info_from_func_pointer(Symbol& sym)
{
    FuncPointerType* func_type = (FuncPointerType*)sym.type;

    FuncCall call_info;
    call_info.reg_slot = sym.reg.slot;
    call_info.sig = func_type->sig;
    call_info.name = sym.name;
    call_info.func_pointer = true;

    return call_info;
}

Result<FuncCall,itl_error> get_symbol_sig(Interloper& itl, SymbolNode* sym_node)
{
    const String& name = sym_node->name;
    NameSpace* name_space = sym_node->name_space;

    // are we looking in the global namespace or no?
    const b32 global = name_space == nullptr;

    FunctionDef* func_call_def = global? lookup_func_def_default(itl,name) : lookup_func_def_scope(itl,name_space,name);

    // Function is known Just we are done
    if(func_call_def)
    {
        auto func_call_res = finalise_func(itl,*func_call_def);

        if(!func_call_res)
        {
            return func_call_res.error();
        }

        Function* func_call = *func_call_res;
        return func_call->call_info;
    }

    // no known function 
    // check if this is instead a function pointer call
    auto sym_ptr = get_sym(itl.symbol_table,name);

    if(!sym_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: function %n%S is not declared",name_space,name);
    }

    auto& sym = *sym_ptr;

    // we have a symbol that is a function pointer we are good to go
    if(!is_func_pointer(sym.type))
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol %S is not a function pointer or function",name);       
    }

    return call_info_from_func_pointer(sym);
}

Result<FuncCall,itl_error> get_calling_sig(Interloper& itl,AstNode* expr)
{
    // just a plain literal
    if(expr->type == ast_type::symbol)
    {
        return get_symbol_sig(itl,(SymbolNode*)expr);
    }

    // is an expression
    assert(false);
}


TypeResult type_check_function_call(Interloper& itl, FuncCallNode* func_call)
{
    // Check for intrinsic.
    // if(func_call->expr->type == ast_type::symbol)
    // {
    //     SymbolNode* sym_node = (SymbolNode*)func_call->expr;
    //     if(!sym_node->name_space)
    //     {
    //         const auto& name = sym_node->name;

    //         // check this is not an intrinsic function
    //         s32 idx = lookup_internal_hashtable(INTRIN_TABLE,INTRIN_TABLE_SIZE,name);

    //         if(idx != INVALID_HASH_SLOT)
    //         {
    //             assert(false);
    //             // const auto handler = INTRIN_TABLE[idx].v;
    //             // return handler(itl,func,node,dst_slot);
    //         }
    //     }
    // }

    // Check function exists.
    // get the signature of what we are actually calling
    // NOTE: this might be plain function, or it could be a function pointer
    const auto call_info_res = get_calling_sig(itl,func_call->expr);
    if(!call_info_res)
    {
        return call_info_res.error();
    }

    const auto call_info = *call_info_res;

    if(call_info.sig.va_args)
    {
        assert(false);
    }

    // ^ this is a bad approximation for the args but roll with it for now.
    const u32 actual_args = count(call_info.sig.args);

    // check we have the right number of params
    if(actual_args != count(func_call->args))
    {
        return compile_error(itl,itl_error::missing_args,"[COMPILE]: function call expected %d args got %d",actual_args,count(func_call->args));
    }  

    // Type check args against the sig.
    for(u32 a = 0; a < actual_args; a++)
    {
        auto rtype_res = type_check_expr(itl,func_call->args[a]);
        if(!rtype_res)
        {
            return rtype_res.error();
        }

        auto& sym = sym_from_slot(itl.symbol_table,call_info.sig.args[a]);
        const auto pass_err = check_assign_arg(itl,sym.type,*rtype_res);
        if(pass_err)
        {
            return *pass_err;
        }
    }

    // Type check tuple return (if any)
    // TODO: Skip for now.
    assert(count(call_info.sig.return_type) == 1);
    return call_info.sig.return_type[0];
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

        case ast_type::function_call:
        {
            FuncCallNode* func_call = (FuncCallNode*)expr;
            return assign_expr_type(expr,type_check_function_call(itl,func_call));
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
                const auto decl_err = type_check_decl(itl, (DeclNode*)stmt);
                if(decl_err)
                {
                    return decl_err;
                }

                break;
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

    return option::none;
}
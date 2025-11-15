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

        const auto func_call = *func_call_res;

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


Option<itl_error> check_unbound_return(Interloper& itl, const FuncCall& call_info)
{
    if(call_info.sig.attr_flags & ATTR_USE_RESULT)
    {
        return compile_error(itl,itl_error::unused_result,"Result of function %S declared with attr use_result must be used",call_info.name);
    }

    else if(is_enum(call_info.sig.return_type[0]))
    {
        const auto& enumeration = enum_from_type(itl.enum_table,call_info.sig.return_type[0]);

        if(enumeration.use_result)
        {
            return compile_error(itl,itl_error::unused_result,"Enum %S declared with attr use_result must be used from func %S",
                enumeration.name,call_info.name);
        }
    }
    
    return option::none;
}


TypeResult type_check_function_call(Interloper& itl, FuncCallNode* func_call, bool result_bound)
{
    // Check for intrinsic.
    if(func_call->expr->type == ast_type::symbol)
    {
        SymbolNode* sym_node = (SymbolNode*)func_call->expr;
        if(!sym_node->name_space)
        {
            const auto& name = sym_node->name;

            // check this is not an intrinsic function
            s32 idx = lookup_internal_hashtable(INTRIN_TABLE,INTRIN_TABLE_SIZE,name);

            if(idx != INVALID_HASH_SLOT)
            {
                func_call->type = func_call_type::intrinsic;
                func_call->intrinsic_idx = idx;

                const auto handler = INTRIN_TABLE[idx].v;
                return assign_expr_type(&func_call->node,handler.type_check(itl,func_call));
            }
        }
    }

    // Check function exists.
    // get the signature of what we are actually calling
    // NOTE: this might be plain function, or it could be a function pointer
    const auto call_info_res = get_calling_sig(itl,func_call->expr);
    if(!call_info_res)
    {
        return call_info_res.error();
    }

    func_call->call = *call_info_res;
    const auto& call_info = func_call->call;

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

    if(!result_bound)
    {
        const auto err = check_unbound_return(itl,call_info);
        if(err)
        {
            return *err;
        }
    }

    // Type check tuple return (if any)
    // TODO: Skip for now.
    assert(count(call_info.sig.return_type) == 1);
    return call_info.sig.return_type[0];
}


Option<itl_error> type_check_return(Interloper& itl, Function& func, RetNode* ret_node)
{
    const u32 return_count = count(ret_node->expr);

    if(return_count != count(func.sig.return_type))
    {
        return compile_error(itl,itl_error::mismatched_args,"Invalid number of return parameters for function %S : %d != %d",
            func.name,return_count,count(func.sig.return_type));
    }

    for(u32 r = 0; r < return_count; r++)
    {
        AstNode* expr = ret_node->expr[r];

        auto expr_res = type_check_expr(itl,expr);

        if(!expr_res)
        {
            return expr_res.error();
        }

        const auto assign_err = check_assign_init(itl,func.sig.return_type[r],*expr_res);

        if(assign_err)
        {
            return assign_err;
        }
    }

    return option::none;
}
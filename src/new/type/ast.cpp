Result<Function*,itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space);
TypeResult type_check_expr(Interloper& itl, AstNode* expr);
void reserve_global_alloc(Interloper& itl, Symbol& sym);
TypeResult assign_expr_type(AstNode* node, TypeResult result);

#include "func/checker.cpp"
#include "arith/checker.cpp"


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

    const auto main_res = check_startup_func(itl,"main",itl.global_namespace);
    if(!main_res)
    {
        return main_res.error();
    }

    const auto start_res = check_startup_func(itl,"start",itl.global_namespace);
    if(!start_res)
    {
        return start_res.error();
    }

    // const auto memcpy_res = check_startup_func(itl,"memcpy",itl.std_name_space);
    // if(!memcpy_res)
    // {
    //     return memcpy_res.error();
    // }

    // itl.memcpy = *memcpy_res;
    
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

    Type* ltype = *type_res;

    decl->node.expr_type = ltype;

    const auto sym_ptr = get_sym(itl.symbol_table,decl->name);

    if(sym_ptr)
    {
        return compile_error(itl,itl_error::redeclaration,"redeclared symbol: %S: %t",decl->name,sym_ptr->type);
    }

    // add new symbol table entry, and cache it in inside the decl
    Symbol &sym = add_symbol(itl,decl->name,ltype);
    decl->sym_slot = sym.reg.slot.sym_slot;

    if(!decl->expr && is_reference(ltype))
    {
        return compile_error(itl,itl_error::pointer_type_error,"References must have an explicit initializer: %s",type_name(itl,ltype).buf);
    }

    // Reserve global data
    reserve_global_alloc(itl,sym);

    return option::none;
}


TypeResult assign_expr_type(AstNode* node, TypeResult result)
{
    if(!result)
    {
        return result;
    }

    return node->expr_type = *result;
}



TypeResult type_check_sym(Interloper& itl, SymbolNode* sym_node)
{
    auto sym_res = symbol(itl,sym_node);
    if(!sym_res)
    {
        return sym_res.error();
    }

    TypedReg reg = *sym_res;
    const auto slot = reg.slot.sym_slot;
    sym_node->sym_slot = slot;

    // Cache known value;
    auto& sym = sym_from_slot(itl.symbol_table,slot);
    sym_node->node.known_value = sym.known_value;

    return sym_node->node.expr_type = sym.type;
}

TypeResult type_check_expr(Interloper& itl, AstNode* expr)
{
    itl.ctx.expr = expr;

    switch(expr->type)
    {
        case ast_type::value:
        {
            ValueNode* value_node = (ValueNode*)expr;
            expr->known_value = value_node->value;

            return expr->expr_type = make_builtin(itl,value_node->type);
        }

        case ast_type::symbol:
        {
            SymbolNode* sym_node = (SymbolNode*)expr;
            return type_check_sym(itl,sym_node);
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
            return assign_expr_type(expr,type_check_function_call(itl,func_call,true));
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

Option<itl_error> type_check_block(Interloper& itl,Function& func, AstBlock& block)
{
    for(AstNode* stmt : block.statement)
    {
        itl.ctx.expr = stmt;

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

            case ast_type::ret:
            {
                const auto ret_err = type_check_return(itl,func,(RetNode*)stmt);
                if(ret_err)
                {
                    return ret_err;
                }

                break;
            }

            case ast_type::function_call:
            {
                FuncCallNode* func_call = (FuncCallNode*)stmt;
                const auto func_res = assign_expr_type(stmt,type_check_function_call(itl,func_call,false));
                if(!func_res)
                {
                    return func_res.error();
                }

                break;
            }

            default:
            {
                return compile_error(itl,itl_error::invalid_expr,"Type checker(stmt) unknown node %s",AST_NAMES[u32(stmt->type)]);
            }
        }
    }

    return option::none;
}


Option<itl_error> type_check_ast(Interloper& itl)
{
    auto start = std::chrono::high_resolution_clock::now();

    const auto const_err = compile_constants(itl);
    if(const_err)
    {
        return const_err;
    }

    // Forcibly check startup funcs
    // This will trigger type checking of further functions
    const auto startup_err = check_startup_defs(itl);
    if(startup_err)
    {
        return startup_err;
    }

    // Type check globals

    auto end = std::chrono::high_resolution_clock::now();
    itl.type_checking_time = std::chrono::duration<double, std::milli>(end-start).count();

    return option::none;
}
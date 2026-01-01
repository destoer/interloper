Result<Function*,itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space);
TypeResult type_check_expr(Interloper& itl, AstNode* expr);
void reserve_global_alloc(Interloper& itl, Symbol& sym);
TypeResult assign_expr_type(AstNode* node, TypeResult result);
SymbolScopeGuard enter_new_anon_scope(SymbolTable& sym_table);

Option<itl_error> type_check_array_initializer(Interloper& itl, InitializerListNode* init_list, ArrayType* type);
Option<itl_error> type_check_struct_initializer_list(Interloper& itl, InitializerListNode* init_list, Struct& structure);
Option<itl_error> type_check_struct_designated_initializer_list(Interloper& itl, Type* ltype, DesignatedListNode* init_list);

Option<itl_error> cache_rtti_structs(Interloper& itl);

#include "func/checker.cpp"
#include "arith/checker.cpp"


Option<itl_error> check_startup_defs(Interloper& itl)
{   
    if(itl.rtti_enable)
    {
        const auto cache_err = cache_rtti_structs(itl);
        if(cache_err)
        {
            return cache_err;
        }
    }

    itl.std_name_space = find_name_space(itl,"std");

    if(!itl.std_name_space)
    {
        return compile_error(itl,itl_error::undeclared,"std namespace is not declared");
    }

    const auto memcpy_res = check_startup_func(itl,"memcpy",itl.std_name_space);
    if(!memcpy_res)
    {
        return memcpy_res.error();
    }

    itl.memcpy = *memcpy_res;

    const auto zero_res = check_startup_func(itl,"zero_mem",itl.std_name_space);
    if(!zero_res)
    {
        return zero_res.error();
    }

    itl.zero_mem = *zero_res;

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


    return option::none;
}

Option<itl_error> type_check_intializer_list(Interloper& itl,Type* ltype, InitializerListNode* init_list)
{
    switch(ltype->kind)
    {
        case type_class::array_t:
        {
            return type_check_array_initializer(itl,init_list,(ArrayType*)ltype);
        }

        case type_class::struct_t:
        {
            auto& structure = struct_from_type(itl.struct_table,(StructType*)ltype);
            return type_check_struct_initializer_list(itl,init_list,structure);
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Initializer lists are only valid on arrays and structs");
        }
    }
}

TypeResult type_check_type_operator(Interloper& itl, AstNode* expr)
{
    TypeOperatorNode* type_oper = (TypeOperatorNode*)expr;
    const auto type_res = get_type(itl,type_oper->type);
    if(!type_res)
    {
        return type_res;
    }

    Type* type = *type_res;


    switch(type_oper->oper)
    {
        case type_operator::sizeof_type_t:
        {
            type_oper->node.known_value = type_size(itl,type);
            break;
        }

        case type_operator::sizeof_data_t:
        {
            type_oper->node.known_value = data_size(itl,type);
            break;
        }
    }

    return itl.usize_type;
}

Option<itl_error> type_check_decl(Interloper &itl, DeclNode* decl, bool global)
{
    auto type_res = get_type(itl,decl->type);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* ltype = *type_res;
    decl->node.expr_type = ltype;

    // Have to add this before checking the init expr or it may fail for globals
    auto sym_res = global? add_global(itl,decl->sym.name,ltype,false) : add_symbol(itl,decl->sym.name,ltype);
    if(!sym_res)
    {
        return sym_res.error();
    }

    decl->sym.slot = *sym_res;

    if(decl->expr)
    {
        const auto expr_err = type_check_init_expr(itl,ltype,decl->expr);
        if(expr_err)
        {
            return expr_err;
        }
    }

    // Now any initializers have been parsed handle any array auto sizing.
    if(is_fixed_array(ltype))
    {
        init_arr_sub_sizes(itl,ltype);
        
        ArrayType* array_type = (ArrayType*)ltype;


        // this has not been initialized by traverse_arr_initializer
        if(array_type->size == DEDUCE_SIZE)
        {
            return compile_error(itl,itl_error::missing_initializer,"auto sized array does not have an initializer");
        }
    }

    if(!decl->expr && is_reference(ltype))
    {
        return compile_error(itl,itl_error::pointer_type_error,"References must have an explicit initializer: %t",ltype);
    }

    return option::none;
}

Option<itl_error> type_check_decl_stmt(Interloper &itl,Function& func, AstNode* node)
{
    UNUSED(func);

    return type_check_decl(itl,(DeclNode*)node,false);
}

Option<itl_error> type_check_auto_decl(Interloper &itl,Function& func, AstNode* stmt)
{
    UNUSED(func);

    AutoDeclNode* decl = (AutoDeclNode*)stmt;

    const auto decl_res = type_check_expr(itl,decl->expr);
    if(!decl_res)
    {
        return decl_res.error();
    }

    const auto rtype = *decl_res;
    decl->node.expr_type = rtype;


    const auto sym_res = add_symbol(itl,decl->sym.name,rtype);
    if(!sym_res)
    {
        return sym_res.error();
    }

    decl->sym.slot = *sym_res;

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



TypeResult type_check_sym(Interloper& itl, AstNode* expr)
{
    SymbolNode* sym_node = (SymbolNode*)expr;

    // We make a copy of this, for oper_eq so it can trip twice on the same node.
    if(sym_node->node.expr_type)
    {
        return sym_node->node.expr_type;
    }

    const auto sym_ptr = get_sym_internal(itl.symbol_table,sym_node->name,sym_node->name_space);
    if(!sym_ptr)
    {
        // Attempt to get a function pointer.
        FunctionDef* func = lookup_func_def(itl,sym_node->name_space,sym_node->name);
        if(!func)
        {
            return compile_error(itl,itl_error::undeclared,"Symbol '%n%S' used before declaration",sym_node->name_space,sym_node->name);
        }


        const auto func_res = finalise_func(itl,*func);
        if(!func_res)
        {
            return func_res.error();
        }

        sym_node->func = *func_res;
        sym_node->type = sym_node_type::func_ptr;


        FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,type_class::func_pointer_t,true);
        type->sig = sym_node->func->sig;

        return (Type*)type;
    }


    const auto &sym = *sym_ptr;

    sym_node->sym_slot = sym.reg.slot.sym_slot;
    sym_node->type = sym_node_type::sym_slot;
    sym_node->node.known_value = sym.known_value;

    return sym.type;
}

Option<itl_error> type_check_assign(Interloper& itl,Function& func, AstNode* stmt) 
{
    UNUSED(func);

    AssignNode* assign = (AssignNode*)stmt;

    if(assign->left->type == ast_type::ignore)
    {
        return type_check_expr(itl,assign->right).remap_to_err();
    }

    const auto left_res = type_check_expr(itl,assign->left);
    if(!left_res) 
    {
        return left_res.error();
    }

    const auto right_res = type_check_expr(itl,assign->right);
    if(!right_res)
    {
        return right_res.error();
    }
    
    const auto left = *left_res;
    const auto right = *right_res;


    const auto assign_err = check_assign(itl,left,right);
    if(assign_err)
    {
        return assign_err;
    }

    return option::none;
}

TypeResult type_check_expr_unk(Interloper& itl, AstNode* expr)
{
    compile_panic(itl,itl_error::invalid_expr,"Type checker(expr) unknown node %s",AST_INFO[u32(expr->type)].name);
}

TypeResult type_check_value(Interloper& itl, AstNode* expr)
{
    ValueNode* value_node = (ValueNode*)expr;
    expr->known_value = value_node->value;

    return make_builtin(itl,value_node->type);
}

TypeResult type_check_float(Interloper& itl, AstNode* expr)
{
    UNUSED(expr);
    return make_builtin(itl,builtin_type::f64_t);
}


TypeResult type_check_expr(Interloper& itl, AstNode* expr)
{
    itl.ctx.expr = expr;
    const auto& ast_info = AST_INFO[u32(expr->type)];
    // NOTE: This is an expensive check to run everywhere.
    // But tagging the ast is too error prone without it.
    return assign_expr_type(expr,ast_info.type_check_expr(itl,expr));
}


Option<itl_error> type_check_stmt_unk(Interloper& itl, Function& func, AstNode* stmt)
{
    UNUSED(func);
    compile_panic(itl,itl_error::invalid_expr,"Type checker(stmt) unknown node %s",AST_INFO[u32(stmt->type)].name);
}

Option<itl_error> type_check_block(Interloper& itl,Function& func, AstBlock& block)
{
    // We now have a new scope.
    auto scope_guard = enter_new_anon_scope(itl.symbol_table);

    for(AstNode* stmt : block.statement)
    {
        itl.ctx.expr = stmt;
        const auto& ast_info = AST_INFO[u32(stmt->type)];
        const auto stmt_err = ast_info.type_check_stmt(itl,func,stmt);

        if(stmt_err)
        {
            return stmt_err;
        }

        if(!stmt->expr_type)
        {
            stmt->expr_type = itl.void_type;
        }
    }

    return option::none;
}

Option<itl_error> type_check_block_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    BlockNode* nested_block = (BlockNode*)stmt;

    return type_check_block(itl,func,nested_block->block);
}


Function* create_dummy_func(Interloper& itl, const String& name);


Option<itl_error> type_check_globals(Interloper& itl)
{
    // create a dummy void func called init_global
    // that we can compile all our global inits into!
    // this needs to be created here even though we don't emit into it yet.
    // otherwise the type checker wont be able to find it
    itl.global_func = create_dummy_func(itl,"init_global");

    for(GlobalDeclNode* decl_node : itl.global_decl)
    {
        auto context_guard = switch_context(itl,decl_node->filename,decl_node->name_space,(AstNode*)decl_node);
        const auto decl_err = type_check_decl(itl,decl_node->decl,true);
        if(decl_err)
        {
            return decl_err;
        }
    }

    finalise_global_offset(itl);
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

    // Type check globals
    const auto global_err = type_check_globals(itl);
    if(global_err)
    {
        return global_err;
    }

    // Forcibly check startup funcs
    // This will trigger type checking of further functions
    const auto startup_err = check_startup_defs(itl);
    if(startup_err)
    {
        return startup_err;
    }

    // Check all declared symbols are used.
    for(auto& sym : itl.symbol_table.slot_lookup)
    {
        if(sym.references == 0 && sym.reg.segment == reg_segment::local && sym.name[0] != '_')
        {
            trash_context(itl,sym.ctx);
            return compile_error(itl,itl_error::unused_symbol,"Symbol %s is never used",sym.name.buf);
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    itl.type_checking_time = std::chrono::duration<double, std::milli>(end-start).count();

    return option::none;
}

Option<itl_error> type_check_init_expr(Interloper& itl, Type* ltype, AstNode* expr)
{
    switch(expr->type)
    {
        // Don't care
        case ast_type::no_init:
        {
            return option::none;
        }

        case ast_type::initializer_list:
        {
            return type_check_intializer_list(itl,ltype,(InitializerListNode*)expr);
        }

        case ast_type::designated_initializer_list:
        {
            return type_check_struct_designated_initializer_list(itl,ltype,(DesignatedListNode*)expr);
        }

        default:
        {
            auto expr_res = type_check_expr(itl,expr);

            if(!expr_res)
            {
                return expr_res.error();
            }

            return check_assign_init(itl,ltype,*expr_res);
        }
    }
}
Option<itl_error> type_check_for_range_idx(Interloper& itl,Function& func, ForRangeNode* range,range_cmp_op cmp_op, bool is_inc)
{
    range->cmp_op = cmp_op;
    if(is_inc)
    {
        range->flags |= RANGE_FOR_INC;
    }

    const auto cmp_res = type_check_comparison(itl,range->cond);
    if(!cmp_res)
    {
        return cmp_res.error();
    }

    CmpNode* cmp = (CmpNode*)range->cond;

    // TODO: The sizing of thee integer on this needs to look at both conds and fit it to range.
    // make index the same sign as the end stmt
    const auto sym_res = add_symbol(itl,range->sym_one.name,is_inc? itl.usize_type : itl.ssize_type);
    if(!sym_res)
    {
        return sym_res.error();
    }

    range->sym_one.slot = *sym_res;
    range->sym_two.slot = {INVALID_HANDLE};

    if(!is_integer(cmp->left->expr_type) || !is_integer(cmp->right->expr_type))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected integer's in range condition got %t %t",
            cmp->left->expr_type,cmp->right->expr_type);
    }    

    return type_check_block(itl,func,range->block);
}


Option<itl_error> type_check_for_range_arr(Interloper& itl, Function& func, ForRangeNode* range)
{
    // This should be an array
    const auto arr_res = type_check_expr(itl,range->cond);
    if(!arr_res)
    {
        return arr_res.error();
    }

    const auto arr_type = *arr_res;
    if(!is_array(arr_type))
    {
        return compile_error(itl,itl_error::array_type_error,"Expected array for range stmt got %t",arr_type);
    }

    Type* contained_type = index_arr(arr_type);

    const bool pointer = (range->flags & RANGE_FOR_TAKE_POINTER) == RANGE_FOR_TAKE_POINTER;

    const auto var_res = add_symbol(itl,range->sym_one.name,pointer? make_reference(itl,contained_type) : contained_type);
    if(!var_res)
    {
        return var_res.error();
    }

    range->sym_one.slot = *var_res;

    // Add the index variable if it is there.
    if(range->flags & RANGE_FOR_ARRAY_IDX)
    {
        const auto idx_res = add_symbol(itl,range->sym_two.name,itl.const_usize_type);
        if(!idx_res)
        {
            return idx_res.error();
        }

        range->sym_two.slot = *idx_res;
    }

    else
    {
        range->sym_two.slot = {INVALID_HANDLE};
    }

    return type_check_block(itl,func,range->block);
}

Option<itl_error> type_check_for_range(Interloper& itl, Function& func, AstNode* stmt)
{
    ForRangeNode* range = (ForRangeNode*)stmt;

    const auto guard = enter_new_anon_scope(itl.symbol_table);

    if(range->flags & RANGE_FOR_ARRAY)
    {
        return type_check_for_range_arr(itl,func,range); 
    }
    
    CmpNode* cmp = (CmpNode*)range->cond;

    // determine what kind of loop term we have
    switch(cmp->oper)
    {
        // <= or < is inc
        case comparison_op::lt:
        {
            return type_check_for_range_idx(itl,func,range,range_cmp_op::lt,true);
        }

        case comparison_op::le:
        {
            return type_check_for_range_idx(itl,func,range,range_cmp_op::le,true);
        }

        // >= or > is dec
        case comparison_op::gt:
        {
            return type_check_for_range_idx(itl,func,range,range_cmp_op::gt,false);
        }

        case comparison_op::ge:
        {
            return type_check_for_range_idx(itl,func,range,range_cmp_op::ge,false);
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Only <, <=, >, >= are defined for range for");
        }
    }
}

Option<itl_error> type_check_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    stmt->expr_type = itl.void_type;
    const auto& ast_info = AST_INFO[u32(stmt->type)];
    return ast_info.type_check_stmt(itl,func,stmt);
}

Option<itl_error> type_check_for_iter(Interloper& itl, Function& func, AstNode* stmt) {
    ForIterNode* iter = (ForIterNode*)stmt;

    // scope for any var decls in the stmt
    auto sym_scope_guard = enter_new_anon_scope(itl.symbol_table);

    // compile the first stmt (usually an assign)
    const auto pre_err = type_check_stmt(itl,func,iter->initializer);
    if(pre_err)
    {
        return pre_err;
    }

    const auto cond_res = type_check_expr(itl,iter->cond);
    if(!cond_res)
    {
        return cond_res.error();
    }

    const auto cond = *cond_res;

    if(!is_bool(cond))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected bool got %t in for condition",cond);
    }    


    const auto post_err = type_check_stmt(itl,func,iter->post);
    if(post_err)
    {
        return post_err;
    }

    return type_check_block(itl,func,iter->block);
}


Option<itl_error> type_check_if_stmt(Interloper& itl, Function& func, IfStmt* stmt)
{
    const auto cond_res = type_check_expr(itl,stmt->expr);
    if(!cond_res)
    {
        return cond_res.error();
    }

    const auto ltype = *cond_res;

    if(is_array(ltype))
    {
        stmt->type = if_stmt_type::array_t;
    }

    else if(is_pointer(ltype) || is_integer(ltype))
    {
        stmt->type = if_stmt_type::not_zero_t;
    }

    else if(is_bool(ltype))
    {
        stmt->type = if_stmt_type::bool_t;
    }

    else
    {
        return compile_error(itl,itl_error::bool_type_error,"expected bool got %t in if condition",ltype);
    }

    return type_check_block(itl,func,*stmt->block);
}

Option<itl_error> type_check_if(Interloper& itl, Function& func, AstNode* stmt)
{
    IfNode* if_node = (IfNode*)stmt;

    // Check if stmt
    const auto if_err = type_check_if_stmt(itl,func,&if_node->if_stmt);
    if(if_err)
    {
        return if_err;
    }
    
    // Check all else if
    for(auto& else_if_stmt : if_node->else_if_stmt)
    {
        const auto else_if_err = type_check_if_stmt(itl,func,&else_if_stmt);
        if(else_if_err)
        {
            return else_if_err;
        }
    }

    // Check else block if any
    if(!if_node->else_clause)
    {
        return option::none;
    }

    return type_check_block(itl,func,if_node->else_stmt);
}


Option<itl_error> type_check_while(Interloper& itl, Function& func, AstNode* stmt)
{
    WhileNode* while_node = (WhileNode*)stmt;

    const auto expr_res = type_check_expr(itl,while_node->expr);
    if(!expr_res) 
    {
        return expr_res.error();
    }

    const auto expr = *expr_res;

    if(is_integer(expr) || is_pointer(expr))
    {
        while_node->cond_type = while_cond_type::not_zero_t;
    }

    else if(is_bool(expr))
    {
        while_node->cond_type = while_cond_type::bool_t;
    }

    else
    {
        return compile_error(itl,itl_error::bool_type_error,"expected integer, pointer or bool got %t in for condition",expr);
    }


    return type_check_block(itl,func,while_node->block);
}
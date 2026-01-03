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

Option<itl_error> type_check_switch_enum(Interloper& itl, Function& func, EnumType* target_type, Case& stmt)
{
    Type* type = nullptr;

    // Override in a switch to get the actual case info.
    if(stmt.statement->type == ast_type::enum_member)
    {
        const auto stmt_res = type_check_enum_member(itl,(EnumMemberNode*)stmt.statement,true);
        if(!stmt_res)
        {
            return stmt_res.error();
        }

        type = *stmt_res;  
    }

    else
    {
        const auto stmt_res = type_check_expr(itl,stmt.statement);
        if(!stmt_res)
        {
            return stmt_res.error();
        }

        type = *stmt_res;
    }

    if(!is_enum(type))
    {
        return compile_error(itl,itl_error::int_type_error,"Expected enum case for switch statement");
    }

    EnumType* case_type = (EnumType*)type;

    if(case_type->enum_idx != target_type->enum_idx)
    {
        return compile_error(itl,itl_error::enum_type_error,"differing enums %t : %t in switch statement",(Type*)target_type,type);
    }

    const auto block_err = type_check_block(itl,func,*stmt.block);
    if(block_err)
    {
        return block_err;
    }

    stmt.value = *stmt.statement->known_value;
    return option::none;    
}

Option<itl_error> type_check_switch_int(Interloper& itl, Function& func, Case& stmt)
{
    const auto stmt_res = type_check_expr(itl,stmt.statement);
    if(!stmt_res)
    {
        return stmt_res.error();
    }

    const auto type = *stmt_res;
    if(!is_integer(type))
    {
        return compile_error(itl,itl_error::int_type_error,"Expected integer case for switch statemnt");
    }

    if(!stmt.statement->known_value)
    {
        return compile_error(itl,itl_error::int_type_error,"Switch case must be a comple time integer");
    }

    const auto block_err = type_check_block(itl,func,*stmt.block);
    if(block_err)
    {
        return block_err;
    }

    stmt.value = *stmt.statement->known_value;
    return option::none;  
}

Option<itl_error> type_check_switch(Interloper& itl, Function& func, AstNode* stmt)
{
    SwitchNode* switch_node = (SwitchNode*)stmt;
    const u32 size = count(switch_node->statements);

    enum class switch_kind
    {
        integer,
        enum_t,
    };

    switch_kind switch_type = switch_kind::integer;
    Enum enumeration;

    if(size == 0)
    {
        return compile_error(itl,itl_error::missing_args,"Switch statement has no cases");
    }
    
    const auto expr_res = type_check_expr(itl,switch_node->expr);
    if(!expr_res)
    {
        return expr_res.error();
    }

    const auto target_type = *expr_res;

    if(is_enum(target_type))
    {
        enumeration = enum_from_type(itl.enum_table,(EnumType*)target_type);
        switch_type = switch_kind::enum_t;
    }

    else if(is_integer(target_type))
    {
        switch_type = switch_kind::integer;
    }

    else
    {
        return compile_error(itl,itl_error::int_type_error,"Expected enum or int for switch stmt");
    }


    switch(switch_type)
    {
        case switch_kind::integer:
        {
            for(auto& stmt : switch_node->statements)
            {
                const auto err = type_check_switch_int(itl,func,stmt);
                if(err)
                {
                    return err;
                }
            }

            break;
        }

        case switch_kind::enum_t:
        {
            for(auto& stmt : switch_node->statements)
            {
                const auto err = type_check_switch_enum(itl,func,(EnumType*)target_type,stmt);
                if(err)
                {
                    return err;
                }
            }
            break;
        }
    }

    if(switch_node->default_statement)
    {
        auto default_stmt = *switch_node->default_statement;

        const auto block_err = type_check_block(itl,func,*default_stmt.block);
        if(block_err)
        {
            return block_err;
        }
    }

    // sort the statements, so we can pull out the gaps
    // and binary search them if we need to when emiting the statement dispatcher
    heap_sort(switch_node->statements,[](const Case& v1, const Case& v2)
    {
        return v1.value > v2.value;
    });

    // check every statement on a enum has a handler
    if(switch_type == switch_kind::enum_t && !switch_node->default_statement && size != enumeration.member_map.size)
    {
        // TODO: print the missing cases
        return compile_error(itl,itl_error::missing_case,"switch on enum %S missing cases:",enumeration.name);    
    }


    // gap check all the statements and figure out 
    // if they are close enough to encode as a binary table
    // or if a binary search should be employed instead
    for(u32 i = 0; i < size - 1; i++)
    {
        const u64 cur_gap = switch_node->statements[i + 1].value - switch_node->statements[i].value;

        // these statements have no gap, this means they are duplicated
        if(cur_gap == 0)
        {
            return compile_error(itl,itl_error::redeclaration,"duplicate case %d",switch_node->statements[i].value);
        }

        switch_node->gap += cur_gap;
    }


    // get the number of extra gaps
    switch_node->gap -= size - 1;

    return option::none;
}
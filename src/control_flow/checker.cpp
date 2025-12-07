

// TypeResult add_named_symbol(Interloper& itl, NamedSymbol* named_sym, Type* type)
// {
//     const auto sym_ptr = get_sym(itl.symbol_table,named_sym->name);
//     if(!sym_ptr)
//     {
//         return compile_error(itl,itl_error::redeclared,"symbol '%S' is already declared",named_sym->name);
//     }

//     const auto& sym = add_symbol(itl,named_sym,type);

//     named_sym->slot = sym.reg.slot.sym_slot;

//     return sym.type;
// }



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


    if(!is_integer(cmp->left->expr_type) || !is_integer(cmp->right->expr_type))
    {
        return compile_error(itl,itl_error::bool_type_error,"expected integer's in range condition got %t %t",
            cmp->left->expr_type,cmp->right->expr_type);
    }    

    return type_check_block(itl,func,range->block);
}

Option<itl_error> type_check_for_range(Interloper& itl, Function& func, AstNode* stmt)
{
    ForRangeNode* range = (ForRangeNode*)stmt;

    const auto guard = enter_new_anon_scope(itl.symbol_table);

    if(range->cond->type == ast_type::comparison)
    {
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

    else
    {
        range->flags |= RANGE_FOR_ARRAY;
        unimplemented("Array for range");
    }
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

    if(is_bool(ltype))
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
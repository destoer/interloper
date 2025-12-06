

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

    // make index the same sign as the end stmt
    const auto sym_res = add_symbol(itl,range->name_one,is_inc? itl.usize_type : itl.ssize_type);
    if(!sym_res)
    {
        return sym_res.error();
    }

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
        unimplemented("Array for range");
    }
}
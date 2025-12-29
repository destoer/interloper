ConstValueResult type_check_const_int_expression(Interloper& itl, AstNode* node)
{   
    const auto type_res = type_check_expr(itl,node);
    if(!type_res)
    {
        return type_res.error();
    }

    // not valid if this is not an int
    if(!is_integer(node->expr_type))
    {
        return compile_error(itl,itl_error::int_type_error,"expected integer for const int expr got %t",node->expr_type); 
    }

    if(!node->known_value)
    {
        return compile_error(itl,itl_error::int_type_error,"Value for const integer expression is not known");
    }

    return ConstValue{node->expr_type,*node->known_value};
}



Option<itl_error> compile_const_list_internal(Interloper& itl,InitializerListNode* init_list, Type* type, PoolSlot slot, u32 offset)
{
    UNUSED(init_list); UNUSED(slot); UNUSED(offset);

    switch(type->kind)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Initializer list only valid for arrays and structs not %t",type);
        }
    }
}

// Option<itl_error> compile_const_struct_list_internal(Interloper& itl,InitializerListNode* init_list, const Struct& structure, PoolSlot slot, u32 offset)
// {
//     const u32 node_len = count(init_list->nodes);
//     const u32 member_size = count(structure.members);

//     if(node_len != member_size)
//     {
//         return compile_error(itl,itl_error::undeclared,"struct initializer missing initializer expected %d got %d",member_size,node_len);
//     }

//     for(u32 i = 0; i < count(structure.members); i++)
//     {
//         const auto member = structure.members[i];
//         AstNode* node = init_list->list[i];
    
//         // either sub struct OR array member initializer
//         switch(list->nodes[i]->type)
//         {
//             case ast_type::initializer_list:
//             {
//                 const auto err = compile_const_list_internal(itl,(InitializerListNode*)node,member.type,slot,offset);
//                 if(err)
//                 {
//                     return err;
//                 }
//                 break;
//             }


//             // we have a list of plain values we can actually initialize
//             default:
//             {
//                 // get the operand and type check it
//                 auto data_res = compile_const_expression(itl,list->nodes[i]);
//                 if(!data_res)
//                 {
//                     return data_res.error();
//                 }

//                 auto data = *data_res;

//                 // Need to upcast this
//                 data.type = member.type;

//                 const auto write_err = write_const_data(itl,slot,member.offset + offset,data);
//                 if(write_err)
//                 {
//                     return write_err;
//                 }
//             }
//         }
//     }

//     return option::none;     
// }

ConstDataResult compile_const_struct_initializer_list(Interloper& itl, Symbol& structure, InitializerListNode* init_list)
{
    // allocate struct
    const u32 data_size = type_size(itl,structure.type);
    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    // assign offset
    structure.reg.offset = data_slot.handle;

    // const auto& struct_info = struct_from_type(itl.struct_table,(StructType*)structure.type);

    // do init
    // const auto err = compile_const_struct_list_internal(itl,init_list,struct_info,data_slot,0);
    // if(err)
    // {
    //     return *err;
    // }

    UNUSED(init_list);

    return make_const_compound(make_const_pool_pointer(data_slot,0),structure.type);
}

ConstDataResult compile_const_struct_expr(Interloper& itl, Symbol& symbol, AstNode* expr)
{
    switch(expr->type)
    {
        case ast_type::initializer_list:
        {   
            return compile_const_struct_initializer_list(itl,symbol,(InitializerListNode*)expr);
        }

        default: 
        {
            return compile_error(itl,itl_error::invalid_expr,"Invalid expr for const struct init: %s",AST_INFO[u32(expr->type)].name);
        }
    }
}

ConstDataResult compile_const_builtin_expr(Interloper& itl, AstNode* expr)
{
    const auto res = type_check_expr(itl,expr);
    if(!res)
    {
        return res.error();
    }

    const auto type = *res;

    if(!expr->known_value)
    {
        return compile_error(itl,itl_error::const_type_error,"Builtin const expression of %t is not statically known",type);
    }
    
    return make_const_builtin(*expr->known_value,type);
}

ConstDataResult compile_const_expr(Interloper& itl, Symbol& symbol, AstNode* expr)
{
    UNUSED(expr);

    switch(symbol.type->kind)
    {
        case type_class::struct_t:
        {
           return compile_const_struct_expr(itl,symbol,expr);
        }

        case type_class::builtin_t:
        {
            return compile_const_builtin_expr(itl,expr);
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Type %t is not allowed in a const expression",symbol.type);
        }
    }
}

Option<itl_error> compile_constant_initializer(Interloper& itl,Symbol& symbol, AstNode* expr)
{
    const auto res = compile_const_expr(itl,symbol,expr);

    if(!res)
    {
        return res.error();
    }

    symbol.known_value = expr->known_value;

    const auto data = *res;
    return check_assign_init(itl,symbol.type,data.type);
}

Option<itl_error> compile_constant_decl(Interloper& itl, DeclNode* decl_node, b32 global)
{
    // pull type and name so we can create a symbol
    const auto name = decl_node->sym.name;

    // force constant
    decl_node->type->is_constant = true;

    // build the typing info
    auto type_res = get_type(itl,decl_node->type);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;

    // add into table
    const auto sym_res = global? add_global(itl,name,type,true) : add_symbol(itl,name,type);
    if(!sym_res)
    {
        return sym_res.error();
    }

    auto& sym = sym_from_slot(itl.symbol_table,*sym_res);

    // make sure this is marked as constant
    // incase it is declared locally
    sym.reg.segment = reg_segment::constant;

    // compile the expression
    return compile_constant_initializer(itl,sym,decl_node->expr);    
}

Option<itl_error> compile_constant(Interloper& itl, GlobalDeclNode* node)
{
    auto context_guard = switch_context(itl,node->filename,node->name_space,(AstNode*)node);
    return compile_constant_decl(itl,node->decl,true);
}


Option<itl_error> compile_constants(Interloper& itl)
{
    for(GlobalDeclNode* decl : itl.constant_decl)
    {
        const auto decl_err = compile_constant(itl,decl);
        if(decl_err)
        {
            return decl_err;
        }
    }

    return option::none;
}
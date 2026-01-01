ConstDataResult compile_const_expr(Interloper& itl, Type* type, AstNode* expr);
Option<itl_error> compile_const_list_internal(Interloper& itl,InitializerListNode* init_list, Type* type, PoolSlot slot, u32* offset);

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

Result<u32,itl_error> const_write_in_string(Interloper& itl, StringNode* string_node, PoolSlot slot, u32 offset)
{
    const String string = string_node->string;

    // add string into const pool
    const auto data_slot = push_const_pool_string(itl.const_pool,string);

    // make sure we pull the section after pushing data
    auto& section = pool_section_from_slot(itl.const_pool,slot);

    // write in vla data
    write_const_pool_vla(itl.const_pool,section,offset,data_slot,string.size);
    offset += VLA_SIZE;
    
    return offset;    
}



Option<itl_error> compile_const_struct_list_internal(Interloper& itl,InitializerListNode* init_list, const Struct& structure, PoolSlot slot, u32* offset)
{
    const u32 member_size = count(structure.members);

    for(u32 i = 0; i < member_size; i++)
    {
        const auto member = structure.members[i];
        AstNode* node = init_list->list[i];
    
        switch(node->type)
        {
            case ast_type::initializer_list:
            {
                const auto err = compile_const_list_internal(itl,(InitializerListNode*)node,member.type,slot,offset);
                if(err)
                {
                    return err;
                }
                break;
            }

            case ast_type::string:
            {
                const auto write_res = const_write_in_string(itl,(StringNode*)node,slot,*offset + member.offset);
                if(!write_res)
                {
                    return write_res.error();
                }
                break;
            }


            // we have a list of plain values we can actually initialize
            default:
            {
                // get the operand and type check it
                auto data_res = compile_const_expr(itl,member.type,node);
                if(!data_res)
                {
                    return data_res.error();
                }

                auto data = *data_res;

                // Need to upcast this
                data.type = member.type;

                const auto write_err = write_const_data(itl,slot,member.offset + *offset,data);
                if(write_err)
                {
                    return write_err;
                }
                break;
            }
        }
    }

    *offset += structure.size;

    return option::none;     
}



Option<itl_error> compile_const_array_list_internal(Interloper& itl,InitializerListNode* init_list, ArrayType* type, PoolSlot slot, u32* offset)
{
    if(is_runtime_size(type))
    {
        return compile_error(itl,itl_error::array_type_error,"Cannot use initializer list with const vla");
    }

    const u32 size = type_size(itl,type->contained_type);

    for(AstNode* node: init_list->list)
    {
        switch(node->type)
        {
            case ast_type::initializer_list:
            {
                const auto err = compile_const_list_internal(itl,(InitializerListNode*)node,type->contained_type,slot,offset);
                if(err)
                {
                    return err;
                }
                break;
            }
            
            default:
            {
                auto data_res = compile_const_expr(itl,type->contained_type,node);
                if(!data_res)
                {
                    return data_res.error();
                }

                auto data = *data_res;
                data.type = type->contained_type;

                const auto write_err = write_const_data(itl,slot,*offset,data);
                if(write_err)
                {
                    return *write_err;
                }
                *offset = *offset + size;
                break;
            }
        }
    }

    return option::none;
}

Option<itl_error> compile_const_list_internal(Interloper& itl,InitializerListNode* init_list, Type* type, PoolSlot slot, u32* offset)
{
    switch(type->kind)
    {
        case type_class::struct_t:
        {
            auto& structure = struct_from_type(itl.struct_table,(StructType*)type);
            return compile_const_struct_list_internal(itl,init_list,structure,slot,offset);
        }

        case type_class::array_t:
        {
            return compile_const_array_list_internal(itl,init_list,(ArrayType*)type,slot,offset);
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Initializer list only valid for arrays and structs not %t",type);
        }
    }
}

ConstDataResult compile_const_array_initializer_list(Interloper& itl, ArrayType* type, InitializerListNode* init_list)
{
    if(is_runtime_size(type))
    {
        return compile_error(itl,itl_error::array_type_error,"Cannot use initializer list with vla");
    }

    // preallocate the arr data
    const u32 data_size = type_memory_size(itl,(Type*)type);

    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    const auto pointer_slot = push_const_pool_fixed_array(itl.const_pool,data_slot);

    // actually insert the data into the const pool for the list
    u32 offset = 0;
    const auto err = compile_const_array_list_internal(itl,init_list,type,data_slot,&offset);
    if(err)
    {
        return *err;
    }

    return make_const_compound(make_const_pool_pointer(pointer_slot,0),0);
}

ConstDataResult compile_const_struct_initializer_list(Interloper& itl, StructType* type, InitializerListNode* init_list)
{
    // allocate struct
    const u32 data_size = type_size(itl,(Type*)type);
    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    const auto& struct_info = struct_from_type(itl.struct_table,type);

    u32 offset = 0;
    const auto err = compile_const_struct_list_internal(itl,init_list,struct_info,data_slot,&offset);
    if(err)
    {
        return *err;
    }

    return make_const_compound(make_const_pool_pointer(data_slot,0),(Type*)type);
}

ConstDataResult compile_const_struct_expr(Interloper& itl, StructType* type, AstNode* expr)
{
    switch(expr->type)
    {
        case ast_type::initializer_list:
        {   
            return compile_const_struct_initializer_list(itl,type,(InitializerListNode*)expr);
        }

        default: 
        {
            return compile_error(itl,itl_error::invalid_expr,"Invalid expr for const struct init: %s",AST_INFO[u32(expr->type)].name);
        }
    }
}

ConstDataResult compile_const_array_expr(Interloper& itl, ArrayType* type, AstNode* expr)
{
    switch(expr->type)
    {
        case ast_type::initializer_list:
        {   
            return compile_const_array_initializer_list(itl,type,(InitializerListNode*)expr);
        }

        default: 
        {
            return compile_error(itl,itl_error::invalid_expr,"Invalid expr for const array init: %s",AST_INFO[u32(expr->type)].name);
        }
    }
}


ConstDataResult compile_const_builtin_expr(Interloper& itl, AstNode* expr)
{
    if(!expr->known_value)
    {
        return compile_error(itl,itl_error::const_type_error,"Builtin const expression(%s) of %t is not statically known",
            AST_INFO[u32(expr->type)].name,expr->expr_type);
    }
    
    return make_const_builtin(*expr->known_value,expr->expr_type);
}

ConstDataResult compile_const_expr(Interloper& itl, Type* type, AstNode* expr)
{
    switch(type->kind)
    {
        case type_class::struct_t:
        {
           return compile_const_struct_expr(itl,(StructType*)type,expr);
        }

        case type_class::array_t:
        {
            return compile_const_array_expr(itl,(ArrayType*)type,expr);
        }

        case type_class::builtin_t:
        {
            return compile_const_builtin_expr(itl,expr);
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Type %t is not allowed in a const expression",type);
        }
    }
}

Option<itl_error> compile_constant_initializer(Interloper& itl,Symbol& symbol, AstNode* expr)
{
    // Type check the expr so we don't have to repeat it in here.
    const auto err = type_check_init_expr(itl,symbol.type,expr);
    if(err)
    {
        return err;
    }

    const auto data_res = compile_const_expr(itl,symbol.type,expr);

    if(!data_res)
    {
        return data_res.error();
    }

    const auto data = *data_res;

    switch(symbol.type->kind)
    {
        case type_class::builtin_t:
        {
            symbol.known_value = expr->known_value;
            break;
        }

        case type_class::struct_t:
        {
            // assign offset
            symbol.reg.offset = data.data_pointer.slot.handle;
            break;
        }

        case type_class::array_t:
        {
            symbol.reg.offset = data.data_pointer.slot.handle;
            break;            
        }

        default:
        {
            return compile_error(itl,itl_error::const_type_error,"Cannot handle const data for type %t",symbol.type);
        }
    }

    return option::none;
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
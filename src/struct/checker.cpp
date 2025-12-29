
Option<itl_error> access_member(Interloper& itl, Type* ltype, AccessMember& member_access, const String& name)
{
    const auto member_opt = get_member(itl.struct_table,ltype,name);

    if(!member_opt)
    {
        return compile_error(itl,itl_error::undeclared,"No such member %S for type %t",name,ltype);
    }

    const auto member = member_opt.value();
    member_access.member = member.index;
    member_access.expr_type = member.type;

    return option::none;
}

TypeResult type_check_access_struct_member(Interloper& itl, Type* ltype, AccessMember& member_access)
{
    switch(ltype->kind)
    {
        case type_class::struct_t:
        {
            member_access.type = member_access_type::struct_t;

            const auto access_err = access_member(itl,ltype,member_access,member_access.name);
            if(access_err)
            {
                return *access_err;
            }
            
            return member_access.expr_type;
        }

        case type_class::array_t:
        {
            member_access.type = member_access_type::array_t;

            if(member_access.name == "len")
            {
                member_access.member = u32(array_member_access::len);
                return itl.usize_type;
            }

            else if(member_access.name == "data")
            {
                ArrayType* array_type = (ArrayType*)ltype;

                if(is_fixed_array(ltype))
                {
                    return compile_error(itl,itl_error::array_type_error,"no .data member on fixed size array");
                }

                member_access.member = u32(array_member_access::data);

                // This is never considered nullable. Arrays should be checked by size
                return make_reference(itl,array_type->contained_type);
            }

            return compile_error(itl,itl_error::undeclared,"unknown array member %S",member_access.name);
        }

        default:
        {
            return compile_error(itl,itl_error::struct_error,"%t is not a struct, enum or array",ltype);
        }
    }
}

TypeResult type_check_struct_initializer(Interloper& itl, StructInitializerNode* init)
{
    // Get structure
    auto struct_res = lookup_struct(itl,init->name_space,init->struct_name);
    if(!struct_res)
    {
        return struct_res.error();
    }

    auto struct_type = *struct_res;
    auto& structure = struct_from_type(itl.struct_table,struct_type);

    switch(init->initializer->type)
    {
        case ast_type::initializer_list:
        {
            const auto init_err = type_check_struct_initializer_list(itl,(InitializerListNode*)init->initializer,structure);
            if(init_err)
            {
                return *init_err;
            }
            break;
        }

        default:
        {
            compile_panic(itl,itl_error::invalid_expr,"%s is not a valid expr for struct init",AST_INFO[u32(init->initializer->type)].name);
            break;
        }
    }

    // This may be used in a stmt needs an explicit assign
    return init->node.expr_type = (Type*)struct_type;
}

TypeResult type_check_struct_initializer_expr(Interloper& itl, AstNode* expr)
{
    return type_check_struct_initializer(itl,(StructInitializerNode*)expr);
}


Option<itl_error> type_check_struct_initializer_stmt(Interloper& itl,Function& func, AstNode* stmt)
{
    StructInitializerNode* struct_init = (StructInitializerNode*)stmt;

    if(!struct_init->is_return)
    {
        return compile_error(itl,itl_error::invalid_statement,"Struct initializer stmt must be for a return");
    }

    if(count(func.sig.args) != 1)
    {
        return compile_error(itl,itl_error::invalid_statement,"Struct initializer stmt must be for a single return");
    }

    auto res = type_check_struct_initializer(itl,struct_init);
    if(!res)
    {
        return res.error();
    }

    // Type check against return
    return check_assign_init(itl,func.sig.return_type[0],*res);
}




TypeResult type_check_access_index_member(Interloper& itl, Type* ltype, AccessMember& member_access)
{
    member_access.type = member_access_type::index_t;

    IndexNode* index = (IndexNode*)member_access.expr;

    const auto access_err = access_member(itl,ltype,member_access,index->name);
    if(access_err)
    {
        return *access_err;
    }
    
    const auto index_res = type_check_index_internal(itl,index,member_access.expr_type);
    if(!index_res)
    {
        return index_res;
    }

    member_access.expr_type = *index_res;
    index->node.expr_type = member_access.expr_type;

    return member_access.expr_type;
}

TypeResult type_check_struct_access(Interloper& itl, AstNode* expr)
{
    StructAccessNode* struct_access = (StructAccessNode*)expr;

    const auto expr_res = type_check_expr(itl,struct_access->expr);
    if(!expr_res)
    {
        return expr_res;
    }

    // currently accessed type
    Type* ltype = *expr_res;

    for(AccessMember& member_access : struct_access->members)
    {
        // Automatically dereference any reference
        if(is_pointer(ltype))
        {
            if(!is_reference(ltype))
            {
                return compile_error(itl,itl_error::pointer_type_error,"Cannot dereference a nullable pointer for struct access %t",ltype);
            }

            ltype = deref_pointer(ltype);
        }


        // Simple case just grab the name
        if(member_access.type < member_access_type::slice_t)
        {
            // Check we have this member and then update the type
            const auto access_res = type_check_access_struct_member(itl,ltype,member_access);
            if(!access_res)
            {
                return access_res;
            }

            ltype = *access_res;
            continue;
        }

        switch(member_access.expr->type)
        {
            case ast_type::index:
            {
                // Check we have this member and then update the type
                const auto access_res = type_check_access_index_member(itl,ltype,member_access);
                if(!access_res)
                {
                    return access_res;
                }

                ltype = *access_res;
                break;                
            }

            default:
            {
                return compile_error(itl,itl_error::struct_error,"Unknown struct access %s",AST_INFO[u32(member_access.expr->type)].name);
            }
        }
    }

    return ltype;
}

Option<itl_error> type_check_struct_initializer_list(Interloper& itl, InitializerListNode* init_list, Struct& structure)
{
    const u32 node_len = count(init_list->list);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        return compile_error(itl,itl_error::undeclared,"Struct initializer missing initializer expected %d got %d",member_size,node_len);
    }

    for(u32 i = 0; i < node_len; i++)
    {
        AstNode* node = init_list->list[i];
        auto& member = structure.members[i];

        switch(node->type)
        {
            case ast_type::initializer_list:
            {
                const auto sub_init_err = type_check_intializer_list(itl,member.type,(InitializerListNode*)node);
                if(sub_init_err)
                {
                    return sub_init_err;
                }
                break;
            }

            case ast_type::designated_initializer_list:
            {
                unimplemented("Type check designated initializer list");
            }

            default: 
            {
                const auto err = type_check_init_expr(itl,member.type,node);
                if(err)
                {
                    return err;
                }

                break;
            }
        }
    }

    return option::none;
}
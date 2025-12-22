TypeResult type_check_access_struct_member(Interloper& itl, Type* ltype, AccessMember& member_access)
{
    switch(ltype->kind)
    {
        case type_class::struct_t:
        {
            // Does the member we are accessing exist?
            const auto member_opt = get_member(itl.struct_table,ltype,member_access.name);

            if(!member_opt)
            {
                return compile_error(itl,itl_error::undeclared,"No such member %S for type %t",
                    member_access.name,ltype);
            }

            const auto member = member_opt.value();
            member_access.member = member.index;

            member_access.type = member_access_type::struct_t;
            return member.type;
        }

        default:
        {
            return compile_error(itl,itl_error::struct_error,"%t is not a struct, enum or array",ltype);
        }
    }
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

    for(AccessMember& member : struct_access->members)
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
        if(member.type < member_access_type::slice_t)
        {
            // Check we have this member and then update the type
            const auto access_res = type_check_access_struct_member(itl,ltype,member);
            if(!access_res)
            {
                return access_res;
            }

            ltype = *access_res;
            continue;
        }

        switch(member.expr->type)
        {
            default:
            {
                return compile_error(itl,itl_error::struct_error,"Unknown struct access %s",AST_INFO[u32(member.expr->type)].name);
            }
        }
    }

    return ltype;
}
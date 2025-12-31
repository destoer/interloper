

TypeResult type_check_enum_member(Interloper& itl, AstNode* expr)
{
    EnumMemberNode* member_node = (EnumMemberNode*)expr;
    
    auto enum_type_res = lookup_enum(itl,member_node->name_space,member_node->name);
    if(!enum_type_res)
    {
        return enum_type_res.error();
    }

    EnumType* enum_type = *enum_type_res;
    Enum enumeration = enum_from_type(itl.enum_table,enum_type);

    EnumMember* member =  lookup(enumeration.member_map,member_node->member);

    if(!member)
    {
        return compile_error(itl,itl_error::enum_type_error,"No such member %S in enum %S",member_node->member, enumeration.name);
    }

    member_node->node.known_value = member->value;

    return (Type*)enum_type;
}
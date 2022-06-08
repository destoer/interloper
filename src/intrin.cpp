

using INTRIN_FUNC = Type (*)(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);

bool is_gpr_size(const Type& type)
{
    return is_trivial_copy(type);
}

Type intrin_syscall(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    UNUSED(dst_slot);
    
    const u32 arg_size = node->nodes.size();

    if(arg_size != 3)
    {
        panic(itl,"expected 3 args for intrin_syscall got %d\n",arg_size);
    }

    emit(func.emitter,op_type::save_regs);


    const auto [v1_type,v1_reg] = compile_oper(itl,func,node->nodes[1],R0_IR);
    const auto [v2_type,v2_reg] = compile_oper(itl,func,node->nodes[2],R1_IR);

    if(!is_trivial_copy(v1_type))
    {
        panic(itl,"arg1 of type %s does not fit inside a gpr\n",type_name(itl,v1_type).c_str());
        return Type(builtin_type::void_t);  
    }

    if(!is_trivial_copy(v2_type))
    {
        panic(itl,"arg1 of type %s does not fit inside a gpr\n",type_name(itl,v1_type).c_str());
        return Type(builtin_type::void_t);  
    }

    const u32 syscall_number = eval_const_expr(node->nodes[0]);
    emit(func.emitter,op_type::swi,syscall_number);



    emit(func.emitter,op_type::restore_regs);
    return Type(builtin_type::void_t);   
}

std::map<std::string,INTRIN_FUNC> intrin_table  = 
{
    {"intrin_syscall",&intrin_syscall},
};
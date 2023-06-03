

using INTRIN_FUNC = Type* (*)(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);

b32 is_gpr_size(const Type* type)
{
    return is_trivial_copy(type);
}

Type* intrin_syscall(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    UNUSED(dst_slot);
    
    FuncCallNode* func_call = (FuncCallNode*)node;

    const u32 arg_size = count(func_call->args);

    if(arg_size != 3)
    {
        panic(itl,itl_error::mismatched_args,"expected 3 args for intrin_syscall got %d\n",arg_size);
    }

    emit(func,op_type::save_regs);


    const auto v1_type = compile_expression(itl,func,func_call->args[1],sym_from_idx(R0_IR));
    const auto v2_type = compile_expression(itl,func,func_call->args[2],sym_from_idx(R1_IR));

    if(!is_trivial_copy(v1_type))
    {
        panic(itl,itl_error::mismatched_args,"arg1 of type %s does not fit inside a gpr\n",type_name(itl,v1_type).buf);
        return make_builtin(itl,builtin_type::void_t);   
    }

    if(!is_trivial_copy(v2_type))
    {
        panic(itl,itl_error::mismatched_args,"arg1 of type %s does not fit inside a gpr\n",type_name(itl,v1_type).buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    const u32 syscall_number = eval_int_expr(func_call->args[0]);
    emit(func,op_type::swi,syscall_number);



    emit(func,op_type::restore_regs);
    return make_builtin(itl,builtin_type::void_t);   
}

static constexpr u32 INTRIN_TABLE_SIZE = 2;

static constexpr HashNode<String,INTRIN_FUNC> INTRIN_TABLE[INTRIN_TABLE_SIZE] = 
{
    {"",nullptr},
    {"intrin_syscall",&intrin_syscall},
};

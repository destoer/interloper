

using INTRIN_FUNC = Type* (*)(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);

b32 is_gpr_size(const Type* type)
{
    return is_trivial_copy(type);
}


void ir_memcpy(Interloper&itl, Function& func, SymSlot dst_slot, SymSlot src_slot, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    static constexpr u32 COPY_LIMIT = 32;

    // multiple of 4 and under the copy limit
    if(size < COPY_LIMIT && (size & 7) == 0) 
    {
        const auto tmp = new_tmp(func, 8);

        const u32 count = size / 8;

        for(u32 i = 0; i < count; i++)
        {
            load_double(itl,func,tmp,src_slot,i * 8);
            store_double(itl,func,tmp,dst_slot,i * 8);
        }    
    }

    else 
    {
        // emit a call to memcpy with args
        // check function is declared

        Function* func_def = lookup(itl.function_table,String("memcpy"));

        if(!func_def)
        {
            panic(itl,itl_error::undeclared,"[COMPILE]: memcpy is required for struct passing\n");
        }
        Function &func_call = *func_def;

        mark_used(itl,func_call);


        const SymSlot imm_slot = mov_imm_res(itl,func,size);

        push_arg(itl,func,imm_slot);
        push_arg(itl,func,src_slot);
        push_arg(itl,func,dst_slot);

        call(itl,func,func_call.label_slot,true);

        clean_args(itl,func,3);
    }
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

    const u32 REG_BITSET = 0b0000'0010;

    // save the regs this will clobber with its direct asm
    // NOTE: we dont save R0 because it is used for returns and callee saved
    save_regs(itl,func,REG_BITSET);


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

    const auto [syscall_number,type] = compile_const_int_expression(itl,func_call->args[0]);

    syscall(itl,func,syscall_number);

    if(dst_slot.handle != NO_SLOT)
    {
        // move result
        mov_reg(itl,func,dst_slot,sym_from_idx(R0_IR));
    }
    
    restore_regs(itl,func,REG_BITSET);

    return make_builtin(itl,builtin_type::s32_t);   
}

static constexpr u32 INTRIN_TABLE_SIZE = 2;

static constexpr HashNode<String,INTRIN_FUNC> INTRIN_TABLE[INTRIN_TABLE_SIZE] = 
{
    {"",nullptr},
    {"intrin_syscall",&intrin_syscall},
};

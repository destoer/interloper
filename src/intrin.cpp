

using INTRIN_FUNC = Type* (*)(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);

Function* find_complete_func(Interloper& itl, const String& name, const String& name_space)
{
    Function* func_def = lookup_opt_scoped_function(itl,name,name_space);

    if(!func_def)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: %s is required for struct passing\n",name.buf);
        return nullptr;
    }

    return  func_def;
}

void ir_memcpy(Interloper&itl, Function& func, AddrSlot dst_addr, AddrSlot src_addr, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    static constexpr u32 COPY_LIMIT = 64;

    // multiple of 8 and under the copy limit
    if(size < COPY_LIMIT && (size & 7) == 0) 
    {
        const auto tmp = new_tmp(func, 8);

        const u32 count = size / 8;

        for(u32 i = 0; i < count; i++)
        {
            load_addr_slot(itl,func,tmp,src_addr,8,false,false);
            store_addr_slot(itl,func,tmp,dst_addr,8,false);

            src_addr.offset += 8;
            dst_addr.offset += 8;
        }    
    }

    else 
    {
        // emit a call to memcpy with args
        // check function is declared
        Function* func_def = find_complete_func(itl,String("memcpy"),String("std"));

        if(!func_def)
        {
            return;
        }

        Function &func_call = *func_def;

        const SymSlot imm_slot = mov_imm_res(itl,func,size);

        collapse_struct_offset(itl,func,&src_addr);
        collapse_struct_offset(itl,func,&dst_addr);

        push_arg(itl,func,imm_slot);
        push_arg(itl,func,src_addr.slot);
        push_arg(itl,func,dst_addr.slot);

        call(itl,func,func_call.label_slot);

        clean_args(itl,func,3);
    }
}

void ir_zero(Interloper&itl, Function& func, SymSlot dst_ptr, u32 size)
{

    static constexpr u32 INLINE_LIMIT = 256;

    // multiple of 8 and under the copy limit
    if(size < INLINE_LIMIT && (size & 7) == 0) 
    {
        const auto zero = imm_zero(itl,func);

        const u32 count = size / 8;

        for(u32 i = 0; i < count; i++)
        {
            store_double(itl,func,zero,dst_ptr,i * 8);
        }    
    }

    // call into zero_mem
    else
    {
        Function* func_def = find_complete_func(itl,String("zero_mem"),String("std"));

        if(!func_def)
        {
            return;
        }

        Function &func_call = *func_def;

        const SymSlot imm_slot = mov_imm_res(itl,func,size);

        push_arg(itl,func,imm_slot);
        push_arg(itl,func,dst_ptr);

        call(itl,func,func_call.label_slot);

        clean_args(itl,func,2);        
    }
}

Type* intrin_syscall_x86(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    UNUSED(dst_slot);
    
    FuncCallNode* func_call = (FuncCallNode*)node;

    const u32 arg_size = count(func_call->args);

    if(arg_size < 1)
    {
        panic(itl,itl_error::mismatched_args,"expected 3 args for intrin_syscall got %d\n",arg_size);
        return make_builtin(itl,builtin_type::void_t);
    }

    // make sure this register doesn't get reused
    lock_reg(itl,func,sym_from_idx(RAX_IR));
    const auto [syscall_number,type] = compile_const_int_expression(itl,func_call->args[0]);
    mov_imm(itl,func,sym_from_idx(RAX_IR),syscall_number);
    
    const u32 REG_ARGS[6] = {RDI_IR,RSI_IR,RDX_IR,R10_IR,R8_IR,R9_IR};

    for(u32 arg = 1; arg <= 6; arg++)
    {
        if(arg_size >= arg + 1)
        {
            const auto reg = sym_from_idx(REG_ARGS[arg-1]);
            lock_reg(itl,func,reg);
            const auto type = compile_expression(itl,func,func_call->args[arg],reg);

            if(!is_trivial_copy(type))
            {
                panic(itl,itl_error::mismatched_args,"arg %d of type %s does not fit inside a gpr\n",arg,type_name(itl,type).buf);
                return make_builtin(itl,builtin_type::void_t);  
            }
        }
    }

    syscall(itl,func);

    if(dst_slot.handle != NO_SLOT)
    {
        // move result
        mov_reg(itl,func,dst_slot,sym_from_idx(RV_IR));
    }
    
    return make_builtin(itl,builtin_type::s64_t);   
}

Type* intrin_syscall(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            return intrin_syscall_x86(itl,func,node,dst_slot);
        }
    }

    assert(false);
}

static constexpr u32 INTRIN_TABLE_SIZE = 2;

static constexpr HashNode<String,INTRIN_FUNC> INTRIN_TABLE[INTRIN_TABLE_SIZE] = 
{
    {"",nullptr},
    {"intrin_syscall",&intrin_syscall},
};

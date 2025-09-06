

using INTRIN_FUNC = TypeResult (*)(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);

Result<Function*,itl_error> find_complete_func(Interloper& itl, NameSpace* name_space, const String& name)
{
    Function* func_def = lookup_opt_scoped_function(itl,name_space,name);

    if(!func_def)
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: %s is required for struct passing",name.buf);
    }

    return func_def;
}

Option<itl_error> ir_memcpy(Interloper&itl, Function& func, AddrSlot dst_addr, AddrSlot src_addr, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    static constexpr u32 COPY_LIMIT = 32;

    // multiple of 8 and under the copy limit (don't do this when stack only, as it generates awful code)
    if(size < COPY_LIMIT && (size & 7) == 0 && !itl.stack_alloc) 
    {
        const auto tmp = new_tmp(func, 8);

        const u32 count = size / 8;

        for(u32 i = 0; i < count; i++)
        {
            load_addr_slot(itl,func,tmp,src_addr,8,false,false);
            store_addr_slot(itl,func,tmp,dst_addr,8,false);

            src_addr.addr.offset += 8;
            dst_addr.addr.offset += 8;
        }    
    }

    else 
    {
        // emit a call to memcpy with args
        // check function is declared
        auto func_def_res = find_complete_func(itl,itl.std_name_space,"memcpy");

        if(!func_def_res)
        {
            return func_def_res.error();
        }

        Function &func_call = *func_def_res.value();

        ArgPass pass = make_arg_pass(func_call.sig);

        const RegSlot imm_slot = mov_imm_res(itl,func,size);

        const RegSlot src_ptr = collapse_struct_addr(itl,func,src_addr);
        const RegSlot dst_ptr = collapse_struct_addr(itl,func,dst_addr);

        const TypedReg imm = {imm_slot,make_builtin(itl,GPR_SIZE_TYPE)};
        const TypedReg src = {src_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
        const TypedReg dst = {dst_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
        pass_arg(itl,func,pass,imm,2);
        pass_arg(itl,func,pass,src,1);
        pass_arg(itl,func,pass,dst,0);

        const u32 arg_clean = pass_args(itl,func,pass);

        call(itl,func,func_call.label_slot);
        unlock_reg_set(itl,func,func_call.sig.locked_set);

        clean_args(itl,func,arg_clean);
    }

    return option::none;
}

Option<itl_error> ir_zero(Interloper&itl, Function& func, RegSlot dst_ptr, u32 size)
{

    static constexpr u32 INLINE_LIMIT = 256;

    // multiple of 8 and under the copy limit
    if(size < INLINE_LIMIT && (size & (GPR_SIZE - 1)) == 0 && !itl.stack_alloc) 
    {
        const auto zero = imm_zero(itl,func);

        const u32 count = size / GPR_SIZE;

        for(u32 i = 0; i < count; i++)
        {
            store_ptr(itl,func,zero,dst_ptr,i * GPR_SIZE,GPR_SIZE,false);
        }    
    }

    // call into zero_mem
    else
    {
        auto func_def_res = find_complete_func(itl,itl.std_name_space,"zero_mem");

        if(!func_def_res)
        {
            return func_def_res.error();
        }

        Function &func_call = *func_def_res.value();

        ArgPass pass = make_arg_pass(func_call.sig);

        const RegSlot imm_slot = mov_imm_res(itl,func,size);

        const TypedReg imm = {imm_slot,make_builtin(itl,GPR_SIZE_TYPE)};
        const TypedReg dst = {dst_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
        pass_arg(itl,func,pass,imm,1);
        pass_arg(itl,func,pass,dst,0);

        const u32 arg_clean = pass_args(itl,func,pass);

        call(itl,func,func_call.label_slot);
        unlock_reg_set(itl,func,func_call.sig.locked_set);

        clean_args(itl,func,arg_clean);        
    }

    return option::none;
}

TypeResult intrin_syscall_x86(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    FuncCallNode* func_call = (FuncCallNode*)node;

    const u32 arg_size = count(func_call->args);

    if(arg_size < 1)
    {
        return compile_error(itl,itl_error::mismatched_args,"expected 3 args for intrin_syscall got %d",arg_size);
    }

    // make sure this register doesn't get reused
    lock_reg(itl,func,make_spec_reg_slot(spec_reg::rax));
    auto syscall_num_res = compile_const_int_expression(itl,func_call->args[0]);
    if(!syscall_num_res)
    {
        return syscall_num_res.error();
    }

    const auto syscall_value = *syscall_num_res;

    mov_imm(itl,func,make_spec_reg_slot(spec_reg::rax),syscall_value.value);
    u32 unlock_set = set_bit(0,special_reg_to_reg(itl.arch,spec_reg::rax));

    
    const spec_reg REG_ARGS[6] = {spec_reg::rdi,spec_reg::rsi,spec_reg::rdx,spec_reg::r10,spec_reg::r8,spec_reg::r9};

    for(u32 arg = 1; arg <= 6; arg++)
    {
        if(arg_size >= arg + 1)
        {
            const spec_reg locked_reg = REG_ARGS[arg-1];
            const auto reg = make_spec_reg_slot(locked_reg);
            lock_reg(itl,func,reg);
            unlock_set = set_bit(unlock_set,special_reg_to_reg(itl.arch,locked_reg));

            const auto type_res = compile_expression(itl,func,func_call->args[arg],reg);
            if(!type_res)
            {
                return type_res;
            }

            const Type* type = *type_res;

            if(!is_trivial_copy(type))
            {
                return compile_error(itl,itl_error::mismatched_args,"arg %d of type %s does not fit inside a gpr",arg,type_name(itl,type).buf);
            }
        }
    }

    syscall(itl,func);

    if(!is_special_reg(dst_slot,spec_reg::null))
    {
        // move result
        mov_reg(itl,func,dst_slot,make_spec_reg_slot(spec_reg::rax));
    }
    
    unlock_reg_set(itl,func,unlock_set);

    return make_builtin(itl,builtin_type::s64_t);   
}

TypeResult intrin_syscall(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot)
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

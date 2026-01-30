void load_addr_slot(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot addr_slot, u32 size, b32 sign, b32 is_float);
void store_addr_slot(Interloper &itl,Function &func,RegSlot src_slot,AddrSlot addr_slot, u32 size,b32 is_float);
RegSlot collapse_struct_addr_oper(Interloper& itl, Function& func, const AddrSlot struct_slot);

Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot);
ConstValueResult type_check_const_int_expression(Interloper& itl, AstNode* node);

void store_ptr(Interloper &itl,Function& func,RegSlot src_slot,RegSlot ptr,u32 offset,u32 size, b32 is_float);
void compile_move(Interloper &itl, Function &func, const TypedReg& dst, const TypedReg& src);

// NOTE: This only handles gpr sized returns, and does not handle hidden args
void call_intrin_func(Interloper& itl, Function& func, const Function& func_call, const ConstSpan<TypedReg>& regs, RegSlot dst_slot)
{
    const auto user_args = sig_user_span(func_call.sig);

    ArgPass pass = make_arg_pass(func_call.sig);
    for(s32 arg_idx = regs.size - 1; arg_idx >= 0; arg_idx--)
    {
        const auto& reg = regs[arg_idx];
        auto& arg = sym_from_slot(itl.symbol_table,user_args[arg_idx]);

        switch(reg.type->kind)
        {
            case type_class::struct_t:
            {
                unimplemented("Pass struct");
                break;
            }

            case type_class::array_t:
            {
                if(is_runtime_size(arg.type))
                {
                    // push in reverse order let our internal functions handle vla conversion
                    const RegSlot len_slot = load_arr_len(itl,func,reg);
                    push_arg(itl,func,pass,len_slot);

                    const RegSlot data_slot = load_arr_data(itl,func,reg);
                    push_arg(itl,func,pass,data_slot);
                }

                // fixed sized array
                else
                {
                    pass_arg(itl,func,pass,reg,arg_idx);
                }                   
                break;
            }

            default:
            {
                pass_arg(itl,func,pass,regs[arg_idx],arg_idx);
                break;
            }
        }        
    }

    const u32 arg_clean = pass_args(itl,func,pass);

    call(itl,func,func_call.label_slot);
    clean_args(itl,func,arg_clean); 

    auto& sig = func_call.sig;
    const bool returns_value = count(sig.return_type) >= 1;

    // store the return value back into a reg (if its actually bound)
    if(returns_value && !is_special_reg(dst_slot,spec_reg::null))
    {
        const RegSlot rv = make_spec_reg_slot(return_reg_from_type(sig.return_type[0]));
        const TypedReg dst = {dst_slot,sig.return_type[0]};
        const TypedReg src = {rv,sig.return_type[0]};
        compile_move(itl,func,dst,src);
    }

    unlock_reg_set(itl,func,func_call.sig.locked_set);
}

void call_intrin_func_no_return(Interloper& itl, Function& func, const Function& func_call, const ConstSpan<TypedReg>& regs)
{
    call_intrin_func(itl,func,func_call,regs,make_spec_reg_slot(spec_reg::null));
}

void ir_mem_equal(Interloper& itl, Function& func, AddrSlot v1_addr, AddrSlot v2_addr, u32 size, RegSlot dst_slot)
{
    const RegSlot imm_slot = mov_imm_res(itl,func,size);

    const TypedReg v1 = {collapse_struct_addr_oper(itl,func,v1_addr),make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
    const TypedReg v2 = {collapse_struct_addr_oper(itl,func,v2_addr),make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
    const TypedReg imm = {imm_slot,itl.usize_type};

    static constexpr u32 REGS_SIZE = 3;
    const TypedReg regs[REGS_SIZE] = {v1,v2,imm};    

    call_intrin_func(itl,func,*itl.mem_equal,make_const_span(regs,0,REGS_SIZE),dst_slot);
}

void ir_array_equal(Interloper& itl, Function& func, TypedReg v1, TypedReg v2, RegSlot dst_slot)
{
    const auto size = type_size(itl,index_arr(v1.type));

    // Already bytes we can just pass this directly with no conversion
    if(size == sizeof(u8))
    {
        static constexpr u32 REGS_SIZE = 2;
        const TypedReg regs[REGS_SIZE] = {v1,v2};    
        call_intrin_func(itl,func,*itl.array_equal,make_const_span(regs,0,REGS_SIZE),dst_slot);
        return;
    }

    // Convert both arrays to byte vla
    const TypedReg v1_bytes = new_typed_tmp(itl,func,itl.const_byte_type);
    const TypedReg v2_bytes = new_typed_tmp(itl,func,itl.const_byte_type);

    recast_array(itl,func,(ArrayType*)itl.const_byte_type,v1,v1_bytes.slot);
    recast_array(itl,func,(ArrayType*)itl.const_byte_type,v2,v2_bytes.slot);

    static constexpr u32 REGS_SIZE = 2;
    const TypedReg regs[REGS_SIZE] = {v1_bytes,v2_bytes};    

    call_intrin_func(itl,func,*itl.array_equal,make_const_span(regs,0,REGS_SIZE),dst_slot);
}


void ir_memcpy(Interloper&itl, Function& func, AddrSlot dst_addr, AddrSlot src_addr, u32 size)
{
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
        
        return;
    }

    const RegSlot imm_slot = mov_imm_res(itl,func,size);

    const RegSlot src_ptr = collapse_struct_addr_oper(itl,func,src_addr);
    const RegSlot dst_ptr = collapse_struct_addr_oper(itl,func,dst_addr);

    const TypedReg imm = {imm_slot,itl.usize_type};
    const TypedReg src = {src_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
    const TypedReg dst = {dst_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
    static constexpr u32 REGS_SIZE = 3;
    const TypedReg regs[REGS_SIZE] = {dst,src,imm};    

    call_intrin_func_no_return(itl,func,*itl.memcpy,make_const_span(regs,0,REGS_SIZE));
}



void ir_zero(Interloper&itl, Function& func, RegSlot dst_ptr, u32 size)
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
        
        return;
    }


    const RegSlot imm_slot = mov_imm_res(itl,func,size);
    const TypedReg imm = {imm_slot,itl.usize_type};
    const TypedReg dst = {dst_ptr,make_reference(itl,make_builtin(itl,builtin_type::byte_t))};
    static constexpr u32 REGS_SIZE = 2;
    const TypedReg regs[REGS_SIZE] = {dst,imm};
    
    call_intrin_func_no_return(itl,func,*itl.zero_mem,make_const_span(regs,0,REGS_SIZE));
}


TypeResult type_check_syscall(Interloper &itl,FuncCallNode *func_call)
{
    assert(itl.arch == arch_target::x86_64_t);

    const u32 arg_size = count(func_call->args);

    if(arg_size < 1)
    {
        return compile_error(itl,itl_error::mismatched_args,"expected 3 args for intrin_syscall got %d",arg_size);
    }

    // TODO: Should probably just replace this with a value.
    auto syscall_num_res = type_check_const_int_expression(itl,func_call->args[0]);
    if(!syscall_num_res)
    {
        return syscall_num_res.error();
    }

    for(u32 arg = 1; arg < arg_size; arg++)
    {
        const auto type_res = type_check_expr(itl,func_call->args[arg]);
        if(!type_res)
        {
            return type_res;
        }

        const Type* type = *type_res;

        if(!is_trivial_copy(type))
        {
            return compile_error(itl,itl_error::mismatched_args,"Arg %d of type %t does not fit inside a gpr",arg,type);
        }
    }

   return make_builtin(itl,builtin_type::s64_t);    
}

void compile_syscall(Interloper &itl,Function &func,FuncCallNode *func_call, RegSlot dst_slot)
{
    const u32 arg_size = count(func_call->args);

    // make sure this register doesn't get reused
    lock_reg(itl,func,spec_reg::rax);
    const auto syscall_value = func_call->args[0]->known_value.gpr;

    mov_imm(itl,func,make_spec_reg_slot(spec_reg::rax),syscall_value);
    u32 unlock_set = set_bit(0,special_reg_to_reg(itl.arch,spec_reg::rax));

    
    const spec_reg REG_ARGS[6] = {spec_reg::rdi,spec_reg::rsi,spec_reg::rdx,spec_reg::r10,spec_reg::r8,spec_reg::r9};

    for(u32 arg = 1; arg <= 6; arg++)
    {
        if(arg_size >= arg + 1)
        {
            const spec_reg locked_reg = REG_ARGS[arg-1];
            lock_reg(itl,func,locked_reg);
            unlock_set = set_bit(unlock_set,special_reg_to_reg(itl.arch,locked_reg));

            compile_expression(itl,func,func_call->args[arg],make_spec_reg_slot(locked_reg));
        }
    }

    syscall(itl,func);

    if(!is_special_reg(dst_slot,spec_reg::null))
    {
        // move result
        mov_reg(itl,func,dst_slot,make_spec_reg_slot(spec_reg::rax));
    }
    
    unlock_reg_set(itl,func,unlock_set);    
}

const HashNode<String,IntrinHandler> INTRIN_TABLE[INTRIN_TABLE_SIZE] = 
{
    {"",{nullptr,nullptr}},
    {"intrin_syscall",{compile_syscall,type_check_syscall}},
};

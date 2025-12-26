
ArgPass make_arg_pass(const FuncSig& sig)
{
    ArgPass pass;

    resize(pass.args,sig.max_reg_pass);
    pass.pass_as_reg = sig.pass_as_reg;
    return pass;
}

void destroy_arg_pass(ArgPass& pass)
{
    destroy_arr(pass.args);
}

void pass_arg(Interloper& itl, Function& func, ArgPass& pass,const TypedReg& reg, u32 arg_idx)
{
    if(pass.pass_as_reg[arg_idx] == NON_ARG)
    {
        is_float(reg.type)? push_float_arg(itl,func,pass,reg.slot) : push_arg(itl,func,pass,reg.slot);
    }

    else
    {
        pass.args[pass.pass_as_reg[arg_idx]] = reg.slot;
    }
}

// WILL DESTROY THE ARG PASS
u32 pass_args(Interloper& itl, Function& func, ArgPass& pass)
{
    // Move the passed args into place
    for(u32 a = 0; a < count(pass.args); a++)
    {
        const RegSlot arg_slot = make_spec_reg_slot(spec_reg(SPECIAL_REG_ARG_START + a));
        mov_reg(itl,func,arg_slot,pass.args[a]);
    }

    const u32 arg_clean = pass.arg_clean;
    destroy_arg_pass(pass);

    return arg_clean;
}

void compile_return(Interloper &itl,Function &func, AstNode* stmt)
{
    RetNode* ret_node = (RetNode*)stmt;

    // No return value just issue a ret
    if(!ret_node->expr)
    {
        ret(itl,func);
        return;
    }

    const size_t args = count(ret_node->expr);

    if(args == 1)
    {
        const RegSlot rv = make_spec_reg_slot(return_reg_from_type(func.sig.return_type[0]));

        AstNode* expr = ret_node->expr[0];

        switch(rv.spec)
        {
            case spec_reg::rv_struct:
            {
                compile_expression(itl,func,expr,rv);
                break;
            }

            case spec_reg::rv_fpr:
            {
                // Compile this into a tmp and then move it out so its easy to lock.
                const auto tmp = new_float(func);
                compile_expression(itl,func,expr,tmp);
                mov_float(itl,func,rv,tmp);
                break;
            }

            case spec_reg::rv_gpr:
            {
                // Compile this into a tmp and then move it out so its easy to lock.
                const auto tmp = new_tmp(func,GPR_SIZE);
                compile_expression(itl,func,expr,tmp);
                mov_reg(itl,func,rv,tmp);
                break;
            }

            default: assert(false);
        }
    }

    // Multiple return store pointer to each
    else
    {
        for(u32 r = 0; r < count(func.sig.return_type); r++)
        {
            const auto src = compile_oper(itl,func,ret_node->expr[r]);

            const TypedReg ptr = {make_sym_reg_slot(func.sig.args[r]),func.sig.return_type[r]};
            do_ptr_store(itl,func,src.slot,ptr);
        }
    }

    ret(itl,func);
}

void push_array(Interloper& itl, Function& func, ArgPass& pass, Type* arg_type, AstNode* node, u32 arg_idx)
{
    // pass a static string, by inserting as const data in the program
    if(node->type == ast_type::string)
    {
        StringNode* string_node = (StringNode*)node;

        const u32 size = string_node->string.size;

        // push the len offset
        const RegSlot len_slot = mov_imm_res(itl,func,size);
        push_arg(itl,func,pass,len_slot);

        // push the data offset
        const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,string_node->string);

        const RegSlot addr_slot = pool_addr_res(itl,func,pool_slot,0);
        push_arg(itl,func,pass,addr_slot);
        return;
    }

    auto reg = compile_oper(itl,func,node);

    if(is_runtime_size(arg_type))
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
}

void push_arg(Interloper& itl, Function& func, ArgPass& pass, Type* arg_type, AstNode* node, u32 arg_idx)
{
    switch(arg_type->kind)
    {
        case type_class::struct_t:
        {
            unimplemented("Push struct");
            break;
        }

        case type_class::array_t:
        {
            push_array(itl,func,pass,arg_type,node,arg_idx);
            break;
        }

        default:
        {
            const auto reg = compile_oper(itl,func,node);
            pass_arg(itl,func,pass,reg,arg_idx);
            break;
        }
    }
}

void push_args(Interloper& itl, Function& func, ArgPass& pass, FuncCallNode* call_node,const FuncSig& sig, u32 start_arg)
{
    const s32 hidden_args = sig.hidden_args;

    // push args in reverse order and type check them
    for(s32 i = start_arg; i >= hidden_args; i--)
    {
        const u32 arg_idx = i - hidden_args;
        auto& sym = sym_from_slot(itl.symbol_table,sig.args[arg_idx]);

        push_arg(itl,func,pass,sym.type,call_node->args[arg_idx],i);
    }
}

// Returns number of arguments to clean
u32 pass_function_args(Interloper& itl, Function& func, FuncCallNode* call_node)
{
    auto& sig = call_node->call.sig;

    // handle argument pushing
    ArgPass pass = make_arg_pass(sig);
    
    const s32 hidden_args = sig.hidden_args;
    u32 start_arg = count(sig.args) - 1;
    // const u32 actual_args = count(sig.args) - hidden_args;

    if(sig.va_args)
    {
        unimplemented("va args");
    }

    push_args(itl,func,pass,call_node,sig,start_arg);


    if(hidden_args)
    {
        unimplemented("hidden args");
    }

    return pass_args(itl,func,pass);
}

void handle_call(Interloper& itl, Function& func, const FuncCall& call_info, RegSlot dst_slot, u32 arg_clean)
{
    auto& sig = call_info.sig;

    // NOTE: func struct will hold a void value if it has nothing
    const bool returns_value = !is_void(sig.return_type[0]);

    if(!call_info.func_pointer)
    {
        // emit call to label slot
        // the actual address will have to resolved as the last compile step
        // once we know the size of all the code
        call(itl,func,call_info.label_slot);
    }

    // func pointer
    else
    {
        call_reg(itl,func,call_info.reg_slot);
    }

    // clean up args after the function call
    clean_args(itl,func,arg_clean);
      
    // normal return
    // store the return value back into a reg (if its actually bound)
    if(returns_value && !is_special_reg(dst_slot,spec_reg::null) && !sig.hidden_args)
    {
        const RegSlot rv = make_spec_reg_slot(return_reg_from_type(sig.return_type[0]));
        const TypedReg dst = {dst_slot,sig.return_type[0]};
        const TypedReg src = {rv,sig.return_type[0]};
        compile_move(itl,func,dst,src);
    }

    unlock_reg_set(itl,func,sig.locked_set);
}

void compile_function_call_expr(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    FuncCallNode* call_node = (FuncCallNode*)expr;

    // Check if we are calling an intrinsic.
    if(call_node->type == func_call_type::intrinsic)
    {
        const auto handler = INTRIN_TABLE[call_node->intrinsic_idx].v;
        handler.emit(itl,func,call_node,dst_slot);
        return;
    }

    const u32 arg_clean = pass_function_args(itl,func,call_node);
    handle_call(itl,func,call_node->call,dst_slot,arg_clean);
}

void compile_function_call_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    compile_function_call_expr(itl,func,stmt,make_spec_reg_slot(spec_reg::null));
}

void setup_passing_convention(Interloper& itl, Function& func)
{
    lock_reg_set(itl,func,func.sig.locked_set);

    // Setup calling convention
    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];
        const u32 arg_reg = func.sig.pass_as_reg[a];

        if(arg_reg != NON_ARG)
        {
            const spec_reg arg = spec_reg(SPECIAL_REG_ARG_START + arg_reg);
            mov_unlock(itl,func,make_sym_reg_slot(slot),make_spec_reg_slot(arg));
        }
    }
}
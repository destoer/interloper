
#include "ir.h"
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

    // Multiple return store pointer to each
    if(args != 1)
    {
        for(u32 r = 0; r < count(func.sig.return_type); r++)
        {
            // Do nothing
            if(ret_node->expr[r]->type == ast_type::no_init)
            {
                continue;
            }

            const auto src = compile_oper(itl,func,ret_node->expr[r]);
            const TypedReg ptr = {make_sym_reg_slot(func.sig.args[r]),func.sig.return_type[r]};
            do_ptr_store(itl,func,src,ptr);
        }

        ret(itl,func);
        return;
    }


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

    ret(itl,func);
}

void push_array(Interloper& itl, Function& func, ArgPass& pass, ArrayType* arg_type, AstNode* node, u32 arg_idx)
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

void push_struct(Interloper& itl, Function& func, ArgPass& pass, StructType* arg_type, AstNode* node)
{
    const auto structure = struct_from_type(itl.struct_table,arg_type);

    const auto arg_reg = compile_oper(itl,func,node);
    const u32 aligned_size = align_val(structure.size,GPR_SIZE);

    // alloc the struct size for our copy
    alloc_stack(itl,func,aligned_size);

    // need to save SP as it will get pushed last
    const RegSlot dst_ptr = copy_reg(itl,func,make_spec_reg_slot(spec_reg::sp));
    const auto dst_addr = make_pointer_addr(dst_ptr,0);

    const auto src_addr = make_struct_addr(arg_reg.slot,0);

    ir_memcpy(itl,func,dst_addr,src_addr,structure.size);

    // clean up the stack push
    pass.arg_clean += aligned_size / GPR_SIZE;
}

u32 compile_any(Interloper& itl, Function& func, AstNode* arg_node);

void push_arg(Interloper& itl, Function& func, ArgPass& pass, Type* arg_type, AstNode* node, u32 arg_idx)
{
    switch(arg_type->kind)
    {
        case type_class::struct_t:
        {
            if(is_any(itl,arg_type))
            {
                const u32 size = compile_any(itl,func,node);
                pass.arg_clean += size / GPR_SIZE;
                break;
            }

            push_struct(itl,func,pass,(StructType*)arg_type,node);
            break;
        }

        case type_class::array_t:
        {
            push_array(itl,func,pass,(ArrayType*)arg_type,node,arg_idx);
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

void push_args(Interloper& itl, Function& func, ArgPass& pass, FuncCallNode* call_node,const FuncSig& sig)
{
    const auto user_args = sig_user_span(sig);

    // push args in reverse order
    for(s32 arg_idx = user_args.size - 1; arg_idx >= 0; arg_idx--)
    {
        auto& sym = sym_from_slot(itl.symbol_table,user_args[arg_idx]);
        push_arg(itl,func,pass,sym.type,call_node->args[arg_idx],arg_idx + sig.hidden_args);
    }
}


void compile_any_arr(Interloper& itl, Function& func, AstNode* arg_node, const Option<AddrSlot>& addr);

void push_va_args(Interloper& itl, Function& func, ArgPass& pass, FuncCallNode* call_node,const FuncSig& sig)
{
    const auto any_args = sig_any_span(sig,call_node->args);

    // alloc storage for array
    // this is easy because we know how many args we have
    auto& rtti_cache = itl.rtti_cache;

    const u32 any_arr_size = align_val(any_args.size * rtti_cache.any_struct_size,GPR_SIZE);

    alloc_stack(itl,func,any_arr_size);

    const auto any_arr_ptr = copy_reg(itl,func,make_spec_reg_slot(spec_reg::sp));
    auto addr_slot = make_pointer_addr(any_arr_ptr, 0);

    for(AstNode* node: any_args)
    {
        compile_any_arr(itl,func,node,addr_slot);
        addr_slot.addr.offset += rtti_cache.any_struct_size;
    }

    // alloc vla
    alloc_stack(itl,func,VLA_SIZE);

    // and store it
    const RegSlot any_len_slot = mov_imm_res(itl,func,any_args.size);

    const auto SP_SLOT = make_spec_reg_slot(spec_reg::sp);

    // store data
    store_ptr(itl,func,any_arr_ptr,SP_SLOT,0,GPR_SIZE,false);
    store_ptr(itl,func,any_len_slot,SP_SLOT,GPR_SIZE,GPR_SIZE,false);      

    const u32 total_size = any_arr_size + VLA_SIZE;
    pass.arg_clean += total_size / GPR_SIZE;
}

void push_struct_return(Interloper& itl, Function& func, ArgPass& pass,const FuncSig& sig, RegSlot dst_slot)
{
    Type* type = sig.return_type[0];

    switch(dst_slot.kind)
    {
        case reg_kind::sym:
        {
            const StructAddr struct_addr = {make_addr(dst_slot,0)};

            const RegSlot addr = addrof_res(itl,func,struct_addr);
            const TypedReg reg = {addr,make_reference(itl,type)};
            pass_arg(itl,func,pass,reg,0);
            break;
        }

        case reg_kind::tmp:
        {
            alloc_slot(itl,func,dst_slot,true);
            const StructAddr struct_addr = {make_addr(dst_slot,0)};

            const RegSlot addr = addrof_res(itl,func,struct_addr);
            const TypedReg reg = {addr,make_reference(itl,type)};
            pass_arg(itl,func,pass,reg,0);
            break;
        }

        case reg_kind::spec:
        {
            switch(dst_slot.spec)
            {
                case spec_reg::rv_struct: 
                {
                    const TypedReg reg = {make_sym_reg_slot(func.sig.args[0]),make_reference(itl,type)};
                    pass_arg(itl,func,pass,reg,0);
                    break;
                }

                default:
                {
                    crash_and_burn("spec reg unhandled: %s\n",spec_reg_name(dst_slot.spec)); 
                    break;  
                } 
            }
        }
    }    
}

TypedReg compile_addrof_res(Interloper& itl,Function &func,AstNode *expr);

void push_tuple_return(Interloper& itl, Function& func, ArgPass& pass, const FuncSig& sig, TupleAssignNode* tuple_assign)
{
    for(s32 a = count(tuple_assign->symbols) -1; a >= 0; a--)
    {
        AstNode* expr = tuple_assign->symbols[a].expr;
        switch(expr->type)
        {
            case ast_type::ignore:
            {
                Type* rtype = sig.return_type[a];

                const auto tmp = new_struct(func,type_size(itl,rtype));
                const StructAddr struct_addr = {make_addr(tmp,0)};

                const auto addr_slot = addrof_res(itl,func,struct_addr);
                const TypedReg reg = {addr_slot,make_reference(itl,rtype)};
                pass_arg(itl,func,pass,reg,a);
                break;
            }

            case ast_type::deref:
            {
                DerefNode* deref = (DerefNode*)expr;
                const auto reg = compile_oper(itl,func,deref->expr);
                pass_arg(itl,func,pass,reg,a);
                break;
            }

            // Fall back on taking an address
            default:
            {
                SymSlot new_decl = tuple_assign->symbols[a].new_decl;

                if(new_decl.handle != INVALID_HANDLE)
                {
                    alloc_slot(itl,func,make_sym_reg_slot(new_decl),is_plain_type(expr->expr_type));
                }

                const auto reg = compile_addrof_res(itl,func,expr);
                pass_arg(itl,func,pass,reg,a);
                break;
            }
        }
    }    
}

void push_hidden_args(Interloper& itl, Function& func, TupleAssignNode* tuple_assign, ArgPass& pass,const FuncSig& sig, RegSlot dst_slot)
{
    if(!sig.hidden_args)
    {
        return;
    }

    // TODO: This does not yet account for tuples
    if(sig.hidden_args == 1)
    {
        push_struct_return(itl,func,pass,sig,dst_slot);
        return;
    }

    else if(tuple_assign)
    {
        push_tuple_return(itl,func,pass,sig,tuple_assign);
    }
}

// Returns number of arguments to clean
u32 pass_function_args(Interloper& itl, Function& func, FuncCallNode* call_node,TupleAssignNode* tuple_assign, RegSlot dst_slot)
{
    auto& sig = call_node->call.sig;

    // handle argument pushing
    ArgPass pass = make_arg_pass(sig);
    
    if(sig.va_args)
    {
        push_va_args(itl,func,pass,call_node,sig);
    }

    push_args(itl,func,pass,call_node,sig);
    push_hidden_args(itl,func,tuple_assign,pass,sig,dst_slot);
    
    return pass_args(itl,func,pass);
}

void handle_call(Interloper& itl, Function& func, const FuncCall& call_info, RegSlot dst_slot, u32 arg_clean)
{
    auto& sig = call_info.sig;

    // NOTE: func struct will hold a void value if it has nothing
    const bool returns_value = !is_void(sig.return_type[0]);

    // func pointer
    if(call_info.flags & FUNC_CALL_FUNC_POINTER_FLAG)
    {
        call_reg(itl,func,call_info.reg_slot);
    }

    else
    {
        // emit call to label slot
        // the actual address will have to resolved as the last compile step
        // once we know the size of all the code
        call(itl,func,call_info.label_slot);
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

void compile_function_call(Interloper& itl, Function& func, AstNode* call_node_ast, RegSlot dst_slot)
{
    FuncCallNode* call_node = nullptr;
    TupleAssignNode* tuple_assign = nullptr;

    if(call_node_ast->type == ast_type::function_call)
    {
        call_node = (FuncCallNode*)call_node_ast;
    }

    else
    {
        tuple_assign = (TupleAssignNode*)call_node_ast;
        call_node = tuple_assign->func_call;
    }

    // Check if we are calling an intrinsic.
    if(call_node->type == func_call_type::intrinsic)
    {
        const auto handler = INTRIN_TABLE[call_node->intrinsic_idx].v;
        handler.emit(itl,func,call_node,dst_slot);
        return;
    }
    
    if(call_node->call.flags & FUNC_CALL_FUNC_POINTER_EXPR_FLAG)
    {
        const auto reg = compile_oper(itl,func,call_node->expr);
        call_node->call.reg_slot = reg.slot;
    }

    const u32 arg_clean = pass_function_args(itl,func,call_node,tuple_assign,dst_slot);
    handle_call(itl,func,call_node->call,dst_slot,arg_clean);    
}

void compile_function_call_expr(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    compile_function_call(itl,func,expr,dst_slot);
}

void compile_function_call_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    compile_function_call(itl,func,stmt,make_spec_reg_slot(spec_reg::null));
}

void compile_tuple_assign_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    compile_function_call(itl,func,stmt,make_spec_reg_slot(spec_reg::null));
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

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

TypeResult push_arg(Interloper& itl, Function& func, ArgPass& pass, Type* arg_type, AstNode* node, u32 arg_idx)
{
    if(is_any(itl,arg_type))
    {
        auto size_res = compile_any(itl,func,node);
        if(!size_res)
        {
            return size_res.error();
        }

        const u32 size = *size_res; 
        pass.arg_clean += size / GPR_SIZE;

        return arg_type;
    }

    else if(is_array(arg_type))
    {
        // pass a static string, by inserting as const data in the program
        if(node->type == ast_type::string)
        {
            LiteralNode* lit_node = (LiteralNode*)node;

            const u32 size = lit_node->literal.size;

            const auto rtype = make_array(itl,make_builtin(itl,builtin_type::c8_t,true),size);

            // push the len offset
            const RegSlot len_slot = mov_imm_res(itl,func,size);
            push_arg(itl,func,pass,len_slot);

            // push the data offset
            const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,lit_node->literal);

            const RegSlot addr_slot = pool_addr_res(itl,func,pool_slot,0);
            push_arg(itl,func,pass,addr_slot);

            return rtype;
        }

        else
        {
            auto res = compile_oper(itl,func,node);

            if(!res)
            {
                return res.error();
            }

            auto arg_reg = *res;

            if(is_runtime_size(arg_type))
            {
                // push in reverse order let our internal functions handle vla conversion
                const RegSlot len_slot = load_arr_len(itl,func,arg_reg);
                push_arg(itl,func,pass,len_slot);

                const RegSlot data_slot = load_arr_data(itl,func,arg_reg);
                push_arg(itl,func,pass,data_slot);
            }

            // fixed sized array
            else
            {
                pass_arg(itl,func,pass,arg_reg,arg_idx);
            }

            return arg_reg.type;
        }
    }

    else if(is_struct(arg_type))
    {
        const auto structure = struct_from_type(itl.struct_table,arg_type);

        const auto res = compile_oper(itl,func,node);
        if(!res)
        {
            return res.error();
        }

        auto arg_reg = *res;
        const u32 aligned_size = align_val(structure.size,GPR_SIZE);

        // alloc the struct size for our copy
        alloc_stack(itl,func,aligned_size);

        // need to save SP as it will get pushed last
        const RegSlot dst_ptr = copy_reg(itl,func,make_spec_reg_slot(spec_reg::sp));
        const auto dst_addr = make_pointer_addr(dst_ptr,0);

        const auto src_addr = make_struct_addr(arg_reg.slot,0);

        const auto memcpy_err = ir_memcpy(itl,func,dst_addr,src_addr,structure.size);
        if(memcpy_err)
        {
            return *memcpy_err;
        }

        // clean up the stack push
        pass.arg_clean += aligned_size / GPR_SIZE;

        return arg_reg.type;
    }

    // plain builtin in variable
    else
    {
        const auto res = compile_oper(itl,func,node);
        if(!res)
        {
            return res.error();
        }

        const auto reg = *res;

        pass_arg(itl,func,pass,reg,arg_idx);

        return reg.type;
    }
}

Option<itl_error> push_args(Interloper& itl, Function& func, ArgPass& pass, FuncCallNode* call_node,const FuncSig& sig, u32 start_arg)
{
    const s32 hidden_args = sig.hidden_args;

    // push args in reverse order and type check them
    for(s32 i = start_arg; i >= hidden_args; i--)
    {
        auto& sym = sym_from_slot(itl.symbol_table,sig.args[i]); 
        Type* arg_type =  sym.type;
  
        const u32 arg_idx = i - hidden_args;

        auto push_res = push_arg(itl,func,pass,arg_type,call_node->args[arg_idx],i);

        if(!push_res)
        {
            return push_res.error();
        }

        Type* passed_type = *push_res;

        // type check the arg
        const auto assign_err = check_assign_arg(itl,arg_type,passed_type);
        if(assign_err)
        {
            return assign_err;
        }
    }

    return option::none;
}

Result<u32,itl_error> push_va_args(Interloper& itl, Function& func, FuncCallNode* call_node,const String& name, u32 actual_args)
{
    u32 arg_clean = 0;

    if(!itl.rtti_enable)
    {
        return compile_error(itl,itl_error::missing_args,"[COMPILE]: attempted to use va_args without rtti: %S",name);
    }

    // va_arg is optional
    if(actual_args - 1 > count(call_node->args))
    {
        return compile_error(itl,itl_error::missing_args,"[COMPILE]: function call va_args expected at least %d args got %d",
            actual_args - 1,count(call_node->args));   
    }

    const u32 normal_args = (actual_args) - 1;

    // how many args are we getting passed?
    const u32 any_args = count(call_node->args) - normal_args;

    //printf("any args: %d\n",any_args);

    // alloc storage for array
    // this is easy because we know how many args we have
    auto& rtti_cache = itl.rtti_cache;

    const u32 any_arr_size = align_val(any_args * rtti_cache.any_struct_size,GPR_SIZE);

    alloc_stack(itl,func,any_arr_size);

    const RegSlot any_arr_ptr = copy_reg(itl,func,make_spec_reg_slot(spec_reg::sp));

    for(u32 a = 0; a < any_args; a++)
    {
        const u32 arg_idx = a + normal_args;
        const u32 arr_offset = a * rtti_cache.any_struct_size;

        const auto any_err = compile_any_arr(itl,func,call_node->args[arg_idx],any_arr_ptr,arr_offset);
        if(any_err)
        {
            return *any_err;
        }
    }


    // alloc vla
    alloc_stack(itl,func,VLA_SIZE);

    // and store it
    const RegSlot any_len_slot = mov_imm_res(itl,func,any_args);

    const auto SP_SLOT = make_spec_reg_slot(spec_reg::sp);

    // store data
    store_ptr(itl,func,any_arr_ptr,SP_SLOT,0,GPR_SIZE,false);
    store_ptr(itl,func,any_len_slot,SP_SLOT,GPR_SIZE,GPR_SIZE,false);      

    
    const u32 total_size = any_arr_size + VLA_SIZE;

    arg_clean += total_size / GPR_SIZE;

    return arg_clean;
}

Option<itl_error> push_tuple_args(Interloper& itl, Function& func, ArgPass& pass, TupleAssignNode* tuple_node, const Array<Type*>& return_type)
{
    const auto tuple_decl = tuple_node->auto_decl;
    bool new_decl = false;

    // TODO: this doesn't do any type checking...
    for(s32 a = count(tuple_node->symbols) - 1; a >= 0; a--)
    {
        AstNode* var_node = tuple_node->symbols[a];
        Type* rtype = return_type[a];

        switch(var_node->type)
        {
            default:
            {
                return compile_error(itl,itl_error::tuple_mismatch,"cannot bind on expr of type %s",AST_NAMES[u32(var_node->type)]);
            }
        }
    }

    if(!new_decl && tuple_decl)
    {
        return compile_error(itl,itl_error::tuple_mismatch,"No new variables declared in tuple assign");
    }

    return option::none;
}

Option<itl_error> push_hidden_args(Interloper& itl, Function& func, ArgPass& pass, TupleAssignNode* tuple_node, 
    RegSlot dst_slot, const Array<Type*>& return_type)
{
    // pass in tuple dst
    if(tuple_node)
    {
        return push_tuple_args(itl,func,pass,tuple_node,return_type);
    }

    // single arg (for struct returns) 
    // use the dst slot
    else
    {
        Type* type = return_type[0];

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
                        // this is nested pass in the current hidden return
                        if(func.sig.hidden_args == 1)
                        {
                            const TypedReg reg = {make_sym_reg_slot(func.sig.args[0]),make_reference(itl,type)};
                            pass_arg(itl,func,pass,reg,0);
                        }

                        else
                        {
                            return compile_error(itl,itl_error::missing_return,"Attempted to return invalid large var inside func");
                        }
                        break;
                    }

                    default:
                    {
                        dump_ir_sym(itl,func,itl.symbol_table);
                        printf("spec reg unhandled: %s\n",spec_reg_name(dst_slot.spec));   
                        assert(false);
                    } 
                }
            }
        }
    }

    return option::none; 
}

struct FuncCall
{
    FuncSig sig = {};
    String name = {};

    union
    {
        LabelSlot label_slot;
        RegSlot reg_slot = {};
    };

    // tag for above union
    b32 func_pointer = false;
};

TypeResult handle_call(Interloper& itl, Function& func, const FuncCall& call_info, RegSlot dst_slot, u32 arg_clean)
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
    // TODO: how should we encode this when we do arg passing in regs
    if(arg_clean)
    {
        clean_args(itl,func,arg_clean);
    }
  
    // Tuples must be bound, don't bother checking this if we are using them.
    if(is_special_reg(dst_slot,spec_reg::null) && count(call_info.sig.return_type) <= 1)
    {
        if(call_info.sig.attr_flags & ATTR_USE_RESULT)
        {
            return compile_error(itl,itl_error::unused_result,"Result of function %S declared with attr use_result must be used",call_info.name);
        }

        else if(is_enum(sig.return_type[0]))
        {
            const auto& enumeration = enum_from_type(itl.enum_table,sig.return_type[0]);

            if(enumeration.use_result)
            {
                return compile_error(itl,itl_error::unused_result,"Enum %S declared with attr use_result must be used from func %S",
                    enumeration.name,call_info.name);
            }
        }
    }

    // restore callee saved values
    //emit(func,op_type::restore_regs);


    // normal return
    // store the return value back into a reg (if its actually binded)
    if(returns_value && !is_special_reg(dst_slot,spec_reg::null) && !sig.hidden_args)
    {
        const RegSlot rv = make_spec_reg_slot(return_reg_from_type(sig.return_type[0]));
        const TypedReg dst = {dst_slot,sig.return_type[0]};
        const TypedReg src = {rv,sig.return_type[0]};
        const auto move_err = compile_move(itl,func,dst,src);
        if(move_err)
        {
            return *move_err;
        }
    }

    unlock_reg_set(itl,func,sig.locked_set);

    // give back the function's return type
    if(count(sig.return_type) == 1)
    {
        // result of expr is the return type
        return sig.return_type[0];
    }

    // tuple 
    else
    {
        return make_builtin(itl,builtin_type::void_t);
    }    
}

Result<FuncCall,itl_error> get_calling_sig(Interloper& itl,NameSpace* name_space,Function& func,FuncCallNode* call_node,TupleAssignNode* tuple_node)
{
    FuncCall call_info;

    AstNode* expr = call_node->expr;

    // just a plain literal
    if(expr->fmt == ast_fmt::literal)
    {
        const LiteralNode* literal_node = (LiteralNode*)expr;
        const String& name = literal_node->literal;

        // are we looking in the global namespace or no?
        const b32 global = name_space == nullptr;

        FunctionDef* func_call_def = global? lookup_func_def_default(itl,name) : lookup_func_def_scope(itl,name_space,name);

        // no known function
        if(!func_call_def)
        {
            // check if this is instead a function pointer on a plain sym?
            auto sym_ptr = get_sym(itl.symbol_table,name);


            if(sym_ptr)
            {
                auto& sym = *sym_ptr;

                // we have a symbol that is a function pointer we are good to go
                if(is_func_pointer(sym.type))
                {
                    FuncPointerType* func_type = (FuncPointerType*)sym.type;

                    call_info.reg_slot = sym.reg.slot;
                    call_info.sig = func_type->sig;
                    call_info.name = sym.name;
                    call_info.func_pointer = true;
                }

                else
                {
                    return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol %S is not a function pointer or function",name);       
                }
            }

            else
            {
                if(global)
                {
                    return compile_error(itl,itl_error::undeclared,"[COMPILE]: function %S is not declared",name);
                }

                else
                {
                    return compile_error(itl,itl_error::undeclared,"[COMPILE]: function %n%S is not declared",name_space,name);
                }
            }
        }

        else
        {
            auto func_call_res = finalise_func(itl,*func_call_def,(AstNode*)call_node);

            if(!func_call_res)
            {
                return func_call_res.error();
            }

            auto& func_call = *func_call_res.value();

            //print_func_decl(itl,func_call);

            call_info.label_slot = func_call.label_slot;
            call_info.sig = func_call.sig;
            call_info.name = func_call.name;
            call_info.func_pointer = false;
        }
    }

    // is an expression
    else 
    {
        auto res = compile_oper(itl,func,expr);
        if(!res)
        {
            return res.error();
        }

        auto reg = *res;

        if(!is_func_pointer(reg.type))
        {
            return compile_error(itl,itl_error::undeclared,"[COMPILE]: expression of type %S is not callable",type_name(itl,reg.type));
        }

        FuncPointerType* func_type = (FuncPointerType*)reg.type;

        call_info.reg_slot = reg.slot;
        call_info.sig = func_type->sig;
        call_info.name = "call_expr";
        call_info.func_pointer = true;
    }

    //print_func_decl(itl,func_call);

    
    // check calls on functions with multiple returns are valid
    if(tuple_node && count(call_info.sig.return_type) == 1)
    {
        return compile_error(itl,itl_error::tuple_mismatch,"attempted to bind %d return values on function with single return",count(tuple_node->symbols));
    }

    if(count(call_info.sig.return_type) > 1)
    {
        if(!tuple_node)
        {
            return compile_error(itl,itl_error::tuple_mismatch,"Tuple result unused\n");
        }

        if(count(call_info.sig.return_type) != count(tuple_node->symbols))
        {
            return compile_error(itl,itl_error::tuple_mismatch,"Numbers of symbols bound for multiple return does not match function: %d != %d",
                count(tuple_node->symbols),count(call_node->args));
        }
    }

    return call_info;   
}


// used for both tuples and ordinary function calls
TypeResult compile_scoped_function_call(Interloper &itl,NameSpace* name_space,Function &func,AstNode *node, RegSlot dst_slot)
{
    TupleAssignNode* tuple_node = nullptr;

    // is this used for tuple binding?
    if(node->type == ast_type::tuple_assign)
    {
        tuple_node = (TupleAssignNode*)node;
    }

    // NOTE: if this is a multiple return the function call is a child node
    FuncCallNode* call_node = tuple_node? tuple_node->func_call : (FuncCallNode*)node;

    if(call_node->expr->fmt == ast_fmt::literal)
    {
        const LiteralNode* literal_node = (LiteralNode*)call_node->expr;
        const String& name = literal_node->literal;

        // check this is not an intrinsic function
        s32 idx = lookup_internal_hashtable(INTRIN_TABLE,INTRIN_TABLE_SIZE,name);

        if(idx != INVALID_HASH_SLOT)
        {
            const auto handler = INTRIN_TABLE[idx].v;
            return handler(itl,func,node,dst_slot);
        }
    }

    // get the signature of what we are actually calling
    // NOTE: this might be plain function, or it could be a function pointer
    const auto call_info_res = get_calling_sig(itl,name_space,func,call_node,tuple_node);
    if(!call_info_res)
    {
        return call_info_res.error();
    }
    const auto call_info = *call_info_res;

    auto& sig = call_info.sig;

    ArgPass pass = make_arg_pass(sig);
    
    // handle argument pushing
    const s32 hidden_args = sig.hidden_args;

    u32 start_arg = count(sig.args) - 1;

    const u32 actual_args = count(sig.args) - hidden_args;

    if(sig.va_args)
    {
        auto arg_clean_res = push_va_args(itl,func,call_node,call_info.name,actual_args);
        if(!arg_clean_res)
        {
            destroy_arg_pass(pass);
            return arg_clean_res.error();
        }

        pass.arg_clean += *arg_clean_res;

        // skip over our va_args
        start_arg = actual_args - 2;
    }

    // normal call
    else
    {
        // check we have the right number of params
        if(actual_args != count(call_node->args))
        {
            destroy_arg_pass(pass);
            return compile_error(itl,itl_error::missing_args,"[COMPILE]: function call expected %d args got %d",actual_args,count(call_node->args));
        }        
    }

    const auto arg_err = push_args(itl,func,pass,call_node,sig,start_arg);
    if(arg_err)
    {
        destroy_arg_pass(pass);
        return *arg_err;
    }

    // push hidden args 

    if(hidden_args)
    {
        const auto hidden_err = push_hidden_args(itl,func,pass,tuple_node,dst_slot,sig.return_type);
        if(hidden_err)
        {
            destroy_arg_pass(pass);
            return *hidden_err;
        }
    }

    const u32 arg_clean = pass_args(itl,func,pass);


    // handle calling and returns
    return handle_call(itl,func,call_info,dst_slot,arg_clean);
}

// used for both tuples and ordinary function calls
TypeResult compile_function_call(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    return compile_scoped_function_call(itl,nullptr,func,node,dst_slot);
}




Option<itl_error> compile_function(Interloper& itl, Function& func)
{
    // NOTE: for compiler generated functions we dont want to 
    // compile this via a standard node
    if(func.root)
    {
        const auto &node = *func.root;

        // inside a new function we dont care about the old context
        // so just trash it
        trash_context(itl,node.filename,func.name_space,(AstNode*)func.root);
        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        auto scope_guard = enter_new_anon_scope(itl.symbol_table);

        new_basic_block(itl,func);
        
        lock_reg_set(itl,func,func.sig.locked_set);

        // put each arg into scope and copy it regs into args
        for(u32 a = 0; a < count(func.sig.args); a++)
        {
            const SymSlot slot = func.sig.args[a];

            auto &sym = sym_from_slot(itl.symbol_table,slot);
            add_sym_to_scope(itl.symbol_table,sym);

            const u32 arg_reg = func.sig.pass_as_reg[a];

            if(arg_reg != NON_ARG)
            {
                const spec_reg arg = spec_reg(SPECIAL_REG_ARG_START + arg_reg);
                mov_unlock(itl,func,make_sym_reg_slot(func.sig.args[a]),make_spec_reg_slot(arg));
            }
        }

        const auto block_err = compile_block(itl,func,node.block);
        if(block_err)
        {
            return block_err;
        }
    }

    // empty functions get a stub
    else
    {
        // produce a dummy basic block
        if(count(func.emitter.program) == 0)
        {
            new_basic_block(itl,func);
        }
    }
 
    // if final block has no return and this is a void func insert one
    if(is_void(func.sig.return_type[0]))
    {    
        auto& end_block = block_from_slot(func,cur_block(func));

        if(!has_func_exit(func,end_block.block_slot))
        {
            ret(itl,func);
        }
    }

    // connect up the cfg
    connect_flow_graph(itl,func); 

    // do liveness analysis
    compute_var_live(itl,func);


    // now check a function exit is reachable from the entry block of the function
    // for a void func this should allways be possible as everything should hit the bottom return
    // that does not have an early return...

    auto& start_block = func.emitter.program[0];

    // check the start block can reach one
    if(!can_reach_exit(start_block))
    {
        auto& label = label_from_slot(itl.symbol_table.label_lookup,start_block.label_slot);

        itl.ctx.expr = (AstNode*)func.root;
        return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %S",label.name); 
    }

    for(u32 b = 0; b < count(start_block.links); b++)
    {
        const auto slot = start_block.links[b];

        // TODO: have this print the source line of the block
        if(!can_reach_exit(func,slot))
        {
            auto& block = block_from_slot(func,slot);
            auto& label = label_from_slot(itl.symbol_table.label_lookup,block.label_slot);

            itl.ctx.expr = (AstNode*)func.root;   
            dump_ir_sym(itl,func,itl.symbol_table);
            return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %S",label.name);
        }
    }

    return option::none;
}

Option<itl_error> compile_functions(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        const auto func_err = compile_function(itl,func);
        if(func_err)
        {
            return func_err;
        }
    }

    return option::none;
}
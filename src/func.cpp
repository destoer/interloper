

using namespace destoer;

void add_func(Interloper& itl, const String& name, FuncNode* root)
{
    Function func;
    func.name = copy_string(itl.string_allocator,name);
    func.root = root;

    add(itl.function_table,func.name,func);    
}


void mark_used(Interloper& itl, Function& func)
{
    if(!func.used)
    {
        push_var(itl.used_func,func.name);
        func.used = true;
    }
}

void mark_used(Interloper& itl, const String& name)
{
    mark_used(itl,*lookup(itl.function_table,name));
}

#include "intrin.cpp"



void print_func_decl(Interloper& itl,const Function &func)
{
    printf("func: %s\n",func.name.buf);

    printf("hidden args: %d\n",func.hidden_args);

    for(u32 a = 0; a < count(func.args); a++)
    {
        const SymSlot slot = func.args[a];
        auto &sym = sym_from_slot(itl.symbol_table,slot);

        print(itl,sym); 
    }

    for(u32 r = 0; r < count(func.return_type); r++)
    {
        printf("return type %s\n",type_name(itl,func.return_type[r]).buf);
    }
}


void finalise_def(Interloper& itl,Function& func, Array<Type*> rt, Array<SymSlot> a, u32 hidden_args)
{
    func.return_type = rt;
    func.args = a;

    // add as a label as it this will be need to referenced by call instrs
    // in the ir to get the name back
    func.label_slot = add_label(itl.symbol_table,func.name);

    func.hidden_args = hidden_args;
}


// add hidden arg pointers for return
u32 add_hidden_return(Interloper& itl, const String& name, Type* return_type, Array<SymSlot>& args ,u32 arg_offset)
{
    Type* ptr_type = make_pointer(itl,return_type);

    Symbol sym = make_sym(itl.symbol_table,name,ptr_type,GPR_SIZE,arg_offset);
    add_var(itl.symbol_table,sym);

    push_var(args,sym.reg.slot);     

    arg_offset += GPR_SIZE;

    return arg_offset;     
}

// used for both tuples and ordinary function calls
Type* compile_function_call(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    TupleAssignNode* tuple_node = nullptr;

    // is this used for tuple binding?
    if(node->type == ast_type::tuple_assign)
    {
        tuple_node = (TupleAssignNode*)node;
    }

    // NOTE: if this is a multiple return the function call is a child node
    FuncCallNode* call_node = tuple_node? tuple_node->func_call : (FuncCallNode*)node;

    s32 idx = lookup_internal_hashtable(INTRIN_TABLE,INTRIN_TABLE_SIZE,call_node->name);

    if(idx != INVALID_SLOT)
    {
        const auto handler = INTRIN_TABLE[idx].v;
        return handler(itl,func,node,dst_slot);
    }

    Function* func_call_ptr = lookup(itl.function_table,call_node->name);

    // check function is declared
    if(!func_call_ptr)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: function %s is not declared\n",call_node->name.buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    auto &func_call = *func_call_ptr;
    mark_used(itl,func_call);




    //print_func_decl(itl,func_call);

    const s32 hidden_args = func_call.hidden_args;

    // check we have the right number of params
    if((count(func_call.args) - hidden_args) != count(call_node->args))
    {
        panic(itl,itl_error::missing_args,"[COMPILE]: function call expected %d args got %d\n",count(func_call.args),count(call_node->args));
        return make_builtin(itl,builtin_type::void_t);
    }


    // check calls on functions with multiple returns are valid
    if(tuple_node && count(func_call.return_type) == 1)
    {
        panic(itl,itl_error::tuple_mismatch,"attempted to bind %d return values on function with single return\n",count(tuple_node->symbols));
        return make_builtin(itl,builtin_type::void_t);
    }

    if(count(func_call.return_type) > 1)
    {
        if(!tuple_node)
        {
            panic(itl,itl_error::tuple_mismatch,"Attempted to call multiple return function nested in a expression\n");
            return make_builtin(itl,builtin_type::void_t);
        }

        if(count(func_call.return_type) != count(tuple_node->symbols))
        {
            panic(itl,itl_error::tuple_mismatch,"Numbers of smybols binded for multiple return does not match function: %d != %d\n",count(tuple_node->symbols),count(call_node->args));
            return make_builtin(itl,builtin_type::void_t);
        }
    }



    // for now we are just going with callee saved
    // if we eventually mix caller and callee saved so we can have the called func overwrite values
    // we dont care about then we need to re add the save and restore directives
    // and we will need a push and pop bitset to implement them
    // with care to rewrite where the return register ends up when we restore
    // save all the used regs
    //emit(func,op_type::save_regs);
    
    // how many args are we pushing to the stack?
    u32 arg_clean = 0;



    // push args in reverse order and type check them
    for(s32 i = count(func_call.args) - 1; i >= hidden_args; i--)
    {
        const auto &arg =  sym_from_slot(itl.symbol_table,func_call.args[i]);
  
        const u32 arg_idx = i - hidden_args;
/*
        if(is_any(itl,arg.type))
        {
            // compile our arg and figure out what we have
            auto [arg_type,reg] = compile_oper(itl,func,call_node->args[arg_idx]);

            const u32 size = any_size(itl,arg_type);

            // alloc the struct size for our copy
            emit(func,op_type::alloc_stack,size);

            const SymSlot SP_SLOT = sym_from_idx(SP_IR);

            // void make_any(Interloper& itl,Function& func, SymSlot ptr_slot, u32 offset, const SymSlot src, const Type* type)
            make_any(itl,func,SP_SLOT,0,reg,arg_type);

            arg_clean += size / GPR_SIZE;
        }

        else*/ if(is_array(arg.type))
        {
        /*
            // pass a static string, by inserting as const data in the program
            if(call_node->args[arg_idx]->type == ast_type::string)
            {
                LiteralNode* lit_node = (LiteralNode*)call_node->args[arg_idx];

                const u32 size = lit_node->literal.size;

                const auto rtype = make_array(itl,make_builtin(itl,builtin_type::c8_t,true),size);
                check_assign_arg(itl,arg.type,rtype);
                
                // push the len offset
                const SymSlot len_slot = mov_imm(func,size);
                push_arg(itl,func,len_slot);

                // push the data offset
                const PoolSlot pool_slot = push_const_pool(itl.const_pool,pool_type::string_literal,lit_node->literal.buf,size);

                const SymSlot addr_slot = pool_addr(func,pool_slot);
                push_arg(itl,func,addr_slot);

                arg_clean += 2;
            }

            else
            {
                auto [arg_type,reg] = compile_oper(itl,func,call_node->args[arg_idx]);

                check_assign_arg(itl,arg.type,arg_type); 

                if(itl.error)
                {
                    return make_builtin(itl,builtin_type::void_t);;
                }

                // push in reverse order let our internal functions handle vla conversion
                const SymSlot len_slot = load_arr_len(itl,func,reg,arg_type);
                push_arg(itl,func,len_slot);

                const SymSlot data_slot = load_arr_data(itl,func,reg,arg_type);
                push_arg(itl,func,data_slot);

                arg_clean += 2;  
            }
        */
            assert(false);    
        }

        else if(is_struct(arg.type))
        {
        /*
           const auto structure = struct_from_type(itl.struct_table,arg.type);

            const auto [arg_type,reg] = compile_oper(itl,func,call_node->args[arg_idx]);
            check_assign_arg(itl,arg.type,arg_type);


            // TODO: support copies with larger loads
            static_assert(GPR_SIZE == sizeof(u32));

            // alloc the struct size for our copy
            emit(func,op_type::alloc_stack,structure.size);

            // need to save SP as it will get pushed last
            const SymSlot dst = emit_res(func,op_type::mov_reg,SP_IR);
            const SymSlot ptr = addrof_res(itl,func,reg);

            ir_memcpy(itl,func,dst,ptr,structure.size);

            // clean up the stack push
            arg_clean += structure.size / GPR_SIZE;
        */
            assert(false);
        }

        // plain builtin in variable
        else
        {
            // builtin type
            const auto [arg_type,reg] = compile_oper(itl,func,call_node->args[arg_idx]);


            // type check the arg
            check_assign_arg(itl,arg.type,arg_type);

            // finally push the arg
            push_arg(itl,func,reg);

            arg_clean++;
        }
    }

    // push hidden args 

    if(hidden_args)
    {
       // pass in tuple dst
        if(tuple_node)
        {
            assert(false);
/*
            // okay how do we wanna structure getting tuple info off of this?
            // do we want to look at the node?
            // 
            for(s32 a = count(tuple_node->symbols) - 1; a >= 0; a--)
            {
                AstNode* var_node = tuple_node->symbols[a];

                switch(var_node->type)
                {
                    case ast_type::symbol:
                    {
                        const LiteralNode *sym_node = (LiteralNode*)var_node;

                        const auto sym_opt = get_sym(itl.symbol_table,sym_node->literal);

                        if(!sym_opt)
                        {
                            panic(itl,itl_error::undeclared,"symbol %s used before declaration\n",sym_node->literal.buf);
                            return make_builtin(itl,builtin_type::void_t);
                        }

                        const auto &sym = sym_opt.value();

                        const SymSlot addr_slot = addrof_res(itl,func,sym.reg.slot);
                        push_arg(itl,func,addr_slot);

                        break;
                    }

                    case ast_type::access_struct:
                    {
                        // get the addr and push it
                        auto [type,ptr_slot,offset] = compute_member_addr(itl,func,var_node);
                        ptr_slot = collapse_offset(func,ptr_slot,&offset);

                        push_arg(itl,func,ptr_slot);
                        break;
                    }

                    case ast_type::index:
                    {
                        auto [type,ptr_slot] = index_arr(itl,func,var_node,new_tmp_ptr(func));

                        push_arg(itl,func,ptr_slot);
                        break;
                    }

                    case ast_type::deref:
                    {
                        UnaryNode* deref_node = (UnaryNode*)var_node;

                        const auto [type,ptr_slot] = load_addr(itl,func,deref_node->next,new_tmp_ptr(func),false);

                        push_arg(itl,func,ptr_slot);
                        break;                     
                    }

                    default:
                    {
                        panic(itl,itl_error::tuple_mismatch,"cannot bind on expr of type %s\n",AST_NAMES[u32(var_node->type)]);
                        return make_builtin(itl,builtin_type::void_t);
                    }
                }

                arg_clean++;
            
            }
            */
        }

        // single arg (for struct returns) 
        // use the dst slot
        else
        {
            if(dst_slot.handle == NO_SLOT)
            {
                unimplemented("no_slot: binding on large return type");
            }

            else if(is_sym(dst_slot))
            {
                arg_clean++;

                const SymSlot addr = addrof_res(itl,func,dst_slot);
                push_arg(itl,func,addr);
            }

            else
            {
                arg_clean++;

                alloc_slot(itl,func,func.registers[dst_slot.handle],true);
                
                const SymSlot addr = addrof_res(itl,func,dst_slot);
                push_arg(itl,func,addr);
            }
        }
    }



    // NOTE: func struct will hold a void value if it has nothing
    const bool returns_value = func_call.return_type[0]->type_idx != u32(builtin_type::void_t);


    // if we have a register in R0 we need to save it so its not overwritten
    const b32 save_regs = returns_value && !hidden_args;


    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    call(itl,func,func_call.label_slot,save_regs);


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    if(arg_clean)
    {
        clean_args(itl,func,arg_clean);
    }
  

    // restore callee saved values
    //emit(func,op_type::restore_regs);


    // normal return
    // store the return value back into a reg (if its actually binded)
    if(returns_value && dst_slot.handle != NO_SLOT && !hidden_args)
    {
        // TODO: is this dst type correct?
        compile_move(itl,func,dst_slot,sym_from_idx(RV_IR),func.return_type[0],func.return_type[0]);
    }
    

    // give back the function's return type

    if(count(func_call.return_type) == 1)
    {
        // result of expr is the return type
        return func_call.return_type[0];
    }

    // tuple 
    else
    {
        return make_raw(itl,TUPLE);
    }    
}



// we wont worry about the scope on functions for now as we wont have namespaces for a while
void parse_function_declarations(Interloper& itl)
{
    

    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            const FuncNode& node = *func.root;

            itl.cur_file = node.filename;

            // parse out out the return type
            Array<SymSlot> args;
            u32 arg_offset = 0;
            u32 hidden_args = 0;

            Array<Type*> return_type;


            // NOTE: void return's will have a void type
            if(count(node.return_type) == 1)
            {
                push_var(return_type,get_complete_type(itl,node.return_type[0]));

                // we are returning a struct add a hidden pointer as first arg
                if(type_size(itl,return_type[0]) > GPR_SIZE)
                {
                    arg_offset = add_hidden_return(itl,"_struct_ret_ptr",return_type[0],args,arg_offset);
                    hidden_args++;
                }   
            }

            // tuple return
            else if(count(node.return_type) > 1)
            {
                for(u32 a = 0; a < count(node.return_type); a++)
                {
                    push_var(return_type,get_complete_type(itl,node.return_type[a]));

                    char name[40] = {0};
                    sprintf(name,"_tuple_ret_0x%x",a);

                    arg_offset = add_hidden_return(itl,name,return_type[a],args,arg_offset);

                    hidden_args++;
                }
            }


            const auto decl = node.args;

            // rip every arg
            for(u32 i = 0; i < count(decl); i++)
            {
                const auto a = decl[i];

                const auto name = a->name;
                const auto type = get_complete_type(itl,a->type);

                const auto size = type_size(itl,type);

                // add the var to slot lookup and link to function
                // we will do a add_scope to put it into the scope later
                Symbol sym = make_sym(itl.symbol_table,name,type,size,arg_offset);
                add_var(itl.symbol_table,sym);

                push_var(args,sym.reg.slot);

                // if size is below GPR just make it take that much
                const u32 arg_size = sym.reg.size < GPR_SIZE? 4 : size;

                arg_offset += arg_size;

                //printf("arg slot %s: %d : %d\n",sym.name.buf,sym.slot, args[args.size()-1]);
            }


            finalise_def(itl,func,return_type,args,hidden_args);
        }
    }
}


void compile_function(Interloper& itl, Function& func)
{
    const auto &node = *func.root;
    itl.cur_file = node.filename;

    
    // put arguments on the symbol table they are marked as args
    // so we know to access them "above" to stack pointer
    new_scope(itl.symbol_table);


    // put each arg into scope
    for(u32 a = 0; a < count(func.args); a++)
    {
        const SymSlot slot = func.args[a];

        auto &sym = sym_from_slot(itl.symbol_table,slot);
        add_scope(itl.symbol_table,sym);
    }



    itl.has_return = false;


    // parse out each line of the function
    compile_basic_block(itl,func,node.block);

    destroy_scope(itl.symbol_table);

    if(!itl.has_return)
    {
        // is a void function this is fine
        // we just need to emit the ret at the end 
        if(func.return_type[0]->type_idx == u32(builtin_type::void_t))
        {
            ret(itl,func);
        }

        else
        {
            panic(itl,itl_error::missing_return,"[COMPILE]: non void function without a return\n");
        }
    }

    if(itl.error)
    {
        return;
    }

    connect_flow_graph(itl,func);    
}

void compile_functions(Interloper &itl)
{
    // global scope
    new_scope(itl.symbol_table);



    // TODO: we need to hide these functions from access via general calling code
    // they should probably get namespaced when we have access to them...
    mark_used(itl,"main");
    mark_used(itl,"start");
    
    for(u32 idx = 0; idx != count(itl.used_func); idx++)
    {
        Function& func = *lookup(itl.function_table,itl.used_func[idx]);
        compile_function(itl,func);
    }

    destroy_scope(itl.symbol_table); 
}

void check_func_exists(Interloper& itl, const String& name)
{
    // ensure the entry functions are defined
    if(!contains(itl.function_table,name))
    {
        panic(itl,itl_error::undeclared,"%s is not defined!\n",name.buf);
        return;
    }    
}
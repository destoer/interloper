

using namespace destoer;

void print_func_decl(Interloper& itl,const Function &func);

FunctionTable make_func_table()
{
    FunctionTable func_table;
    func_table.table = make_table<String,FunctionDef>();
    func_table.arena = make_allocator(16 * 1024);

    return func_table;
}

void destroy_func_table(FunctionTable& func_table)
{
    for(u32 f = 0; f < count(func_table.used); f++)
    {
        auto& func = *func_table.used[f];
        destroy_func(func);
    }

    destroy_table(func_table.table);
    destroy_arr(func_table.used);
    destroy_allocator(func_table.arena);
}

void add_func(Interloper& itl, const String& name, const String& name_space, FuncNode* root)
{
    FunctionDef func_def;

    // Make sure our function is not allocated on the
    // same string allocator as the AST
    func_def.name = alloc_name_space_name(itl.string_allocator,name_space,name);

    func_def.root = root;
    func_def.func = nullptr;
    
    // add def
    add(itl.func_table.table,func_def.name,func_def);    
}

Function* finalise_func_internal(Interloper& itl, FunctionDef& func_def, b32 parse_sig = true)
{
    // havent finalised this func
    if(!func_def.func)
    {
        Function func;
        func.name = func_def.name;
        func.root = func_def.root;

        // parse in function signature on demand
        if(func.root)
        {
            if(parse_sig)
            {
                parse_func_sig(itl,func.sig,*func.root);
            }
        }

        // dummy func creation
        else
        {
            push_var(func.sig.return_type,make_builtin(itl,builtin_type::void_t));

            // create a dummy basic block
            new_basic_block(itl,func);
        }

        // add as a label as it this will be need to referenced by call instrs
        // in the ir to get the name back
        func.label_slot = add_label(itl.symbol_table,func.name);

        // write back the slot
        func_def.func = (Function*)allocate(itl.func_table.arena,sizeof(Function));

        // add the actual func
        *func_def.func = func;

        // mark it used
        push_var(itl.func_table.used,func_def.func);
    }

    return func_def.func;   
}

// NOTE: we probably want to switch over to a global definiton table
// for any top level statement to make this less cumbersome to implement
// it will also let make sure different domains dont have the same name


// NOTE: this gets a function ONLY in the requested scope
FunctionDef* lookup_func_def_scope(Interloper& itl, const String& name, const String& name_space)
{
    // attempt to find in the specified scope and nothing else
    const auto full_name = tmp_name_space_name(itl,name_space,name);

    //printf("lookup scoped: %s\n",full_name.buf);

    return lookup(itl.func_table.table,full_name);
}

// get a function only in the global scope
FunctionDef* lookup_func_def_global(Interloper& itl, const String& name)
{
    //printf("lookup global: %s\n",name.buf);

    return lookup(itl.func_table.table,name);
}

// Search each scope from the bottom for a function
FunctionDef* lookup_func_def_default(Interloper& itl, const String& name)
{
    // attempt to find in current name space first
    FunctionDef* opt = lookup_func_def_scope(itl,name,itl.ctx.name_space);

    // found in current scope
    if(opt)
    {
        return opt;
    }

    // fail attempt to find globally
    return lookup_func_def_global(itl,name);
}


 
Function& create_dummy_func(Interloper& itl, const String& name)
{
    add_func(itl,name,"",nullptr);

    FunctionDef& func_def = *lookup_func_def_global(itl,name);
    
    finalise_func_internal(itl,func_def);

    // get its new home
    return lookup_internal_function(itl,name);
}

StringBuffer func_generic_name(Interloper& itl, const String& old_name,Array<TypeNode*> generic)
{
    StringBuffer buffer;

    // old name appended with _g_
    push_string(buffer,old_name);
    push_string(buffer,"_g_");

    
    for(u32 g = 0; g < count(generic); g++)
    {
        Type* type = get_type(itl,generic[g]);

        b32 done = false;

        while(!done)
        {
            switch(type->type_idx)
            {
                case POINTER:
                {
                    push_string(buffer,"_ptr@_");
                    type = deref_pointer(type);
                    break;
                }

                case STRUCT: 
                {
                    auto& structure  = struct_from_type(itl.struct_table,type);
                    push_var(buffer,'_');
                    push_string(buffer,structure.name);
                    done = true;
                    break;               
                }

                case ENUM: 
                {
                    const auto& enumeration = enum_from_type(itl.enum_table,type);
                    push_var(buffer,'_');
                    push_string(buffer,enumeration.name);
                    done = true;
                    break;             
                }

                case ARRAY:
                {
                    assert(false);
                    break;
                }

                case FUNC_POINTER:
                {
                    assert(false);
                    break;
                }

                // builtin
                default:
                {
                    const auto plain = builtin_type_name(builtin_type(type->type_idx));
                    push_var(buffer,'_');
                    push_string(buffer,plain);
                    done = true;
                    break;
                }
            }
        }
    }

    // null term string
    push_var(buffer,'\0');

    return buffer;
}

void push_generic_type_alias(Interloper& itl, Function& func)
{
    for(u32 g = 0; g < count(func.generic_override); g++)
    {
        push_temp_type_alias(itl,func.generic_override[g],func.generic_override_name[g],itl.ctx.filename);
    }
}

void pop_generic_type_alias(Interloper& itl, Function& func)
{
    for(u32 g = 0; g < count(func.generic_override); g++)
    {
        pop_temp_type_alias(itl);
    }
}

Function* specialise_func(Interloper& itl, FunctionDef& base_func, FuncCallNode* call_node)
{
    // pull specialised name
    auto specialised_name_buffer = func_generic_name(itl,base_func.name,call_node->generic);

    // NOTE: we need to copy this string onto the main pool
    // as the buffer we used to build it is going to get freed
    const auto specialised_name = make_string(specialised_name_buffer);

    // check if function has allready been specialised
    // if so just return it
    auto spec_func_opt = lookup_opt_scoped_function(itl,specialised_name,"");

    if(spec_func_opt)
    {
        destroy_arr(specialised_name_buffer);
        return spec_func_opt;
    }

    // okay this generic has not been generated for with this signature before
    // we need to build a specialised version
    
    // duplicate function so we can specilaise it

    // NOTE: this puts this implicitly puts the function in the same namespace as the old one
    add_func(itl,specialised_name,"",base_func.root);

    auto& spec_func = *finalise_func_internal(itl,*lookup_func_def_global(itl,specialised_name),false);

    // add in the type overrides so we can finalise the function

    // copy over all the generic names to the func
    // so we don't get dead refs when the ast dies

    // along with the type
    for(u32 g = 0; g < count(base_func.root->generic_name); g++)
    {
        const auto str = copy_string(itl.string_allocator,base_func.root->generic_name[g]);
        push_var(spec_func.generic_override_name,str);

        // TODO: this requires modification for a proper array later
        auto type = get_type(itl,call_node->generic[g]);
        push_var(spec_func.generic_override,type);
    }

    // push overrides so we can build the sig
    push_generic_type_alias(itl,spec_func);

    // now parse the sig
    parse_func_sig(itl,spec_func.sig,*spec_func.root);

    // now remove the override
    pop_generic_type_alias(itl,spec_func);

    // cleanup our mem now that we have copied it
    destroy_arr(specialised_name_buffer);

    // return newly overrided function
    return &spec_func;
}

Function* finalise_func(Interloper& itl, FunctionDef& base_func, AstNode* ast_node)
{ 
    const b32 is_generic = base_func.root != nullptr && count(base_func.root->generic_name);

    switch(ast_node->type)
    {
        case ast_type::function_call:
        {
            FuncCallNode* call_node = (FuncCallNode*)ast_node;

            // check we aernt accidentally invoking a generic
            // on a plain function
            if(count(call_node->generic) && !is_generic)
            {
                panic(itl,itl_error::generic_type_error,"Function %s is not a generic\n",base_func.name.buf);
                return nullptr;
            }

            // ordinary function just return it
            if(!is_generic)
            {
                return finalise_func_internal(itl,base_func);
            }

            // else this is a generic function

            // check we are actually specifying the types
            if(!count(call_node->generic))
            {
                panic(itl,itl_error::generic_type_error,"Missing type specification for generic function call: %s\n",base_func.name.buf);
                return nullptr;
            } 

            return specialise_func(itl,base_func,call_node);
        }

        default:
        {
            return finalise_func_internal(itl,base_func);
        }
    }
}

b32 func_exists(Interloper& itl, const String& name, const String& name_space)
{
    return lookup_func_def_scope(itl,name,name_space) != nullptr;
}

void check_startup_func(Interloper& itl, const String& name, const String& name_space)
{
    auto def_opt = lookup_func_def_scope(itl,name,name_space);

    // ensure the entry functions are defined
    if(!def_opt)
    {
        panic(itl,itl_error::undeclared,"%s is not defined!\n",name.buf);
        return;
    }

    finalise_func_internal(itl,*def_opt);    
}

Function* lookup_opt_scoped_function(Interloper& itl, const String& name,const String& name_space)
{
    FunctionDef* func_def_opt = lookup_func_def_scope(itl,name,name_space);

    if(!func_def_opt)
    {
        return nullptr;
    }

    auto& func_def = *func_def_opt;

    return func_def.func;
}

Function* lookup_opt_global_function(Interloper& itl, const String& name)
{
    FunctionDef* func_def_opt = lookup_func_def_global(itl,name);

    if(!func_def_opt)
    {
        return nullptr;
    }

    auto& func_def = *func_def_opt;

    return func_def.func;
}

Function& lookup_internal_function(Interloper& itl, const String& name)
{
    Function* func_opt = lookup_opt_scoped_function(itl,name,"");

    // assert this just for saftey
    assert(func_opt);

    return *func_opt;
}

#include "intrin.cpp"



void print_func_decl(Interloper& itl,const Function &func)
{
    printf("func: %s\n\n",func.name.buf);

    printf("hidden args: %d\n",func.sig.hidden_args);

    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];
        auto &sym = sym_from_slot(itl.symbol_table,slot);

        print(itl,sym); 
        putchar('\n');
    }

    for(u32 r = 0; r < count(func.sig.return_type); r++)
    {
        printf("return type %s\n",type_name(itl,func.sig.return_type[r]).buf);
    }

    printf("\n\n");
}


// add hidden arg pointers for return
void add_hidden_return(Interloper& itl, FuncSig& sig, const String& name, Type* return_type, u32* arg_offset)
{
    Type* ptr_type = make_pointer(itl,return_type);

    Symbol sym = make_sym(itl,name,ptr_type,*arg_offset);
    add_var(itl.symbol_table,sym);

    push_var(sig.args,sym.reg.slot);     

    *arg_offset += GPR_SIZE;
    sig.hidden_args++;
}

u32 push_args(Interloper& itl, Function& func, FuncCallNode* call_node,const FuncSig& sig, u32 start_arg)
{
    u32 arg_clean = 0;

    const s32 hidden_args = sig.hidden_args;

    // push args in reverse order and type check them
    for(s32 i = start_arg; i >= hidden_args; i--)
    {
        auto& sym = sym_from_slot(itl.symbol_table,sig.args[i]); 
        Type* arg_type =  sym.type;
  
        const u32 arg_idx = i - hidden_args;

        if(is_any(itl,arg_type))
        {
            const u32 size = compile_any(itl,func,call_node->args[arg_idx]);
            arg_clean += size / GPR_SIZE;
        }

        else if(is_array(arg_type))
        {
            // pass a static string, by inserting as const data in the program
            if(call_node->args[arg_idx]->type == ast_type::string)
            {
                LiteralNode* lit_node = (LiteralNode*)call_node->args[arg_idx];

                const u32 size = lit_node->literal.size;

                const auto rtype = make_array(itl,make_builtin(itl,builtin_type::c8_t,true),size);
                check_assign_arg(itl,arg_type,rtype);
                
                // push the len offset
                const SymSlot len_slot = mov_imm_res(itl,func,size);
                push_arg(itl,func,len_slot);

                // push the data offset
                const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,lit_node->literal);

                const SymSlot addr_slot = pool_addr_res(itl,func,pool_slot,0);
                push_arg(itl,func,addr_slot);

                arg_clean += 2;
            }

            else
            {
                auto [rtype,reg] = compile_oper(itl,func,call_node->args[arg_idx]);

                check_assign_arg(itl,arg_type,rtype); 

                if(itl.error)
                {
                    return arg_clean;
                }


                if(is_runtime_size(arg_type))
                {
                    // push in reverse order let our internal functions handle vla conversion
                    const SymSlot len_slot = load_arr_len(itl,func,reg,rtype);
                    push_arg(itl,func,len_slot);

                    const SymSlot data_slot = load_arr_data(itl,func,reg,rtype);
                    push_arg(itl,func,data_slot);

                    arg_clean += 2;  
                }

                // fixed sized array
                else
                {
                    push_arg(itl,func,reg);

                    arg_clean += 1;
                }
            }
        }

        else if(is_struct(arg_type))
        {
           const auto structure = struct_from_type(itl.struct_table,arg_type);

            const auto [rtype,reg] = compile_oper(itl,func,call_node->args[arg_idx]);
            check_assign_arg(itl,arg_type,rtype);

            // alloc the struct size for our copy
            alloc_stack(itl,func,structure.size);

            // need to save SP as it will get pushed last
            const SymSlot dst_ptr = copy_reg(itl,func,sym_from_idx(SP_IR));
            const auto dst_addr = make_addr(dst_ptr,0);

            const auto src_addr = make_struct_addr(reg,0);

            ir_memcpy(itl,func,dst_addr,src_addr,structure.size);

            // clean up the stack push
            arg_clean += structure.size / GPR_SIZE;
        }

        // plain builtin in variable
        else
        {
            // builtin type
            const auto [rtype,reg] = compile_oper(itl,func,call_node->args[arg_idx]);


            // type check the arg
            check_assign_arg(itl,arg_type,rtype);

            // finally push the arg
            push_arg(itl,func,reg);

            arg_clean++;
        }
    }

    return arg_clean;
}

u32 push_va_args(Interloper& itl, Function& func, FuncCallNode* call_node,const String& name, u32 actual_args)
{
    u32 arg_clean = 0;

    if(!itl.rtti_enable)
    {
        panic(itl,itl_error::missing_args,"[COMPILE]: attempted to use va_args without rtti: %s\n",name.buf);
        return arg_clean;
    }

    // va_arg is optional
    if(actual_args - 1 > count(call_node->args))
    {
        panic(itl,itl_error::missing_args,"[COMPILE]: function call va_argsexpected at least %d args got %d\n",actual_args - 1,count(call_node->args));
        return arg_clean;      
    }

    const u32 normal_args = (actual_args) - 1;

    // how many args are we getting passed?
    const u32 any_args = count(call_node->args) - normal_args;

    //printf("any args: %d\n",any_args);

    // alloc storage for array
    // this is easy because we know how many args we have
    auto& rtti_cache = itl.rtti_cache;

    const u32 any_arr_size = any_args * rtti_cache.any_struct_size;

    alloc_stack(itl,func,any_arr_size);

    const SymSlot any_arr_ptr = copy_reg(itl,func,sym_from_idx(SP_IR));

    for(u32 a = 0; a < any_args; a++)
    {
        const u32 arg_idx = a + normal_args;
        const u32 arr_offset = a * rtti_cache.any_struct_size;

        compile_any_arr(itl,func,call_node->args[arg_idx],any_arr_ptr,arr_offset);
    }


    // alloc vla
    alloc_stack(itl,func,VLA_SIZE);

    // and store it
    const SymSlot any_len_slot = mov_imm_res(itl,func,any_args);

    // store data
    store_ptr(itl,func,any_arr_ptr,sym_from_idx(SP_IR),0,GPR_SIZE);
    store_ptr(itl,func,any_len_slot,sym_from_idx(SP_IR),GPR_SIZE,GPR_SIZE);      

    
    const u32 total_size = any_arr_size + VLA_SIZE;

    arg_clean += total_size / GPR_SIZE;

    return arg_clean;
}

u32 push_hidden_args(Interloper& itl, Function& func, TupleAssignNode* tuple_node, SymSlot dst_slot)
{
    u32 arg_clean = 0;

    // pass in tuple dst
    if(tuple_node)
    {
        // TODO: this doesn't do any type checking...
        for(s32 a = count(tuple_node->symbols) - 1; a >= 0; a--)
        {
            AstNode* var_node = tuple_node->symbols[a];

            switch(var_node->type)
            {
                case ast_type::symbol:
                {
                    const LiteralNode *sym_node = (LiteralNode*)var_node;

                    const auto sym_ptr = get_sym(itl.symbol_table,sym_node->literal);

                    if(!sym_ptr)
                    {
                        panic(itl,itl_error::undeclared,"symbol %s used before declaration\n",sym_node->literal.buf);
                        return arg_clean;
                    }

                    const auto &sym = *sym_ptr;
                    spill_slot(itl,func,sym.reg);

                    const SymSlot addr_slot = addrof_res(itl,func,sym.reg.slot);
                    push_arg(itl,func,addr_slot);

                    break;
                }

                case ast_type::access_struct:
                {
                    // get the addr and push it
                    auto [type,ptr_slot] = compute_member_ptr(itl,func,var_node);

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

                    const auto [ptr_type,ptr_slot] = take_pointer(itl,func,deref_node->next);

                    if(itl.error)
                    {
                        return arg_clean;
                    }

                    push_arg(itl,func,ptr_slot);
                    break;                     
                }

                default:
                {
                    panic(itl,itl_error::tuple_mismatch,"cannot bind on expr of type %s\n",AST_NAMES[u32(var_node->type)]);
                    return arg_clean;
                }
            }

            arg_clean++;
        
        }
    }

    // single arg (for struct returns) 
    // use the dst slot
    else
    {
        if(dst_slot.handle == NO_SLOT)
        {
            // TODO: pass some dummy memory
            unimplemented("no_slot: binding on large return type");
        }

        else if(dst_slot.handle == RV_IR)
        {
            // this is nested pass in the current hidden return
            if(func.sig.hidden_args == 1)
            {
                arg_clean++;
                push_arg(itl,func,func.sig.args[0]);
            }

            else
            {
                panic(itl,itl_error::missing_return,"Attempted to return invalid large var inside func");
                return arg_clean;
            }
        }

        else if(is_special_reg(dst_slot))
        {
            print_slot(itl.symbol_table,dst_slot);
            dump_ir_sym(itl,func,itl.symbol_table);   
            assert(false);
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

            alloc_slot(itl,func,dst_slot,true);
            
            const SymSlot addr = addrof_res(itl,func,dst_slot);
            push_arg(itl,func,addr);
        }
    }

    return arg_clean;    
}

struct FuncCall
{
    FuncSig sig = {};
    String name = {};

    union
    {
        LabelSlot label_slot;
        SymSlot sym_slot;
    };

    // tag for above union
    b32 func_pointer = false;
};

Type* handle_call(Interloper& itl, Function& func, const FuncCall& call_info, SymSlot dst_slot, u32 arg_clean)
{
    auto& sig = call_info.sig;

    // NOTE: func struct will hold a void value if it has nothing
    const bool returns_value = sig.return_type[0]->type_idx != u32(builtin_type::void_t);



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
        call_reg(itl,func,call_info.sym_slot);
    }


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
    if(returns_value && dst_slot.handle != NO_SLOT && !sig.hidden_args)
    {
        compile_move(itl,func,dst_slot,sym_from_idx(RV_IR),sig.return_type[0],sig.return_type[0]);
    }
    

    // give back the function's return type

    if(count(sig.return_type) == 1)
    {
        // result of expr is the return type
        return sig.return_type[0];
    }

    // tuple 
    else
    {
        return make_raw(itl,TUPLE);
    }    
}

FuncCall get_calling_sig(Interloper& itl,const String& name_space,Function& func,FuncCallNode* call_node,TupleAssignNode* tuple_node)
{
    UNUSED(name_space);

    FuncCall call_info;

    AstNode* expr = call_node->expr;

    // just a plain literal
    if(expr->fmt == ast_fmt::literal)
    {
        const LiteralNode* literal_node = (LiteralNode*)expr;
        const String& name = literal_node->literal;

        // are we looking in the global namespace or no?
        const b32 global = name_space == "";

        FunctionDef* func_call_def = global? lookup_func_def_default(itl,name) : lookup_func_def_scope(itl,name,name_space);

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

                    call_info.sym_slot = sym.reg.slot;
                    call_info.sig = func_type->sig;
                    call_info.name = sym.name;
                    call_info.func_pointer = true;
                }

                else
                {
                    panic(itl,itl_error::undeclared,"[COMPILE]: symbol %s is not a function pointer",name.buf);
                    return {};         
                }
            }

            else
            {
                if(name_space == "")
                {
                    panic(itl,itl_error::undeclared,"[COMPILE]: function %s is not declared\n",name.buf);
                }

                else
                {
                    panic(itl,itl_error::undeclared,"[COMPILE]: function %s::%s is not declared\n",name_space.buf,name.buf);
                }

                return {};
            }
        }

        else
        {
            auto func_call_opt = finalise_func(itl,*func_call_def,(AstNode*)call_node);

            if(!func_call_opt)
            {
                return {};
            }

            auto& func_call = *func_call_opt;

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
        auto [type, slot] = compile_oper(itl,func,expr);

        if(!is_func_pointer(type))
        {
            panic(itl,itl_error::undeclared,"[COMPILE]: expression of type %s is not callable",type_name(itl,type).buf);
            return {};
        }

        FuncPointerType* func_type = (FuncPointerType*)type;

        call_info.sym_slot = slot;
        call_info.sig = func_type->sig;
        call_info.name = "call_expr";
        call_info.func_pointer = true;
    }

    //print_func_decl(itl,func_call);

    
    // check calls on functions with multiple returns are valid
    if(tuple_node && count(call_info.sig.return_type) == 1)
    {
        panic(itl,itl_error::tuple_mismatch,"attempted to bind %d return values on function with single return\n",count(tuple_node->symbols));
        return {};
    }

    if(count(call_info.sig.return_type) > 1)
    {
        if(!tuple_node)
        {
            panic(itl,itl_error::tuple_mismatch,"Attempted to call multiple return function nested in a expression\n");
            return {};
        }

        if(count(call_info.sig.return_type) != count(tuple_node->symbols))
        {
            panic(itl,itl_error::tuple_mismatch,"Numbers of smybols binded for multiple return does not match function: %d != %d\n",
                count(tuple_node->symbols),count(call_node->args));
            return {};
        }
    }

    return call_info;   
}

void handle_tuple_decl(Interloper& itl,Function& func, TupleAssignNode* tuple_node,const FuncSig& sig)
{
    b32 new_decl = false;

    // go thru each var check the type matches exactly
    for(u32 s = 0; s < count(tuple_node->symbols); s++)
    {
        AstNode* node = tuple_node->symbols[s];

        // get ltype and handle any decl
        if(node->type == ast_type::symbol)
        {
            LiteralNode* literal_node = (LiteralNode*)node;
            const auto& name = literal_node->literal;

            // no such symbol, infer the type
            if(!symbol_exists(itl.symbol_table,name))
            {
                // add new symbol table entry with return type
                auto& sym = add_symbol(itl,name,sig.return_type[s]);
                alloc_slot(itl,func,sym.reg.slot,!is_plain_type(sym.type));

                new_decl = true;
            }
        }
    }
    
    if(!new_decl)
    {
        panic(itl,itl_error::tuple_mismatch,"No new variables declared in tuple assign");
    }
}

// used for both tuples and ordinary function calls
Type* compile_function_call(Interloper &itl,const String& name_space,Function &func,AstNode *node, SymSlot dst_slot)
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
    const auto call_info = get_calling_sig(itl,name_space,func,call_node,tuple_node);
    auto& sig = call_info.sig;

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }


    if(tuple_node && tuple_node->auto_decl)
    {
        handle_tuple_decl(itl,func,tuple_node,sig);
    }

    
    // handle argument pushing
    const s32 hidden_args = sig.hidden_args;

    // how many args are we pushing to the stack?
    u32 arg_clean = 0;


    u32 start_arg = count(sig.args) - 1;

    const u32 actual_args = count(sig.args) - hidden_args;

    if(sig.va_args)
    {
        arg_clean += push_va_args(itl,func,call_node,call_info.name,actual_args);

        // skip over our va_args
        start_arg = actual_args - 2;
    }

    // normal call
    else
    {
        // check we have the right number of params
        if(actual_args != count(call_node->args))
        {
            panic(itl,itl_error::missing_args,"[COMPILE]: function call expected %d args got %d\n",actual_args,count(call_node->args));
            return make_builtin(itl,builtin_type::void_t);
        }        
    }

    arg_clean += push_args(itl,func,call_node,sig,start_arg);

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }

    // push hidden args 

    if(hidden_args)
    {
        arg_clean += push_hidden_args(itl,func,tuple_node,dst_slot);
    }


    // handle calling and returns
    return handle_call(itl,func,call_info,dst_slot,arg_clean);
}

void parse_func_sig(Interloper& itl,FuncSig& sig,const FuncNode& node)
{
    // about to move to a different context
    push_context(itl);

    trash_context(itl,node.filename,node.name_space,(AstNode*)&node);

    u32 arg_offset = 0;

    // NOTE: void return's will have a void type
    if(count(node.return_type) == 1)
    {
        itl.ctx.expr = (AstNode*)node.return_type[0];

        push_var(sig.return_type,get_complete_type(itl,node.return_type[0]));

        // we are returning a struct add a hidden pointer as first arg
        // TODO: if the struct is small enough we should not return it in this manner
        if(type_size(itl,sig.return_type[0]) > GPR_SIZE || is_struct(sig.return_type[0]))
        {
            add_hidden_return(itl,sig,"_struct_ret_ptr",sig.return_type[0],&arg_offset);
        }   
    }

    // tuple return
    else if(count(node.return_type) > 1)
    {
        for(u32 a = 0; a < count(node.return_type); a++)
        {
            itl.ctx.expr = (AstNode*)node.return_type[a];

            push_var(sig.return_type,get_complete_type(itl,node.return_type[a]));

            char name[40] = {0};
            sprintf(name,"_tuple_ret_0x%x",a);

            add_hidden_return(itl,sig,name,sig.return_type[a],&arg_offset);
        }
    }


    const auto decl = node.args;

    // rip every arg
    for(u32 i = 0; i < count(decl); i++)
    {
        const auto a = decl[i];
        itl.ctx.expr = (AstNode*)a;

        const auto name = a->name;
        const auto type = get_complete_type(itl,a->type);


        // add the var to slot lookup and link to function
        // we will do a add_scope to put it into the scope later
        Symbol sym = make_sym(itl,name,type,arg_offset);
        add_var(itl.symbol_table,sym);

        push_var(sig.args,sym.reg.slot);

        const u32 size = type_size(itl,type);

        // if size is below GPR just make it take that much
        const u32 arg_size = promote_size(size);

        arg_offset += arg_size;

        //printf("arg slot %s: %d : %d\n",sym.name.buf,sym.slot, args[args.size()-1]);
    }

    // add va args
    if(node.va_args && itl.rtti_enable)
    {
        Type* type = make_struct(itl,itl.rtti_cache.any_idx,true);
        Type* array_type = make_array(itl,type,RUNTIME_SIZE);

        Symbol sym = make_sym(itl,node.args_name,array_type,arg_offset);
        add_var(itl.symbol_table,sym);

        push_var(sig.args,sym.reg.slot);

        sig.va_args = true;
    }

    pop_context(itl);
}



void compile_function(Interloper& itl, Function& func)
{
    push_generic_type_alias(itl,func);

    // NOTE: for compiler generated functions we dont want to 
    // compile this via a standard node
    if(func.root)
    {
        const auto &node = *func.root;

        // inside a new function we dont care about the old context
        // so just trash it
        trash_context(itl,node.filename,node.name_space,(AstNode*)func.root);
        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        new_scope(itl.symbol_table);


        // put each arg into scope
        for(u32 a = 0; a < count(func.sig.args); a++)
        {
            const SymSlot slot = func.sig.args[a];

            auto &sym = sym_from_slot(itl.symbol_table,slot);
            add_scope(itl.symbol_table,sym);
        }



        // parse out each line of the function
        compile_basic_block(itl,func,node.block);

        destroy_scope(itl.symbol_table);

        if(itl.error)
        {
            return;
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
    if(func.sig.return_type[0]->type_idx == u32(builtin_type::void_t))
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
        panic(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s\n",label.name.buf);  
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
            panic(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s\n",label.name.buf);
        }
    }

    pop_generic_type_alias(itl,func);
}

void compile_functions(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        compile_function(itl,func);
    }
}
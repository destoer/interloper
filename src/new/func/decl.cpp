void print_func_decl(Interloper& itl,const Function &func);
Option<itl_error> type_check_block(Interloper& itl,Function& func, AstBlock &block);

FunctionTable make_func_table()
{
    FunctionTable func_table;
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

    destroy_arr(func_table.table);
    destroy_arr(func_table.used);
    destroy_allocator(func_table.arena);
}

void add_func(Interloper& itl, const String& name, NameSpace* name_space, FuncNode* root)
{
    FunctionDef func_def;

    // Make sure our function is not allocated on the
    // same string allocator as the AST
    func_def.name = alloc_name_space_name(itl.string_allocator,name_space->full_name,name);
    func_def.root = root;
    func_def.func = nullptr;
    func_def.name_space = name_space;
    
    const auto handle = count(itl.func_table.table);
    push_var(itl.func_table.table,func_def);

    const DefInfo info = {definition_type::function,handle};
    add(name_space->table,copy_string(itl.string_allocator,name), info);  
}

Result<Function*,itl_error> finalise_func(Interloper& itl, FunctionDef& func_def)
{
    // have finalised this func
    if(func_def.func)
    {
        return func_def.func;
    }

    Function func;
    func.name = func_def.name;
    func.root = func_def.root;
    func.name_space = func_def.name_space;

    // parse in function signature on demand
    if(func.root)
    {
        const auto sig_err = parse_func_sig(itl,func_def.name_space,func.sig,*func.root,func_sig_kind::function);
        if(sig_err)
        {
            return *sig_err;
        }
        
        if(func_def.root)
        {
            func.sig.attr_flags = func_def.root->attr_flags;
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

    // Cache the call information for the func
    func.call_info.label_slot = func.label_slot;
    func.call_info.sig = func.sig;
    func.call_info.name = func.name;
    func.call_info.func_pointer = false;

    // add the actual func
    *func_def.func = func;

    // mark it used
    push_var(itl.func_table.used,func_def.func);

    if(func.root)
    {
        const auto block_err = type_check_block(itl,func,func.root->block);

        if(block_err)
        {
            return *block_err;
        }
    }

    return func_def.func;   
}
 
Function& create_dummy_func(Interloper& itl, const String& name)
{
    add_func(itl,name,itl.global_namespace,nullptr);

    FunctionDef& func_def = *lookup_func_def_global(itl,name);
    
    if(!finalise_func(itl,func_def))
    {
        assert(false);
    }

    // get its new home
    return lookup_internal_function(itl,name);
}

b32 func_exists(Interloper& itl, const String& name, NameSpace* name_space)
{
    return lookup_func_def_scope(itl,name_space,name) != nullptr;
}

Option<itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space)
{
    auto def_opt = lookup_func_def_scope(itl,name_space,name);

    // ensure the entry functions are defined
    if(!def_opt)
    {
        return compile_error(itl,itl_error::undeclared,"%S is not defined!",name);
    }

    const auto func_res = finalise_func(itl,*def_opt);
    if(!func_res)
    {
        return func_res.error();
    }

    return option::none;    
}

Function* lookup_opt_scoped_function(Interloper& itl, NameSpace* name_space, const String& name)
{
    FunctionDef* func_def_opt = lookup_func_def_scope(itl,name_space, name);

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
    Function* func_opt = lookup_opt_scoped_function(itl,itl.global_namespace,name);

    // assert this just for saftey
    // functions looked up by this should never be missing due to startup checks
    assert(func_opt);

    return *func_opt;
}

// #include "intrin.cpp"


void print_func_sig(Interloper& itl, const FuncSig& sig)
{
    printf("arg count: %d\n",count(sig.args));
    printf("hidden args: %d\n",sig.hidden_args);
    printf("va args: %s\n",sig.va_args? "true" : "false");

    for(u32 a = 0; a < count(sig.args); a++)
    {
        const SymSlot slot = sig.args[a];
        auto &sym = sym_from_slot(itl.symbol_table,slot);

        print(itl,sym); 
        putchar('\n');
    }

    for(u32 r = 0; r < count(sig.return_type); r++)
    {
        printf("return type %s\n",type_name(itl,sig.return_type[r]).buf);
    }

    printf("\n\n");
}

void print_func_decl(Interloper& itl,const Function &func)
{
    printf("func: %s\n\n",func.name.buf);
    print_func_sig(itl,func.sig);
}


void add_sig_arg(Interloper& itl, FuncSig& sig, const String& name, Type* type, u32* arg_offset)
{
    if(is_trivial_copy(type) && !is_float(type) && count(sig.args) < 2)
    {
        Symbol sym = make_sym(itl,name,type);
        add_var(itl.symbol_table,sym);
        push_var(sig.args,sym.reg.slot.sym_slot);

        push_var(sig.pass_as_reg,sig.max_reg_pass);

        const u32 location = special_reg_to_reg(itl.arch,spec_reg(SPECIAL_REG_ARG_START + sig.max_reg_pass));

        sig.locked_set = set_bit(sig.locked_set,location);

        sig.max_reg_pass += 1;
    }   

    else
    {
        Symbol sym = make_sym(itl,name,type,*arg_offset);
        add_var(itl.symbol_table,sym);

        push_var(sig.args,sym.reg.slot.sym_slot);

        push_var(sig.pass_as_reg,NON_ARG);
        *arg_offset += promote_size(type_size(itl,type));
    }
}

// add hidden arg pointers for return
void add_hidden_return(Interloper& itl, FuncSig& sig, const String& name, Type* return_type, u32* arg_offset)
{
    Type* ptr_type = make_reference(itl,return_type);

    add_sig_arg(itl,sig,name,ptr_type,arg_offset);
    sig.hidden_args++;
}



Option<itl_error> parse_func_sig(Interloper& itl,NameSpace* name_space,FuncSig& sig,const FuncNode& node, func_sig_kind kind)
{
    // about to move to a different context
    auto context_guard = switch_context(itl,node.filename,name_space,(AstNode*)&node);

    u32 arg_offset = 0;

    // NOTE: void return's will have a void type
    if(count(node.return_type) == 1)
    {
        itl.ctx.expr = (AstNode*)node.return_type[0];
        auto type_res = get_complete_type(itl,node.return_type[0]);
        if(!type_res)
        {
            return type_res.error();
        }

        push_var(sig.return_type,*type_res);

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
            auto type_res = get_complete_type(itl,node.return_type[a]);
            if(!type_res)
            {
                return type_res.error();    
            }

            push_var(sig.return_type,*type_res);

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
        const auto type_res = get_complete_type(itl,a->type);
        if(!type_res)
        {
            return type_res.error();
        }

        add_sig_arg(itl,sig,name,*type_res,&arg_offset);
    }

    // add va args
    if(node.va_args && itl.rtti_enable)
    {
        Type* type = make_struct(itl,itl.rtti_cache.any_idx,true);
        Type* array_type = make_array(itl,type,RUNTIME_SIZE);

        add_sig_arg(itl,sig,node.args_name,array_type,&arg_offset);
        sig.va_args = true;
    }

    // If this is just a function pointer definition we need
    // To make sure these are referenced.
    if(kind == func_sig_kind::function_pointer)
    {
        for(auto& slot : sig.args)
        {
            auto& sym = sym_from_slot(itl.symbol_table,slot);
            sym.references += 1;
        }
    }

    sig.call_stack_size = arg_offset;

    return option::none;
}

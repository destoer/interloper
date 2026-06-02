#include "destoer/destoer.h"
#include "ir.h"
#include "parser.h"
#include "sym.h"
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
    for(auto& def : func_table.table)
    {
        destroy_arr(def.generic_overload);
    }

    for(auto& func_ptr : func_table.used)
    {
        auto& func = *func_ptr;
        destroy_func(func);
    }

    for(auto& func_ptr : func_table.unused)
    {
        auto& func = *func_ptr;
        destroy_func(func);
    }

    destroy_arr(func_table.table);
    destroy_arr(func_table.used);
    destroy_arr(func_table.unused);
    destroy_allocator(func_table.arena);
}

void add_func(Interloper& itl, const String& name, NameSpace* name_space, const Option<TopLevelDefinition>& parser_def)
{
    FunctionDef func_def;

    // Make sure our function is not allocated on the
    // same string allocator as the AST
    func_def.name = alloc_name_space_name(itl.string_allocator,name_space->full_name,name);
    if(parser_def)
    {
        func_def.parser_def = *parser_def;
    }
    
    func_def.root = nullptr;
    func_def.func = nullptr;
    func_def.name_space = name_space;
    
    const auto handle = count(itl.func_table.table);
    push_var(itl.func_table.table,func_def);

    const DefInfo info = {definition_type::function,handle};
    add(name_space->table,copy_string(itl.string_allocator,name), info);  
}

GenericScopeGuard switch_generic_context(Interloper& itl, const GenericOverload& overload)
{
    return GenericScopeGuard(itl.generic_overload,overload);
}

Option<itl_error> type_check_function(Interloper& itl, Function& func)
{
    const auto context_guard = switch_context(itl,func.root->filename,func.name_space,(AstNode*)func.root);
    const auto scope_guard = enter_new_anon_scope(itl.symbol_table);
    const auto generic_guard = switch_generic_context(itl,func.root->generic);

    push_var(itl.defer_stack,itl.cur_defer_node);
    itl.cur_defer_node = NULL;


    // put each arg into scope and copy it regs into args
    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];
        auto &sym = sym_from_slot(itl.symbol_table,slot);
        add_sym_to_scope(itl.symbol_table,sym);
    }

    const u32 error_count = itl.error_count;

    const auto result = type_check_block(itl,func,func.root->block);

    if(itl.error_count != error_count && func.from_generic)
    {
        print_itl(itl,"Error in generic function %S with substitution from:",func.root->name);

        const auto caller_filename = func.root->generic_call_filename;

        // Print line we are setup from
        const auto [line,col] = get_line_info(caller_filename,func.root->generic_call->node.idx);
        print_line(caller_filename,line);

        for(const auto& generic : func.root->generic)
        {
            print_itl(itl,"%S = %t",generic.name,generic.type);
        }

        putchar('\n');   
    }

    itl.cur_defer_node = pop(itl.defer_stack);

    return result;
}

TypeResult cut_generic_compound(Interloper& itl, Type* type, TypeNode* decl)
{
    for(const auto& compound : decl->compound)
    {
        switch(compound.type)
        {
            case compound_type::ptr:
            {
                if(!is_pointer(type))
                {
                    return compile_error(itl,itl_error::generic,"Generic type requires pointer got %t",type);
                }

                type = deref_pointer(type);
                break;
            }

            case compound_type::arr_var_size:
            {
                if(!is_array(type))
                {
                    return compile_error(itl,itl_error::generic,"Generic type requires variable sized array got %t",type);
                }

                type = index_arr(type);
                break;
            }

            default: unimplemented("Deduce generic for compound type: %s",COMPOUND_TYPE_NAMES[u32(compound.type)]);
        }
    }

    return type;
}

Result<Array<Generic>,itl_error> deduce_generic_types(Interloper& itl, FuncNode& node, FuncCallNode* func_call)
{
    assert(func_call);

    auto generic_overload = copy_array(node.generic);

    // TODO: This ideally needs caching.
    auto generic_lookup = make_table<String,Generic*>();

    for(u32 i = 0; i < count(generic_overload); i++)
    {
        const auto& generic = generic_overload[i];

        if(contains(generic_lookup,generic.name))
        {
            destroy_table(generic_lookup);
            destroy_arr(generic_overload);
            return compile_error(itl,itl_error::redeclaration,"Generic %S is declared twice",generic.name);
        }

        add(generic_lookup,generic.name,&generic_overload[i]);
    }

    // Deduce generic types
    for(u32 a = 0; a < count(node.args); a++)
    {
        DeclNode* decl = node.args[a];

        TypeNode* type_node = decl->type;

        if(type_node->kind != type_node_kind::generic)
        {
            continue;
        }

        AstNode* expr = func_call->args[a];

        const auto generic_opt = lookup(generic_lookup,type_node->name);

        if(!generic_opt)
        {
            const auto err = compile_error(itl,itl_error::undeclared,"Generic type %S does not exist",type_node->name);

            destroy_table(generic_lookup);
            destroy_arr(generic_overload);
            return err;
        }

        auto& generic = *generic_opt;

        auto deduced_type_res = cut_generic_compound(itl,expr->expr_type,decl->type);
        if(!deduced_type_res)
        {
            return deduced_type_res.error();
        }

        Type* deduced_type = *deduced_type_res;


        if(!generic->type)
        {
            generic->type = deduced_type;
        }

        if(type_equal(generic->type,deduced_type))
        {
            continue;
        }

        // Types are not equal attempt to promote them
        if(!(is_integer(generic->type) && is_integer(deduced_type)))
        {
            const auto err = compile_error(itl,itl_error::generic,"Mismatched generic types %t and %t",generic->type,deduced_type);

            destroy_table(generic_lookup);
            destroy_arr(generic_overload);
            return err;
        }
        
        const auto promotion_res = promote_integer_type(itl,generic->type,deduced_type);
        if(!promotion_res)
        {
            destroy_table(generic_lookup);
            destroy_arr(generic_overload);
            return promotion_res.error();
        }

        generic->type = *promotion_res;
    }

    destroy_table(generic_lookup);

    // Check constraints are met.
    for(auto& generic : generic_overload)
    {
        if(!generic.type)
        {
            const auto err = compile_error(itl,itl_error::generic,"Generic %S could not be deduced",generic.name);

            destroy_arr(generic_overload);
            return err;      
        }

        switch(generic.constraint)
        {
            case constraint_type::integer:
            {
                if(!is_integer(generic.type))
                {
                    const auto err = compile_error(itl,itl_error::generic,"Generic %S : %t does not meet constraint Integer",generic.name,generic.type);

                    destroy_arr(generic_overload);
                    return err;
                }

                break;
            }

            case constraint_type::real:
            {
                if(!is_integer(generic.type) && !is_float(generic.type))
                {
                    const auto err = compile_error(itl,itl_error::generic,"Generic %S : %t does not meet constraint Real",generic.name,generic.type);

                    destroy_arr(generic_overload);
                    return err;
                }

                break;
            }

            case constraint_type::sized:
            {
                if(type_size(itl,generic.type) == 0)
                {
                    const auto err = compile_error(itl,itl_error::generic,"Generic %S : %t is not sized",generic.name,generic.type);

                    destroy_arr(generic_overload);
                    return err;
                }

                break;
            }
        }
    }

    return generic_overload;
}

bool check_overload(const Function* func, const Array<Generic>& generic_overload)
{
    for(u32 a = 0; a < count(generic_overload); a++)
    {
        if(!type_equal(generic_overload[a].type,func->root->generic[a].type))
        {
            return false;
        }
    }

    return true;
}

Function* find_overload(const OverloadTable& overload, const Array<Generic>& generic_overload)
{
    for(Function* func : overload)
    {
        if(check_overload(func,generic_overload))
        {
            return func;
        }
    }

    return nullptr;

}

Result<Function*,itl_error> finalise_func(Interloper& itl, FunctionDef& func_def, FuncCallNode* func_call, bool forced)
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

    // Handle generic functions.
    if(func.root && func.root->generic)
    {
        // This is a forced type check as we have no calling context we do actually have an instantiation of the function.
        if(forced)
        {
            return nullptr;
        }

        if(!func_call)
        {
            return compile_error(itl,itl_error::invalid_statement,"Generic function call %S has no calling context.",func_def.name);
        }

        func.from_generic = true;

        const auto res = deduce_generic_types(itl,*func.root,func_call);
        if(!res)
        {
            return res.error();
        }

        Array<Generic> generic_overload = *res;

        // Scan and see if overload already exists
        if(func_def.generic_overload)
        {
            Function* func = find_overload(func_def.generic_overload,generic_overload);

            if(func)
            {
                destroy_arr(generic_overload);
                return func;
            }
        }

        func.root = (FuncNode*)copy_ast(itl,(AstNode*)func.root);

        func.root->generic = generic_overload;
        add_copy_data_pointer(itl,&func.root->generic.data);

        func.root->generic_call = func_call;
        func.root->generic_call_filename = itl.ctx.filename;


        const auto sig_err = parse_func_sig(itl,func_def.name_space,func.sig,*func.root,func_sig_kind::generic);
        if(sig_err)
        {
            destroy_func(func);
            return *sig_err;
        } 

        func.root->attr = func.sig.attribute;

        char overload_name[64] = {0};
        sprintf(overload_name,"__generic_0x%x",count(func_def.generic_overload));

        // TODO: Generate a full generic name
        func.name = cat_string(itl.string_allocator,func.name,overload_name); 
    }

    // parse in function signature on demand
    else if(func.root)
    {
        const auto sig_err = parse_func_sig(itl,func_def.name_space,func.sig,*func.root,func_sig_kind::function);
        if(sig_err)
        {
            return *sig_err;
        }

        func.root->attr = func.sig.attribute;
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
    Function* func_ptr = (Function*)allocate(itl.func_table.arena,sizeof(Function));

    // Cache the call information for the func
    func.call_info.label_slot = func.label_slot;
    func.call_info.sig = func.sig;
    func.call_info.name = func.name;
    func.call_info.flags = 0;

    *func_ptr = func;

    if(func.from_generic)
    {
        // Mark this overload as instanced
        push_var(func_def.generic_overload,func_ptr);
    }

    else
    {
        // If this is an ordinary function mark it as concrete
        func_def.func = func_ptr;
    }

    if(!forced)
    {
        // mark it used
        push_var(itl.func_table.used,func_ptr);
    }

    // Type checked only delete later after checking done
    else
    {
        push_var(itl.func_table.unused,func_ptr);
    }

    if(func.root)
    {
        const auto block_err = type_check_function(itl,func);
        if(block_err)
        {
            return *block_err;
        }
    }

    return func_ptr;   
}
 
Function* create_dummy_func(Interloper& itl, const String& name)
{
    add_func(itl,name,itl.global_namespace,option::none);

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

Result<Function*,itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space)
{
    auto def_opt = lookup_func_def_scope(itl,name_space,name);

    // ensure the entry functions are defined
    if(!def_opt)
    {
        return compile_error(itl,itl_error::undeclared,"Startup function %S is not defined!",name);
    }

    return finalise_func(itl,*def_opt);
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

Function* lookup_internal_function(Interloper& itl, const String& name)
{
    Function* func_opt = lookup_opt_scoped_function(itl,itl.global_namespace,name);

    // assert this just for safety
    // functions looked up by this should never be missing due to startup checks
    assert(func_opt);

    return func_opt;
}



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
        Symbol sym = make_sym_arg(itl,name,type,*arg_offset);
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
    const auto context_guard = switch_context(itl,node.filename,name_space,(AstNode*)&node);
    const auto generic_guard = switch_generic_context(itl,node.generic);
    sig.attribute = node.attr;

    u32 arg_offset = 0;

    // NOTE: void return's will have a void type
    if(count(node.return_type) == 1)
    {
        itl.ctx.expr = (AstNode*)node.return_type[0].type;
        auto type_res = get_complete_type(itl,node.return_type[0].type);
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
            itl.ctx.expr = (AstNode*)node.return_type[a].type;
            auto type_res = get_complete_type(itl,node.return_type[a].type);
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
    HashTable<String,u32> name_to_arg = make_table<String,u32>();

    // rip every arg
    for(u32 i = 0; i < count(decl); i++)
    {
        const auto a = decl[i];
        itl.ctx.expr = (AstNode*)a;

        const auto name = a->sym.name;
        if(contains(name_to_arg,name))
        {
            destroy_table(name_to_arg);
            return compile_error(itl,itl_error::redeclaration,"Arg %S has been declared twice",name);
        }

        add(name_to_arg,name,i);


        const auto type_res = get_complete_type(itl,a->type);
        if(!type_res)
        {
            destroy_table(name_to_arg);
            return type_res.error();
        }

        add_sig_arg(itl,sig,name,*type_res,&arg_offset);
    }

    for(auto& attr : sig.attribute.attr)
    {
        switch(attr.type)
        {
            case attr_type::fmt_t:
            {
                const auto arg_opt = lookup(name_to_arg,attr.var_name);
                if(!arg_opt)
                {
                    destroy_table(name_to_arg);
                    return compile_error(itl,itl_error::invalid_statement,"Could not find attr name in sig %S",attr.var_name);
                }

                attr.resolved_idx = *arg_opt;
                break;
            }
        }
    }

    destroy_table(name_to_arg);


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

ConstSpan<SymSlot> sig_user_span(const FuncSig& sig)
{
    const u32 user_arg_len = count(sig.args) - (u32(sig.va_args) + sig.hidden_args);
    return make_const_span(sig.args, sig.hidden_args, user_arg_len);    
}

ConstSpan<AstNode*> sig_any_span(const FuncSig& sig, const Array<AstNode*>& args)
{
    const u32 any_arg_len = count(sig.args) - (u32(sig.va_args) + sig.hidden_args);
    return make_const_span(args, any_arg_len, count(args) - any_arg_len);      
}
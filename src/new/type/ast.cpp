// TODO: Split this fairly immediately.

Option<itl_error> check_startup_func(Interloper& itl, const String& name, NameSpace* name_space);

Option<itl_error> check_startup_defs(Interloper& itl)
{   
    // if(itl.rtti_enable)
    // {
    //     const auto cache_err = cache_rtti_structs(itl);
    //     if(cache_err)
    //     {
    //         return cache_err;
    //     }
    // }

    // itl.std_name_space = find_name_space(itl,"std");

    // if(!itl.std_name_space)
    // {
    //     return compile_error(itl,itl_error::undeclared,"std namespace is not declared");
    // }

    const auto main_err = check_startup_func(itl,"main",itl.global_namespace);
    if(main_err)
    {
        return main_err;
    }

    const auto start_err = check_startup_func(itl,"start",itl.global_namespace);
    if(start_err)
    {
        return start_err;
    }

    // const auto memcpy_err = check_startup_func(itl,"memcpy",itl.std_name_space);
    // if(memcpy_err)
    // {
    //     return memcpy_err;
    // }
    
    // const auto zero_err = check_startup_func(itl,"zero_mem",itl.std_name_space);
    // if(zero_err)
    // {
    //     return zero_err;
    // }

    return option::none;
}

Option<itl_error> type_check_block(Interloper& itl, AstBlock& block)
{
    UNUSED(itl); UNUSED(block);

    assert(false);
}


Option<itl_error> type_check_ast(Interloper& itl)
{
    // Forcibly check startup funcs
    // This will trigger type checking of further functions
    const auto startup_err = check_startup_defs(itl);
    if(startup_err)
    {
        return startup_err;
    }

    // Type check globals

    assert(false);
}
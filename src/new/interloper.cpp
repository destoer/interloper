
RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_data(Interloper& itl,Function& func,const TypedReg& reg);
RegSlot load_arr_len(Interloper& itl,Function& func,const TypedReg& reg);

#include "parser/lexer.cpp"
#include "namespace.cpp"
#include "symbol.cpp"
#include "ir.cpp"
#include "elf.cpp"
#include "memory.cpp"
#include "array.cpp"
#include "parser.cpp"
#include "func.cpp"


void print_ast(Interloper& itl)
{
    // print type defs
    for(u32 t = 0; t < count(itl.type_decl); t++)
    {
        print(itl.type_decl[t]);
    }

    // print function defs
    for(u32 f = 0; f < count(itl.func_table.table); f++)
    {
        auto& func = itl.func_table.table[f];
        print((AstNode*)func.root);    
    }    
}

Option<parse_error> parsing(Interloper& itl, const String& initial_filename)
{
    // parse intial input file
    auto start = std::chrono::high_resolution_clock::now();

    // build ast
    const auto parse_err = parse(itl,initial_filename);
    if(!!parse_err)
    {
        if(itl.print_ast)
        {
            print_ast(itl);
        }

        // flag as generic parser error
        if(itl.error_count == 0)
        {
            itl.error_count = std::max(u32(1),itl.error_count);
            itl.first_error_code = *parse_err == parse_error::lexer_error? itl_error::lexer_error : itl_error::parse_error;
        }

        destroy_itl(itl);
        return parse_err;
    }

    auto end = std::chrono::high_resolution_clock::now();

    itl.parsing_time = std::chrono::duration<double, std::milli>(end-start).count();

    if(itl.print_ast)
    {
        print_ast(itl);
    }

    return option::none;
}

Option<itl_error> compile(Interloper &itl,const String& initial_filename, const String& executable_path)
{
    printf("compiling file: %s\n",initial_filename.buf);

    itl.first_error_code = itl_error::unimplemented;
    itl.error_count = 0;

    itl.ast_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);
    itl.ast_string_allocator = make_allocator(STRING_INITIAL_SIZE);

    itl.string_allocator = make_allocator(STRING_INITIAL_SIZE);
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.type_allocator = make_allocator(TYPE_INITIAL_SIZE);
    itl.namespace_allocator = make_allocator(2 * 1024);

    itl.symbol_table.string_allocator = &itl.string_allocator;
    itl.symbol_table.namespace_allocator = &itl.namespace_allocator;
    itl.symbol_table.ctx = &itl.ctx;

    itl.func_table = make_func_table();

    setup_namespace(itl);

    setup_type_table(itl);
    declare_compiler_type_aliases(itl);

    const auto parse_err = parsing(itl,initial_filename);
    if(!!parse_err)
    {
        puts("Parsing error");
        destroy_itl(itl);
        return itl_error::parse_error;
    }

    assert(false);

    return option::none;
}
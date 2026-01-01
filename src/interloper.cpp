
RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_data(Interloper& itl,Function& func,const TypedReg& reg);
RegSlot load_arr_len(Interloper& itl,Function& func,const TypedReg& reg);

Option<itl_error> type_check_init_expr(Interloper& itl, Type* ltype, AstNode* expr);

#include "itl/context.cpp"

#include "parser/lexer.cpp"
#include "namespace.cpp"
#include "symbol.cpp"
#include "backend.cpp"
#include "array.cpp"
#include "parser.cpp"
#include "func/decl.cpp"
#include "struct.cpp"
#include "enum.cpp"
#include "rtti.cpp"
#include "constant.cpp"
#include "control_flow.cpp"
#include "ast_info.inl"


void setup_parser_allocator(Interloper& itl)
{
    itl.parser_alloc.ast_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);
    itl.parser_alloc.string_allocator = make_allocator(AST_STRING_INITIAL_SIZE);
    itl.parser_alloc.global_string_allocator = &itl.string_allocator;
    itl.parser_alloc.namespace_allocator = &itl.namespace_allocator;
}

void destory_parser_allocator(ParserAllocator& alloc)
{
    for(void** array_ptr : alloc.ast_arrays)
    {
       free(*array_ptr);
    }

    destroy_arr(alloc.ast_arrays);

    destroy_allocator(alloc.ast_allocator);
    destroy_allocator(alloc.string_allocator);
}

void destroy_file_tokens(Interloper& itl)
{
    for(auto& token_arr : itl.file_tokens)
    {
        destroy_arr(token_arr);
    }

    destroy_arr(itl.file_tokens);    
}

void destroy_ast(Interloper& itl)
{
    destory_parser_allocator(itl.parser_alloc);
    destroy_file_tokens(itl);

    destroy_arr(itl.global_decl);
    destroy_arr(itl.constant_decl);
    destroy_arr(itl.saved_ctx);

    itl.ctx.expr = nullptr;
    itl.ctx.filename = ""; 
    itl.ctx.name_space = nullptr;

    destroy_parser(itl.parser);
}



static constexpr u32 LIST_INITIAL_SIZE = 16 * 1024;
static constexpr u32 STRING_INITIAL_SIZE = 4 * 1024;
static constexpr u32 TYPE_INITIAL_SIZE =  4 * 1024;

void setup_type_table(Interloper& itl)
{
    // add all the builtin types  
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        add_internal_type_decl(itl,i,TYPE_NAMES[i],type_kind::builtin);
    }

    itl.usize_type = make_builtin(itl,builtin_type::u64_t);
    itl.const_usize_type = make_builtin(itl,builtin_type::u64_t,true);
    
    itl.ssize_type = make_builtin(itl,builtin_type::s64_t);

    itl.void_type = make_builtin(itl,builtin_type::void_t);

}

void destroy_itl(Interloper &itl)
{
    destroy_asm_emitter(itl.asm_emitter);
    destroy_arr(itl.program);
    destroy_const_pool(itl.const_pool);
    destroy_sym_table(itl.symbol_table);
    destroy_namespace_tree(itl);
    destroy_arr(itl.type_decl);
    destroy_arr(itl.constant_decl);
    destroy_arr(itl.global_decl);
    destroy_arr(itl.global_alloc.array_allocation);
    
    destroy_ast(itl);

    destroy_func_table(itl.func_table);

    // destroy typing tables
    // destroy_struct_table(itl.struct_table);
    // destroy_enum_table(itl.enum_table);
    destroy_arr(itl.alias_table);

    destroy_arr(itl.name_space_buffer);

    destroy_allocator(itl.list_allocator);
    destroy_allocator(itl.string_allocator);

    
    for(u32 p = 0; p < count(itl.func_pointer); p++)
    {
        destroy_sig(*itl.func_pointer[p]);
    }

    destroy_arr(itl.func_pointer);

    // destroy_rtti_cache(itl.rtti_cache);

    destroy_allocator(itl.type_allocator);
}

void print_itl(Interloper& itl)
{
    // print type defs
    for(u32 t = 0; t < count(itl.type_decl); t++)
    {
        print(itl, itl.type_decl[t]->root);
    }

    // print function defs
    for(u32 f = 0; f < count(itl.func_table.table); f++)
    {
        auto& func = itl.func_table.table[f];
        print(itl, (AstNode*)func.root);    
    }    
}

Option<parse_error> parsing(Interloper& itl, const String& initial_filename)
{
    // parse initial input file
    auto start = std::chrono::high_resolution_clock::now();

    // build ast
    const auto parse_err = parse(itl,initial_filename);
    if(parse_err)
    {
        if(itl.print_ast)
        {
            print_itl(itl);
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
        print_itl(itl);
    }


    return option::none;
}

Option<itl_error> compile(Interloper &itl,const String& initial_filename, const String& executable_path)
{
    UNUSED(executable_path);
    printf("compiling file: %s\n",initial_filename.buf);

    itl.first_error_code = itl_error::unimplemented;
    itl.error_count = 0;

    itl.string_allocator = make_allocator(STRING_INITIAL_SIZE);
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.type_allocator = make_allocator(TYPE_INITIAL_SIZE);
    itl.namespace_allocator = make_allocator(2 * 1024);

    itl.symbol_table.string_allocator = &itl.string_allocator;
    itl.symbol_table.namespace_allocator = &itl.namespace_allocator;
    itl.symbol_table.ctx = &itl.ctx;

    itl.func_table = make_func_table();

    setup_parser_allocator(itl);

    setup_namespace(itl);

    setup_type_table(itl);
    declare_compiler_type_aliases(itl);

    itl.parser = make_parser(itl.global_namespace,&itl.parser_alloc);

    const auto parse_err = parsing(itl,initial_filename);
    if(parse_err)
    {
        puts("Parsing error");
        destroy_itl(itl);
        return itl_error::parse_error;
    }

    const auto type_check_err = type_check_ast(itl);
    if(type_check_err)
    {
        destroy_itl(itl);
        return *type_check_err;
    }

    if(itl.print_ast)
    {
        print_itl(itl);
    }
    

    const auto backend_err = backend(itl,executable_path);
    if(backend_err)
    {
        destroy_itl(itl);
        return *backend_err;
    }

    return option::none;
}
#pragma once
#include <lib.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>
#include <type.h>
#include <ir.h>
#include <interpretter.h>

struct Interloper
{
    Array<u8> program;

    b32 error;

    Parser parser;

    StructDefMap struct_def;

    AstNode *cur_expr = nullptr;
    String cur_file = "";


    HashTable<String,Function> function_table;
    Array<String> used_func;
    // did the last compiled function have a return
    b32 has_return;

    SymbolTable symbol_table;

    Array<PoolSection> pool_sections;
    Array<u8> const_pool;

    // Arena's
    ArenaAllocator list_allocator;
    ArenaAllocator ast_allocator;
    ArenaAllocator ast_string_allocator;

    AstPointers ast_arrays;
    
    // for longer lived strings, e.g func defs symbol names etc
    ArenaAllocator string_allocator;

    // allocating all things types!
    ArenaAllocator type_allocator;

    // Type lookup
    HashTable<String,TypeDecl> type_table;
    
    StructTable struct_table;
    EnumTable enum_table;

    // compilier config

    // TODO: make this be a flag
    b32 print_ast = false;
    b32 print_ir = false;

    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false; 

    b32 print_types = false;
};


void print(const AstNode *root);

inline void panic(Interloper &itl,const char *fmt, ...)
{
    if(itl.cur_expr)
    {
        const u32 line = itl.cur_expr->line;
        const u32 col = itl.cur_expr->col;
        const String filename = itl.cur_file;

        printf("error: %s %d:%d: ",filename.buf,line + 1,col + 1);


        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);

        print_line(filename,line+1);
    }

    else 
    {
        printf("error: ");

        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);
    }

    putchar('\n');
    
    itl.error = true;
}

u32 eval_int_expr(AstNode *node);
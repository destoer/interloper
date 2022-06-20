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

    Interpretter interpretter;

    Parser parser;

    // top level decl tree's
    AstNode *func_root = nullptr;

    StructDefMap struct_def;

    AstNode *cur_line = nullptr;

    FuncTable function_table;
    // did the last compiled function have a return
    b32 has_return;

    SymbolTable symbol_table;

    Array<u8> const_pool;

    // TODO: move other structures to an arena
    ArenaAllocator list_allocator;

    // struct lookup
    StructTable struct_table;


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
    printf("error: ");
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    
    itl.error = true;

    print(itl.cur_line);
}

std::string get_program_name(const std::string &filename);
u32 eval_const_expr(const AstNode *node);
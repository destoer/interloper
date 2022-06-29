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

    StructDefMap struct_def;

    AstNode *cur_expr = nullptr;
    std::string cur_file = "";


    FuncTable function_table;
    std::vector<std::string> used_func;
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
    if(itl.cur_expr)
    {
        const u32 line = itl.cur_expr->line;
        const u32 col = itl.cur_expr->col;
        const std::string filename = itl.cur_file;

        printf("error: %s %d:%d: ",filename.c_str(),line + 1,col + 1);


        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);

        // this is slow, but we are about to terminate anyways
        std::fstream fp{filename};

        if(!fp)
        {
            printf("could not open file %s for error printing\n",filename.c_str());
        }

        std::string str;
        for(u32 i = 0; i < line + 1; i++)
        {
            std::getline(fp,str);
        }

        printf("%s\n",str.c_str());
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

std::string get_program_name(const std::string &filename);
u32 eval_const_expr(const AstNode *node);
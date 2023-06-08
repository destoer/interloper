#pragma once
#include <destoer.h>
using namespace destoer;

#include <token.h>
#include <lexer.h>
#include <parser.h>
#include <type.h>
#include <ir.h>
#include <pool.h>
#include <interpretter.h>

enum class itl_error
{
    none,
    lexer_error,
    parse_error,
    array_type_error,
    int_type_error,
    bool_type_error,
    string_type_error,
    enum_type_error,
    pointer_type_error,
    out_of_bounds,
    undeclared,
    missing_initializer,
    missing_name,
    redeclaration,
    missing_args,
    tuple_mismatch,
    missing_return,
    invalid_expr,
    invalid_statement,
    mismatched_args,
    black_hole,
    struct_error,
    undefined_type_oper,
    const_type_error,
    rtti_error,
};

static const char* ERROR_NAME[] = 
{
    "none",
    "parse error",
    "lexer error",
    "array type error",
    "int type error",
    "bool type error",
    "string type error",
    "enum type error",
    "pointer type error",
    "out of bounds",
    "not declared",
    "missing initializer",
    "missing name",
    "redeclaration",
    "missing args",
    "tuple mismatch",
    "missing return",
    "invalid expr",
    "invalid statement",
    "mismatched args",
    "black hole",
    "struct error",
    "undefined type operation",
    "const type error",
    "rtti error",
};

struct Interloper
{
    Array<u8> program;

    b32 error;
    itl_error error_code;

    AstNode *cur_expr = nullptr;
    String cur_file = "";

    HashTable<String,Function> function_table;
    Array<DeclNode*> global_def;
    Array<String> used_func;
    // did the last compiled function have a return
    b32 has_return;

    SymbolTable symbol_table;

    ConstPool const_pool;

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

    // type definitions
    HashTable<String,TypeDef> type_def;


    StructTable struct_table;
    EnumTable enum_table;
    AliasTable alias_table;
    RttiCache rtti_cache;

    // compilier config

    // diagnostic
    b32 print_ast = false;
    b32 print_ir = false;
    b32 print_tokens = false;
    
    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false; 

    b32 print_types = false;

    // compiler options
    b32 rtti_enable = true;
};


void print(const AstNode *root);

inline void panic(Interloper &itl,itl_error error,const char *fmt, ...)
{
    // dont bother reporting multiple error's
    if(itl.error)
    {
        return;
    }

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
    itl.error_code = error;
}

void itl_warning(const char* fmt, ...)
{
    printf("warning: ");

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);

    putchar('\n');    
}

u32 eval_int_expr(AstNode *node);

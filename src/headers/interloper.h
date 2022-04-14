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
    // probably  needs to be moved to a uint8_t
    // when we have static data in the program
    std::vector<Opcode> program;

    b32 error;

    Interpretter interpretter;

    Lexer lexer;

    Parser parser;

    AstNode *root = nullptr;

    std::unordered_map<std::string, Function> function_table;
    // did the last compiled function have a return
    b32 has_return;

    SymbolTable symbol_table;

    // TODO: move other structures to an arena
    ArenaAllocator list_allocator;
};


template<typename... Args>
inline void panic(Interloper &itl,const char *fmt, Args... args)
{
    printf("error: ");
    printf(fmt,args...);
    itl.error = true;
}
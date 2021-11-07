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

    AstNode *root = nullptr;

    std::unordered_map<std::string, Function> function_table;
    // did the last compiled function have a return
    b32 has_return;

    SymbolTable symbol_table;
};

u32 convert_imm(const std::string &imm);
void optimise_ir(Interloper &itl);
void allocate_registers(Function &func, SlotLookup &slot_lookup);
void compile(Interloper &itl,const std::vector<std::string> &lines);
void emit_asm(Interloper &itl);

template<typename... Args>
inline void panic(Interloper &itl,const char *fmt, Args... args)
{
    printf("error: ");
    printf(fmt,args...);
    itl.error = true;
}
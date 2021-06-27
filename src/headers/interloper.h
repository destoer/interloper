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
    Interloper();

    void compile(const std::vector<std::string> &lines);

    ~Interloper()
    {
        delete_tree(root);
    }

private:
    void parse_function_declarations();

    std::string type_name(const Type &type);

    void compile_functions();
    void compile_block(AstNode *node);
    void compile_expression(AstNode *node);
    void compile_arith_op(AstNode *node, op_type type);
    void emit_asm();


    Lexer lexer;
    Parser parser;

    // ir functions (defined in ir.cpp)
    void emit_ir(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);
    std::string get_ir_operand(uint32_t v);
    void dump_ir_sym();

    void allocate_registers();

    IrEmitter emitter;




    AstNode *root = nullptr;

    std::unordered_map<std::string, Function> function_table;

    SymbolTable symbol_table;


    // probably  needs to be moved to a uint8_t
    // when we have static data in the program
    std::vector<Opcode> program;
};

uint32_t convert_imm(const std::string &imm);
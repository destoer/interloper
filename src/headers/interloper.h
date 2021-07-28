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


     // probably  needs to be moved to a uint8_t
    // when we have static data in the program
    std::vector<Opcode> program;

    bool error;

private:

    template<typename... Args>
    void panic(const char *fmt, Args... args)
    {
        printf(fmt,args...);
        error = true;
    }


    void parse_function_declarations();

    

    // IR and assembly
    void compile_functions();

    void compile_block(Function &func, AstNode *node);
    Type compile_expression(Function &func,AstNode *node);
    Type compile_arith_op(Function &func, AstNode *node, op_type type);
    Type compile_function_call(Function &func,AstNode *node);

    void emit_asm();

    // ir functions (defined in ir.cpp)
    void emit_ir(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);
    std::string get_ir_operand(uint32_t v);

    void allocate_registers(Function &func);



    // typing
    std::string type_name(const Type &type);
    void check_assign(const Type &ltype, const Type &rtype);
    Type effective_arith_type(const Type &ltype, const Type &rtype);
    void handle_cast(IrEmitter &emitter, const Type &old_type, const Type &new_type);
    u32 type_size(const Type &type);


    Lexer lexer;
    Parser parser;

    AstNode *root = nullptr;

    std::unordered_map<std::string, Function> function_table;
    // did the last compiled function have a return
    bool has_return;

    SymbolTable symbol_table;
};

uint32_t convert_imm(const std::string &imm);
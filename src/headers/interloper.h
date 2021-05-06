#pragma once
#include <lib.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>
#include <type.h>

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

    Lexer lexer;
    Parser parser;

    AstNode *root = nullptr;

    std::unordered_map<std::string, Function> function_table;
    std::unordered_map<std::string, Symbol> symbol_table; 
};

uint32_t convert_imm(const std::string &imm);
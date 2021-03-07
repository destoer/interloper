#pragma once
#include <lib.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>

struct Interloper
{
    void compile(const std::vector<std::string> &lines);

    ~Interloper()
    {
        delete_tree(root);
    }

private:
    Lexer lexer;
    Parser parser;

    AstNode *root = nullptr;
};

int32_t convert_imm(const std::string &imm);
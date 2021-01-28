#pragma once
#include <lib.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>


struct Interloper
{
    void compile(const std::vector<std::string> &lines);
private:
    Lexer lexer;
    Parser parser;
};
#pragma once
#include <token.h>


struct Lexer
{
    u32 column;
    u32 row;
    std::vector<Token> tokens;
};
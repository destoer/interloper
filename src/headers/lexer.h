#pragma once
#include <token.h>


struct Lexer
{
    u32 column = 0;
    u32 row = 0;
    std::vector<Token> tokens;

    b32 in_comment = false;
};
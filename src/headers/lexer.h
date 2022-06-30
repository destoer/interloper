#pragma once
#include <token.h>


struct Lexer
{
    s32 column = 0;
    s32 row = 0;
    u32 idx;
    std::vector<Token> tokens;

    b32 in_comment = false;
};
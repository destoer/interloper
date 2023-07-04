#pragma once
#include <token.h>


struct Lexer
{
    s32 column = 0;
    s32 row = 0;
    u32 idx;
    Array<Token> tokens;

    ArenaAllocator* string_allocator;

    b32 in_comment = false;
    b32 error = false;
};
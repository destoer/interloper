#pragma once
#include <token.h>


struct Lexer
{
    u32 idx = 0;
    Array<Token> tokens;

    ArenaAllocator* string_allocator;

    b32 error = false;
};
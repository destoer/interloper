#pragma once
#include <token.h>
#include <alloc.h>


struct Lexer
{
    s32 column = 0;
    s32 row = 0;
    u32 idx;
    std::vector<Token> tokens;

    ArenaAllocator* string_allocator;

    b32 in_comment = false;
};
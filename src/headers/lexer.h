#pragma once
#include <token.h>


struct Lexer
{
    u32 idx = 0;
    Array<Token> tokens;

    ArenaAllocator* string_allocator;

    b32 error = false;
};


struct NameSpace;

struct ParserContext 
{
    String cur_file = "";
    NameSpace* cur_namespace = nullptr;
    NameSpace* global_namespace = nullptr;
    String cur_path = "";    
};

struct TopLevelDefiniton
{
    // Have the tokens been read out into the relevant structure?
    bool parsed = false;
    Span<Token> tokens;
    ParserContext context;
    u32 flags = 0;
};

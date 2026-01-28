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

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);
static constexpr u32 ATTR_USE_RESULT = (1 << 2);

enum class attr_type
{
    fmt_t,
};

struct Attribute
{
    union
    {
        String var_name = {};
    };

    u32 resolved_idx = 0;
    attr_type type = attr_type::fmt_t;
};

struct ParsedAttr
{
    u32 flags = 0;
    Array<Attribute> attr;
};

struct TopLevelDefinition
{
    // Have the tokens been read out into the relevant structure?
    bool parsed = false;
    Span<Token> tokens;
    ParserContext context;
    ParsedAttr attr;
};

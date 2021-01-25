#pragma once
#include <lib.h>

enum class token_type
{
    value,
    symbol, 
    
    semi_colon,

    left_c_brace,
    right_c_brace,

    left_paren,
    right_paren,

    equal,

    s32,

    func,
    ret,

    error,

    eof,

    // make getting enum size easy
    END 
};


static constexpr size_t TOKEN_SIZE = static_cast<size_t>(token_type::END)+1;
inline const char *TOKEN_NAMES[TOKEN_SIZE] =
{
    "value",
    "symbol",

    ";",

    "{",
    "}",

    "(",
    ")",

    "=",

    "s32",

    "func",
    "return",

    "error",

    "eof",

    // should not be used...
    "END"
};

struct Token
{

    Token() {}

    Token(token_type type)
    {
        this->type = type;
    }

    Token(token_type type, std::string literal)
    {
        this->type = type;
        this->literal = literal;
    }

    token_type type;
    std::string literal;
};

inline void print_tokens(const std::vector<Token> &tokens)
{
    for(const auto &t: tokens)
    {
        printf("type: %s\n",TOKEN_NAMES[static_cast<size_t>(t.type)]);
        printf("literal: %s\n\n",t.literal.c_str());
    }
}

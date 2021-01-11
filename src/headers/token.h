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

    s32,

    func,
    ret,

    error,

    // make getting enum size easy
    END 
};


static constexpr size_t TOKEN_SIZE = static_cast<size_t>(token_type::END);
inline const char *TOKEN_NAMES[TOKEN_SIZE] =
{
    "value",
    "symbol",

    ";",

    "{",
    "}",

    "(",
    ")",

    "s32",

    "func",
    "return",

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
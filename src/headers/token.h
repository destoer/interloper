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

    times,
    plus,
    minus,

    error,

    eof,

    // make getting enum size easy
    END 
};


struct TokInfo
{
    TokInfo(token_type t,const char *n,int32_t l) : type(t), name(n), lbp(l)
    {

    }

    token_type type;
    const char *name;
    int32_t lbp;
};

static constexpr size_t TOKEN_SIZE = static_cast<size_t>(token_type::END)+1;

static const TokInfo TOKEN_INFO[TOKEN_SIZE] = 
{
    {token_type::value,"value",0},
    {token_type::symbol,"symbol",0},

    {token_type::semi_colon,";",0},

    {token_type::left_c_brace,"{",-1},
    {token_type::right_c_brace,"}",-1},

    {token_type::left_paren,"(",-1},
    {token_type::right_paren,")",0},

    {token_type::equal,"=",2},

    {token_type::s32,"s32",-1},

    {token_type::func,"func",-1},
    {token_type::ret,"return",-1},

    {token_type::times,"*",13},
    {token_type::plus,"+",12},
    {token_type::minus,"-",12},

    {token_type::error,"error",-1},

    {token_type::eof,"eof",0},

    {token_type::END,"end",-1}
};

inline const char *tok_name(token_type t)
{
    return TOKEN_INFO[static_cast<size_t>(t)].name;
}

struct Token
{

    Token() {}

    Token(token_type type, const std::string &literal = "", uint32_t line = 0, uint32_t col = 0)
    {
        this->type = type;
        this->literal = literal;
        this->line = line;
        this->col = col;
    }


    friend bool operator == (const Token &t1, const Token &t2);
    friend bool operator != (const Token &t1, const Token &t2);


    uint32_t line;
    uint32_t col;

    token_type type;
    std::string literal;
};

inline bool operator == (const Token &t1, const Token &t2)
{
    return t1.type == t2.type && t1.literal == t2.literal;
}


inline bool operator != (const Token &t1, const Token &t2)
{
    return !operator==(t1,t2);
}


inline void print_tokens(const std::vector<Token> &tokens)
{
    for(const auto &t: tokens)
    {
        printf("type: %s\n",tok_name(t.type));
        printf("literal: %s\n\n",t.literal.c_str());
    }
}

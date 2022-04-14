#pragma once
#include <lib.h>

enum class token_type
{
    value,
    symbol, 
    
    true_t,
    false_t,

    semi_colon,

    left_c_brace,
    right_c_brace,

    left_paren,
    right_paren,

    comma,

    equal,
    plus_eq,
    minus_eq,
    times_eq,
    divide_eq,

    decl,

    u8,
    u16,
    u32,

    s8,
    s16,
    s32,

    bool_t,

    cast,

    func,
    ret,

    times,
    plus,
    minus,
    divide,
    mod,

    sl_brace,
    sr_brace,

    dot,

    shift_l,
    shift_r,

    deref,

    operator_and,
    bitwise_or,
    bitwise_not,
    bitwise_xor,    

    logical_or,
    logical_and,
    logical_not,
    logical_eq,
    logical_ne,
    logical_lt,
    logical_gt,
    logical_le,
    logical_ge,


    for_t,

    if_t,
    else_t,

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
    s32 lbp;
};

static constexpr size_t TOKEN_SIZE = static_cast<size_t>(token_type::END)+1;

static const TokInfo TOKEN_INFO[TOKEN_SIZE] = 
{
    {token_type::value,"value",0},
    {token_type::symbol,"symbol",0},

    {token_type::true_t,"true",0},
    {token_type::false_t,"false",0},

    {token_type::semi_colon,";",0},

    {token_type::left_c_brace,"{",-1},
    {token_type::right_c_brace,"}",0},

    {token_type::left_paren,"(",-1},
    {token_type::right_paren,")",0},

    {token_type::comma,",",0},

    {token_type::equal,"=",2},
    {token_type::plus_eq,"+=",2},
    {token_type::minus_eq,"-=",2},
    {token_type::times_eq,"*=",2},
    {token_type::divide_eq,"/=",2},


    {token_type::decl,"decl",-1},

    {token_type::u8,"u8",-1},
    {token_type::u16,"u16",-1},
    {token_type::u32,"u32",-1},

    {token_type::s8,"s8",-1},
    {token_type::s16,"s16",-1},
    {token_type::s32,"s32",-1},

    {token_type::bool_t,"bool",-1},

    {token_type::cast,"cast",-1},

    {token_type::func,"func",-1},
    {token_type::ret,"return",-1},

    {token_type::times,"*",21},
    {token_type::plus,"+",20},
    {token_type::minus,"-",20},
    {token_type::divide,"/",21},
    {token_type::mod,"%",21},

    {token_type::sl_brace,"[",30},
    {token_type::sr_brace,"]",0},

    {token_type::dot,".",-1},

    {token_type::shift_l,"<<",13},
    {token_type::shift_r,">>",13},

    {token_type::deref,"@",30},

    {token_type::operator_and,"&",10},
    {token_type::bitwise_or,"|",9},
    {token_type::bitwise_not,"~",-1}, // unary
    {token_type::bitwise_xor,"^",8},

    {token_type::logical_or,"||",6},
    {token_type::logical_and,"&&",5},
    {token_type::logical_not,"!",7},

    {token_type::logical_eq,"==",11},
    {token_type::logical_ne,"!=",11},

    {token_type::logical_lt,"<",12},
    {token_type::logical_gt,">",12},
    {token_type::logical_le,"<=",12},
    {token_type::logical_ge,">=",12},

    {token_type::for_t,"for",-1},

    {token_type::if_t,"if",-1},
    {token_type::else_t,"else",-1},

    



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

    Token(token_type type, const std::string &literal = "", u32 line = 0, u32 col = 0)
    {
        this->type = type;
        this->literal = literal;
        this->line = line;
        this->col = col;
    }


    friend bool operator == (const Token &t1, const Token &t2);
    friend bool operator != (const Token &t1, const Token &t2);


    u32 line = 0;
    u32 col = 0;

    token_type type = token_type::eof;
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

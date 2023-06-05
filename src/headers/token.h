#pragma once
#include <destoer.h>

enum class token_type
{
    value,
    symbol, 
    char_t,
    string,

    true_t,
    false_t,
    null_t,

    semi_colon,
    colon,

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

    const_t,

    u8,
    u16,
    u32,

    s8,
    s16,
    s32,

    byte_t,

    bool_t,

    cast,
    sizeof_t,
    type_alias,

    import,
    struct_t,
    enum_t,
    scope,

    func,
    ret,

    times,
    plus,
    minus,
    divide,
    mod,
    increment,
    decrement,

    sl_brace,
    sr_brace,

    dot,
    qmark,

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
    while_t,

    if_t,
    else_t,
    switch_t,
    case_t,
    default_t,

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
    {token_type::char_t,"char",0},
    {token_type::string,"string",0},

    {token_type::true_t,"true",0},
    {token_type::false_t,"false",0},
    {token_type::null_t,"NULL",0},

    {token_type::semi_colon,";",-1},
    {token_type::colon,":",-1},

    {token_type::left_c_brace,"{",-1},
    {token_type::right_c_brace,"}",-1},

    {token_type::left_paren,"(",-1},
    {token_type::right_paren,")",-1},

    {token_type::comma,",",-1},

    {token_type::equal,"=",2},
    {token_type::plus_eq,"+=",2},
    {token_type::minus_eq,"-=",2},
    {token_type::times_eq,"*=",2},
    {token_type::divide_eq,"/=",2},


    {token_type::decl,"decl",-1},

    {token_type::const_t,"const",-1},

    {token_type::u8,"u8",-1},
    {token_type::u16,"u16",-1},
    {token_type::u32,"u32",-1},

    {token_type::s8,"s8",-1},
    {token_type::s16,"s16",-1},
    {token_type::s32,"s32",-1},

    {token_type::byte_t,"byte",-1},

    {token_type::bool_t,"bool",-1},

    {token_type::cast,"cast",-1},
    {token_type::sizeof_t,"sizeof",-1},
    {token_type::type_alias,"type_alias",-1},

    {token_type::import,"import",-1},
    
    {token_type::struct_t,"struct",-1},
    {token_type::enum_t,"enum",-1},
    {token_type::scope,"::",-1},


    {token_type::func,"func",-1},
    {token_type::ret,"return",-1},

    {token_type::times,"*",21},
    {token_type::plus,"+",20},
    {token_type::minus,"-",20},
    {token_type::divide,"/",21},
    {token_type::mod,"%",21},
    {token_type::increment,"++",-1},
    {token_type::decrement,"--",-1},

    {token_type::sl_brace,"[",30},
    {token_type::sr_brace,"]",-1},

    {token_type::dot,".",-1},
    {token_type::qmark,"?",-1},

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
    {token_type::while_t,"while",-1},

    {token_type::if_t,"if",-1},
    {token_type::else_t,"else",-1},
    {token_type::switch_t,"switch",-1},
    {token_type::case_t,"case",-1},
    {token_type::default_t,"default",-1},

    



    {token_type::error,"error",-1},

    {token_type::eof,"eof",0},

    {token_type::END,"end",-1}
};

inline const char *tok_name(token_type t)
{
    return TOKEN_INFO[static_cast<size_t>(t)].name;
}



struct Value
{
    friend bool operator == (const Value &t1, const Value &t2);
    friend bool operator != (const Value &t1, const Value &t2);   

    u32 v = 0;
    b32 sign = false;
};



inline bool operator == (const Value &v1, const Value &v2)
{
    return v1.v == v2.v && v1.sign == v2.sign;
}


inline bool operator != (const Value &v1, const Value &v2)
{
    return !operator==(v1,v2);
}


struct Token
{

    Token() {}


    friend bool operator == (const Token &t1, const Token &t2);
    friend bool operator != (const Token &t1, const Token &t2);


    u32 line = 0;
    u32 col = 0;
    token_type type = token_type::eof;


    union 
    {
        String literal;
        Value value;
        char character;
    };
};


Token token_plain(token_type type, u32 line = 0, u32 col = 0)
{
    Token token;

    token.type = type;
    token.line = line;
    token.col = col;
    token.literal = {};
    
    return token;
}

Token token_literal(token_type type,const String& literal, u32 line = 0, u32 col = 0)
{
    Token token;

    token.type = type;
    token.line = line;
    token.col = col;
    token.literal = literal;
    
    return token;    
}

Token token_char(char c,u32 line = 0, u32 col = 0)
{
    Token token;

    token.type = token_type::char_t;
    token.line = line;
    token.col = col;
    token.character = c;

    return token;
}

Token token_value(const Value& value, u32 line = 0, u32 col = 0)
{
    Token token;

    token.type = token_type::value;
    token.line = line;
    token.col = col;
    token.value = value;

    return token;
}

inline bool operator == (const Token &t1, const Token &t2)
{
    switch(t1.type)
    {
        case token_type::value:
        {
            return t1.type == t2.type && t1.value == t2.value;
        }

        case token_type::char_t:
        {
            return t1.type == t2.type && t1.character == t2.character;
        }

        default:
        {
            return t1.type == t2.type && t1.literal == t2.literal;
        }
    }
}


inline bool operator != (const Token &t1, const Token &t2)
{
    return !operator==(t1,t2);
}

inline void print_token(const Token& t)
{
    printf("type: '%s' (%d)\n",tok_name(t.type), u32(t.type));

    switch(t.type)
    {
        case token_type::char_t:
        {
            printf("char %c\n",t.character);
            break;
        }

        case token_type::value:
        {
            printf("value: %s%d\n",t.value.sign? "-"  : "",t.value.v);
            break;
        }

        default:
        {
            if(t.literal.buf)
            {
                printf("literal: %s\n",t.literal.buf);
            }
            break;
        }
    }

    
    printf("loc: (%d:%d)\n\n",t.line+1,t.col+1);    
}
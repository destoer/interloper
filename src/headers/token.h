#pragma once
#include <destoer/destoer.h>

enum class token_type
{
    value,
    float_t,
    symbol, 
    char_t,
    string,

    true_t,
    false_t,
    null_t,

    semi_colon,
    colon,

    hash,

    left_c_brace,
    right_c_brace,

    left_paren,
    right_paren,

    comma,
    va_args,

    equal,
    plus_eq,
    minus_eq,
    times_eq,
    divide_eq,

    bitwise_or_eq,

    decl,

    const_assert,

    const_t,

    constant_t,
    global_t,

    namespace_t,

    u8,
    u16,
    u32,
    u64,

    s8,
    s16,
    s32,
    s64,

    c8_t,

    byte_t,

    bool_t,

    f64_t,

    cast,
    sizeof_t,
    sizeof_type_t,
    sizeof_data_t,
    type_alias,

    import,
    struct_t,
    enum_t,
    scope,

    func,
    extern_t,
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
    in_t,
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
    token_type type;
    const char *name;
    s32 lbp;
    u32 size = 0;
};

static constexpr size_t TOKEN_SIZE = static_cast<size_t>(token_type::END)+1;

static const TokInfo TOKEN_INFO[TOKEN_SIZE] = 
{
    {token_type::value,"value",0},
    {token_type::float_t,"float",0},
    {token_type::symbol,"symbol",0},
    {token_type::char_t,"char",0},
    {token_type::string,"string",0},

    {token_type::true_t,"true",0},
    {token_type::false_t,"false",0},
    {token_type::null_t,"NULL",0},

    {token_type::semi_colon,";",-1},
    {token_type::colon,":",-1,1},

    {token_type::hash,"#",-1},

    {token_type::left_c_brace,"{",-1},
    {token_type::right_c_brace,"}",-1},

    {token_type::left_paren,"(",-1},
    {token_type::right_paren,")",-1},

    {token_type::comma,",",-1},
    {token_type::va_args,"...",-1},

    {token_type::equal,"=",2,1},
    {token_type::plus_eq,"+=",2,2},
    {token_type::minus_eq,"-=",2,2},
    {token_type::times_eq,"*=",2,2},
    {token_type::divide_eq,"/=",2,2},

    {token_type::bitwise_or_eq,"|=",2,2},

    {token_type::decl,":=",-1,2},

    {token_type::const_assert,"const_assert",-1},

    {token_type::const_t,"const",-1},

    {token_type::constant_t,"constant",-1},
    {token_type::global_t,"global",-1},

    {token_type::namespace_t,"namespace",-1},

    {token_type::u8,"u8",-1},
    {token_type::u16,"u16",-1},
    {token_type::u32,"u32",-1},
    {token_type::u64,"u64",-1},

    {token_type::s8,"s8",-1},
    {token_type::s16,"s16",-1},
    {token_type::s32,"s32",-1},
    {token_type::s64,"s64",-1},

    {token_type::c8_t,"c8",-1},

    {token_type::byte_t,"byte",-1},

    {token_type::bool_t,"bool",-1},

    {token_type::f64_t,"f64",-1},

    {token_type::cast,"cast",-1},
    {token_type::sizeof_t,"sizeof",-1},
    {token_type::sizeof_type_t,"sizeof_type",-1},
    {token_type::sizeof_data_t,"sizeof_data",-1},
    {token_type::type_alias,"type_alias",-1},

    {token_type::import,"import",-1},
    
    {token_type::struct_t,"struct",-1},
    {token_type::enum_t,"enum",-1},
    {token_type::scope,"::",-1,2},


    {token_type::func,"func",-1},
    {token_type::extern_t,"extern",-1},
    {token_type::ret,"return",-1},

    {token_type::times,"*",21,1},
    {token_type::plus,"+",20,1},
    {token_type::minus,"-",20,1},
    {token_type::divide,"/",21,1},
    {token_type::mod,"%",21,1},
    {token_type::increment,"++",-1,2},
    {token_type::decrement,"--",-1,2},

    {token_type::sl_brace,"[",30},
    {token_type::sr_brace,"]",-1},

    {token_type::dot,".",-1},
    {token_type::qmark,"?",-1},

    {token_type::shift_l,"<<",13,2},
    {token_type::shift_r,">>",13,2},

    {token_type::deref,"@",30,1},

    {token_type::operator_and,"&",10,1},
    {token_type::bitwise_or,"|",9,1},
    {token_type::bitwise_not,"~",-1}, // unary
    {token_type::bitwise_xor,"^",8,1},

    {token_type::logical_or,"||",6,2},
    {token_type::logical_and,"&&",5,2},
    {token_type::logical_not,"!",7,1},

    {token_type::logical_eq,"==",11,2},
    {token_type::logical_ne,"!=",11,2},

    {token_type::logical_lt,"<",12,1},
    {token_type::logical_gt,">",12,1},
    {token_type::logical_le,"<=",12,2},
    {token_type::logical_ge,">=",12,2},

    {token_type::for_t,"for",-1},
    {token_type::in_t,"in",-1},
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

    u64 v = 0;
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


    u32 idx = 0;
    token_type type = token_type::eof;


    union 
    {
        String literal;
        Value value;
        char character;
        f64 fp;
    };
};


Token token_plain(token_type type, u32 idx = 0)
{
    Token token;

    token.type = type;
    token.idx = idx;
    token.literal = {};
    
    return token;
}

Token token_literal(token_type type,const String& literal, u32 idx = 0)
{
    Token token;

    token.type = type;
    token.idx = idx;
    token.literal = literal;
    
    return token;    
}

Token token_char(char c,u32 idx = 0)
{
    Token token;

    token.type = token_type::char_t;
    token.idx = idx;
    token.character = c;

    return token;
}

Token token_value(const Value& value, u32 idx = 0)
{
    Token token;

    token.type = token_type::value;
    token.idx = idx;
    token.value = value;

    return token;
}

Token token_float(f64 v, u32 idx = 0)
{
    Token token;

    token.type = token_type::float_t;
    token.idx = idx;
    token.fp = v;

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
            printf("value: %s%lu\n",t.value.sign? "-"  : "",t.value.v);
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


    printf("loc: %d\n\n",t.idx);    
}
#pragma once
#include <lib.h>




// if type idx is >= to this then this is a custom defined type
static constexpr int BUILTIN_TYPE_SIZE = 2;

enum class builtin_type
{
    void_t, 
    s32_t,
};

static const char *TYPE_NAMES[BUILTIN_TYPE_SIZE] =
{
    "void",
    "s32",
};

inline const char *builtin_type_name(builtin_type t)
{
    return TYPE_NAMES[static_cast<size_t>(t)];
}


inline int conv_builtin_type(builtin_type t)
{
    return static_cast<int>(t);
}

inline builtin_type conv_type_idx(int type_idx)
{
    return static_cast<builtin_type>(type_idx);
}


enum class contained_type
{
    plain,
    array,
    pointer,
};


struct Type
{
    Type() {}

    // plain builtin type
    Type(builtin_type t) : type_idx(conv_builtin_type(t)) 
    {
        
    }


    int type_idx;

    // arrays etc (ignore for now)
    /*
    int array_indirection;
    int ptr_indirection;

    // is this an array holding pointers
    // pointers to an array
    // for simplicty mixing containers like
    // int[]@[]@ is not permitted
    // or just a plain type?
    contained_type held_type;
    */

    // type specifiers here i.e const

};


static constexpr uint32_t SYMBOL_NO_SLOT = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t) : name(n), type(t), slot(SYMBOL_NO_SLOT)
    {}

    Symbol(const std::string &n, Type t, uint32_t s) : name(n), type(t), slot(s)
    {}

    std::string name;
    Type type;

    // what slot does this symbol hold inside the ir?
    uint32_t slot;
};


struct Function
{
    Function() {}

    Function(const std::string &n, Type rt, std::vector<Symbol> a) : name(n), return_type(rt), args(a)
    {}


    std::string name;
    Type return_type;
    std::vector<Symbol> args;
};
#pragma once
#include <lib.h>

enum class var_type
{
    void_t, 
    s32_t,
    struct_t
};


static constexpr size_t TYPE_SIZE = 3;


/*
static const char *type_names[TYPE_SIZE] = 
{
    "void",
    "s32",
    "struct_t",
};
*/

struct Symbol
{
    Symbol(const std::string &n, var_type t) : name(n), type(t)
    {}

    std::string name;
    var_type type;
};


struct Function
{
    Function(const std::string &n, var_type rt, std::vector<Symbol> a) : name(n), return_type(rt), args(a)
    {}


    std::string name;
    var_type return_type;
    std::vector<Symbol> args;
};
#pragma once
#include <destoer/destoer.h>

enum class [[nodiscard]] itl_error
{
    parse_error,
    lexer_error,
    array_type_error,
    int_type_error,
    illegal_cast,
    bool_type_error,
    string_type_error,
    enum_type_error,
    pointer_type_error,
    generic_type_error,
    out_of_bounds,
    undeclared,
    missing_initializer,
    missing_name,
    redeclaration,
    missing_args,
    tuple_mismatch,
    missing_return,
    invalid_expr,
    invalid_statement,
    mismatched_args,
    black_hole,
    struct_error,
    undefined_type_oper,
    const_type_error,
    const_assert,
    rtti_error,
    unimplemented,
};

static const char* ERROR_NAME[] = 
{
    "parse error",
    "lexer error",
    "array type error",
    "int type error",
    "illegal cast",
    "bool type error",
    "string type error",
    "enum type error",
    "pointer type error",
    "generic type error",
    "out of bounds",
    "not declared",
    "missing initializer",
    "missing name",
    "redeclaration",
    "missing args",
    "tuple mismatch",
    "missing return",
    "invalid expr",
    "invalid statement",
    "mismatched args",
    "black hole",
    "struct error",
    "undefined type operation",
    "const type error",
    "const assert",
    "rtti error",
    "unimplemented",
};

struct Type;
using TypeResult = destoer::Result<Type*,itl_error>;

struct ConstValue
{
    Type* type;
    destoer::u64 value;
};

using ConstValueResult = destoer::Result<ConstValue,itl_error>;

struct Type;
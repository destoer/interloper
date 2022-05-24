#pragma once
#include <lib.h>
#include <token.h>
#include <type.h>

enum class ast_type
{
    root,
    block,

    function,
    function_args,
    function_call,

    value,
    symbol,
    char_t,
    string,

    cast,

    ret,

    type,
    ptr_indirection,
    arr_dimensions,
    arr_var_size,
    arr_initializer,
    arr_deduce_size,

    declaration,
    auto_decl,
    



    equal,
    times,
    plus,
    minus,
    divide,
    mod,
    
    shift_l,
    shift_r,

    deref,
    addrof,

    bitwise_or,
    bitwise_not,
    bitwise_and,
    bitwise_xor,

    logical_or,
    logical_not,
    logical_and,

    logical_eq,
    logical_ne,

    logical_lt,
    logical_gt,
    logical_le,
    logical_ge,

    false_t,
    true_t,

    for_block,

    if_block,
    if_t,
    else_if_t,
    else_t,

    access_member,
    member,
    array_access,

    END
};

static constexpr size_t AST_TYPE_SIZE = static_cast<size_t>(ast_type::END)+1;

inline const char *AST_NAMES[AST_TYPE_SIZE] =
{
    "root",
    "block",


    "function",
    "function_args",
    "function_call",

    "value",
    "symbol",
    "char",
    "string",

    "cast",

    "return",

    "type",
    "ptr_indirection",
    "arr_dimensions",
    "arr_var_size",
    "arr_intializer",
    "deduced_arr_size",

    "declaration",
    "auto_decl",

    "=",
    "*",
    "+",
    "-",
    "/",
    "%",

    "<<",
    ">>",

    "@",
    "addrof",

    "|",
    "~",
    "&",
    "^",

    "||",
    "!",
    "&&",

   "==",
   "!=",

    "<",
    ">",
    "<=",
    ">=",

    "false",
    "true",

    "for_block",

    "if_block",
    "if",
    "else if",
    "else",

    "access_member",
    "member",
    "array_access",

    // should not be used...
    "END"
};

struct Value
{
    Value(u32 value, bool s) : v(value), sign(s) {}

    u32 v;
    bool sign;
};

// TODO: guess we need a Array struct that we can allocate on
// as well as a String if we want deletion on this to be fast
// with a Arena Allocator


// TODO: Optimise the memory usage on this
// and 80 byte struct is way to much for this

struct AstNode
{
    AstNode()
    {
        value = Value(0,false);
    }

    // node data
    ast_type type;
    std::string literal;
    u32 line;

    // if we put anything in here
    // we need to make sure we add the copy into
    // copy_node
    union
    {
        // ast_type::value
        Value value;

        u32 type_idx;
    };

    // TODO: should this even be a pointer?


    std::vector<AstNode *> nodes;
};


struct Parser
{
    const std::vector<Token> *tokens = nullptr;

    // pratt parser
    Token expr_tok;
    u32 tok_idx = 0;
    u32 brace_count = 0;
    b32 terminate = false;
    token_type termination_type = token_type::eof;

    // error handling
    b32 error = false;
    s32 line = 0;
};

AstNode* alloc_node()
{
    return new AstNode();
}

AstNode *ast_plain(ast_type type)
{
    AstNode* node = alloc_node();
    node->type = type;
    node->value = Value(0,false);

    return node;    
}

AstNode *ast_literal(ast_type type,const std::string &literal)
{
    AstNode* node = alloc_node();
    node->type = type;
    node->literal = literal;
    node->value = Value(0,false);

    return node;    
}


AstNode *ast_binary(AstNode *l, AstNode *r, ast_type type, std::string literal = "")
{
    AstNode* node = alloc_node();

    node->nodes.push_back(l);
    node->nodes.push_back(r);
    node->type = type;
    node->literal = literal;
    node->value = Value(0,false);  

    return node;  
}

AstNode *ast_unary(AstNode *l, ast_type type, std::string literal = "")
{
    AstNode* node = alloc_node();

    node->nodes.push_back(l);
    node->type = type;
    node->literal = literal;
    node->value = Value(0,false);  

    return node;  
}


AstNode *ast_binary_value(AstNode *l, AstNode *r, Value value, std::string literal = "")
{
    AstNode* node = alloc_node();

    node->nodes.push_back(l);
    node->nodes.push_back(r);
    node->type = ast_type::value;
    node->literal = literal;
    node->value = value; 

    return node;  
}

AstNode *ast_value(Value value, std::string literal = "")
{
    AstNode* node = alloc_node();

    node->type = ast_type::value;
    node->literal = literal;
    node->value = value; 

    return node;  
}



template<typename... Args>
inline void panic(Parser &parser,const Token &token,const char *fmt, Args... args)
{
    printf(fmt,args...);
    printf("at: line %d col %d\n\n",token.line + 1,token.col + 1);
    parser.error = true;
    parser.line = token.line;
}


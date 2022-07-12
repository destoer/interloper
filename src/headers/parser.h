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
    sizeof_t,
    struct_t,

    ret,

    type,
    ptr_indirection,
    arr_dimensions,
    arr_var_size,
    initializer_list,
    arr_deduce_size,
    const_t,

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
    null_t,

    for_block,

    if_block,
    if_t,
    else_if_t,
    else_t,

    access_member,
    access_members,
    access_struct,
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
    "sizeof",
    "struct",

    "return",

    "type",
    "ptr_indirection",
    "arr_dimensions",
    "arr_var_size",
    "intializer_list",
    "deduced_arr_size",
    "const",

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
    "NULL",

    "for_block",

    "if_block",
    "if",
    "else if",
    "else",

    "access_member",
    "access_members",
    "access_struct",
    "member",
    "array_access",

    // should not be used...
    "END"
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
    u32 line;
    u32 col;

    String literal;

    // TODO: this should be only for top levle decl just leave it in all of them for now
    String filename;

    // if we put anything in here
    // we need to make sure we add the copy into
    // copy_node
    union
    {
        // ast_type::value
        Value value;

        // type decl
        u32 type_idx;

        char character;
    };


    std::vector<AstNode *> nodes;
};


struct Parser
{
    Array<Token> tokens;

    // pratt parser
    Token expr_tok;
    u32 tok_idx = 0;
    u32 brace_count = 0;
    b32 terminate = false;
    token_type termination_type = token_type::eof;

    ArenaAllocator* allocator;
    ArenaAllocator* string_allocator;

    // error handling
    b32 error = false;
    s32 line = 0;
};

AstNode* alloc_node(Parser& parser)
{
    void* ptr = allocate(*parser.allocator,sizeof(AstNode));

    // for now placement new this
    AstNode* node = new(ptr) AstNode();

    return node;  
}

AstNode *ast_plain(Parser& parser,ast_type type, const Token& token)
{
    AstNode* node = alloc_node(parser);
    node->type = type;
    node->value = Value(0,false);
    node->line = token.line;
    node->col = token.col;

    return node;    
}

AstNode *ast_literal(Parser& parser,ast_type type,const String &literal, const Token& token)
{
    AstNode* node = alloc_node(parser);
    node->type = type;
    node->literal = literal;
    node->value = Value(0,false);
    node->line = token.line;
    node->col = token.col;

    return node;    
}

AstNode *ast_char(Parser& parser,const char character, const Token& token)
{
    AstNode* node = alloc_node(parser);
    node->type = ast_type::char_t;
    node->character = character;
    node->line = token.line;
    node->col = token.col;

    return node;    
}


AstNode *ast_func(Parser& parser,const String &literal, const String& filename, const Token& token)
{
    AstNode* node = alloc_node(parser);
    node->type = ast_type::function;

    node->filename = filename;
    node->literal = literal;
    node->value = Value(0,false);

    node->line = token.line;
    node->col = token.col;

    return node;
}

AstNode *ast_struct(Parser& parser,const String &literal, const String& filename, const Token& token)
{
    AstNode* node = alloc_node(parser);
    node->type = ast_type::struct_t;

    node->filename = filename;
    node->literal = literal;
    node->value = Value(0,false);

    node->line = token.line;
    node->col = token.col;

    return node;
}    

AstNode *ast_binary(Parser& parser,AstNode *l, AstNode *r, ast_type type, const Token& token)
{
    AstNode* node = alloc_node(parser);

    node->nodes.push_back(l);
    node->nodes.push_back(r);
    node->type = type;
    node->value = Value(0,false); 
    node->line = token.line;
    node->col = token.col;

    return node;  
}

AstNode *ast_unary(Parser& parser,AstNode *l, ast_type type, const Token& token)
{
    AstNode* node = alloc_node(parser);

    node->nodes.push_back(l);
    node->type = type;
    node->value = Value(0,false); 
    node->line = token.line;
    node->col = token.col;

    return node;  
}


AstNode *ast_value(Parser& parser,Value value, const Token& token)
{
    AstNode* node = alloc_node(parser);

    node->type = ast_type::value;
    node->literal = ""; // TODO:
    node->value = value;
    node->line = token.line;
    node->col = token.col;

    return node;  
}




inline void panic(Parser &parser,const Token &token,const char *fmt, ...)
{
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    printf("at: line %d col %d\n\n",token.line + 1,token.col + 1);
    
    parser.error = true;
    parser.line = token.line;
}


bool match(Parser &parser,token_type type);
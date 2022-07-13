#pragma once
#include <lib.h>
#include <token.h>
#include <type.h>

enum class ast_type
{
    root,
    block,

    function,
    function_call,

    value,
    symbol,
    char_t,
    string,

    cast,
    sizeof_t,
    struct_t,

    ret,

    // TODO: we dont need some of these now
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
    while_block,

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
    "while_block",

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


enum class ast_fmt
{
    plain,
    binary,
    unary,
    literal,
    value,
    function,
    struct_t,
    char_t,
    type,
    declaration,
    auto_decl,
    block,
    function_call,
    for_block,
};

inline const char *FMT_NAMES[] =
{
    "plain",
    "binary",
    "unary",
    "literal",
    "value",
    "function",
    "struct",
    "char",
    "type",
    "declaration",
    "auto_decl",
    "block",
    "function_call",
    "for",
};


struct AstNode
{
    // node data
    ast_type type;
    ast_fmt fmt;
    u32 line;
    u32 col;
};

struct BinNode
{
    AstNode node;

    AstNode* left = nullptr;
    AstNode* right = nullptr;
};


struct LiteralNode
{
    AstNode node;
    String literal;
};

struct UnaryNode
{
    AstNode node;

    AstNode* next = nullptr;
};

struct ValueNode
{
    AstNode node;

    Value value;
};

struct CharNode
{
    AstNode node;

    char character;
};

struct TypeNode
{
    AstNode node;

    String name;
    u32 type_idx;
    
    bool is_const;
};


struct DeclNode
{
    AstNode node;

    String name;
    TypeNode* type = nullptr;
    AstNode* expr = nullptr;
};

struct BlockNode
{
    AstNode node;

    Array<AstNode*> statements;
};


struct FuncNode
{
    AstNode node;

    String name;
    String filename;

    TypeNode* return_type = nullptr;
    BlockNode* block = nullptr;
    Array<DeclNode*> args;
};


struct StructNode
{
    AstNode node;

    String name;
    String filename;
    Array<DeclNode*> members;
};



struct AutoDeclNode
{
    AstNode node;

    String name;
    AstNode* expr = nullptr;
};

struct FuncCallNode
{
    AstNode node;

    String name;
    Array<AstNode*> args;
};

struct ForNode
{
    AstNode node;

    AstNode* initializer;
    AstNode* cond;
    AstNode* post;

    BlockNode* block;
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


template<typename T>
T* alloc_node(Parser& parser, ast_type type, ast_fmt fmt, const Token& token)
{
    AstNode* node = (AstNode*)allocate(*parser.allocator,sizeof(T));

    // default init the actual type
    T* ret_node = (T*)node;
    *ret_node = {};

    node->type = type;
    node->fmt = fmt;
    node->line = token.line;
    node->col = token.col;

    return ret_node;
}

AstNode *ast_plain(Parser& parser,ast_type type, const Token& token)
{
    AstNode* node = alloc_node<AstNode>(parser,type,ast_fmt::plain,token);

    return node;    
}

AstNode *ast_literal(Parser& parser,ast_type type,const String &literal, const Token& token)
{
    LiteralNode* literal_node = alloc_node<LiteralNode>(parser,type,ast_fmt::literal,token);

    literal_node->literal = literal;

    return (AstNode*)literal_node;    
}

AstNode *ast_char(Parser& parser,const char character, const Token& token)
{
    CharNode* char_node = alloc_node<CharNode>(parser,ast_type::char_t,ast_fmt::char_t,token);

    char_node->character = character;

    return (AstNode*)char_node;    
}


AstNode *ast_func(Parser& parser,const String &name, const String& filename, const Token& token)
{
    FuncNode* func_node = alloc_node<FuncNode>(parser,ast_type::function,ast_fmt::function,token);

    func_node->name = name;
    func_node->filename = filename;

    return (AstNode*)func_node;
}

AstNode *ast_struct(Parser& parser,const String &name, const String& filename, const Token& token)
{
    StructNode* struct_node = alloc_node<StructNode>(parser,ast_type::struct_t,ast_fmt::struct_t,token);

    struct_node->name = name;
    struct_node->filename = filename;

    return (AstNode*)struct_node;
}    

AstNode *ast_binary(Parser& parser,AstNode *l, AstNode *r, ast_type type, const Token& token)
{
    BinNode* bin_node = alloc_node<BinNode>(parser,type,ast_fmt::binary,token);

    bin_node->left = l;
    bin_node->right = r;

    return (AstNode*)bin_node;  
}

AstNode *ast_unary(Parser& parser,AstNode *next, ast_type type, const Token& token)
{
    UnaryNode* unary_node = alloc_node<UnaryNode>(parser,type,ast_fmt::unary,token);

    unary_node->next = next;

    return (AstNode*)unary_node;  
}


AstNode *ast_value(Parser& parser,Value value, const Token& token)
{
    ValueNode* value_node = alloc_node<ValueNode>(parser,ast_type::value,ast_fmt::value,token);

    value_node->value = value;

    return (AstNode*)value_node;  
}

AstNode* ast_type_decl(Parser& parser, const String& name, const Token& token)
{
    TypeNode* type_node = alloc_node<TypeNode>(parser,ast_type::type,ast_fmt::type,token);

    type_node->name = name;

    return (AstNode*)type_node;        
}


AstNode* ast_decl(Parser& parser, const String& name,TypeNode* type, const Token& token)
{
    DeclNode* decl_node = alloc_node<DeclNode>(parser,ast_type::declaration,ast_fmt::declaration,token);

    decl_node->name = name;
    decl_node->type = type;

    return (AstNode*)decl_node;        
}

AstNode* ast_auto_decl(Parser& parser, const String& name, AstNode* expr, const Token& token)
{
    AutoDeclNode* decl_node = alloc_node<AutoDeclNode>(parser,ast_type::auto_decl,ast_fmt::auto_decl,token);

    decl_node->name = name;
    decl_node->expr = expr;

    return (AstNode*)decl_node;        
}

AstNode* ast_block(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<BlockNode>(parser,ast_type::block,ast_fmt::block,token);

    return node;
}

AstNode* ast_if_block(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<BlockNode>(parser,ast_type::if_block,ast_fmt::block,token);

    return node;
}

AstNode* ast_call(Parser& parser, const String& name, const Token& token)
{
    FuncCallNode* func_call = alloc_node<FuncCallNode>(parser,ast_type::function_call,ast_fmt::function_call,token);

    func_call->name = name;

    return (AstNode*)func_call;
}

AstNode* ast_for(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<ForNode>(parser,ast_type::for_block,ast_fmt::for_block,token);

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
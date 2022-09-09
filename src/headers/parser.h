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
    scope,

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

    tuple_assign,
    
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
    switch_t,
    case_t,
    default_t,

    access_member,
    access_members,
    access_struct,
    member,
    index,

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
    "scope",

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

    "tuple_assign",

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
    "switch",
    "case",
    "default",

    "access_member",
    "access_members",
    "access_struct",
    "member",
    "index",

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
    record,
    index,
    switch_t,
    case_t,
    scope,
    tuple_assign,
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
    "record",
    "index",
    "switch",
    "case",
    "scope",
    "tuple_assign",
};


struct AstNode
{
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

struct RecordNode
{
    AstNode node;

    Array<AstNode*> nodes;
};

struct TypeNode
{
    AstNode node;

    String name;

    b32 is_const;
    u32 type_idx;

    
    b32 contains_ptr = false;
    u32 ptr_indirection = 0;
    RecordNode* arr_decl = nullptr;
};


struct IndexNode 
{
    AstNode node;

    String name;
    Array<AstNode*> indexes;
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

    Array<TypeNode*> return_type;
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

struct CaseNode
{
    // final value
    u32 value;

    // what label does this statement have
    u32 label;

    // end block of the case
    u32 end_block;

    AstNode* statement;
    BlockNode* block;
};

struct SwitchNode
{
    AstNode node;

    AstNode* expr;
    Array<CaseNode*> statements;
    UnaryNode* default_statement;
};


struct ScopeNode 
{
    AstNode node;

    // TODO: this should probably be an array
    // when we add scopes
    String scope;

    AstNode* expr;
};


struct TupleAssignNode
{
    AstNode node;

    Array<AstNode*> symbols;
    FuncCallNode* func_call;
};

using AstPointers = Array<void***>;

struct Parser
{
    Array<Token> tokens;

    // pratt parser
    Token expr_tok;
    u32 tok_idx = 0;
    b32 terminate = false;
    token_type termination_type = token_type::eof;

    ArenaAllocator* allocator;
    ArenaAllocator* string_allocator;
    AstPointers* ast_arrays;
    

    String cur_file;

    // error handling
    b32 error = false;
    s32 line = 0;
};

void add_ast_pointer(Parser& parser, void* pointer);

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

    add_ast_pointer(parser,&func_node->args.data);
    add_ast_pointer(parser,&func_node->return_type.data);

    func_node->name = name;
    func_node->filename = filename;

    return (AstNode*)func_node;
}

AstNode *ast_struct(Parser& parser,const String &name, const String& filename, const Token& token)
{
    StructNode* struct_node = alloc_node<StructNode>(parser,ast_type::struct_t,ast_fmt::struct_t,token);

    add_ast_pointer(parser,&struct_node->members.data);

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
    BlockNode* block_node = alloc_node<BlockNode>(parser,ast_type::block,ast_fmt::block,token);

    add_ast_pointer(parser,&block_node->statements.data);

    return (AstNode*)block_node;
}

AstNode* ast_if_block(Parser& parser, const Token& token)
{
    BlockNode* block_node = alloc_node<BlockNode>(parser,ast_type::if_block,ast_fmt::block,token);

    add_ast_pointer(parser,&block_node->statements.data);

    return (AstNode*)block_node;
}

AstNode* ast_call(Parser& parser, const String& name, const Token& token)
{
    FuncCallNode* func_call = alloc_node<FuncCallNode>(parser,ast_type::function_call,ast_fmt::function_call,token);

    add_ast_pointer(parser,&func_call->args.data);

    func_call->name = name;

    return (AstNode*)func_call;
}

AstNode* ast_for(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<ForNode>(parser,ast_type::for_block,ast_fmt::for_block,token);

    return node;
}


AstNode* ast_record(Parser& parser,ast_type type, const Token& token)
{
    RecordNode* record_node = alloc_node<RecordNode>(parser,type,ast_fmt::record,token);

    add_ast_pointer(parser,&record_node->nodes.data);

    return (AstNode*)record_node;
}


AstNode* ast_index(Parser& parser,const String &name, const Token& token)
{
    IndexNode* index_node = alloc_node<IndexNode>(parser,ast_type::index,ast_fmt::index,token);

    add_ast_pointer(parser,&index_node->indexes.data);

    index_node->name = name;

    return (AstNode*)index_node;
}


AstNode* ast_switch(Parser& parser, AstNode* expr, const Token& token)
{
    SwitchNode* switch_node = alloc_node<SwitchNode>(parser,ast_type::switch_t,ast_fmt::switch_t,token);
    
    switch_node->expr = expr;

    add_ast_pointer(parser,&switch_node->statements.data);

    return (AstNode*)switch_node;
}

AstNode* ast_case(Parser& parser, AstNode* expr, BlockNode* block, const Token& token)
{
    CaseNode* case_node = alloc_node<CaseNode>(parser,ast_type::case_t,ast_fmt::case_t,token);
    
    case_node->statement = expr;
    case_node->block = block;

    return (AstNode*)case_node;
}

AstNode* ast_scope(Parser& parser, AstNode* expr, String scope, const Token& token)
{
    ScopeNode* scope_node = alloc_node<ScopeNode>(parser,ast_type::scope,ast_fmt::scope,token);
    
    scope_node->expr = expr;
    scope_node->scope = scope;

    return (AstNode*)scope_node;
}


AstNode* ast_tuple_assign(Parser& parser, const Token& token)
{
    TupleAssignNode* tuple_node = alloc_node<TupleAssignNode>(parser,ast_type::tuple_assign,ast_fmt::tuple_assign,token);

    add_ast_pointer(parser,&tuple_node->symbols.data);

    return (AstNode*)tuple_node;
}

inline void panic(Parser &parser,const Token &token,const char *fmt, ...)
{
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    printf("at: %s line %d col %d\n\n",parser.cur_file.buf,token.line + 1,token.col + 1);
    
    parser.error = true;
    parser.line = token.line;
}


bool match(Parser &parser,token_type type);
void consume(Parser &parser,token_type type);
Token peek(Parser &parser,u32 v);
void prev_token(Parser &parser);
AstNode* func_call(Parser& parser,const Token& t);
AstNode* arr_access(Parser& parser, const Token& t);
AstNode *struct_access(Parser& parser, AstNode* expr_node,const Token& t);
AstNode* array_index(Parser& parser,const Token& t);
AstNode* var(Parser& parser, const Token& sym_tok);
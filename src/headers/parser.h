#pragma once
#include <destoer.h>
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

    const_assert,
    cast,
    sizeof_t,
    enum_t,
    struct_t,
    scope,
    type_alias,

    ret,

    type,
    ptr_indirection,
    arr_dimensions,
    arr_var_size,
    arr_fixed,
    initializer_list,
    arr_deduce_size,
    no_init,
    const_t,

    declaration,
    const_decl,
    auto_decl,
    
    global_declaration,


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

    for_range,
    for_iter,
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

    builtin_type_info,

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

    "const_assert",
    "cast",
    "sizeof",
    "struct",
    "enum",
    "scope",
    "type_alias",

    "return",

    "type",
    "ptr_indirection",
    "arr_dimensions",
    "arr_var_size",
    "arr_fixed",
    "intializer_list",
    "deduced_arr_size",
    "no_init",
    "const",

    "declaration",
    "const declaration",
    "auto_decl",

    "global declaration",

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

    "for_range",
    "for_iter",
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

    "builtin_type_info",

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
    enum_t,
    char_t,
    type,
    declaration,
    global_declaration,
    auto_decl,
    block,
    function_call,
    for_iter,
    for_range,
    record,
    index,
    switch_t,
    case_t,
    scope,
    tuple_assign,
    type_alias,
    builtin_type_info,
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
    "enum",
    "char",
    "type",
    "declaration",
    "global declaration",
    "auto_decl",
    "block",
    "function_call",
    "for_iter",
    "for_range",
    "record",
    "index",
    "switch",
    "case",
    "scope",
    "tuple_assign",
    "type_alias",
    "builtin_type_info",
};


struct AstNode
{
    ast_type type;
    ast_fmt fmt;
    u32 idx;
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

struct BuiltinAccessNode
{
    AstNode node;

    builtin_type type;
    String field;
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
    b32 is_constant;
    u32 type_idx;

    FuncNode* func_type = nullptr;

    Array<AstNode*> compound_type;
};


struct IndexNode 
{
    AstNode node;

    String name;
    Array<AstNode*> indexes;
};

// can be ast_type::declaration
// or ast_type::const_decl
struct DeclNode
{
    AstNode node;

    String name;
    TypeNode* type = nullptr;
    AstNode* expr = nullptr;
};

struct GlobalDeclNode
{
    AstNode node;

    DeclNode *decl = nullptr;
    String filename;
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

    Array<String> template_name;

    b32 va_args = false;
    String args_name;
};


struct StructNode
{
    AstNode node;

    String name;
    String filename;
    Array<DeclNode*> members;
    // is there a member forced to be first in the memory layout?
    DeclNode* forced_first = nullptr;
};

struct EnumMemberDecl
{
    String name;
    AstNode* initializer = nullptr;
};

struct EnumNode
{
    AstNode node;

    String name;
    String filename;
    Array<EnumMemberDecl> member;

    // do we have a struct?
    String struct_name = "";
};


struct AliasNode 
{
    AstNode node;

    String name;
    String filename;

    TypeNode* type;
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

    AstNode* expr =  nullptr;
    TypeNode* generic = nullptr;
    Array<AstNode*> args;
};



struct ForIterNode
{
    AstNode node;

    AstNode* initializer = nullptr;
    AstNode* cond = nullptr;
    AstNode* post = nullptr;

    BlockNode* block;
};

struct ForRangeNode
{
    AstNode node;

    AstNode* cond;
    BlockNode* block;

    // encode
    // [@v, i]
    String name_one;
    String name_two;

    b8 take_pointer = false;
};


struct CaseNode
{
    // final value
    u64 value;

    // what label does this statement have
    LabelSlot label;

    // end block of the case
    BlockSlot end_block;

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
    b32 auto_decl;
};

using AstPointers = Array<void***>;

enum class termination_type
{
    normal,
    list,
};

struct Parser
{
    Array<Token> tokens;

    // pratt parser
    Token expr_tok;
    u32 tok_idx = 0;
    b32 terminate = false;
    token_type terminating_tok = token_type::eof;
    termination_type term_type = termination_type::normal;

    ArenaAllocator* allocator;
    ArenaAllocator* string_allocator;
    AstPointers* ast_arrays;
    
    String expression_name;

    String cur_file;

    // error handling
    b32 error = false;
    u32 idx = 0;
    u32 line = 0;
    u32 col = 0;
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
    node->idx = token.idx;

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
    add_ast_pointer(parser,&func_node->template_name.data);
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

    add_ast_pointer(parser,&type_node->compound_type.data);

    return (AstNode*)type_node;        
}


AstNode* ast_decl(Parser& parser, const String& name,TypeNode* type, b32 is_const_decl, const Token& token)
{
    const auto decl_type = is_const_decl? ast_type::const_decl : ast_type::declaration;

    DeclNode* decl_node = alloc_node<DeclNode>(parser,decl_type,ast_fmt::declaration,token);

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

AstNode* ast_call(Parser& parser, AstNode* expr, TypeNode* generic,const Token& token)
{
    FuncCallNode* func_call = alloc_node<FuncCallNode>(parser,ast_type::function_call,ast_fmt::function_call,token);

    func_call->generic = generic;

    add_ast_pointer(parser,&func_call->args.data);

    // NOTE: this can encompass just a plain name for a function
    // or it could be for a symbol as part of a function pointer!
    func_call->expr = expr;

    return (AstNode*)func_call;
}

AstNode* ast_for_iter(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<ForIterNode>(parser,ast_type::for_iter,ast_fmt::for_iter,token);

    return node;
}

AstNode* ast_for_range(Parser& parser, const Token& token)
{
    AstNode* node = (AstNode*)alloc_node<ForRangeNode>(parser,ast_type::for_range,ast_fmt::for_range,token);

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

    tuple_node->auto_decl = false;
    add_ast_pointer(parser,&tuple_node->symbols.data);

    return (AstNode*)tuple_node;
}

AstNode *ast_alias(Parser& parser,TypeNode* type,const String &literal, const String& filename, const Token& token)
{
    AliasNode* alias_node = alloc_node<AliasNode>(parser,ast_type::type_alias,ast_fmt::type_alias,token);

    alias_node->filename = filename;
    alias_node->name = literal;
    alias_node->type = type;

    return (AstNode*)alias_node;
}

AstNode *ast_global_decl(Parser& parser,DeclNode* decl_node, const String& filename, const Token& token)
{
    GlobalDeclNode* global_node = alloc_node<GlobalDeclNode>(parser,ast_type::global_declaration,ast_fmt::global_declaration,token);

    global_node->filename = filename;
    global_node->decl = decl_node;

    return (AstNode*)global_node;
}

AstNode *ast_builtin_access(Parser& parser, builtin_type type, const String& field,const Token& token)
{
    BuiltinAccessNode* builtin_access = alloc_node<BuiltinAccessNode>(parser,ast_type::builtin_type_info,ast_fmt::builtin_type_info,token);

    builtin_access->type = type;
    builtin_access->field = field;

    return (AstNode*)builtin_access;
}

AstNode* ast_enum(Parser& parser, const String& name,const String& struct_name, const String& filename, const Token& token)
{
    EnumNode* enum_node = alloc_node<EnumNode>(parser,ast_type::enum_t,ast_fmt::enum_t,token);

    enum_node->name = name;
    enum_node->filename = filename;
    enum_node->struct_name = struct_name;
    
    add_ast_pointer(parser,&enum_node->member.data);

    return (AstNode*)enum_node;    
}

// scan file for row and column info
std::pair<u32,u32> get_line_info(const String& filename, u32 idx);

inline void panic(Parser &parser,const Token &token,const char *fmt, ...)
{
    // further reporting becomes pointless past a single parser error
    if(parser.error)
    {
        return;
    }

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    const auto [line,col] = get_line_info(parser.cur_file,token.idx);
    printf("at: %s line %d col %d\n\n",parser.cur_file.buf,line,col);

    parser.error = true;
    parser.line = line;
    parser.col = col;
    parser.idx = token.idx;
}

void print_depth(int depth);
bool match(Parser &parser,token_type type);
void consume(Parser &parser,token_type type);
Token peek(Parser &parser,u32 v);
void prev_token(Parser &parser);
AstNode* func_call(Parser& parser,AstNode *expr, const Token& t, TypeNode* generic = nullptr);
AstNode* arr_access(Parser& parser, const Token& t);
AstNode *struct_access(Parser& parser, AstNode* expr_node,const Token& t);
AstNode* array_index(Parser& parser,const Token& t);
AstNode* var(Parser& parser, const Token& sym_tok, b32 allow_call = false);
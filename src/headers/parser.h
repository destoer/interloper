#pragma once
#include <destoer/destoer.h>
#include <token.h>
#include <type.h>
#include <sym.h>

enum class ast_type
{
    root,
    block,

    function,
    function_call,

    value,
    float_t,
    symbol,
    char_t,
    string,

    const_assert,
    cast,
    sizeof_t,
    sizeof_type_t,
    sizeof_data_t,
    enum_t,
    struct_t,
    scope,
    type_alias,

    struct_initializer,
    struct_return,
    ret,

    type,
    ptr_indirection,
    nullable_ptr_indirection,
    arr_dimensions,
    arr_var_size,
    arr_fixed,
    initializer_list,
    designated_initializer_list,
    arr_deduce_size,
    no_init,
    const_t,

    declaration,
    const_decl,
    auto_decl,
    
    global_declaration,


    equal,
    arith_bin_op,
    arith_unary_op,

    tuple_assign,
    
    shift_l,
    shift_r,

    deref,
    addrof,

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
    slice,

    ignore,

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
    "float",
    "symbol",
    "char",
    "string",

    "const_assert",
    "cast",
    "sizeof",
    "sizeof_type",
    "sizeof_data",
    "struct",
    "enum",
    "scope",
    "type_alias",

    "struct_initializer",
    "struct_return",
    "return",

    "type",
    "ptr_indirection",
    "nullable_ptr_indirection",
    "arr_dimensions",
    "arr_var_size",
    "arr_fixed",
    "intializer_list",
    "designated_initializer_list",
    "deduced_arr_size",
    "no_init",
    "const",

    "declaration",
    "const declaration",
    "auto_decl",

    "global declaration",

    "=",
    "binary arithmetic operation",
    "unary arithmetic operation"

    "tuple_assign",

    "<<",
    ">>",

    "@",
    "addrof",

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
    "slice",

    "ignore",

    "builtin_type_info",

    // should not be used...
    "END"
};


enum class ast_fmt
{
    plain,
    arith_bin_op,
    arith_unary_op,
    binary,
    unary,
    literal,
    value,
    float_t,
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
    slice,
    switch_t,
    case_t,
    scope,
    tuple_assign,
    type_alias,
    builtin_type_info,
    struct_initializer,
    designated_initializer_list,
};

inline const char *FMT_NAMES[] =
{
    "plain",
    "binary",
    "unary",
    "literal",
    "value",
    "float",
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
    "slice",
    "switch",
    "case",
    "scope",
    "tuple_assign",
    "type_alias",
    "builtin_type_info",
    "struct_initializer",
    "designed_initializer_list"
};


struct AstNode
{
    ast_type type;
    ast_fmt fmt;
    u32 idx;
};

struct ArithBinNode
{
    AstNode node;
    arith_bin_op oper;
    AstNode* left = nullptr;
    AstNode* right = nullptr;
};

struct ArithUnaryNode
{
    AstNode node;
    arith_unary_op oper;
    AstNode* expr = nullptr;
};


enum class [[nodiscard]] parse_error
{
    invalid_consume,
    invalid_lbp,
    unexpected_token,
    invalid_terminator,
    malformed_stmt,
    missing_expr,
    itl_error,
    lexer_error,
};

using ParserResult = Result<AstNode*,parse_error>;


struct DesignatedInitializer
{
    AstNode* expr = nullptr;
    String name;
};

struct DesignatedListNode
{   
    AstNode node;
    Array<DesignatedInitializer> initializer;
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

struct FloatNode
{
    AstNode node;

    f64 value = 0.0;
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

struct ScopeNode 
{
    AstNode node;

    // NOTE: depending on the context the last member may be an enum member
    Array<String> scope;

    AstNode* expr;
};


enum class type_node_kind
{
    builtin,
    func_pointer,
    user
};

struct TypeNode
{
    AstNode node;

    String name;

    b32 is_const;
    b32 is_constant;
    builtin_type builtin = builtin_type::void_t;
    type_node_kind kind = type_node_kind::builtin;
    

    FuncNode* func_type = nullptr;
    NameSpace* name_space = nullptr;

    Array<AstNode*> compound_type;
};


struct IndexNode 
{
    AstNode node;

    String name;
    Array<AstNode*> indexes;
};


struct SliceNode
{
    AstNode node;

    String name;
    AstNode* lower = nullptr;
    AstNode* upper = nullptr;
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
    NameSpace* name_space;
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

    b32 va_args = false;
    String args_name;
    u32 attr_flags = 0;
};


struct StructNode
{
    AstNode node;

    String name;
    String filename;
    Array<DeclNode*> members;
    // is there a member forced to be first in the memory layout?
    DeclNode* forced_first = nullptr;

    u32 attr_flags = 0;
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

    TypeNode* type = nullptr; 
    u32 attr_flags = 0;
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


struct TupleAssignNode
{
    AstNode node;

    Array<AstNode*> symbols;
    FuncCallNode* func_call;
    b32 auto_decl;
};

struct StructInitializerNode
{
    AstNode node;

    NameSpace* name_space = nullptr;
    String struct_name;

    // Initializer list or designated initializer
    AstNode* initializer;
};

using AstPointers = Array<void**>;

struct Parser
{
    // what is our current token?
    u32 tok_idx = 0;

    Array<Token> tokens;

    // Destroy with ast
    ArenaAllocator* ast_allocator;
    ArenaAllocator* string_allocator;

    // Longer lived string allocator lasts until compiler end
    ArenaAllocator* global_string_allocator;
    AstPointers* ast_arrays;

    ArenaAllocator* namespace_allocator;
    
    String cur_file = "";
    NameSpace* cur_namespace = nullptr;
    NameSpace* global_namespace = nullptr;
    String cur_path = "";

    // error handling
    u32 error_count = 0;
    u32 idx = 0;
    u32 line = 0;
    u32 col = 0;
};


const u32 EXPR_TERMINATED_FLAG_BIT = 0;
const u32 EXPR_HIT_TERMINATOR_FLAG_BIT = 1;
const u32 EXPR_TERMINATED_FLAG = (1 << EXPR_TERMINATED_FLAG_BIT);
// expression may terminate without hitting the termiantor due to other flags
const u32 EXPR_HIT_TERMINATOR = (1 << EXPR_HIT_TERMINATOR_FLAG_BIT); 
const u32 EXPR_MUST_TERMINATE_FLAG = (1 << 2);
const u32 EXPR_TERM_LIST_FLAG = (1 << 3);


// Current state of the expression parser
struct ExprCtx
{
    // what kind of expression are we in?
    // NOTE: this is only used for error messaging
    String expression_name = "";

    // current token
    Token expr_tok;

    u32 expr_flags = 0;

    // make pratt parser terminate as soon as it sees
    // this token
    token_type term = token_type::error; 
};

void add_ast_pointer(Parser& parser, void* pointer);

template<typename T>
T* alloc_node(Parser& parser, ast_type type, ast_fmt fmt, const Token& token)
{
    AstNode* node = (AstNode*)allocate(*parser.ast_allocator,sizeof(T));

    // default init the actual type
    T* ret_node = (T*)node;
    *ret_node = {};

    node->type = type;
    node->fmt = fmt;
    node->idx = token.idx;

    return ret_node;
}

ParserResult ast_bin_arith(Parser& parser, arith_bin_op oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }

    ArithBinNode* arith_node  = alloc_node<ArithBinNode>(parser,ast_type::arith_bin_op,ast_fmt::arith_bin_op,token);

    arith_node->oper = oper;
    arith_node->left = *left_res;
    arith_node->right = *right_res;

    return (AstNode*)arith_node;
}


ParserResult ast_unary_arith(Parser& parser, arith_unary_op oper, ParserResult expr_res, const Token& token)
{
    if(!expr_res)
    {
        return expr_res;
    }

    ArithUnaryNode* arith_node  = alloc_node<ArithUnaryNode>(parser,ast_type::arith_unary_op,ast_fmt::arith_unary_op,token);

    arith_node->oper = oper;
    arith_node->expr = *expr_res;

    return (AstNode*)arith_node;
}



AstNode *ast_struct_initializer(Parser& parser,const String& literal, AstNode* initializer, NameSpace* name_space, const Token& token)
{
    StructInitializerNode* struct_initializer_node = alloc_node<StructInitializerNode>(parser,ast_type::struct_initializer,ast_fmt::struct_initializer,token);

    struct_initializer_node->struct_name = literal;
    struct_initializer_node->initializer = initializer;
    struct_initializer_node->name_space = name_space;

    return (AstNode*)struct_initializer_node;
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

ParserResult ast_binary(Parser& parser,ParserResult l_res, ParserResult r_res, ast_type type, const Token& token)
{
    if(!l_res)
    {
        return l_res;
    }

    if(!r_res)
    {
        return r_res;
    }

    BinNode* bin_node = alloc_node<BinNode>(parser,type,ast_fmt::binary,token);

    bin_node->left = *l_res;
    bin_node->right = *r_res;

    return (AstNode*)bin_node;  
}

ParserResult ast_unary(Parser& parser,ParserResult next_res, ast_type type, const Token& token)
{
    if(!next_res)
    {
        return next_res;
    }

    UnaryNode* unary_node = alloc_node<UnaryNode>(parser,type,ast_fmt::unary,token);

    unary_node->next = *next_res;

    return (AstNode*)unary_node;  
}


AstNode *ast_value(Parser& parser,Value value, const Token& token)
{
    ValueNode* value_node = alloc_node<ValueNode>(parser,ast_type::value,ast_fmt::value,token);

    value_node->value = value;

    return (AstNode*)value_node;  
}

AstNode *ast_float(Parser& parser, f64 value, const Token& token)
{
    FloatNode* float_node = alloc_node<FloatNode>(parser,ast_type::float_t,ast_fmt::float_t,token);

    float_node->value = value;

    return (AstNode*)float_node;  
}

AstNode* ast_type_decl(Parser& parser, NameSpace* name_space, const String& name, const Token& token)
{
    TypeNode* type_node = alloc_node<TypeNode>(parser,ast_type::type,ast_fmt::type,token);

    type_node->name = name;
    type_node->name_space = name_space;

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

AstNode* ast_call(Parser& parser, AstNode* expr, const Token& token)
{
    FuncCallNode* func_call = alloc_node<FuncCallNode>(parser,ast_type::function_call,ast_fmt::function_call,token);

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

AstNode* ast_slice(Parser& parser,const String &name, const Token& token)
{
    SliceNode* slice_node = alloc_node<SliceNode>(parser,ast_type::slice,ast_fmt::slice,token);
    slice_node->name = name;

    return (AstNode*)slice_node;
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

AstNode* ast_scope(Parser& parser, AstNode* expr, Array<String> scope, const Token& token)
{
    ScopeNode* scope_node = alloc_node<ScopeNode>(parser,ast_type::scope,ast_fmt::scope,token);
    
    scope_node->expr = expr;
    scope_node->scope = scope;
    add_ast_pointer(parser,&scope_node->scope.data);

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

AstNode *ast_global_decl(Parser& parser,DeclNode* decl_node, const String& filename,NameSpace* name_space, const Token& token)
{
    GlobalDeclNode* global_node = alloc_node<GlobalDeclNode>(parser,ast_type::global_declaration,ast_fmt::global_declaration,token);

    global_node->filename = filename;
    global_node->name_space = name_space;
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

AstNode* ast_enum(Parser& parser, const String& name,const String& filename,const Token& token)
{
    EnumNode* enum_node = alloc_node<EnumNode>(parser,ast_type::enum_t,ast_fmt::enum_t,token);

    enum_node->name = name;
    enum_node->filename = filename;
    
    add_ast_pointer(parser,&enum_node->member.data);

    return (AstNode*)enum_node;    
}


AstNode* ast_type_operator(TypeNode* type,ast_type kind)
{
    type->node.type = kind;

    return (AstNode*)type;    
}

AstNode* ast_designated_initializer_list(Parser& parser, const Token& token)
{
    DesignatedListNode* list = alloc_node<DesignatedListNode>(parser,ast_type::designated_initializer_list,ast_fmt::designated_initializer_list,token);
    add_ast_pointer(parser,&list->initializer.data);

    return (AstNode*)list;
}

// scan file for row and column info
std::pair<u32,u32> get_line_info(const String& filename, u32 idx);


inline parse_error parser_error(Parser &parser,parse_error error ,const Token &token,const char *fmt, ...)
{
    parser.error_count += 1;

    // further reporting becomes pointless past a single parser error
    if(parser.error_count > 1)
    {
        return error;
    }

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    const auto [line,col] = get_line_info(parser.cur_file,token.idx);
    printf("at: %s line %d col %d\n\n",parser.cur_file.buf,line,col);

    parser.line = line;
    parser.col = col;
    parser.idx = token.idx;
    return error;
}

void print_depth(int depth);
bool match(Parser &parser,token_type type);
Option<parse_error> consume(Parser &parser,token_type type);
Token peek(Parser &parser,u32 v);
void prev_token(Parser &parser);
ParserResult func_call(Parser& parser,AstNode *expr, const Token& t);
ParserResult arr_access(Parser& parser, const Token& t);
ParserResult struct_access(Parser& parser, AstNode* expr_node,const Token& t);
ParserResult array_index(Parser& parser,const Token& t);
ParserResult var(Parser& parser, const Token& sym_tok, b32 allow_call = false);
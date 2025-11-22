#pragma once
#include <destoer/destoer.h>
#include <token.h>
#include <type.h>
#include <sym.h>

enum class ast_type
{
    assign,
    arith_bin,
    arith_unary,
    shift,
    comparison,
    boolean_logic,
    symbol,
    builtin_access,
    type_operator,
    cast,
    initializer_list,
    designated_initializer_list,
    struct_initializer,
    sizeof_t,
    no_init,
    ignore,
    value,
    float_t,
    null_t,
    deref,
    addrof,
    string,
    block,
    type,
    type_alias,
    struct_t,
    enum_t,
    decl,
    global_decl,
    auto_decl,
    tuple_assign,
    function_call,
    struct_access,
    access_member,
    index,
    slice,
    for_iter,
    for_range,
    switch_t,
    if_t,
    while_t,
    const_assert,
    function,
    ret,
};

inline const char *AST_NAMES[] =
{
    "assign",
    "arith_bin",
    "arith_unary",
    "shift",
    "comparison",
    "boolean_logic",
    "symbol",
    "builtin_access",
    "type_operator",
    "cast",
    "initializer_list",
    "designated_initializer_list",
    "struct_initializer",
    "sizeof",
    "no_init",
    "ignore",
    "value",
    "float",
    "null",
    "deref",
    "addrof",
    "string",
    "block",
    "type",
    "type_alias",
    "struct",
    "enum",
    "decl",
    "global_decl",
    "auto_decl",
    "tuple_assign",
    "function_call",
    "struct_access",
    "access_member",
    "index",
    "slice",
    "for_iter",
    "for_range",
    "switch",
    "if",
    "while",
    "const_assert",
    "function",
    "ret"
};

struct AstNode
{
    Type* expr_type = nullptr;
    ast_type type;
    u32 idx;
    // Resulting value of this node is known.
    // Stored here to keep consistency with const expressions.
    Option<u64> known_value = option::none;
};

// Subtype expr bin oper
template<typename T>
struct ExprBinOperNode
{
    AstNode node;
    T oper;
    AstNode* left = nullptr;
    AstNode* right = nullptr;
};

using ArithBinNode = ExprBinOperNode<arith_bin_op>;
using ShiftNode = ExprBinOperNode<shift_op>;
using CmpNode = ExprBinOperNode<comparison_op>;
using BooleanLogicNode = ExprBinOperNode<boolean_logic_op>;

struct ArithUnaryNode
{
    AstNode node;
    arith_unary_op oper;
    AstNode* expr = nullptr;
};

// Subtype bin node
template<ast_type type>
struct ExprBinNode
{
    AstNode node;
    AstNode* left = nullptr;
    AstNode* right = nullptr;
};

using AssignNode = ExprBinNode<ast_type::assign>;


struct BuiltinAccessNode
{
    AstNode node;

    builtin_type type;
    String field;
};

template<ast_type type>
struct UnaryNode
{
    AstNode node;

    AstNode* expr = nullptr;
};

using SizeOfNode = UnaryNode<ast_type::sizeof_t>;
using ConstAssert = UnaryNode<ast_type::const_assert>;
using DerefNode = UnaryNode<ast_type::deref>;
using AddrOfNode = UnaryNode<ast_type::addrof>;

enum class compound_type
{
    nullable_ptr,
    ptr,
    arr_deduce_size,
    arr_var_size,
    arr_fixed_size,
};

static const char* COMPOUND_TYPE_NAMES[] =
{
    "nullable ptr",
    "ptr",
    "array deduce size",
    "array variable size",
    "array fixed size",
};

struct CompoundType
{ 
    compound_type type;
    // ^ on arr_fixed_sized
    AstNode* array_size = nullptr;
};

CompoundType make_compound_type(compound_type type)
{
    return CompoundType {type,nullptr};
}

CompoundType make_compound_type_fixed(AstNode* expr)
{
    return CompoundType {compound_type::arr_deduce_size,expr};
}


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

    Array<CompoundType> compound;
};

struct CastNode
{
    AstNode node;
    TypeNode* type = nullptr;
    AstNode* expr = nullptr;
};

struct SymbolNode
{
    AstNode node;
    NameSpace* name_space;

    union
    {
        // Before Type checking
        String name;
        // After Type checking;
        SymSlot sym_slot;
    };
};

enum class type_operator
{
    sizeof_type_t,
    sizeof_data_t,
};

static const char* TYPE_OPER_NAMES[] =
{
    "sizeof_type",
    "sizeof_data"
};

struct TypeOperatorNode
{
    AstNode node;
    type_operator oper;
    TypeNode* type = nullptr;
};

struct InitializerListNode
{
    AstNode node;
    Array<AstNode*> list;
};

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

struct StructInitializerNode
{
    AstNode node;

    NameSpace* name_space = nullptr;
    String struct_name;

    // Initializer list or designated initializer
    AstNode* initializer = nullptr;
    b32 is_return = false;
};


struct AstPlain 
{
    AstNode node;
};

struct ValueNode
{
    AstNode node;
    u64 value = 0;
    builtin_type type = builtin_type::void_t;
};

struct FloatNode
{
    AstNode node;
    f64 value = 0.0;
};

struct StringNode
{
    AstNode node;
    String string;
};

struct AstBlock
{
    Array<AstNode*> statement;
};

struct BlockNode
{
    AstNode node;
    AstBlock block;
};

struct AliasNode 
{
    AstNode node;

    String name;
    String filename;

    TypeNode* type = nullptr;
};


struct AutoDeclNode
{
    AstNode node;

    AstNode* expr = nullptr;

    union
    {
        // Before type checking
        String name;
        // After type checking
        SymSlot sym_slot = {INVALID_HANDLE};
    };
};

struct DeclNode
{
    AstNode node;

    TypeNode* type = nullptr;
    AstNode* expr = nullptr;

    b32 is_const = false;

    union
    {
        // Before type checking
        String name;
        // After type checking
        SymSlot sym_slot = {INVALID_HANDLE};
    };
};

struct GlobalDeclNode
{
    AstNode node;

    DeclNode *decl = nullptr;
    String filename;
    NameSpace* name_space;
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

struct AccessMemberNode
{
    AstNode node;
    String name;
};

struct StructAccessNode
{
    AstNode node;
    AstNode* expr;
    Array<AstNode*> members;
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

enum class func_call_type
{
    intrinsic,
    call,
};

struct FuncCallNode
{
    AstNode node;

    AstNode* expr = nullptr;
    func_call_type type = func_call_type::call;

    union
    {
        FuncCall call = {};
        size_t intrinsic_idx;
    };

    Array<AstNode*> args;
};


struct TupleAssignNode
{
    AstNode node;

    Array<AstNode*> symbols;
    FuncCallNode* func_call;
    b32 auto_decl;
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


struct ForIterNode
{
    AstNode node;

    AstNode* initializer = nullptr;
    AstNode* cond = nullptr;
    AstNode* post = nullptr;

    AstBlock block;
};

struct ForRangeNode
{
    AstNode node;

    AstNode* cond;
    AstBlock block;

    // encode
    // [@v, i]
    String name_one;
    String name_two;

    b8 take_pointer = false;
};


struct Case
{
    // final value
    u64 value = 0;

    // what label does this statement have
    LabelSlot label = {INVALID_HANDLE};

    // end block of the case
    BlockSlot end_block = {INVALID_HANDLE};

    // Allowed to be blank for a default
    AstNode* statement = nullptr;

    // Cannot get a fixed address on this. So we have to shove it on the heap
    AstBlock* block = nullptr;
};


struct SwitchNode
{
    AstNode node;

    AstNode* expr = nullptr;
    Array<Case> statements;
    Option<Case> default_statement = option::none;
};

struct IfStmt
{
    AstNode* expr = nullptr;
    AstBlock* block = nullptr;
};

struct IfNode
{
    AstNode node;

    IfStmt if_stmt;
    Array<IfStmt> else_if_stmt;
    AstBlock else_stmt;
};

struct WhileNode
{
    AstNode node;
    AstNode* expr = nullptr;
    AstBlock block;
};


struct FuncNode
{
    AstNode node;

    String name;
    String filename;

    Array<TypeNode*> return_type;
    AstBlock block;
    Array<DeclNode*> args;

    b32 va_args = false;
    String args_name;
    u32 attr_flags = 0;
};

struct RetNode
{
    AstNode node;
    Array<AstNode*> expr;
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

static constexpr u32 ATTR_NO_REORDER = (1 << 0);
static constexpr u32 ATTR_FLAG = (1 << 1);
static constexpr u32 ATTR_USE_RESULT = (1 << 2);


using ParserResult = Result<AstNode*,parse_error>;


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
// expression may terminate without hitting the terminator due to other flags
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
T* alloc_node(Parser& parser,ast_type type, const Token& token)
{
    AstNode* node = (AstNode*)allocate(*parser.ast_allocator,sizeof(T));

    // default init the actual type
    T* ret_node = (T*)node;
    *ret_node = {};

    node->type = type;
    node->idx = token.idx;

    return ret_node;
}

template<ast_type type, typename T>
ParserResult ast_expr_oper_bin(Parser& parser, T oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }

    ExprBinOperNode<T>* arith_node  = alloc_node<ExprBinOperNode<T>>(parser,type,token);

    arith_node->oper = oper;
    arith_node->left = *left_res;
    arith_node->right = *right_res;

    return (AstNode*)arith_node;
}


ParserResult ast_bin_arith(Parser& parser, arith_bin_op oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    return ast_expr_oper_bin<ast_type::arith_bin>(parser,oper,left_res,right_res,token);
}

ParserResult ast_shift(Parser& parser, shift_op oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    return ast_expr_oper_bin<ast_type::shift>(parser,oper,left_res,right_res,token);
}

ParserResult ast_comparison(Parser& parser, comparison_op oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    return ast_expr_oper_bin<ast_type::comparison>(parser,oper,left_res,right_res,token);
}

ParserResult ast_logic(Parser& parser, boolean_logic_op oper, ParserResult left_res, ParserResult right_res, const Token& token)
{
    return ast_expr_oper_bin<ast_type::boolean_logic>(parser,oper,left_res,right_res,token);
}


ParserResult ast_unary_arith(Parser& parser, arith_unary_op oper, ParserResult expr_res, const Token& token)
{
    if(!expr_res)
    {
        return expr_res;
    }

    ArithUnaryNode* arith_node  = alloc_node<ArithUnaryNode>(parser,ast_type::arith_unary,token);

    arith_node->oper = oper;
    arith_node->expr = *expr_res;

    return (AstNode*)arith_node;
}

template<ast_type type>
ParserResult ast_expr_bin(Parser& parser, ParserResult left_res, ParserResult right_res, const Token& token)
{
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }

    ExprBinNode<type>* bin_node  = alloc_node<ExprBinNode<type>>(parser,type,token);
    bin_node->left = *left_res;
    bin_node->right = *right_res;

    return (AstNode*)bin_node;    
}


ParserResult ast_equal(Parser& parser, ParserResult left_res, ParserResult right_res, const Token& token)
{
    return ast_expr_bin<ast_type::assign>(parser,left_res,right_res,token);
}

AstNode* ast_symbol(Parser& parser, NameSpace* name_space, const String& name, const Token &token)
{
    SymbolNode* sym_node  = alloc_node<SymbolNode>(parser,ast_type::symbol,token);
    sym_node->name = name;
    sym_node->name_space = name_space;

    return (AstNode*)sym_node;        
}


AstNode *ast_builtin_access(Parser& parser, builtin_type type, const String& field,const Token& token)
{
    BuiltinAccessNode* builtin_access = alloc_node<BuiltinAccessNode>(parser,ast_type::builtin_access,token);

    builtin_access->type = type;
    builtin_access->field = field;

    return (AstNode*)builtin_access;
}

AstNode* ast_type_operator(Parser& parser, TypeNode* type,type_operator kind, const Token& token)
{
    TypeOperatorNode* type_operator = alloc_node<TypeOperatorNode>(parser,ast_type::type_operator,token);
    type_operator->type = type;
    type_operator->oper = kind;

    return (AstNode*)type_operator;    
}

AstNode* ast_cast(Parser& parser, TypeNode* type, AstNode* expr, const Token& token)
{
    CastNode* cast = alloc_node<CastNode>(parser,ast_type::cast,token);
    cast->type = type;
    cast->expr = expr;

    return (AstNode*)cast;
}

InitializerListNode* ast_initializer_list(Parser& parser, const Token& token)
{
    InitializerListNode* list = alloc_node<InitializerListNode>(parser,ast_type::initializer_list,token);
    add_ast_pointer(parser,&list->list.data);

    return list;
}

DesignatedListNode* ast_designated_initializer_list(Parser& parser, const Token& token)
{
    DesignatedListNode* list = alloc_node<DesignatedListNode>(parser,ast_type::designated_initializer_list,token);
    add_ast_pointer(parser,&list->initializer.data);

    return list;
}

AstNode* ast_struct_initializer(Parser& parser,const String& literal, AstNode* initializer, NameSpace* name_space, const Token& token)
{
    StructInitializerNode* struct_initializer_node = alloc_node<StructInitializerNode>(parser,ast_type::struct_initializer,token);

    struct_initializer_node->struct_name = literal;
    struct_initializer_node->initializer = initializer;
    struct_initializer_node->name_space = name_space;

    return (AstNode*)struct_initializer_node;
}

template<ast_type type>
ParserResult ast_unary(Parser& parser,ParserResult expr_res, const Token& token)
{
    if(!expr_res)
    {
        return expr_res;
    }

    UnaryNode<type>* unary = alloc_node<UnaryNode<type>>(parser,type,token);
    unary->expr = expr_res.value();

    return (AstNode*)unary;
}

ParserResult ast_sizeof(Parser& parser,ParserResult expr, const Token& token)
{
    return ast_unary<ast_type::sizeof_t>(parser,expr,token);
}

ParserResult ast_deref(Parser& parser, ParserResult expr, const Token& token)
{
    return ast_unary<ast_type::deref>(parser,expr,token);
}

ParserResult ast_addrof(Parser& parser, ParserResult expr, const Token& token)
{
    return ast_unary<ast_type::addrof>(parser,expr,token);
}


ParserResult ast_const_assert(Parser& parser,ParserResult expr, const Token& token)
{
    return ast_unary<ast_type::const_assert>(parser,expr,token);
}

AstNode* ast_plain(Parser& parser, ast_type type, const Token& token)
{
    return alloc_node<AstNode>(parser,type,token);
}

AstNode* ast_value(Parser& parser, builtin_type type,  u64 value, const Token& token)
{
    ValueNode* value_node = alloc_node<ValueNode>(parser,ast_type::value,token);
    value_node->type = type;
    value_node->value = value;

    return (AstNode*)value_node;
}

AstNode *ast_float(Parser& parser, f64 value, const Token& token)
{
    FloatNode* float_node = alloc_node<FloatNode>(parser,ast_type::float_t,token);
    float_node->value = value;

    return (AstNode*)float_node;  
}

AstNode *ast_string(Parser& parser,const String &string, const Token& token)
{
    StringNode* string_node = alloc_node<StringNode>(parser,ast_type::string,token);
    string_node->string = string;

    return (AstNode*)string_node;    
}

TypeNode* ast_type_decl(Parser& parser, NameSpace* name_space, const String& name, const Token& token)
{
    TypeNode* type_node = alloc_node<TypeNode>(parser,ast_type::type,token);

    type_node->name = name;
    type_node->name_space = name_space;

    add_ast_pointer(parser,&type_node->compound.data);

    return type_node;   
}


AstNode *ast_alias(Parser& parser,TypeNode* type,const String &literal, const String& filename, const Token& token)
{
    AliasNode* alias_node = alloc_node<AliasNode>(parser,ast_type::type_alias,token);

    alias_node->filename = filename;
    alias_node->name = literal;
    alias_node->type = type;

    return (AstNode*)alias_node;
}

AstNode *ast_struct(Parser& parser,const String &name, const String& filename, const Token& token)
{
    StructNode* struct_node = alloc_node<StructNode>(parser,ast_type::struct_t,token);

    add_ast_pointer(parser,&struct_node->members.data);

    struct_node->name = name;
    struct_node->filename = filename;


    return (AstNode*)struct_node;
}  

AstNode *ast_global_decl(Parser& parser,DeclNode* decl_node, const String& filename,NameSpace* name_space, const Token& token)
{
    GlobalDeclNode* global_node = alloc_node<GlobalDeclNode>(parser,ast_type::global_decl,token);

    global_node->filename = filename;
    global_node->name_space = name_space;
    global_node->decl = decl_node;

    return (AstNode*)global_node;
}

AstNode* ast_decl(Parser& parser, const String& name,TypeNode* type, b32 is_const, const Token& token)
{
    DeclNode* decl_node = alloc_node<DeclNode>(parser,ast_type::decl,token);

    decl_node->name = name;
    decl_node->type = type;
    decl_node->is_const = is_const;

    return (AstNode*)decl_node;        
}


AstNode* ast_auto_decl(Parser& parser, const String& name, AstNode* expr, const Token& token)
{
    AutoDeclNode* decl_node = alloc_node<AutoDeclNode>(parser,ast_type::auto_decl,token);

    decl_node->name = name;
    decl_node->expr = expr;

    return (AstNode*)decl_node;        
}

AstNode* ast_enum(Parser& parser, const String& name,const String& filename,const Token& token)
{
    EnumNode* enum_node = alloc_node<EnumNode>(parser,ast_type::enum_t,token);

    enum_node->name = name;
    enum_node->filename = filename;
    
    add_ast_pointer(parser,&enum_node->member.data);

    return (AstNode*)enum_node;    
}


AstNode* ast_tuple_assign(Parser& parser, const Token& token)
{
    TupleAssignNode* tuple_node = alloc_node<TupleAssignNode>(parser,ast_type::tuple_assign,token);

    tuple_node->auto_decl = false;
    add_ast_pointer(parser,&tuple_node->symbols.data);

    return (AstNode*)tuple_node;
}

AstNode* ast_call(Parser& parser, AstNode* expr, const Token& token)
{
    FuncCallNode* func_call = alloc_node<FuncCallNode>(parser,ast_type::function_call,token);

    add_ast_pointer(parser,&func_call->args.data);

    // NOTE: this can encompass just a plain name for a function
    // or it could be for a symbol as part of a function pointer!
    func_call->expr = expr;

    return (AstNode*)func_call;
}

StructAccessNode* ast_struct_access(Parser& parser, AstNode* expr, const Token& token)
{
    StructAccessNode* struct_access = alloc_node<StructAccessNode>(parser,ast_type::struct_access,token);

    add_ast_pointer(parser,&struct_access->members.data);
    // Base access on the struct
    struct_access->expr = expr;

    return struct_access;
}

AstNode* ast_access_member(Parser& parser, const String& name, const Token& token)
{
    AccessMemberNode* member_access = alloc_node<AccessMemberNode>(parser,ast_type::access_member,token);
    member_access->name = name;

    return (AstNode*)member_access;
}

AstNode* ast_index(Parser& parser,const String &name, const Token& token)
{
    IndexNode* index_node = alloc_node<IndexNode>(parser,ast_type::index,token);
    index_node->name = name;

    add_ast_pointer(parser,&index_node->indexes.data);

    return (AstNode*)index_node;
}


AstNode* ast_slice(Parser& parser,const String &name, const Token& token)
{
    SliceNode* slice_node = alloc_node<SliceNode>(parser,ast_type::slice,token);
    slice_node->name = name;

    return (AstNode*)slice_node;
}

ForIterNode* ast_for_iter(Parser& parser, const Token& token)
{
    ForIterNode* for_iter = alloc_node<ForIterNode>(parser,ast_type::for_iter,token);

    return for_iter;
}

ForRangeNode* ast_for_range(Parser& parser, const Token& token)
{
    ForRangeNode* for_range = alloc_node<ForRangeNode>(parser,ast_type::for_range,token);

    return for_range;
}

SwitchNode* ast_switch(Parser& parser, AstNode* expr, const Token& token)
{
    SwitchNode* switch_node = alloc_node<SwitchNode>(parser,ast_type::switch_t,token);
    switch_node->expr = expr;

    add_ast_pointer(parser,&switch_node->statements.data);

    return switch_node;
}

IfNode* ast_if(Parser& parser, const Token& token)
{
    IfNode* if_node = alloc_node<IfNode>(parser,ast_type::if_t,token);

    add_ast_pointer(parser,&if_node->else_if_stmt.data);
    return if_node;
}

WhileNode* ast_while(Parser& parser,AstNode* expr,const Token& token)
{
    WhileNode* while_node = alloc_node<WhileNode>(parser,ast_type::while_t,token);
    while_node->expr = expr;

    return while_node;   
}

AstNode *ast_func(Parser& parser,const String &name, const String& filename, const Token& token)
{
    FuncNode* func_node = alloc_node<FuncNode>(parser,ast_type::function,token);

    add_ast_pointer(parser,&func_node->args.data);
    add_ast_pointer(parser,&func_node->return_type.data);

    func_node->name = name;
    func_node->filename = filename;
    return (AstNode*)func_node;
}

RetNode* ast_ret(Parser& parser, const Token& token)
{
    RetNode* ret = alloc_node<RetNode>(parser,ast_type::ret,token);
    add_ast_pointer(parser,&ret->expr.data);

    return ret;
}

BlockNode* ast_block(Parser& parser, const Token& token)
{
    return alloc_node<BlockNode>(parser,ast_type::block,token);
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
ParserResult var(Parser& parser, NameSpace* name_space, const Token& sym_tok, b32 allow_call = false);
void print(Interloper& itl, const AstNode *root);
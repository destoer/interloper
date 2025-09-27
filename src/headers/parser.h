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
};

inline const char *AST_NAMES[] =
{
    "assign"
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
    "sizeof_t",
    "no_init",
    "ignore",
    "value",
    "float",
    "null",
    "deref",
    "addrof",
    "string"
};

struct AstNode
{
    ast_type type;
    u32 idx;
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
using ShiftBinNode = ExprBinOperNode<shift_op>;
using CmpBinNode = ExprBinOperNode<comparison_op>;

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

using ExprEqualNode = ExprBinNode<ast_type::assign>;


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


enum class compound_type
{
    nullable_ptr,
    ptr,
    arr_deduce_size,
    arr_var_size,
    arr_fixed_size,
};

struct CompoundType
{ 
    compound_type type;
    // ^ on arr_fixed_sized
    AstNode* array_size = nullptr;
};

enum class type_node_kind
{
    builtin,
    func_pointer,
    user
};

struct TypeNode
{
    String name;

    b32 is_const;
    b32 is_constant;
    builtin_type builtin = builtin_type::void_t;
    type_node_kind kind = type_node_kind::builtin;
    
    FuncNode* func_type = nullptr;
    NameSpace* name_space = nullptr;

    Array<CompoundType> compound_type;
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

    union
    {
        // Before Type checking
        String name;
        // After Type checking;
        SymSlot slot;
    };
};

enum class type_operator
{
    sizeof_type_t,
    sizeof_data_t,
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

    ArithBinNode* arith_node  = alloc_node<ExprBinOperNode<T>>(parser,type,token);

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

AstNode* ast_symbol(Parser& parser, const String& name, const Token &token)
{
    SymbolNode* sym_node  = alloc_node<SymbolNode>(parser,ast_type::symbol,token);
    sym_node->name = name;

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
// ParserResult arr_access(Parser& parser, const Token& t);
// ParserResult struct_access(Parser& parser, AstNode* expr_node,const Token& t);
// ParserResult array_index(Parser& parser,const Token& t);
ParserResult var(Parser& parser, const Token& sym_tok, b32 allow_call = false);
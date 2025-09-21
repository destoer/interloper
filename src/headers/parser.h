#pragma once
#include <destoer/destoer.h>
#include <token.h>
#include <type.h>
#include <sym.h>

enum class expr_type
{
    assign,
    arith_bin,
    arith_unary,
    shift,
    comparison,
    boolean_logic,
};

inline const char *AST_KIND_NAMES[] =
{
    "assign"
    "arith_bin",
    "arith_unary",
    "shift",
    "comparison",
    "boolean_logic",
};

struct ExprNode
{
    expr_type type;
    u32 idx;
};

// Subtype expr bin oper
template<typename T>
struct ExprBinOperNode
{
    ExprNode node;
    T oper;
    ExprNode* left = nullptr;
    ExprNode* right = nullptr;
};

using ArithBinNode = ExprBinOperNode<arith_bin_op>;
using ShiftBinNode = ExprBinOperNode<shift_op>;
using CmpBinNode = ExprBinOperNode<comparison_op>;

struct ArithUnaryNode
{
    ExprNode node;
    arith_unary_op oper;
    ExprNode* expr = nullptr;
};

// Subtype bin node
template<expr_type type>
struct ExprBinNode
{
    ExprNode node;
    ExprNode* left = nullptr;
    ExprNode* right = nullptr;
};

using ExprEqualNode = ExprBinNode<expr_type::assign>;

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
    ExprNode* array_size = nullptr;
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

using ExprResult = Result<ExprNode*,parse_error>;


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

template<typename B,typename T,typename K>
T* alloc_node(Parser& parser,K type, const Token& token)
{
    B* node = (B*)allocate(*parser.ast_allocator,sizeof(T));

    // default init the actual type
    T* ret_node = (T*)node;
    *ret_node = {};

    node->type = type;
    node->idx = token.idx;

    return ret_node;
}

template<expr_type type, typename T>
ExprResult ast_expr_oper_bin(Parser& parser, T oper, ExprResult left_res, ExprResult right_res, const Token& token)
{
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }

    ArithBinNode* arith_node  = alloc_node<ExprNode,ExprBinOperNode<T>>(parser,type,token);

    arith_node->oper = oper;
    arith_node->left = *left_res;
    arith_node->right = *right_res;

    return (ExprNode*)arith_node;
}


ExprResult ast_bin_arith(Parser& parser, arith_bin_op oper, ExprResult left_res, ExprResult right_res, const Token& token)
{
    return ast_expr_oper_bin<expr_type::arith_bin>(parser,oper,left_res,right_res,token);
}

ExprResult ast_shift(Parser& parser, shift_op oper, ExprResult left_res, ExprResult right_res, const Token& token)
{
    return ast_expr_oper_bin<expr_type::shift>(parser,oper,left_res,right_res,token);
}

ExprResult ast_comparison(Parser& parser, comparison_op oper, ExprResult left_res, ExprResult right_res, const Token& token)
{
    return ast_expr_oper_bin<expr_type::comparison>(parser,oper,left_res,right_res,token);
}

ExprResult ast_logic(Parser& parser, boolean_logic_op oper, ExprResult left_res, ExprResult right_res, const Token& token)
{
    return ast_expr_oper_bin<expr_type::boolean_logic>(parser,oper,left_res,right_res,token);
}


ExprResult ast_unary_arith(Parser& parser, arith_unary_op oper, ExprResult expr_res, const Token& token)
{
    if(!expr_res)
    {
        return expr_res;
    }

    ArithUnaryNode* arith_node  = alloc_node<ExprNode,ArithUnaryNode>(parser,expr_type::arith_unary,token);

    arith_node->oper = oper;
    arith_node->expr = *expr_res;

    return (ExprNode*)arith_node;
}

template<expr_type type>
ExprResult ast_expr_bin(Parser& parser, ExprResult left_res, ExprResult right_res, const Token& token)
{
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }

    ExprBinNode<type>* bin_node  = alloc_node<ExprNode,ExprBinNode<type>>(parser,type,token);
    bin_node->left = *left_res;
    bin_node->right = *right_res;

    return (ExprNode*)bin_node;    
}


ExprResult ast_equal(Parser& parser, ExprResult left_res, ExprResult right_res, const Token& token)
{
    return ast_expr_bin<expr_type::assign>(parser,left_res,right_res,token);
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
// ParserResult func_call(Parser& parser,AstNode *expr, const Token& t);
// ParserResult arr_access(Parser& parser, const Token& t);
// ParserResult struct_access(Parser& parser, AstNode* expr_node,const Token& t);
// ParserResult array_index(Parser& parser,const Token& t);
// ParserResult var(Parser& parser, const Token& sym_tok, b32 allow_call = false);
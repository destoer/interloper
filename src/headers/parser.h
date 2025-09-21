#pragma once
#include <destoer/destoer.h>
#include <token.h>
#include <type.h>
#include <sym.h>

enum class ast_kind
{
    plain,
    arith_bin_op,
    arith_unary_op,
};

inline const char *AST_KIND_NAMES[] =
{
    "plain",
    "arith_bin_op",
    "arith_unary_op",
};


struct AstNode
{
    ast_kind type;
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
T* alloc_node(Parser& parser, ast_kind type, const Token& token)
{
    AstNode* node = (AstNode*)allocate(*parser.ast_allocator,sizeof(T));

    // default init the actual type
    T* ret_node = (T*)node;
    *ret_node = {};

    node->type = type;
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

    ArithBinNode* arith_node  = alloc_node<ArithBinNode>(parser,ast_kind::arith_bin_op,token);

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

    ArithUnaryNode* arith_node  = alloc_node<ArithUnaryNode>(parser,ast_kind::arith_unary_op,token);

    arith_node->oper = oper;
    arith_node->expr = *expr_res;

    return (AstNode*)arith_node;
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
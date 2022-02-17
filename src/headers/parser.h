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

    cast,

    ret,

    type,

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

    "cast",

    "return",

    "type",

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

    // should not be used...
    "END"
};

struct Value
{
    Value(u32 value, bool s) : v(value), sign(s) {}

    u32 v;
    bool sign;
};

struct AstNode
{
    AstNode() 
    {
        this->value = Value(0,false);
    }

    // general astdata no literal required eg '+'
    AstNode(ast_type type)
    {
        this->type = type;
        this->value = Value(0,false);
    }

    // general astdata string literal required eg symbol
    AstNode(ast_type type, const std::string &literal)
    {
        this->type = type;
        this->literal = literal;
        this->value = Value(0,false);
    }

    // astdata decleration
    AstNode(Type type, const std::string &literal)
    {
        this->type = ast_type::type;
        this->literal = literal;
        this->variable_type = type;
    }

    
    // binary op
    AstNode(AstNode *l, AstNode *r, ast_type type, std::string literal = "")
    {
        nodes.push_back(l);
        nodes.push_back(r);
        this->type = type;
        this->literal = literal;
        this->value = Value(0,false);
    }


    // binary op value
    AstNode(AstNode *l, AstNode *r, Value value, std::string literal = "")
    {
        nodes.push_back(l);
        nodes.push_back(r);
        this->type = ast_type::value;
        this->literal = literal;
        this->value = value;
    }


    // node data
    ast_type type;
    std::string literal;

    // if we put anything in here
    // we need to make sure we add the copy into
    // copy_node
    union
    {
        // ast_type::type
        Type variable_type;

        // ast_type::value
        Value value;
    };

    // TODO: should this even be a pointer?
    std::vector<AstNode *> nodes;
};

// copy an entire set of nodes
AstNode *copy_node(const AstNode *node);



inline void delete_tree(AstNode *node)
{
    if(!node)
    {
        return;
    }


    for(auto &n: node->nodes)
    {
        delete_tree(n);
    }

    delete node;
    node = nullptr;
}

void print(const AstNode *root);


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

Token next_token(Parser &parser);
void prev_token(Parser &parser);
Token peek(Parser &parser,u32 v);
void consume(Parser &parser,token_type type);
bool match(Parser &parser,token_type type);
Value read_value(const Token &t);

AstNode *expr(Parser &parser,const Token &t);
AstNode *expr_terminate(Parser &parser,token_type t);
AstNode *expr_terminate(Parser &parser,token_type t, token_type &term);

std::optional<Type> get_type(Parser &parser,std::string &type_literal);
void type_panic(Parser &parser);


template<typename... Args>
inline void panic(Parser &parser,const Token &token,const char *fmt, Args... args)
{
    printf(fmt,args...);
    printf("\nat: line %d col %d\n",token.line,token.col);
    parser.error = true;
    parser.line = token.line;
}



bool parse(AstNode **root_ptr, const std::vector<Token> &tokens, const std::vector<std::string> &lines);
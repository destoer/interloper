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
    void init(const std::vector<std::string> *file,const std::vector<Token> *tokens);

    void parse(AstNode **root_ptr);
    
    AstNode *expr(const Token &t);
    Token next_token();

    bool error;
private:
    bool initialized = false;

    template<typename... Args>
    void panic(const Token &token,const char *fmt, Args... args)
    {
        printf(fmt,args...);
        const auto &lv = *file;
        printf("%s",lv[token.line].c_str());
        printf("\nat: line %d col %d\n",token.line,token.col);
        error = true;
        terminate = true;
    }

    AstNode *func();
    AstNode *block();
    AstNode *statement();

    Type get_type(std::string &type_literal);
    AstNode *declaration(const Type &var_type, const std::string &type_str);

    void prev_token();

    // pratt parser
    AstNode *expr_terminate(token_type t);
    AstNode *expression(int32_t rbp);

    int32_t lbp(const Token &t);
    AstNode *led(Token &t,AstNode *left);
    AstNode *nud(Token &t);
    AstNode *oper_eq(AstNode *left,Token t,ast_type oper);

    void consume(token_type type);
    void consume_expr(token_type type);
    bool match(token_type type);

    Token next_token_expr();

    Token peek(uint32_t v);

    const std::vector<Token> *tokens = nullptr;
    const std::vector<std::string> *file = nullptr;

    // pratt parser
    Token expr_tok;
    size_t tok_idx;
    uint32_t brace_count;
    bool terminate;
    token_type termination_type;
};
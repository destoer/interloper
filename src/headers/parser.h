#pragma once
#include <lib.h>
#include <token.h>

enum class ast_type
{
    root,
    block,

    function,
    function_args,

    value,
    symbol,

    ret,

    type,

    declaration,
    
    equal,
    plus,

    END
};

static constexpr size_t AST_TYPE_SIZE = static_cast<size_t>(ast_type::END)+1;

inline const char *AST_NAMES[TOKEN_SIZE] =
{
    "root",
    "block",


    "function",
    "function_args",

    "value",
    "symbol",

    "return",

    "type",

    "declaration",

    "=",
    "+",

    // should not be used...
    "END"
};


struct AstData
{
    AstData() {}

    AstData(ast_type type)
    {
        this->type = type;
    }

    AstData(ast_type type, std::string literal)
    {
        this->type = type;
        this->literal = literal;
    }

    ast_type type;
    std::string literal;
};


struct AstNode
{
    // plain node
    AstNode(ast_type type)
    {
        data = AstData(type); 
    }

    AstNode(ast_type type, std::string literal)
    {
        data = AstData(type,literal);
    }
    
    // binary op
    AstNode(AstNode *l, const AstData &d, AstNode *r)
    {
        nodes.push_back(l);
        nodes.push_back(r);
        data = d;
    }



    AstData data;
    std::vector<AstNode *> nodes;
};

struct Parser
{
    Parser(const std::vector<Token> &t) : tokens(t) 
    {
        tok_idx = 0;
    }

    AstNode *parse();
    void print(const AstNode *root) const;
private:

    AstNode *func();
    AstNode *block();
    AstNode *statement();

    AstNode *type();
    AstNode *declartion(const std::string &type);

    AstNode *expr(const Token &t);

    // pratt parser
    AstNode *expression(int32_t rbp);

    int32_t lbp(const Token &t);
    AstNode *led(Token &t,AstNode *left);
    AstNode *nud(Token &t);

    void consume(token_type type);
    bool match(token_type type);

    Token next_token();
    Token peek(uint32_t v);

    const std::vector<Token> &tokens;


    // pratt parser
    bool seen_eq;
    Token expr_tok;
    size_t tok_idx;
};
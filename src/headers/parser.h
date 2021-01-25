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

    type,

    declare,
    

    END
};

static constexpr size_t AST_TYPE_SIZE = static_cast<size_t>(ast_type::END)+1;

inline const char *AST_NAMES[TOKEN_SIZE] =
{
    "root",
    "block",


    "function",
    "function_args"

    "value",

    "type",

    "declare",


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
    
    AstData data;
    std::vector<AstNode *> nodes;
};

struct Parser
{
    AstNode *parse(const std::vector<Token> &tokens);
private:

    AstNode *func(const std::vector<Token> &tokens);
    AstNode *type(const std::vector<Token> &tokens);

    void consume(const std::vector<Token> &tokens, token_type type);
    bool match(const std::vector<Token> &tokens, token_type type);

    Token next_token(const std::vector<Token> &tokens);

    size_t tok_idx;
};
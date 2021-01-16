#pragma once
#include <token.h>

struct Lexer
{
    std::vector<Token> tokenize(const std::vector<std::string> &file);
private:

    void tokenize_line(const std::string &line, std::vector<Token> &tokens);


    void decode_imm(const std::string &file, size_t &i,std::vector<Token> &tokens);

    bool is_keyword(const std::string &literal) const
    {
        return keywords.count(literal);
    }

    token_type keyword_token_type(const std::string &literal)
    {
        return keywords[literal];
    }


    std::unordered_map<std::string, token_type> keywords = 
    {
        {TOKEN_NAMES[static_cast<size_t>(token_type::s32)],token_type::s32},
        {TOKEN_NAMES[static_cast<size_t>(token_type::func)],token_type::func},
        {TOKEN_NAMES[static_cast<size_t>(token_type::ret)],token_type::ret}
    };
};


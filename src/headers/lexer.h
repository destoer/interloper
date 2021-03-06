#pragma once
#include <token.h>

struct Lexer
{
    std::vector<Token> tokenize(const std::vector<std::string> *file);

    bool error;
private:

    void tokenize_line(const std::string &line, std::vector<Token> &tokens);

    uint32_t column;
    uint32_t row;

    const std::vector<std::string> *file = nullptr;

    template<typename... Args>
    void panic(const char *fmt, Args... args)
    {
        printf(fmt,args...);
        const auto &lv = *file;
        printf("%s",lv[row].c_str());
        printf("\nat: line %d col %d\n",row,column);
        error = true;
    }

    void insert_token(std::vector<Token> &tokens,token_type type, const std::string &literal = "")
    {
        tokens.push_back(Token(type,literal,row,column));
    }



    void decode_imm(const std::string &file, uint32_t &i,std::vector<Token> &tokens);

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
        {tok_name(token_type::u8),token_type::u8},
        {tok_name(token_type::u16),token_type::u16},
        {tok_name(token_type::u32),token_type::u32},

        {tok_name(token_type::s8),token_type::s8},
        {tok_name(token_type::s16),token_type::s16},
        {tok_name(token_type::s32),token_type::s32},

        {tok_name(token_type::cast),token_type::cast},
        {tok_name(token_type::func),token_type::func},
        {tok_name(token_type::ret),token_type::ret}
    };
};


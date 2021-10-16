#pragma once
#include <token.h>


struct Lexer
{
    u32 column;
    u32 row;
    std::vector<Token> tokens;
};


// NOTE: return -> was there an error?
bool tokenize(Lexer &lexer,const std::vector<std::string> *file);
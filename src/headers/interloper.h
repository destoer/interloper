#pragma once
#include <lib.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>

struct Interloper
{
    void compile(const std::vector<std::string> &lines);

    ~Interloper()
    {
        delete_tree(root);
    }

private:


    void delete_tree(AstNode *node)
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

    Lexer lexer;
    Parser parser;

    AstNode *root = nullptr;
};
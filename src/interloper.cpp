#include <interloper.h>

void Interloper::compile(const std::vector<std::string> &lines)
{

    const auto tokens = lexer.tokenize(&lines);

    //print_tokens(tokens);

    parser.parse(&lines,&tokens,&root);
    parser.print(root);
}

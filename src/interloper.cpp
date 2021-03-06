#include <interloper.h>

void Interloper::compile(const std::vector<std::string> &lines)
{

    const auto tokens = lexer.tokenize(&lines);

    if(lexer.error)
    {
        exit(1);
    }

    //print_tokens(tokens);

    parser.parse(&lines,&tokens,&root);
    parser.print(root);

    if(parser.error)
    {
        exit(1);
    }
}

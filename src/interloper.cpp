#include <interloper.h>

void Interloper::compile(const std::vector<std::string> &lines)
{

    const auto tokens = lexer.tokenize(&lines);

    //print_tokens(tokens);

    auto ast = parser.parse(&lines,&tokens);
    parser.print(ast);
}

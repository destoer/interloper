#include <interloper.h>

int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc != 2)
    {
        printf("usage: %s <file to compile>\n",argv[0]);
        return -1;
    }

    

    const auto file = read_file(argv[1]);

    printf("compiling file:\n%s\n",file.c_str());

    Lexer lexer;

    auto tokens = lexer.tokenize(file);
}
#include <interloper.h>
#include <test.h>


// test for expression parser
// semantic analysis
// code gen (2 stage), emit operations on vars
// then actual bytecode that worries about where things are placed
// control flow and scoping
// ...

int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc != 2)
    {
        printf("usage: %s <file to compile>\n",argv[0]);
        return -1;
    }

    // run tests
    if(std::string(argv[1]) == "-t")
    {
        run_tests();
        return 0;
    }

    

    const std::vector<std::string> file = read_string_lines(read_file(argv[1]));

    printf("compiling file: ");
    for(const auto &line: file)
    {
        printf("%s\n",line.c_str());
    }

    Interloper interloper;
    interloper.compile(file);
}
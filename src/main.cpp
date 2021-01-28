#include <interloper.h>


int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc != 2)
    {
        printf("usage: %s <file to compile>\n",argv[0]);
        return -1;
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
#include <interloper.h>

#include "lib.cpp"
#include "alloc.cpp"
#include "interloper.cpp"
#include "interpretter.cpp"
#include "test/test.cpp"



int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc != 2)
    {
        printf("usage: %s <file to compile>\n",argv[0]);
        return -1;
    }

    // parse compiler flags


    // run tests
    if(std::string(argv[1]) == "-t")
    {
        run_tests();
        return 0;
    }

    Interloper itl;
    compile(itl,get_program_name(argv[1]));

    if(itl.error)
    {
        return 1;
    }

    
    run(itl.interpretter,reinterpret_cast<uint8_t*>(itl.program.data()),itl.program.size() * sizeof(Opcode));

    destroy_itl(itl);
}
#include <interloper.h>

#include "lib.cpp"
#include "interloper.cpp"
#include "interpretter.cpp"
#include "test.cpp"



struct Config
{
    b32 print_ast = false;
    b32 print_ir = false;
    b32 print_tokens = false;

    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false;

    b32 print_types = false;     
};


Config parse_flags(const char* flags)
{
    Config cfg;

    u32 i = 1;
    while(flags[i])
    {
        switch(flags[i])
        {
            case 'i': cfg.print_ir = true; break;
            case 'a': cfg.print_ast = true; break;
            case 'r': cfg.print_reg_allocation = true; break;
            case 's': cfg.print_stack_allocation = true; break;
            case 'c': cfg.print_types = true; break;
            case 'l': cfg.print_tokens = true; break;

            default: crash_and_burn("unknown flag: %c\n",flags[i]); 
        }

        i++;
    }

    return cfg;
}

int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc <= 1)
    {
        printf("usage: %s <flags> <file to compile>\n",argv[0]);
        return -1;
    }

    // run tests
    if(string_equal(argv[1],"-t"))
    {
        run_tests();
        return 0;
    }


    Interloper itl;

    // parse compiler flags
    const char* filename = "";

    if(argv[1][0] == '-')
    {
        // we have flags of some kind
        auto cfg = parse_flags(argv[1]);

        // move over our config
        itl.print_ast = cfg.print_ast;
        itl.print_ir = cfg.print_ir;
        itl.print_tokens = cfg.print_tokens;

        itl.print_reg_allocation = cfg.print_reg_allocation;
        itl.print_stack_allocation = cfg.print_stack_allocation;
        itl.print_types = cfg.print_types;

        // for now just assume flags are in a batch
        if(argc == 3)
        {
            filename = argv[2];
        }
    }

    else
    {
        filename = argv[1];
    }


    compile(itl,filename);

    if(itl.error)
    {
        return 1;
    }

    Interpretter interpretter = make_interpretter();
    run(interpretter,itl.program);

    destroy_itl(itl);
    destroy_interpretter(interpretter);
}
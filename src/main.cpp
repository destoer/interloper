#include <interloper.h>

#include <destoer.cpp>
#include "interloper.cpp"
#include "test.cpp"




void parse_flags(Interloper& itl,const char* flags)
{
    u32 i = 1;
    while(flags[i])
    {
        switch(flags[i])
        {
            case 'i': itl.print_ir = true; break;
            case 'a': itl.print_ast = true; break;
            case 'r': itl.print_reg_allocation = true; break;
            case 's': itl.print_stack_allocation = true; break;
            case 'g': itl.global_alloc.print_global = true; break;
            case 'c': itl.print_types = true; break;
            case 'l': itl.print_tokens = true; break;
            case 'q': itl.compile_only = true; break;
            case 'z': itl.optimise = true; break;
            case 't': break;

            default: crash_and_burn("unknown flag: %c\n",flags[i]); 
        }

        i++;
    }
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
    if(argv[1][0] == '-' && string_contains(argv[1],"t"))
    {
        run_tests(argv[1]);
        return 0;
    }


    Interloper itl;

    // parse compiler flags
    const char* filename = "";

    if(argv[1][0] == '-')
    {
        // we have flags of some kind
        parse_flags(itl,argv[1]);

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


    destroy_itl(itl);

    if(!itl.compile_only)
    {
        s32 rc = WEXITSTATUS(system("./test-prog"));

        printf("exit code: %d\n",rc);

        return rc;
    }
}
#include <interloper.h>

#include "interloper_new.cpp"
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
            case 'y': itl.stack_alloc = true; break;
            case 'd': itl.itl_log = true; break;
            case 't': break;

            default: crash_and_burn("unknown flag: %c\n",flags[i]); 
        }

        i++;
    }

    itl.debug = !itl.optimise;
}

void print_usage(const char* name)
{
    printf("usage: %s <flags> <file to compile> <output name>\n",name);
    puts("-i print ir");
    puts("-a print ast");
    puts("-r print register allocation");
    puts("-s print stack allocation");
    puts("-g print global");
    puts("-c print types");
    puts("-l print tokens");
    puts("-q compile only");
    puts("-z optimize");
    puts("-y disable register allocation (stack only)");
    puts("-t run tests");
    puts("-d interloper logging");
}

int main(int argc, char *argv[])
{
    // just one file arg for now
    if(argc <= 1)
    {
        print_usage(argv[0]);
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
    const char* source_filename = "";
    const char* executable_path = "./out";

    if(argv[1][0] == '-')
    {
        // we have flags of some kind
        parse_flags(itl,argv[1]);

        // for now just assume flags are in a batch
        if(argc >= 3)
        {
            source_filename = argv[2];
        }

        if(argc >= 4)
        {
            executable_path = argv[3];
        }
    }

    else if(argc >= 2)
    {
        source_filename = argv[1];

        if(argc >= 3)
        {
            executable_path = argv[2];
        }
    }

    else
    {
        print_usage(argv[0]);
        return -1;
    }


    const auto compile_err = compile(itl,source_filename,executable_path);
    if(!!compile_err)
    {
        puts("NOT OK");
        return 1;
    }

    printf("Parsing time %.2lf ms\n",itl.parsing_time);
    printf("Code gen and type checking %0.2lf ms\n",itl.code_gen_time);
    printf("Optimisation time %.2lf ms\n",itl.optimise_time);
    printf("Backend time %.2lf ms\n",itl.backend_time);
    printf("Total compiler time: %.2lf ms\n",itl.parsing_time + itl.code_gen_time + itl.optimise_time + itl.backend_time);

    destroy_itl(itl);

    if(!itl.compile_only)
    {
        s32 rc = WEXITSTATUS(system(executable_path));

        printf("exit code: %d\n",rc);

        return rc;
    }
}
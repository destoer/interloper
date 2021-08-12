#include <interloper.h>
#include <test.h>


struct ProgramTest
{
    const char *name;
    s32 expected;
    bool error;
};

//TODO:
// tests for void return
// and for no return from non void

static constexpr ProgramTest PROGRAM_TEST[] = 
{
    {"tests/first.itl",9,false},
    {"tests/arith.itl",31,false},
    {"tests/narrow.itl",-1,true},
    {"tests/assign.itl",1190,false},
    {"tests/unary.itl",-11,false},
    {"tests/builtin_type.itl",271,false},
    {"tests/redeclare.itl",-1,true},
    {"tests/undeclared.itl",-1,true},
    {"tests/scope.itl",16,false},
    {"tests/out_of_scope.itl",-1,true},
    {"tests/func.itl",7,false},
    {"tests/no_main.itl",-1,true},
    {"tests/invalid_args.itl",-1,true},
    {"tests/void_assign.itl",-1,true},
    {"tests/void_return.itl",0,false},
    {"tests/void_no_return.itl",0,false},
    {"tests/bitwise.itl",-713,false},
    {"tests/logical.itl",1,false},
    {"tests/invalid_sign_cmp.itl",-1,true},
    {"tests/u32_eq_s32.itl",-1,true},
    {"tests/decl.itl",255,false},
    {"tests/redefine_func.itl",-1,true},
};

static constexpr u32 PROGRAM_TEST_SIZE = sizeof(PROGRAM_TEST) / sizeof(ProgramTest);

void run_tests()
{
    puts("running tests....");


    lexer_test();
    expr_test();

    puts("\nprogram tests\n");

    Interloper interloper;
    Interpretter interpretter;
    for(u32 i = 0; i < PROGRAM_TEST_SIZE; i++)
    {
        const auto &test = PROGRAM_TEST[i];

        const std::vector<std::string> file = read_string_lines(read_file(test.name));
        if(!file.size())
        {
            printf("no such file: %s\n",test.name);
            return;
        }

        
        interloper.compile(file);

        if(test.error && interloper.error)
        {
            printf("Pass %s\n",test.name);
            continue;
        }

        else if(interloper.error && !test.error)
        {
            printf("Fail %s error does not match %d != %d\n",test.name,test.error,interloper.error);
            return;
        }


        const auto r = interpretter.run(reinterpret_cast<uint8_t*>(interloper.program.data()),interloper.program.size() * sizeof(Opcode));      


        if(test.expected != r)
        {
            printf("Fail %s return code does not match %d != %d\n",test.name,test.expected,r);
            return;
        }

        printf("Pass: %s\n",test.name);
    }

    puts("\nfinished testing\n");

}
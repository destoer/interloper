#include <interloper.h>

#include "expr.cpp"
#include "lexer_test.cpp"

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
    {"tests/void_return.itl",255,false},
    {"tests/void_no_return.itl",0,false},
    {"tests/bitwise.itl",-713,false},
    {"tests/logical.itl",1,false},
    {"tests/invalid_sign_cmp.itl",-1,true},
    {"tests/u32_eq_s32.itl",-1,true},
    {"tests/decl.itl",255,false},
    {"tests/redefine_func.itl",-1,true},
    {"tests/invalid_compare.itl",-1,true},
    {"tests/if.itl",25,false},
    {"tests/else_if.itl",45077,false},
    {"tests/nested_if.itl",575,false},
    {"tests/recur.itl",1,false},
    {"tests/arith_eq.itl",294,false},
    {"tests/for.itl",32,false},
    {"tests/for_outer_decl.itl",32,false},
    {"tests/for_single.itl",32,false},
    {"tests/mod.itl",233168,false},
    {"tests/shift.itl",1,false},
    {"tests/pointer.itl",-2,false},
    {"tests/deref_plain.itl",-1,true},
    {"tests/ptr_to_ptr.itl",1,true},
    {"tests/expected_ptr.itl",-1,true},
    {"tests/array.itl",1061,false},
    {"tests/array_size.itl",4,false},
    {"tests/array_ptr.itl",5,false},
    {"tests/array_initializer.itl",16,false},
    {"tests/array_conv.itl",6,false},
};

static constexpr u32 PROGRAM_TEST_SIZE = sizeof(PROGRAM_TEST) / sizeof(ProgramTest);

void run_tests()
{
    puts("running tests....");
    auto start = std::chrono::system_clock::now();

    lexer_test();
    expr_test();

    puts("\nprogram tests\n");

    Interloper itl;
    for(u32 i = 0; i < PROGRAM_TEST_SIZE; i++)
    {
        destroy_itl(itl);
        
        const auto &test = PROGRAM_TEST[i];

        const std::vector<std::string> file = read_string_lines(read_file(test.name));
        if(!file.size())
        {
            printf("no such file: %s\n",test.name);
            return;
        }

        
        compile(itl,file);

        if(test.error && itl.error)
        {
            printf("Pass %s\n",test.name);
            continue;
        }

        else if(itl.error && !test.error)
        {
            printf("Fail %s error does not match %d != %d\n",test.name,test.error,itl.error);
            return;
        }


        const auto r = run(itl.interpretter,reinterpret_cast<uint8_t*>(itl.program.data()),itl.program.size() * sizeof(Opcode));      


        if(test.expected != r)
        {
            printf("Fail %s return code does not match %d != %d\n",test.name,test.expected,r);
            return;
        }

        printf("Pass: %s\n",test.name);
    }

    destroy_itl(itl);

    puts("\nfinished testing\n");
    auto current = std::chrono::system_clock::now();

    auto count = static_cast<double>(std::chrono::duration_cast<std::chrono::milliseconds>(current - start).count()) / 1000.0;
    printf("total time taken %f\n",count);
}
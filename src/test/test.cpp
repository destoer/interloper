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
    // basic
    {"tests/basic/first.itl",9,false},
    {"tests/basic/arith.itl",31,false},
    {"tests/basic/narrow.itl",-1,true},
    {"tests/basic/assign.itl",1190,false},
    {"tests/basic/unary.itl",-11,false},
    {"tests/basic/builtin_type.itl",271,false},
    {"tests/basic/redeclare.itl",-1,true},
    {"tests/basic/undeclared.itl",-1,true},
    {"tests/basic/scope.itl",16,false},
    {"tests/basic/out_of_scope.itl",-1,true},
    {"tests/basic/void_assign.itl",-1,true},
    {"tests/basic/bitwise.itl",-713,false},
    {"tests/basic/logical.itl",1,false},
    {"tests/basic/invalid_sign_cmp.itl",-1,true},
    {"tests/basic/u32_eq_s32.itl",-1,true},
    {"tests/basic/decl.itl",255,false},
    {"tests/basic/invalid_compare.itl",-1,true},
    {"tests/basic/arith_eq.itl",294,false},
    {"tests/basic/mod.itl",233168,false},
    {"tests/basic/shift.itl",1,false},


    {"tests/func/func.itl",7,false},
    {"tests/func/no_main.itl",-1,true},
    {"tests/func/invalid_args.itl",-1,true},
    {"tests/func/void_return.itl",255,false},
    {"tests/func/void_no_return.itl",0,false},
    {"tests/func/redefine_func.itl",-1,true},
    {"tests/func/recur.itl",1,false},

    
    // control flow
    {"tests/control_flow/if.itl",25,false},
    {"tests/control_flow/else_if.itl",45077,false},
    {"tests/control_flow/nested_if.itl",575,false},
    {"tests/control_flow/for.itl",32,false},
    {"tests/control_flow/for_outer_decl.itl",32,false},
    {"tests/control_flow/for_single.itl",32,false},

    // pointers
    {"tests/ptr/pointer.itl",-2,false},
    {"tests/ptr/deref_plain.itl",-1,true},
    {"tests/ptr/ptr_to_ptr.itl",1,true},
    {"tests/ptr/expected_ptr.itl",-1,true},

    // arrays
    {"tests/array/array.itl",1061,false},
    {"tests/array/array_size.itl",4,false},
    {"tests/array/array_of_ptr.itl",5,false},
    {"tests/array/array_initializer.itl",16,false},
    {"tests/array/array_conv.itl",6,false},
    {"tests/array/array_var_size.itl",16,false},
    {"tests/array/array_mismatched_type.itl",-1,true},
    {"tests/array_assign_fixed_vla.itl",-1,true},
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
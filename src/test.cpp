#include <interloper.h>


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
    {"tests/basic/first",9,false},
    {"tests/basic/arith",31,false},
    {"tests/basic/narrow",-1,true},
    {"tests/basic/assign",1190,false},
    {"tests/basic/unary",-11,false},
    {"tests/basic/redeclare",-1,true},
    {"tests/basic/undeclared",-1,true},
    {"tests/basic/scope",16,false},
    {"tests/basic/out_of_scope",-1,true},
    {"tests/basic/void_assign",-1,true},
    {"tests/basic/bitwise",-713,false},
    {"tests/basic/logical",1,false},
    {"tests/basic/invalid_sign_cmp",-1,true},
    {"tests/basic/u32_eq_s32",-1,true},
    {"tests/basic/invalid_compare",-1,true},
    {"tests/basic/arith_eq",294,false},
    {"tests/basic/mod",233168,false},
    {"tests/basic/shift",1,false},
    {"tests/basic/comment",0,false},

    // type
    {"tests/type/decl",255,false},
    {"tests/type/builtin_type",271,false},
    {"tests/type/sizeof",1,false},
    {"tests/type/default_initializer",1,false},
    {"tests/type/byte",0,false},
    {"tests/type/byte_invalid",-1,true},

    // const
    {"tests/const/const_invalid_assign",-1,true},
    {"tests/const/const_pass_copy.itl",100,false},
    {"tests/const/const_pass_invalid_ptr.itl",-1,true},
    {"tests/const/const_valid.itl",10,false},
    {"tests/const/const_assign_value.itl",5,false},
    {"tests/const/const_invalid_ptr_assign.itl",-1,true},
    {"tests/const/const_array_index_invalid.itl",-1,true},

    // func
    {"tests/func/func",7,false},
    {"tests/func/no_main",-1,true},
    {"tests/func/invalid_args",-1,true},
    {"tests/func/void_return",255,false},
    {"tests/func/void_no_return",0,false},
    {"tests/func/redefine_func",-1,true},
    {"tests/func/recur",1,false},

    // control flow
    {"tests/control_flow/if",25,false},
    {"tests/control_flow/else_if",45077,false},
    {"tests/control_flow/nested_if",575,false},
    {"tests/control_flow/for",32,false},
    {"tests/control_flow/for_outer_decl",32,false},
    {"tests/control_flow/while",32,false},
    {"tests/control_flow/switch_no_default",73,false},
    {"tests/control_flow/switch",447,false},
    {"tests/control_flow/switch_duplicate",-1,true},

    // pointers
    // TODO: impl pointer casting (wait for coerce operation)
    {"tests/ptr/pointer",-2,false},
    {"tests/ptr/deref_plain",-1,true},
    {"tests/ptr/ptr_to_ptr",1,true},
    {"tests/ptr/expected_ptr",-1,true},
    {"tests/ptr/cast_ptr",1020,false},
    {"tests/ptr/null",1,false},
    {"tests/ptr/invalid_ptr_compare",-1,true},
    {"tests/ptr/null_invalid",-1,true},

    // wait for heap allocation
    //{"tests/ptr/ptr_to_array",3,false},
    //{"tests/ptr/ptr_to_fixed_array",-1,true},
    //{"tests/ptr/ptr_to_array_member",4,false},




    // arrays
    {"tests/array/array",1061,false},
    {"tests/array/array_size",16,false},
    {"tests/array/array_initializer",16,false},
    {"tests/array/array_conv",6,false},
    {"tests/array/array_pass_u32",-1,true},
    {"tests/array/array_auto_size",16,false},
    {"tests/array/array_mismatched_type",-1,true},
    {"tests/array/array_take_pointer",5,false},
    {"tests/array/array_of_ptr",6,false},
    {"tests/array/deref_array_of_ptr_invalid",-1,true},
    {"tests/array/array_multi_fixed_size.itl",7200,false},

    // TODO: type check array initalizer assignemnts

    // wait for heap allocation
    //{"tests/array/array_assign_vla",3,false},
    //{"tests/array/array_assign_vla_fixed",-1,true},
    

    // strings
    {"tests/string/char_array",7,false},
    {"tests/string/write_string",0,false},
    {"tests/string/write_string_static",0,false},

    // structs
    {"tests/struct/struct",495,false},
    {"tests/struct/struct_initializer",495,false},
    {"tests/struct/pass_struct",2,false},
    {"tests/struct/return_struct",3,false},
    {"tests/struct/point",5,false},
    {"tests/struct/array_of_struct",179,false},
    {"tests/struct/redeclare_struct",-1,true},
    {"tests/struct/redeclare_member",-1,true},
    {"tests/struct/nested_struct",20,false},
    {"tests/struct/recursive_struct_invalid",-1,true},
    {"tests/struct/struct_of_ptr",154,false},
    {"tests/struct/struct_of_arrays",28,false},
    {"tests/struct/struct_assign",6,false},

    // TODO: get this working when we overhaul our reg allocator
    //{"tests/struct/return_struct_tmp",3,false}.

    // stl
    {"tests/stl/linked_list",66,false},
    {"tests/stl/mem",0,false},
};

static constexpr u32 PROGRAM_TEST_SIZE = sizeof(PROGRAM_TEST) / sizeof(ProgramTest);

void run_tests()
{
    puts("running tests....");
    auto start = std::chrono::system_clock::now();


    puts("\nprogram tests\n");

    Interloper itl;
    Interpretter interpretter = make_interpretter();
    for(u32 i = 0; i < PROGRAM_TEST_SIZE; i++)
    {
        destroy_itl(itl);
        
        const auto &test = PROGRAM_TEST[i];
        
        compile(itl,test.name);

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


        const auto r = run(interpretter,itl.program);      


        if(test.expected != r)
        {
            printf("Fail %s return code does not match %d != %d\n",test.name,test.expected,r);
            return;
        }

        printf("Pass: %s\n",test.name);
    }

    destroy_itl(itl);
    destroy_interpretter(interpretter);

    puts("\nfinished testing\n");
    auto current = std::chrono::system_clock::now();

    auto count = static_cast<double>(std::chrono::duration_cast<std::chrono::milliseconds>(current - start).count()) / 1000.0;
    printf("total time taken %f\n",count);
}
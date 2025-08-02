#include <interloper.h>

struct ProgramCorrectTest
{
    const char *name;
    s32 expected;
};

struct ProgramErrorTest
{
    const char *name;
    itl_error error;
};

//TODO:
// tests for void return
// and for no return from non void

static constexpr ProgramErrorTest PROGRAM_ERROR_TEST[] = 
{
    // basic
    {"tests/basic/narrow",itl_error::int_type_error},
    {"tests/basic/redeclare",itl_error::redeclaration},
    {"tests/basic/undeclared",itl_error::undeclared},
    {"tests/basic/out_of_scope",itl_error::undeclared}, // TODO: should we add analysis for scope drops on this?
    {"tests/basic/void_assign",itl_error::undefined_type_oper},
    {"tests/basic/invalid_sign_cmp",itl_error::int_type_error},
    {"tests/basic/u32_eq_s32",itl_error::int_type_error},
    {"tests/basic/invalid_compare",itl_error::out_of_bounds},
    {"tests/basic/invalid_literal",itl_error::lexer_error},
    {"tests/basic/const_assert_fail",itl_error::const_assert},

    // type
    {"tests/type/byte_invalid",itl_error::pointer_type_error},
    {"tests/type/use_enum_result_fail",itl_error::unused_result},

    // const
    {"tests/const/const_invalid_assign",itl_error::const_type_error},
    {"tests/const/const_pass_invalid_ptr.itl",itl_error::const_type_error},
    {"tests/const/const_invalid_ptr_assign.itl",itl_error::const_type_error},
    //{"tests/const/const_array_index_invalid.itl",itl_error::const_type_error},

    // func
    {"tests/func/no_main",itl_error::undeclared},
    {"tests/func/invalid_args",itl_error::missing_args},
    {"tests/func/redefine_func",itl_error::redeclaration},
    {"tests/func/missing_return",itl_error::missing_return},

    // control_flow
    {"tests/control_flow/switch_duplicate",itl_error::redeclaration},

    // pointers
    {"tests/ptr/deref_plain",itl_error::pointer_type_error},
    {"tests/ptr/expected_ptr",itl_error::pointer_type_error},
    {"tests/ptr/invalid_ptr_compare",itl_error::pointer_type_error},
    {"tests/ptr/null_invalid",itl_error::mismatched_args},
    {"tests/ptr/ptr_to_fixed_array",itl_error::array_type_error},
    {"tests/ptr/reference_assign_null",itl_error::pointer_type_error},
    {"tests/ptr/reference_no_initializer",itl_error::pointer_type_error},

    // arrays
    {"tests/array/array_pass_u32",itl_error::array_type_error},
    {"tests/array/array_mismatched_type",itl_error::array_type_error},
    {"tests/array/deref_array_of_ptr_invalid",itl_error::pointer_type_error},
    {"tests/array/array_assign_vla_fixed",itl_error::array_type_error},
    {"tests/array/array_assign_vla_multi",itl_error::array_type_error},


    // strings

    // structs
    {"tests/struct/redeclare_struct",itl_error::redeclaration},
    {"tests/struct/redeclare_member",itl_error::redeclaration},
    {"tests/struct/recursive_struct_invalid",itl_error::black_hole},
    {"tests/struct/designated_missing_initializer",itl_error::struct_error},

    // stl


    // enum
    {"tests/enum/enum_invalid_member",itl_error::enum_type_error},
    {"tests/enum/enum_redeclare_member",itl_error::redeclaration},
    {"tests/enum/enum_redeclare",itl_error::redeclaration},

    // type alias

    // tuple
    {"tests/tuple/tuple_invalid_assign",itl_error::undefined_type_oper},
};

static constexpr u32 PROGRAM_ERROR_TEST_SIZE = sizeof(PROGRAM_ERROR_TEST) / sizeof(ProgramErrorTest);

static constexpr ProgramCorrectTest PROGRAM_CORRECT_TEST[] = 
{
    // basic
    {"tests/basic/first",9},
    {"tests/basic/arith",29},
    {"tests/basic/assign",0}, // 1190
    {"tests/basic/unary",0},// -11
    {"tests/basic/scope",16},
    {"tests/basic/bitwise",0}, // -713
    {"tests/basic/logical",1},
    {"tests/basic/short_circuit",60},
    {"tests/basic/arith_eq",1}, // 294
    {"tests/basic/mod",1}, // 233168
    {"tests/basic/shift",1},
    {"tests/basic/comment",0},
    {"tests/basic/constant",1},
    {"tests/basic/global",26},
    {"tests/basic/overflow",1},  // 65286
    {"tests/basic/const_assert_pass",0},
    {"tests/basic/float",1},

    // type
    {"tests/type/decl",255},
    {"tests/type/builtin_type",1}, // 271
    {"tests/type/sizeof",1},
    {"tests/type/default_initializer",1},
    {"tests/type/byte",0},
    {"tests/type/struct_punning",11},
    {"tests/type/type_query",1},
    {"tests/type/rtti",1},

    // const
    {"tests/const/const_pass_copy",100},
    {"tests/const/const_valid",10},
    {"tests/const/const_assign_value",5},
    {"tests/const/const_ptr_ptr",0},

    // func
    {"tests/func/func",9},
    {"tests/func/void_return",255},
    {"tests/func/void_no_return",0},
    {"tests/func/recur",1},
    {"tests/func/func_pointer",50},
    {"tests/func/func_pointer_struct_recur",21},

    // control flow
    {"tests/control_flow/if",25},
    {"tests/control_flow/else_if",1}, // 45077
    {"tests/control_flow/else_if_no_else",1}, // 94220
    {"tests/control_flow/else_empty",7},
    {"tests/control_flow/nested_if",1}, // 5727
    {"tests/control_flow/else_if_empty",4},
    {"tests/control_flow/for",32},
    {"tests/control_flow/for_idx",10},
    {"tests/control_flow/for_in",1},
    {"tests/control_flow/for_outer_decl",32},
    {"tests/control_flow/while",32},

    {"tests/control_flow/switch_no_default",73},
    {"tests/control_flow/switch",1}, // 447


    // pointers
    {"tests/ptr/pointer",1}, // -2
    {"tests/ptr/cast_ptr",1}, // 1020
    {"tests/ptr/ptr_to_ptr",1,},
    {"tests/ptr/null",1},
    {"tests/ptr/alias",1},
    {"tests/ptr/ptr_to_array",3},




    // arrays
    {"tests/array/array",1}, // 1061
    {"tests/array/array_size",16},
    {"tests/array/array_initializer",16},
    {"tests/array/array_conv",6},
    {"tests/array/array_conv_struct",6},
    {"tests/array/array_auto_size",16},
    {"tests/array/array_take_pointer",5},
    {"tests/array/array_of_ptr",6},
    {"tests/array/array_multi_fixed_size",1}, // 7200
    {"tests/array/array_vla_from_parts",11},
    {"tests/array/array_assign_vla",15},
    {"tests/array/array_recast",1},
    {"tests/array/array_slice",1},

    // strings
    {"tests/string/char_array",6},
    {"tests/string/write_string",0},
    {"tests/string/write_string_static",0},
    {"tests/string/str_conv",1},
    {"tests/string/stl_string",27},



    // structs
    {"tests/struct/struct",1}, // 495
    {"tests/struct/struct_initializer",1}, // 495
    {"tests/struct/pass_struct",2},
    {"tests/struct/return_struct",3},
    {"tests/struct/return_struct_tmp",3},
    {"tests/struct/point",5},
    {"tests/struct/array_of_struct",179},
    {"tests/struct/nested_struct",20},
    {"tests/struct/struct_of_ptr",154},
    {"tests/struct/struct_of_arrays",28},
    {"tests/struct/struct_assign",6},
    {"tests/struct/reorder",16},
    {"tests/struct/struct_ptr",10},
    {"tests/struct/struct_return",1},
    {"tests/struct/initializer_assign",14},
    {"tests/struct/designated_initializer",1},



    // stl
    {"tests/stl/mem",1},
    {"tests/stl/array",1},
    {"tests/stl/alloc",1},
    {"tests/stl/file_str",6},
    {"tests/stl/file_bin",1},
    {"tests/stl/hash_table",1},



    // enum
    {"tests/enum/enum",1},
    {"tests/enum/switch_enum",10},
    {"tests/enum/enum_struct",11},
    {"tests/enum/enum_flag",1},


    // type alias
    {"tests/type_alias/type_alias",65},

    // tuple
    {"tests/tuple/tuple",1},
};

static constexpr u32 PROGRAM_CORRECT_TEST_SIZE = sizeof(PROGRAM_CORRECT_TEST) / sizeof(ProgramCorrectTest);

void parse_flags(Interloper& itl,const char* flags);

b32 run_correctness_test(const ProgramCorrectTest& test, Interloper& itl, const char* flags, b32 optimized)
{
    destroy_itl(itl);
    
    itl.optimise = optimized;

    parse_flags(itl,flags);

    (void)compile(itl,test.name,"./out");

    printf("%s: ",optimized? "optimized" : "unoptimized");
    
    if(itl.error_count)
    {
        printf("fail %s compilation error: %s\n",test.name,ERROR_NAME[u32(itl.first_error_code)]);
        return true;
    }

    const auto r = WEXITSTATUS(system("./out"));     


    if(test.expected != r)
    {
        printf("Fail %s return code does not match %d != %d\n",test.name,test.expected,r);
        return true;
    }

    printf("Pass: %s\n",test.name);

    return false;
}

void run_tests(const char* flags)
{
    const char* itl_path = nullptr;

    if(file_exists("interloper"))
    {
        itl_path = ".";
    }

    else
    {
        itl_path = getenv("INTERLOPER_INSTALL_DIR");
        if(chdir(itl_path) == -1)
        {
            const int saved_errno = errno;
            crash_and_burn("Could not change to interloper dir %s: %s",itl_path,strerror(saved_errno));
        }
    }

    puts("running tests....");
    auto start = std::chrono::system_clock::now();


    b32 fail = false;

    puts("\nprogram tests\n");

    Interloper itl;

    for(u32 i = 0; i < PROGRAM_CORRECT_TEST_SIZE; i++)
    {
        auto& test = PROGRAM_CORRECT_TEST[i];

        if(run_correctness_test(test,itl,flags,false))
        {
            fail = true;
            break;
        }

        // if(run_correctness_test(test,itl,flags,true))
        // {
        //     fail = true;
        //     break;
        // }
    }

    if(!fail)
    {
        for(u32 i = 0; i < PROGRAM_ERROR_TEST_SIZE; i++)
        {
            destroy_itl(itl);
            
            const auto &test = PROGRAM_ERROR_TEST[i];

            (void)compile(itl,test.name,"./out");

            if(itl.first_error_code != test.error)
            {
                printf("Fail %s expected error code '%s' got '%s'\n",test.name,ERROR_NAME[u32(test.error)],ERROR_NAME[u32(itl.first_error_code)]);
                fail = true;
                break;
            }

            printf("Pass: %s\n",test.name);
        }
    }

    remove("./out");
    destroy_itl(itl);

    puts("\nfinished testing\n");
    auto current = std::chrono::system_clock::now();

    auto count = static_cast<double>(std::chrono::duration_cast<std::chrono::milliseconds>(current - start).count()) / 1000.0;
    printf("total time taken %f\n",count);
}
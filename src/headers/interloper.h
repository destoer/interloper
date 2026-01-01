#pragma once
#include <destoer/destoer.h>
using namespace destoer;

#include <error.h>
#include <token.h>
#include <lexer.h>
#include <parser.h>
#include <pool.h>
#include <type.h>
#include <sym.h>
#include <ir.h>
#include <interpretter.h>


struct Interloper
{
    Array<u8> program;

    u32 error_count = 0;
    itl_error first_error_code;

    u32 arith_depth = 0;

    String stl_path = "";

    Array<FileContext> saved_ctx;
    FileContext ctx;

    Parser parser;

    FunctionTable func_table;

    // Cur scope saved in FileContext
    NameSpace* global_namespace = nullptr;
    NameSpace* std_name_space = nullptr;

    SymbolTable symbol_table;
    GlobalAlloc global_alloc;

    ConstPool const_pool;

    // TODO: do we want a temp string buffer for other things?
    StringBuffer name_space_buffer;

    // Arena's
    ArenaAllocator list_allocator;
    ArenaAllocator namespace_allocator;

    ParserAllocator parser_alloc;
    Array<Array<Token>> file_tokens;
    
    // for longer lived strings, e.g func defs symbol names etc
    ArenaAllocator string_allocator;

    // allocating all things types!
    ArenaAllocator type_allocator;

    Array<Type*> alias_table;

    Type* usize_type = nullptr;
    Type* const_usize_type = nullptr;
    Type* ssize_type = nullptr;
    Type* void_type = nullptr;
    Type* byte_ptr_type = nullptr;
    Type* string_type = nullptr;

    // Array copy for debug printing of ast
    Array<TypeDef*> type_decl;

    Array<GlobalDeclNode*> constant_decl;
    Array<GlobalDeclNode*> global_decl;
    Function* global_func = nullptr;

    // memory collection for func pointer types
    Array<FuncSig*> func_pointer;

    StructTable struct_table;
    EnumTable enum_table;
    RttiCache rtti_cache;

    // targeting info
    arch_target arch = arch_target::x86_64_t;
    os_target os = os_target::linux_t;

    AsmEmitter asm_emitter;

    // compiler config

    // diagnostic
    b32 print_ast = false;
    b32 print_ir = false;
    b32 print_tokens = false;
    
    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false; 

    b32 print_types = false;

    // compiler options
    b32 rtti_enable = true;

    b32 optimise = false;
    b32 stack_alloc = false;

    b32 compile_only = false;

    b32 debug = true;

    b32 itl_log = false;

    double backend_time = 0.0;
    double code_gen_time = 0.0;
    double parsing_time = 0.0;
    double optimise_time = 0.0;
    double type_checking_time = 0.0;

    // Startup functions
    Function* memcpy = nullptr;

    Function* zero_mem = nullptr;
};

void pop_context(Interloper& itl);

struct [[nodiscard]] FileContextGuard
{
    FileContextGuard(Interloper& itl) : itl(itl) {}
    ~FileContextGuard()
    {
        pop_context(itl);
    }

    Interloper& itl;
};

void vprint_itl(Interloper& itl, const String& fmt, va_list args);
void print_itl(Interloper& itl, const String& fmt, ...);

inline itl_error compile_verror(Interloper &itl,itl_error error,const char *fmt, va_list args)
{
    itl.error_count += 1;

    if(itl.error_count == 1)
    {    
        itl.first_error_code = error;
    }

    // Only report the first 15 errors
    else if(itl.error_count > 15)
    {
        return error;
    }

    if(itl.ctx.filename != "")
    {
        const auto [line,col] = get_line_info(itl.ctx.filename,itl.ctx.expr->idx);

        printf("error: %s %d:%d: ",itl.ctx.filename.buf,line,col);


        vprint_itl(itl,fmt,args);
        putchar('\n');

        print_line(itl.ctx.filename,line);
    }

    else 
    {
        printf("error: ");

        vprint_itl(itl,fmt,args);
        putchar('\n');
    }

    return error;
}

inline itl_error compile_error(Interloper &itl,itl_error error,const char *fmt, ...)
{
    va_list args; 
    va_start(args, fmt);
    const auto err = compile_verror(itl,error,fmt,args);
    va_end(args);

    return err;
}

[[noreturn]]
inline void compile_panic(Interloper &itl,itl_error error,const char *fmt, ...)
{
    puts("Panic: ");
    va_list args; 
    va_start(args, fmt);
    (void)compile_verror(itl,error,fmt,args);
    va_end(args);
    exit(1);
}

void itl_warning(const char* fmt, ...)
{
    printf("warning: ");

    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);

    putchar('\n');    
}

#include <bit>

inline u32 log2(u32 idx)
{
    if(idx == 0)
    {
        return 0;
    }

    return std::bit_width(idx) - 1;      
}

inline bool is_pow2(s32 x)
{
    return (x & (x - 1)) == 0 && x != 0;
}


template<typename T, typename Y>
inline T bit_cast(Y v)
{
    static_assert(sizeof(T) == sizeof(Y));

    T out;
    memcpy(&out,&v,sizeof(out));

    return out;
}

inline u64 bit_cast_from_f64(f64 v)
{
    return bit_cast<u64,f64>(v);
}

inline f64 bit_cast_to_f64(u64 v)
{
    return bit_cast<f64,u64>(v);
}

ConstValueResult type_check_const_int_expression(Interloper& itl, AstNode* node);
u32 align_val(u32 v,u32 alignment);

void push_context(Interloper& itl);
void pop_context(Interloper& itl);
void trash_context(Interloper& itl, String filename,NameSpace* cur_scope, AstNode* expr);
FileContextGuard switch_context(Interloper& itl, String filename,NameSpace* cur_scope, AstNode* expr);
Option<itl_error> compile_constants(Interloper& itl);
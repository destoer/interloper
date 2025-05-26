#pragma once
#include <destoer/destoer.h>
using namespace destoer;

#include <token.h>
#include <lexer.h>
#include <parser.h>
#include <pool.h>
#include <type.h>
#include <sym.h>
#include <ir.h>
#include <interpretter.h>


enum class [[nodiscard]] itl_error
{
    none,
    parse_error,
    lexer_error,
    array_type_error,
    int_type_error,
    illegal_cast,
    bool_type_error,
    string_type_error,
    enum_type_error,
    pointer_type_error,
    generic_type_error,
    out_of_bounds,
    undeclared,
    missing_initializer,
    missing_name,
    redeclaration,
    missing_args,
    tuple_mismatch,
    missing_return,
    invalid_expr,
    invalid_statement,
    mismatched_args,
    black_hole,
    struct_error,
    undefined_type_oper,
    const_type_error,
    const_assert,
    rtti_error,
    unimplemented,
};

static const char* ERROR_NAME[] = 
{
    "none",
    "parse error",
    "lexer error",
    "array type error",
    "int type error",
    "illegal cast",
    "bool type error",
    "string type error",
    "enum type error",
    "pointer type error",
    "generic type error",
    "out of bounds",
    "not declared",
    "missing initializer",
    "missing name",
    "redeclaration",
    "missing args",
    "tuple mismatch",
    "missing return",
    "invalid expr",
    "invalid statement",
    "mismatched args",
    "black hole",
    "struct error",
    "undefined type operation",
    "const type error",
    "const assert",
    "rtti error",
    "unimplemented",
};

struct FileContext
{
    AstNode *expr = nullptr;
    String filename = "";
    NameSpace *name_space = nullptr;
};


struct Interloper
{
    Array<u8> program;

    u32 error_count = 0;
    itl_error first_error_code;

    u32 arith_depth = 0;

    String stl_path = "";

    Array<FileContext> saved_ctx;
    FileContext ctx;

    FunctionTable func_table;
    Array<DeclNode*> global_def;

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
    ArenaAllocator ast_allocator;
    ArenaAllocator ast_string_allocator;
    ArenaAllocator namespace_allocator;

    AstPointers ast_arrays;
    
    // for longer lived strings, e.g func defs symbol names etc
    ArenaAllocator string_allocator;

    // allocating all things types!
    ArenaAllocator type_allocator;

    Array<Type*> alias_table;
    // Array copy for debug printing of ast
    Array<AstNode*> type_decl;

    Array<GlobalDeclNode*> constant_decl;
    Array<GlobalDeclNode*> global_decl;

    // memory collection for func pointer types
    Array<FuncSig*> func_pointer;

    StructTable struct_table;
    EnumTable enum_table;
    RttiCache rtti_cache;

    // targetting info
    arch_target arch = arch_target::x86_64_t;
    os_target os = os_target::linux_t;

    AsmEmitter asm_emitter;

    // compilier config

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

    double backend_time = 0.0;
    double code_gen_time = 0.0;
    double parsing_time = 0.0;
    double optimise_time = 0.0;
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


void print(const AstNode *root, b32 override_seperator = false);

inline void compile_error(Interloper &itl,itl_error error,const char *fmt, ...)
{
    itl.error_count += 1;

    if(itl.error_count == 1)
    {    
        itl.first_error_code = error;
    }

    // Only report the first 15 errors
    else if(itl.error_count > 15)
    {
        return;
    }

    if(itl.ctx.expr)
    {
        const auto [line,col] = get_line_info(itl.ctx.filename,itl.ctx.expr->idx);

        printf("error: %s %d:%d: ",itl.ctx.filename.buf,line,col);


        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);

        print_line(itl.ctx.filename,line);
    }

    else 
    {
        printf("error: ");

        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);
    }

    putchar('\n');
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

Option<std::pair<u64,Type*>> compile_const_int_expression(Interloper& itl, AstNode* node);
u32 align_val(u32 v,u32 alignment);

void push_context(Interloper& itl);
void pop_context(Interloper& itl);
void trash_context(Interloper& itl, String filename,NameSpace* cur_scope, AstNode* expr);
FileContextGuard switch_context(Interloper& itl, String filename,NameSpace* cur_scope, AstNode* expr);
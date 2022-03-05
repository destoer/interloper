#pragma once
#include <lib.h>
#include <ir.h>



// if type idx is >= to this then this is a custom defined type
static constexpr int BUILTIN_TYPE_SIZE = 8;

enum class builtin_type
{
    void_t, 

    u8_t,
    u16_t,
    u32_t,

    s8_t,
    s16_t,
    s32_t,

    bool_t,
};

static const char *TYPE_NAMES[BUILTIN_TYPE_SIZE] =
{
    "void",

    "u8",
    "u16",
    "u32",

    "s8",
    "s16",
    "s32",

    "bool",
};

struct BuiltinTypeInfo
{
    builtin_type type;

    // integer type?
    bool is_integer;

    bool is_signed;

    // how many bytes is the type
    u32 size;

    u32 min;

    u32 max;
};


extern const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE];


enum class contained_type
{
    plain,
    array,
    pointer,
};


inline const char *builtin_type_name(builtin_type t)
{
    return TYPE_NAMES[static_cast<size_t>(t)];
}


inline int conv_builtin_type(builtin_type t)
{
    return static_cast<int>(t);
}

inline builtin_type conv_type_idx(int type_idx)
{
    return static_cast<builtin_type>(type_idx);
}

struct AstNode;

static constexpr u32 MAX_ARR_SIZE = 4;
static constexpr u32 ARRAY_LEN_OFFSET = 1;

struct Type
{
    Type()
    {

    }

    // plain builtin type
    Type(builtin_type t) : type_idx(conv_builtin_type(t))
    {
        
    }

    u32 type_idx = 0;

    u32 ptr_indirection = 0;


    // array definiton

    // i.e is a pointer to an array
    // rather than a array of pointers
    b8 contains_array = 0;

    // either a number of RUNTIME_SIZE i.e variable length
    u32 dimensions[MAX_ARR_SIZE] = {0};
    u32 degree = 0;


    // type specifiers here i.e const

};

static const Type GPR_SIZE_TYPE = Type(builtin_type::u32_t);

inline bool is_builtin(const Type &t)
{
    return t.type_idx < BUILTIN_TYPE_SIZE;
}


inline bool is_pointer(const Type &t)
{
    return t.ptr_indirection >= 1;
}


inline bool is_array(const Type &t)
{
    return t.degree >= 1;
}


inline bool is_plain(const Type &t)
{
    return !is_pointer(t);
}


inline bool is_plain_builtin(const Type &t)
{
    return is_builtin(t) && is_plain(t);
}

inline bool is_bool(const Type &t)
{
    return is_plain_builtin(t) && conv_type_idx(t.type_idx) == builtin_type::bool_t;
}

inline bool is_integer(const Type &t)
{
    return is_plain_builtin(t) && builtin_type_info[t.type_idx].is_integer;
}

inline bool is_signed(const Type &t)
{
    return is_plain_builtin(t) && builtin_type_info[t.type_idx].is_signed;
}

inline bool is_signed_integer(const Type &t)
{
    return is_signed(t) && is_integer(t);
}

inline u32 builtin_size(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].size;
}

inline u32 builtin_max(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].max;
}

inline u32 builtin_min(builtin_type t)
{
    return builtin_type_info[static_cast<u32>(t)].min;
}


inline builtin_type cast_builtin(Type &type)
{
    return static_cast<builtin_type>(type.type_idx);
}



static constexpr u32 SYMBOL_NO_SLOT = 0xffffffff;
static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 UNALLOCATED_OFFSET = 0xffffffff;
static constexpr u32 PENDING_ALLOCATION = 0xf0000000;

static constexpr u32 LOCATION_MEM = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t, u32 s, u32 a = NON_ARG) : name(n), type(t), size(s), arg_num(a), 
        slot(SYMBOL_NO_SLOT), offset(UNALLOCATED_OFFSET), location(LOCATION_MEM), referenced(false)
    {}


    std::string name;
    Type type;

    u32 size;
    u32 arg_num;

    // what slot does this symbol hold inside the ir?
    u32 slot;

    
    // intialized during register allocation

    // where is it this is stored on the stack?
    u32 offset;

    // where is this item stored?
    // is it in memory or is it in register?
    u32 location;

    b8 referenced;
};
u32 slot_idx(const Symbol &sym);

inline bool is_arg(const Symbol &sym)
{
    return sym.arg_num != NON_ARG;
}

void print_sym(const Symbol &sym);


struct Label 
{
    Label(const std::string &name, u32 offset)
    {
        this->name = name;
        this->offset = offset;
    }

    std::string name;
    u32 offset;  
};


struct Function
{
    Function() {}

    Function(const std::string &n, Type rt, std::vector<u32> a, u32 s) : name(n), return_type(rt), args(a), slot(s)
    {}


    std::string name;
    Type return_type;

    // TODO: if we need debugging information on local vars we need an array
    // of slots for both normal vars so we know whats in the functions

    // gives slots into the main symbol table
    std::vector<u32> args;
    
    // IR code for function
    IrEmitter emitter;

    u32 slot;
};
void dump_ir(Function &func, const SlotLookup &slot_lookup, const LabelLookup &label_lookup);
void add_var(Function &func, u32 size);


// TODO: start by fixing all the compile errors
// for using a var alloc

// then actually try the symbol table impl
// maybe i will wrap it up internally inside just two
// std::maps while we get it off the ground


using SlotLookup = std::vector<Symbol>;
using LabelLookup = std::vector<Label>;

struct SymbolTable
{
    std::vector<std::unordered_map<std::string, u32>> table;

    SlotLookup slot_lookup;
    LabelLookup label_lookup;

    u32 sym_count = 0;
};

void new_scope(SymbolTable &sym_table);
void destroy_scope(SymbolTable &sym_table);
std::optional<Symbol> get_sym(SymbolTable &sym_table,const std::string &sym);
Symbol get_sym(SymbolTable &sym_table, u32 slot);
void add_symbol(SymbolTable &sym_table,Symbol &symbol);
void add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 size);
void add_var(SymbolTable &sym_table,Symbol &sym);
void add_scope(SymbolTable &sym_table, Symbol &sym);
void add_label(SymbolTable &sym_table,const std::string &label);
void clear(SymbolTable &sym_table);





struct Interloper;
Type effective_arith_type(Interloper& itl,const Type &ltype, const Type &rtype);
void check_logical_operation(Interloper& itl,const Type &ltype, const Type &rtype);
void check_assign(Interloper& itl,const Type &ltype, const Type &rtype);

u32 type_size(Interloper& itl,const Type &type);
u32 type_min(Interloper& itl,const Type &type);
u32 type_max(Interloper& itl,const Type &type);
std::string type_name(Interloper& itl,const Type &type);

void handle_cast(Interloper& itl,IrEmitter &emitter, u32 slot,u32 src_slot,const Type &old_type, const Type &new_type);
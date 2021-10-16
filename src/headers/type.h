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


struct Type
{
    Type() {}

    // plain builtin type
    Type(builtin_type t) : type_idx(conv_builtin_type(t)) 
    {
        
    }


    int type_idx;

    // arrays etc (ignore for now)
    /*
    int array_indirection;
    int ptr_indirection;

    // is this an array holding pointers
    // pointers to an array
    // for simplicty mixing containers like
    // int[]@[]@ is not permitted
    // or just a plain type?
    contained_type held_type;
    */

    // type specifiers here i.e const

};

inline bool is_builtin(const Type &t)
{
    return t.type_idx < BUILTIN_TYPE_SIZE;
}


inline bool is_bool(const Type &t)
{
    return is_builtin(t) && conv_type_idx(t.type_idx) == builtin_type::bool_t;
}

inline bool is_integer(const Type &t)
{
    return is_builtin(t) && builtin_type_info[t.type_idx].is_integer;
}

inline bool is_signed(const Type &t)
{
    return is_builtin(t) && builtin_type_info[t.type_idx].is_signed;
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



static constexpr uint32_t SYMBOL_NO_SLOT = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t, bool a = false) : name(n), type(t), is_arg(a), slot(SYMBOL_NO_SLOT)
    {}

    Symbol(const std::string &n, Type t, u32 s, bool a = false) : name(n), type(t), is_arg(a), slot(s)
    {}


    std::string name;
    Type type;

    // is this symbol a function argument?
    bool is_arg;

    // what slot does this symbol hold inside the ir?
    u32 slot;
};
u32 slot_idx(const Symbol &sym);



static constexpr u32 UNALLOCATED_OFFSET = 0xffffffff;
static constexpr u32 LOCATION_MEM = 0xffffffff;

struct VarAlloc
{
    VarAlloc(u32 s, const std::string &n, bool sign) : size(s), offset(UNALLOCATED_OFFSET), 
        location(LOCATION_MEM),is_signed(sign),name(n)
    {}

    // size of one item
    u32 size;

    // initialized during reg alloc
    u32 offset;

    // where is this item stored?
    // is it in memory or is it in register?
    u32 location;

    bool is_signed;

    // how many items do we have (for arrays)
    //u32 count;

    // symbol name
    std::string name;
};

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

    Function(const std::string &n, Type rt, std::vector<Symbol> a, u32 s) : name(n), return_type(rt), args(a), slot(s)
    {}


    std::string name;
    Type return_type;
    std::vector<Symbol> args;
    
    // IR code for function
    IrEmitter emitter;

    u32 slot;

    // get the back the symbol name from an allocated IR slot
    std::vector<VarAlloc> slot_lookup;


    // how many vars of each size have we got
    // this is the max count
    u32 size_count[3] = {0};

    // current size count
    u32 size_count_cur[3] = {0};
};
void dump_ir(Function &func,const std::vector<Label> &label_lookup);
u32 add_arg(Function &func,const std::string &name,const Type &type, u32 size);
u32 add_var(Function &func,const std::string &name,const Type &type, u32 size);


struct SymbolTable
{
    std::vector<std::unordered_map<std::string, Symbol>> table; 

    std::vector<Label> label_lookup;

    u32 sym_count = 0;
};

void new_scope(SymbolTable &sym_table);
void destroy_scope(SymbolTable &sym_table);
std::optional<Symbol> get_sym(SymbolTable &sym_table,const std::string &sym);
void add_symbol(SymbolTable &sym_table,Symbol &symbol, u32 slot);
void add_symbol(SymbolTable &sym_table,const std::string &name, const Type &type, u32 slot);
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

void handle_cast(Interloper& itl,IrEmitter &emitter,const Type &old_type, const Type &new_type);
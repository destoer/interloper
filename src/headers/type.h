#pragma once
#include <lib.h>




// if type idx is >= to this then this is a custom defined type
static constexpr int BUILTIN_TYPE_SIZE = 7;

enum class builtin_type
{
    void_t, 

    u8_t,
    u16_t,
    u32_t,

    s8_t,
    s16_t,
    s32_t,
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

inline bool is_builtin(uint32_t t)
{
    return t < BUILTIN_TYPE_SIZE;
}

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

inline bool is_integer(builtin_type t)
{
    return builtin_type_info[static_cast<size_t>(t)].is_integer;
}

inline bool is_signed(builtin_type t)
{
    return builtin_type_info[static_cast<size_t>(t)].is_signed;
}

inline bool is_signed_integer(builtin_type t)
{
    return is_signed(t) && is_integer(t);
}

inline u32 size(builtin_type t)
{
    return builtin_type_info[static_cast<size_t>(t)].size;
}

inline u32 max(builtin_type t)
{
    return builtin_type_info[static_cast<size_t>(t)].max;
}

inline u32 min(builtin_type t)
{
    return builtin_type_info[static_cast<size_t>(t)].min;
}


enum class contained_type
{
    plain,
    array,
    pointer,
};


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



static constexpr uint32_t SYMBOL_NO_SLOT = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t) : name(n), type(t), slot(SYMBOL_NO_SLOT)
    {}

    Symbol(const std::string &n, Type t, uint32_t s) : name(n), type(t), slot(s)
    {}

    std::string name;
    Type type;

    // what slot does this symbol hold inside the ir?
    uint32_t slot;
};


struct Function
{
    Function() {}

    Function(const std::string &n, Type rt, std::vector<Symbol> a) : name(n), return_type(rt), args(a)
    {}


    std::string name;
    Type return_type;
    std::vector<Symbol> args;
};

static constexpr u32 UNALLOCATED_OFFSET = 0xffffffff;


struct VarAlloc
{
    VarAlloc(u32 s, const std::string &n) : size(s), offset(UNALLOCATED_OFFSET), name(n)
    {}

    // size of one item
    u32 size;

    // initialized during reg alloc
    u32 offset;

    // how many items do we have (for arrays)
    //u32 count;

    // symbol name
    std::string name;
};

struct SymbolTable
{
    bool exists(const std::string &sym)
    {
        return table.count(sym);
    }

    const Symbol &operator[](const std::string &sym)
    {
        return table[sym];
    }

    void add_symbol(const std::string &name, const Type &type,u32 size)
    {
        // we only want to move quantitys under 4 bytes
        // larger thigns i.e structs
        // will allready have memory semantics
        assert(size <= 4);

        // shift by 1 to turn 1, 2, 4 
        // into and idx 
        size_count[size >> 1] += 1;

        slot_lookup.push_back(VarAlloc(size,name));
        table[name] = Symbol(name,type,sym_count++);
    }

    void clear()
    {
        slot_lookup.clear();
        table.clear();
        memset(size_count,0,sizeof(size_count));
        sym_count = 0;
    }

    std::unordered_map<std::string, Symbol> table; 

    // get the back the symbol name from an allocated IR slot
    std::vector<VarAlloc> slot_lookup;

    u32 sym_count = 0;

    // how many of each size have we added?
    u32 size_count[3] = {0};
};
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




static constexpr uint32_t SYMBOL_NO_SLOT = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t, bool a = false) : name(n), type(t), is_arg(a), slot(SYMBOL_NO_SLOT)
    {}

    Symbol(const std::string &n, Type t, u32 s, bool a = false) : name(n), type(t), is_arg(a), slot(s)
    {}

    u32 slot_idx(u32 slot) const
    {
        return is_arg? arg(slot) : symbol(slot);
    }


    std::string name;
    Type type;

    // is this symbol a function argument?
    bool is_arg;

    // what slot does this symbol hold inside the ir?
    u32 slot;
};



static constexpr u32 UNALLOCATED_OFFSET = 0xffffffff;

struct VarAlloc
{
    VarAlloc(u32 s, const std::string &n, bool sign) : size(s), offset(UNALLOCATED_OFFSET), 
        is_signed(sign), name(n)
    {}

    // size of one item
    u32 size;

    // initialized during reg alloc
    u32 offset;

    bool is_signed;

    // how many items do we have (for arrays)
    //u32 count;

    // symbol name
    std::string name;
};


struct Function
{
    Function() {}

    Function(const std::string &n, Type rt, std::vector<Symbol> a, u32 s) : name(n), return_type(rt), args(a), slot(s)
    {}

    // TODO: remove the need to pass a type by emitting dedicated 
    // mov signed instrs
    u32 add_var(const std::string &name,const Type &type, u32 size)
    {
        // we only want to move quantitys under 4 bytes
        // larger thigns i.e structs
        // will allready have memory semantics
        assert(size <= 4);

        const auto slot = slot_lookup.size();

        // shift by 1 to turn 1, 2, 4 
        // into and idx 
        size_count_cur[size >> 1] += 1;

        const bool sign = is_signed_integer(type);

        slot_lookup.push_back(VarAlloc(size,name,sign));

        return slot;
    }

    // allocation on stack is callee handled
    // so dont add to size count
    u32 add_arg(const std::string &name,const Type &type, u32 size)
    {

        // we only want to move quantitys under 4 bytes
        // larger thigns i.e structs
        // will allready have memory semantics
        assert(size <= 4);

        const auto slot = slot_lookup.size();

        const bool sign = is_signed_integer(type);

        auto alloc = VarAlloc(size,name,sign);

        // we allready know where this going to go for now
        // TODO: how do we want to mark an argument being inside a register
        // when we actually allocate them
        alloc.offset = arg_offset;
        arg_offset += sizeof(u32);

        slot_lookup.push_back(alloc); 

        return slot;       
    }

    void dump_ir(const std::vector<std::string> &label_lookup)
    {
        printf("%s:\n",name.c_str());

        u32 l = 0;
        for(const auto &block : emitter.program)
        {
            printf("L%d:\n",l);
            for(const auto &opcode : block)
            {
                printf("\t");
                disass_opcode_sym(opcode,slot_lookup,label_lookup);
            }
            l++;
        }

        printf("\n");       
    }

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

    u32 arg_offset = 0;

    // where is the funciton code located in the binary?
    u32 func_offset = 0;
};

struct SymbolTable
{
    void new_scope()
    {
        table.push_back({});
    }

    void destroy_scope()
    {
        sym_count -= table.back().size();
        table.pop_back();
    }

    // declare the global scope
    SymbolTable() { new_scope(); }

    std::optional<Symbol> get_sym(const std::string &sym)
    {
        for(int i = table.size()-1; i >= 0; i--)
        {
            if(table[i].count(sym))
            {
                return std::optional<Symbol>(table[i][sym]);
            }
        }

        return std::nullopt;
    }

    void add_symbol(Symbol &symbol, u32 slot)
    {
        symbol.slot = slot;
        table[table.size()-1][symbol.name] = symbol;
        sym_count++;
    }

    void add_symbol(const std::string &name, const Type &type, u32 slot)
    {
        table[table.size()-1][name] = Symbol(name,type,slot);
        sym_count++;
    }

    void add_label(const std::string &label)
    {
        label_lookup.push_back(label);
    }

    void clear()
    {
        table.clear();
        sym_count = 0;
    }

    std::vector<std::unordered_map<std::string, Symbol>> table; 

    std::vector<std::string> label_lookup;

    u32 sym_count = 0;
};
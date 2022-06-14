#pragma once
#include <lib.h>
#include <ir.h>



// if type idx is >= to this then this is a custom defined type
static constexpr u32 BUILTIN_TYPE_SIZE = 8;
static constexpr u32 STRUCT_IDX = 0xf0000000;
static constexpr u32 INVALID_TYPE = 0xffffffff;

// NOTE: expects to be defined in same order as tokens
enum class builtin_type
{
    u8_t,
    u16_t,
    u32_t,

    s8_t,
    s16_t,
    s32_t,

    bool_t,

    void_t, 
};

static const char *TYPE_NAMES[BUILTIN_TYPE_SIZE] =
{
    "u8",
    "u16",
    "u32",

    "s8",
    "s16",
    "s32",

    "bool",

    "void",
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


static constexpr u32 MAX_ARR_SIZE = 4;
static constexpr u32 ARRAY_LEN_OFFSET = 1;

// is runtime but has an initial stack allocation
static constexpr u32 RUNTIME_SIZE = 0xfffffff0;
static constexpr u32 DEDUCE_SIZE = RUNTIME_SIZE + 1;


inline int conv_builtin_type(builtin_type t)
{
    return static_cast<int>(t);
}


struct Type
{
    // TODO: remove these
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

    // either a number of RUNTIME_SIZE i.e variable length
    u32 dimensions[MAX_ARR_SIZE] = {0};
    u32 degree = 0;

    // array of pointers as opposed to a pointer to an array
    b32 contains_ptr = false;

    // type specifiers
    b32 is_const = false;

};

// struct entry
// TODO: how should we store default values?
// should we just hold pointers to the initial definition and not delete the struct tree?
struct Member
{
    std::string name;
    u32 offset;
    Type type;
};

struct Struct
{
    std::string name;

    std::vector<Member> members;

    // what do we hold?
    std::map<std::string,u32> member_map;

    // total size of the struct
    u32 size = 0;

    u32 type_idx = 0;
};


using StructLookup = std::vector<Struct>;

struct StructTable
{
    std::unordered_map<std::string, u32> table;

    StructLookup lookup;
};

std::optional<Struct> get_struct(StructTable& struct_table, const std::string& name);
Struct struct_from_type_idx(StructTable& struct_table, u32 type_idx);
Struct struct_from_type(StructTable& struct_table, const Type& type);

static const Type GPR_SIZE_TYPE = Type(builtin_type::u32_t);


static constexpr u32 SYMBOL_NO_SLOT = 0xffffffff;
static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 UNALLOCATED_OFFSET = 0xffffffff;
static constexpr u32 PENDING_ALLOCATION = 0xf0000000;

static constexpr u32 LOCATION_MEM = 0xffffffff;

struct Symbol
{
    Symbol() {}

    Symbol(const std::string &n, Type t, u32 s, u32 a = NON_ARG) : name(n), type(t), size(s), arg_offset(a), 
        slot(SYMBOL_NO_SLOT), offset(UNALLOCATED_OFFSET), location(LOCATION_MEM), referenced(false)
    {}


    std::string name;
    Type type;

    // cached sized of type
    u32 size;
    u32 arg_offset;

    // what slot does this symbol hold inside the ir?
    u32 slot;

    
    // intialized during register allocation

    // where is it this is stored on the stack?
    u32 offset;

    // where is this item stored?
    // is it in memory or is it in register?
    u32 location;

    b32 referenced;
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

using FuncTable = std::unordered_map<std::string, Function>;


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

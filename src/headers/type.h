#pragma once
#include <lib.h>
#include <ir.h>



// if type idx is >= to this then this is a custom defined type
static constexpr u32 BUILTIN_TYPE_SIZE = 10;
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

    byte_t,

    bool_t,

    void_t, 

    null_t,
};

static const char *TYPE_NAMES[BUILTIN_TYPE_SIZE] =
{
    "u8",
    "u16",
    "u32",

    "s8",
    "s16",
    "s32",

    "byte",

    "bool",

    "void",

    "NULL",
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

struct AstNode;

// struct entry
struct Member
{
    String name;
    u32 offset;
    Type type;

    AstNode* expr = nullptr;
};

struct Struct
{
    String name;

    Array<Member> members;

    // what do we hold?
    HashTable<u32> member_map;

    // total size of the struct
    u32 size = 0;

    u32 type_idx = 0;
};

enum class struct_state 
{
    not_checked,
    checking,
    checked,
};

struct AstNode;

struct StructDef
{
    struct_state state;
    AstNode* root;

    // current reserved struct slot
    u32 slot;
};

using StructDefMap = HashTable<StructDef>;



using StructLookup = std::vector<Struct>;

struct StructTable
{
    HashTable<u32> table;

    StructLookup lookup;
};

std::optional<Struct> get_struct(StructTable& struct_table, const String& name);
Struct struct_from_type_idx(StructTable& struct_table, u32 type_idx);
Struct struct_from_type(StructTable& struct_table, const Type& type);

static const Type GPR_SIZE_TYPE = Type(builtin_type::u32_t);

// TODO: delete the constructor, and dont forget to copy strings when we make symbols
struct Symbol
{
    String name;
    Type type;

    // cached sized of type
    u32 size = 0;
    u32 arg_offset = NON_ARG;

    // what slot does this symbol hold inside the ir?
    u32 slot = SYMBOL_NO_SLOT;

    
    // intialized during register allocation

    // where is it this is stored on the stack?
    u32 offset = UNALLOCATED_OFFSET;

    // where is this item stored?
    // is it in memory or is it in register?
    u32 location = LOCATION_MEM;

    b32 referenced = false;

/*
    need to think where we perorm the marking for this, because it has to be done after the optimisation pass

    u32 uses = 0;

    // NOTE: this uses absolute offsets
    // but we dont really care if they are broken by insertions during reg alloc 
    // because we only want to know when usage gap is largest
    Array<u32> usage = {};
*/
};




struct Label 
{
    String name;
    u32 offset;  
};


struct Function
{
    String name;
    Type return_type;

    // TODO: if we need debugging information on local vars we need an array
    // of slots for both normal vars so we know whats in the functions

    // gives slots into the main symbol table
    Array<u32> args;
    
    // IR code for function
    IrEmitter emitter;

    u32 slot;

    AstNode* root = nullptr;

    b32 used = false;
};

using FuncTable = HashTable<Function>;


void finalise_def(Function& func, Type rt, Array<u32> a, u32 s)
{
    func.return_type = rt;
    func.args = a;
    func.slot = s;
}


struct Interloper;

void mark_used(Interloper& itl, Function& func);


// TODO: start by fixing all the compile errors
// for using a var alloc

// then actually try the symbol table impl
// maybe i will wrap it up internally inside just two
// std::maps while we get it off the ground


using SlotLookup = std::vector<Symbol>;
using LabelLookup = std::vector<Label>;

struct SymbolTable
{
    Array<HashTable<u32>> table;

    SlotLookup slot_lookup;
    LabelLookup label_lookup;

    u32 sym_count = 0;

    u32 var_count = 0;

    ArenaAllocator *string_allocator;
};

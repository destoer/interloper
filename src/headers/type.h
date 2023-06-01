#pragma once
#include <destoer.h>
#include <ir.h>


// if type idx is >= to this then this is a custom defined type
static constexpr u32 BUILTIN_TYPE_SIZE = 10;
static constexpr u32 USER_TYPE = 0xf0000000;
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


enum class type_kind
{
    builtin,
    enum_t,
    struct_t,
    alias_t,
};


static constexpr u32 KIND_SIZE = 4;


inline const char* KIND_NAMES[KIND_SIZE] = 
{
    "builtin",
    "enum",
    "struct",
    "alias",
};



struct Enum;
struct Struct;

struct TypeDecl
{
    String name;
    type_kind kind;
    u32 type_idx;
};

enum class def_kind
{
    enum_t,
    struct_t,
    alias_t,
};

enum class def_state
{
    not_checked,
    checking,
    checked,
};

struct AstNode;

struct TypeDef
{
    String name;
    String filename;

    def_kind kind;
    def_state state;

    // what is the slot in the type table about to be?
    u32 slot;

    // the defintion root -> depends on the type!
    AstNode* root;
};

struct Type
{
    u32 type_idx;

    // specifiers
    b32 is_const;
};


struct PointerType
{
    Type type;

    Type* contained_type;
};

struct StructType
{
    Type type;

    u32 struct_idx;
};

struct EnumType
{
    Type type;

    u32 enum_idx;
};



struct ArrayType
{
    Type type;

    Type* contained_type;

    // RUNTIME_SIZE or current size!
    u32 size;

    
    // size of indexing array
    u32 sub_size;
};



static constexpr u32 POINTER = BUILTIN_TYPE_SIZE;
static constexpr u32 ARRAY = BUILTIN_TYPE_SIZE + 1;
static constexpr u32 STRUCT = BUILTIN_TYPE_SIZE + 3;
static constexpr u32 ENUM = BUILTIN_TYPE_SIZE + 4;
static constexpr u32 TUPLE = BUILTIN_TYPE_SIZE + 5;



struct EnumMember
{
    String name;
    u32 value;
};

struct Enum
{
    String name;
    String filename;

    HashTable<String,EnumMember> member_map;

    u32 type_idx;
};


using EnumTable = Array<Enum>;


std::optional<Enum> get_enum(EnumTable& enum_table, const String& name);
Enum enum_from_type(EnumTable& enum_table, const Type* type);

struct AstNode;

// struct entry
struct Member
{
    String name;
    u32 offset;
    Type *type;

    AstNode* expr = nullptr;
};

struct Struct
{
    String name;

    Array<Member> members;

    // what do we hold?
    HashTable<String,u32> member_map;

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


using StructTable = Array<Struct>;
Struct struct_from_type(StructTable& struct_table, const Type* type);

static const builtin_type GPR_SIZE_TYPE = builtin_type::u32_t;


struct TypeAlias
{
    String name;
    String filename;

    Type* type;
};

using AliasTable = Array<TypeAlias>;

// TODO: delete the constructor, and dont forget to copy strings when we make symbols
struct Symbol
{
    String name;
    Type* type;

    BlockSlot scope_end;

    Reg reg;

    u32 arg_offset = NON_ARG;
};




struct Label 
{
    String name;
    u32 offset;  
};


struct FuncNode;
struct Function
{
    String name;
    Array<Type*> return_type;

    u32 hidden_args = 0;

    // TODO: if we need debugging information on local vars we need an array
    // of slots for both normal vars so we know whats in the functions

    // gives slots into the main symbol table
    Array<SymSlot> args;
    
    // tmp's in the function
    Array<Reg> registers;

    // IR code for function
    IrEmitter emitter;

    LabelSlot label_slot;

    FuncNode* root = nullptr;

    b32 used = false;
};


struct Interloper;

void mark_used(Interloper& itl, Function& func);


using SlotLookup = Array<Symbol>;
using LabelLookup = Array<Label>;

struct SymbolTable
{
    Array<HashTable<String,SymSlot>> table;

    SlotLookup slot_lookup;

    // offset is the block slot until full resolution
    // after label resolution this holds the address of the label
    // I.e the address of the block
    LabelLookup label_lookup;

    u32 sym_count = 0;

    u32 var_count = 0;

    ArenaAllocator *string_allocator;
};

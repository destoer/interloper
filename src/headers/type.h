#pragma once
#include <destoer/destoer.h>
#include <ir.h>
#include <pool.h>


// if type idx is >= to this then this is a custom defined type
static constexpr u32 BUILTIN_TYPE_SIZE = 14;
static constexpr u32 USER_TYPE = 0xf0000000;
static constexpr u32 INVALID_TYPE = 0xffffffff;

// NOTE: expects to be defined in same order as tokens
enum class builtin_type
{
    u8_t,
    u16_t,
    u32_t,
    u64_t,

    s8_t,
    s16_t,
    s32_t,
    s64_t,

    c8_t,

    byte_t,

    bool_t,

    f64_t,

    null_t,

    // NOTE: used internally by compiler -> not accessible via RTTI
    // keep on the end

    void_t, 
};

static constexpr u32 FLOAT_SIZE = 8;

static constexpr u32 POINTER = BUILTIN_TYPE_SIZE;
static constexpr u32 ARRAY = BUILTIN_TYPE_SIZE + 1;
static constexpr u32 STRUCT = BUILTIN_TYPE_SIZE + 3;
static constexpr u32 ENUM = BUILTIN_TYPE_SIZE + 4;
static constexpr u32 TUPLE = BUILTIN_TYPE_SIZE + 5;
static constexpr u32 FUNC_POINTER = BUILTIN_TYPE_SIZE + 6;

static constexpr u32 RTTI_BUILTIN_SIZE = BUILTIN_TYPE_SIZE - 1;

static constexpr u32 POINTER_RTTI = RTTI_BUILTIN_SIZE;
static constexpr u32 ARRAY_RTTI = RTTI_BUILTIN_SIZE + 1;
static constexpr u32 STRUCT_RTTI = RTTI_BUILTIN_SIZE + 3;
static constexpr u32 ENUM_RTTI = RTTI_BUILTIN_SIZE + 4;

static const char *TYPE_NAMES[BUILTIN_TYPE_SIZE] =
{
    "u8",
    "u16",
    "u32",
    "u64",

    "s8",
    "s16",
    "s32",
    "s64",

    "c8",

    "byte",

    "bool",

    "f64",

    "NULL",

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

    u64 min;

    u64 max;
};


extern const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE];


// is runtime but has an initial stack allocation
static constexpr u32 RUNTIME_SIZE = 0xffff'fff0;
static constexpr u32 DEDUCE_SIZE = RUNTIME_SIZE + 1;


inline int conv_builtin_type(builtin_type t)
{
    return static_cast<int>(t);
}


enum class type_kind
{
    enum_t,
    struct_t,
    builtin,
};


static constexpr u32 KIND_SIZE = 3;


inline const char* KIND_NAMES[KIND_SIZE] = 
{
    "enum",
    "struct",
    "builtin",
};


struct Enum;
struct Struct;

static constexpr u32 INVALID_TYPE_IDX = 0xffff'ffff;

b32 invalid_type_idx(u32 type_idx)
{
    return type_idx == INVALID_TYPE_IDX;
}

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

struct TypeDecl
{
    String name;
    // what kind of type is it, and what index does it hold 
    // in the relevant typing table
    type_kind kind;
    u32 type_idx = INVALID_TYPE_IDX;

    u32 flags = 0;
};

static constexpr u32 TYPE_DECL_DEF_FLAG = (1 << 0);
static constexpr u32 TYPE_DECL_ALIAS_FLAG = (1 << 1);

struct TypeDef
{
    TypeDecl decl;

    String filename;

    // current definition state
    def_state state;
    def_kind kind;

    // the defintion root -> depends on the type!
    AstNode* root;
};

struct Type
{
    u32 type_idx;

    // specifiers
    b32 is_const;
};

static constexpr u32 TYPE_ATTR = 2;

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

static constexpr u32 VLA_SIZE = GPR_SIZE * 2;

// cache for rtti, these contain offsets and struct idx for all rtti structs
// NOTE: These are pulled from the defs in the type.itl file rather than having the compiler insert them
// so that any struct differences as a result of compile target is handled for us

// TODO: im quite sure for caching compound types
// we need to have a trie with a node holding something like
/*
struct TypeCacheNode
{
    Type* type;
    PoolSlot slot;
}

*/

struct TypeTrieNode
{
    PoolSlot slot = {NO_SLOT};
    const Type* type = nullptr;

    Array<TypeTrieNode> nodes;
};


struct RttiCache
{
    TypeTrieNode builtin_type_cache[TYPE_ATTR][BUILTIN_TYPE_SIZE];

    // how many bytes does this take in the binary?
    u32 type_data_size = 0;

    b32 struct_cached = false;

    // any cache
    u32 any_idx = 0;
    u32 any_data_offset = 0;
    u32 any_type_offset = 0;
    u32 any_struct_size = 0;

    // type struct cache
    u32 type_struct_size = 0;
    u32 is_const_offset = 0;
    u32 type_idx_offset = 0;

    // pointer struct cache
    u32 pointer_contained_offset = 0;
    u32 pointer_struct_size = 0;

    u32 array_contained_offset = 0;
    u32 array_size_offset = 0;
    u32 array_sub_size_offset = 0;
    u32 array_struct_size = 0;

    u32 enum_idx = 0;

    u32 struct_idx = 0;
};


struct EnumMember
{
    String name;
    u32 value;
};

enum class enum_type
{
    plain_t,
    struct_t,
    int_t,
};

struct Enum
{
    String name;
    String filename;

    HashTable<String,EnumMember> member_map;

    // self referential idx
    u32 type_idx = 0;

    // idx of struct for enum struct
    // or of a builtin type for a integer 
    u32 underlying_type_idx = INVALID_TYPE_IDX;

    PoolSlot struct_slot;

    enum_type kind;
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

    // data size without end padding
    // if no re order used
    u32 data_size = 0;

    u32 type_idx = 0;
};

enum class struct_state 
{
    not_checked,
    checking,
    checked,
};


using StructTable = Array<Struct>;
Struct& struct_from_type(StructTable& struct_table, const Type* type);

static const builtin_type GPR_SIZE_TYPE = builtin_type::u64_t;

std::optional<Member> get_member(StructTable& struct_table, const Type* type, const String& member_name);


struct TypeAlias
{
    String name;
    String filename;

    Type* type;
};

using AliasTable = Array<TypeAlias>;
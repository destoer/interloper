#pragma once
#include <error.h>
#include <parser.h>

struct FileContext
{
    String filename = "";
    NameSpace *name_space = nullptr;
    AstNode* expr = nullptr;
};

// NOTE: this may move during expression compilation
// prefer holding a slot to a reference
struct Symbol
{
    String name;
    Type* type;

    Reg reg;

    u32 arg_offset = NON_ARG;
    u32 references = 0;

    Option<u64> known_value = option::none;

    FileContext ctx;
};

struct SymbolNode;
RegResult symbol(Interloper &itl, SymbolNode* sym_node);
Result<SymSlot,itl_error> add_symbol(Interloper &itl,const String &name, Type *type);
Result<SymSlot,itl_error> add_global(Interloper& itl,const String &name, Type *type, b32 constant);

struct Label 
{
    String name;

    // NOTE: holds final label after asm pass
    // before this it holds a block slot associated with it
    u64 offset;  
};




struct FuncNode;

// NOTE: this is everything required to describe an abstract function
// so it can be used for function pointers
struct FuncSig
{
    Array<Type*> return_type;

    // gives slots into the main symbol table
    Array<SymSlot> args;
    Array<u32> pass_as_reg;
    u32 max_reg_pass = 0;
    u32 locked_set = 0;

    b32 va_args = false;
    u32 hidden_args = 0;
    u32 call_stack_size = 0;
    u32 attr_flags = 0;
};


struct ArgPass
{
    Array<RegSlot> args;
    Array<u32> pass_as_reg;
    u32 arg_clean = 0;
};

ArgPass make_arg_pass(const FuncSig& sig);
u32 pass_args(Interloper& itl, Function& func, ArgPass& pass);
void pass_arg(Interloper& itl, Function& func, ArgPass& pass,const TypedReg& reg, u32 arg_idx);

// NOTE: a func pointer is not a pointer to this struct
// just this struct  
struct FuncPointerType
{
    Type type;

    FuncSig sig;
};

const u32 FUNC_CALL_FUNC_POINTER_FLAG = (1 << 0);
const u32 FUNC_CALL_FUNC_POINTER_EXPR_FLAG = (1 << 1);

struct FuncCall
{
    FuncSig sig = {};
    String name = {};

    union
    {
        LabelSlot label_slot;
        RegSlot reg_slot = {};
    };

    u32 flags = 0;
};

struct Function
{
    String name;
    NameSpace* name_space = nullptr;

    FuncSig sig;

    // tmp's in the function
    Array<Reg> registers;

    // IR code for function
    IrEmitter emitter;

    LabelSlot label_slot;

    FuncNode* root = nullptr;
    b32 used = false;

    b32 leaf_func = true;

    FuncCall call_info;
};

struct FunctionDef
{
    FuncNode* root = nullptr;
    TopLevelDefiniton parser_def;

    // NOTE: we may not actually compile the function
    // we also don't want the memory to move when we insert
    // new ones
    Function* func = nullptr;
    NameSpace* name_space = nullptr;
    String name;
};

struct FunctionTable
{
    Array<FunctionDef> table;
    Array<Function*> used;
    ArenaAllocator arena;
};

Result<Function*,itl_error> finalise_func(Interloper& itl, FunctionDef& func_def, bool forced = false);

b32 func_exists(Interloper& itl, const String& name, const String& name_space);

Function* lookup_opt_scoped_function(Interloper& itl, NameSpace* name_space, const String& name);
Function* lookup_opt_global_function(Interloper& itl, const String& name);

Function* lookup_internal_function(Interloper& itl, const String& name);

enum class func_sig_kind
{
    function,
    function_pointer,
};

Option<itl_error> parse_func_sig(Interloper& itl,NameSpace* name_space,FuncSig& sig,const FuncNode& node, func_sig_kind kind);

struct Interloper;

void mark_used(Interloper& itl, Function& func);


using SlotLookup = Array<Symbol>;
using LabelLookup = Array<Label>;

enum class definition_type
{
    function,
    variable,
    type,
};


inline const char* DEFINITION_TYPE_NAMES[] = 
{
    "function",
    "variable",
    "type",
};

struct DefInfo
{
    definition_type type = definition_type::variable;
    union
    {
        u32 handle;
        TypeDecl* type_decl = nullptr;
    };
};

inline const char* definition_type_name(const DefInfo* info)
{
    return DEFINITION_TYPE_NAMES[u32(info->type)];
}

struct NameSpace
{
    NameSpace* parent = nullptr;
    String name_space;
    String full_name;
    HashTable<String,DefInfo> table;
    Array<NameSpace*> nodes;
};

struct FileContext;

struct SymbolTable
{
    SlotLookup slot_lookup;
    Array<SymSlot> global;

    // offset is the block slot until full resolution
    // after label resolution this holds the address of the label
    // I.e the address of the block
    LabelLookup label_lookup;

    ArenaAllocator *string_allocator;
    ArenaAllocator *namespace_allocator;
    
    FileContext* ctx;
};

void destroy_scope(SymbolTable &sym_table);

struct [[nodiscard]] SymbolScopeGuard
{
    SymbolScopeGuard(SymbolTable& table) : table(table) {}
    ~SymbolScopeGuard()
    {
        destroy_scope(this->table);
    }

    SymbolTable& table;
};

std::pair<u32,u32> calc_arr_allocation(Interloper& itl, const Type* type);
Symbol* get_sym(SymbolTable &sym_table,const String &sym);
Symbol* get_sym_internal(SymbolTable &sym_table,const String &sym, NameSpace* name_space);
Symbol& sym_from_slot(SymbolTable &table, SymSlot slot);

void default_construct_array(Interloper& itl, Function& func,ArrayType* type, AddrSlot addr_slot);
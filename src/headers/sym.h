#pragma once
// NOTE: this may move during expression compilation
// prefer holding a slot to a reference
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

    b32 va_args = false;
    u32 hidden_args = 0;
    u32 call_stack_size = 0;
};



// NOTE: a func pointer is not a pointer to this struct
// just this struct  
struct FuncPointerType
{
    Type type;

    FuncSig sig;
};


struct Function
{
    String name;
    DefNode* name_space = nullptr;

    FuncSig sig;

    // tmp's in the function
    Array<Reg> registers;

    // IR code for function
    IrEmitter emitter;

    LabelSlot label_slot;

    FuncNode* root = nullptr;
    b32 used = false;

    // TODO: we need locked ranges
    // this is just nice and simple for now
    u32 locked_set = 0;

    b32 leaf_func = true;
};


struct FunctionDef
{
    FuncNode* root = nullptr;
    // NOTE: we may not actually compile the function
    // we also don't want the memory to move when we insert
    // new ones
    Function* func = nullptr;
    DefNode* name_space = nullptr;
    String name;
};

struct FunctionTable
{
    Array<FunctionDef> table;
    Array<Function*> used;
    ArenaAllocator arena;
};


b32 func_exists(Interloper& itl, const String& name, const String& name_space);

Function* lookup_opt_scoped_function(Interloper& itl, const String& name,const String& name_space);
Function* lookup_opt_global_function(Interloper& itl, const String& name);

Function& lookup_internal_function(Interloper& itl, const String& name);

void parse_func_sig(Interloper& itl,DefNode* name_space,FuncSig& sig,const FuncNode& node);

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

struct DefNode
{
    DefNode* parent = nullptr;
    String name_space;
    String full_name;
    HashTable<String,DefInfo> table;
    Array<DefNode*> nodes;
};

struct SymbolTable
{
    SlotLookup slot_lookup;
    Array<SymSlot> global;

    // offset is the block slot until full resolution
    // after label resolution this holds the address of the label
    // I.e the address of the block
    LabelLookup label_lookup;

    ArenaAllocator *string_allocator;
    
    // Current symbol table scope
    // TODO: it may be better if this was accessed by reference to the file ctx
    DefNode* scope;
};

std::pair<u32,u32> calc_arr_allocation(Interloper& itl, Symbol& sym);
Symbol* get_sym(SymbolTable &sym_table,const String &sym);
Symbol& sym_from_slot(SymbolTable &table, SymSlot slot);

void default_construct_arr(Interloper& itl, Function& func,ArrayType* type, AddrSlot addr_slot);
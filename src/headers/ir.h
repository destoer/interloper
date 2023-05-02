#pragma once
#include <destoer.h>

enum class op_type
{
    mov_reg,
    add_reg,
    sub_reg,
    mul_reg,
    div_reg,
    mod_reg,


    lsl_reg,
    asr_reg,
    lsr_reg,

    xor_reg,
    or_reg,
    and_reg,
    not_reg,
    

    sxb,
    sxh,

    mov_imm,
    add_imm,
    sub_imm,
    mul_imm,

    and_imm,
    xor_imm,

    lb,
    lh,
    lw,

    lsb,
    lsh,

    sb,
    sh,
    sw,

    lea,

    push,
    pop,

    pushm,
    popm,

    call,
    ret,

    swi,

    // compare unsigned
    cmpugt_imm,
    cmpult_reg,
    cmpule_reg,
    cmpugt_reg,
    cmpuge_reg,


    // compare signed
    cmpsgt_imm,
    cmpslt_reg,
    cmpsle_reg,
    cmpsgt_reg,
    cmpsge_reg,

    // dont care about sign for equality
    cmpeq_reg,
    cmpne_reg,

    bnc,
    bc,
    b,
    b_reg,

    load_arr_len,
    load_arr_data,

    // DIRECTIVES
    // varabile on the stack is out of scope
    // so we can reclaim allocation on the stack
    alloc_slot,
    free_slot,
    alloc,

    alloc_vla,

    // allocate and store offset inside a var
    // used for runtime array alloc
    buf_alloc,
    state_dump,

    // give a function call arg
    // how it will be passed will be decided in reg alloc
    push_arg,

    // perform cleanup after a function call
    // free the stack space for args
    alloc_stack,
    free_stack,
    clean_args,

    // branch to end of if statement chain
    exit_block,

    // used when the end of the block is read past in the optimiser
    placeholder,

    spill_rv,
    spill,
    spill_all,
    
    load,

    addrof,

    save_regs,
    restore_regs,

    pool_addr,

    // just c++ things not used
    END,
};

// general operation defined for unsigned or unsigned ints
// will be convered to a specific conterpart in op_type
enum class logic_op
{
    cmpgt_imm,

    cmplt_reg,
    cmple_reg,
    cmpgt_reg,
    cmpge_reg,

    cmpeq_reg,
    cmpne_reg,

    and_reg,
    or_reg,
};

static constexpr u32 LOGIC_OP_SIZE = 9;

static constexpr u32 OPCODE_SIZE = static_cast<u32>(op_type::END)+1;


// POOL
enum class pool_type 
{
    string_literal,
    label,
};


struct PoolSection
{
    u32 offset;
    u32 size;

    pool_type type;
};


enum class op_group
{
    reg_t,
    regm_t,
    imm_t,
    load_t,
    store_t,
    implicit_t,
    branch_t,
    branch_reg_t,
    slot_t,
};

enum class arg_type
{
    src_reg,
    dst_reg,
    imm,
    label,
    directive,
    none,
};


struct OpInfo
{
    op_group group;
    String fmt_string;
    u32 args;
    arg_type type[3];
};


enum class slot_type
{
    symbol,
    label,
    block,
};

static constexpr u32 INVALID_HANDLE = 0xffff'ffff;

template<slot_type type>
struct Slot
{
    u32 handle = INVALID_HANDLE;
};


template<slot_type type>
u32 hash_slot(u32 size, Slot<type> v)
{
    // TODO: impl a better integer hash func
    const u32 hash = v.handle;
    const u32 slot = hash & (size - 1); 

    return slot;
}


static constexpr u32 SYMBOL_NO_SLOT = 0xffff'ffff;
using SymSlot = Slot<slot_type::symbol>;

SymSlot sym_from_idx(u32 idx)
{
    return {idx};
}

static constexpr SymSlot SYM_ERROR = {SYMBOL_NO_SLOT};


using LabelSlot = Slot<slot_type::label>;

using BlockSlot = Slot<slot_type::block>;

extern const OpInfo OPCODE_TABLE[OPCODE_SIZE];

static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 UNALLOCATED_OFFSET = 0xe0000000;
static constexpr u32 PENDING_ALLOCATION = 0xf0000000;

static constexpr u32 LOCATION_MEM = 0xffffffff;
static constexpr u32 LOCATION_GLOBAL = 0xfffffffe;


struct Opcode
{
    Opcode() {}

    Opcode(op_type op, u32 v1, u32 v2, u32 v3)
    {
        this->op = op;
        this->v[0] = v1;
        this->v[1] = v2;
        this->v[2] = v3;
    }

    op_type op; 

    // operands (either a register id)
    // or an immediate (depends implictly on opcode type)
    // or a label number
    u32 v[3];
};

enum class reg_kind
{
    local,
    global,
    tmp,
};

static constexpr u32 SIGNED_FLAG = 1 << 0;

struct Reg
{
    reg_kind kind;

    // what slot does this symbol hold inside the ir?
    SymSlot slot = {SYMBOL_NO_SLOT};

    // how much memory does this thing use GPR_SIZE max (spilled into count if larger)
    // i.e this is for stack allocation to get actual var sizes use type_size();
    u32 size = 0;
    u32 count = 0;

    // intialized during register allocation

    // where is it this is stored on the stack?
    u32 offset = UNALLOCATED_OFFSET;

    // where is this item stored?
    // is it in memory or is it in register?
    u32 location = LOCATION_MEM;

    u32 flags = 0;

    // how many times has this currently been used?
    u32 uses = 0;

    // NOTE: this uses absolute offsets
    // but we dont really care if they are broken by insertions during reg alloc 
    // because we only want to know when usage gap is largest
    Array<u32> usage = {};
};

Reg make_reg(reg_kind kind,u32 size, u32 slot, b32 is_signed);
void print(const Reg& reg);

// standard symbols
static constexpr u32 SYMBOL_START = 0x80000000;


// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define some targets
static constexpr u32 MACHINE_REG_SIZE = 8;

static constexpr u32 SPECIAL_PURPOSE_REG_START = 0x7fffff00;

static constexpr u32 SP_IR = SPECIAL_PURPOSE_REG_START;
static constexpr u32 PC_IR = SPECIAL_PURPOSE_REG_START + 1;
static constexpr u32 RV_IR = SPECIAL_PURPOSE_REG_START + 2;
static constexpr u32 R0_IR = SPECIAL_PURPOSE_REG_START + 3;
static constexpr u32 R1_IR = SPECIAL_PURPOSE_REG_START + 4;

// dummy reg to tell compilier loads are not necessary for fixed arrays
static constexpr u32 ACCESS_FIXED_LEN_REG = SPECIAL_PURPOSE_REG_START + 5;

static constexpr SymSlot ACCESS_FIXED_LEN_REG_SLOT = {ACCESS_FIXED_LEN_REG};

// dont perform any moves
static constexpr u32 NO_SLOT = SPECIAL_PURPOSE_REG_START + 6;


const String SPECIAL_REG_NAMES[] = 
{
    "sp",
    "pc",
    "rv",
    "r0",
    "r1",
    "fixed_len",
    "null",
};

static constexpr u32 SP_NAME_IDX = 0;

static constexpr u32 SPECIAL_REG_SIZE = sizeof(SPECIAL_REG_NAMES) / sizeof(SPECIAL_REG_NAMES[0]);




// for use in the interpretter
static constexpr u32 SP = MACHINE_REG_SIZE;
static constexpr u32 PC = MACHINE_REG_SIZE + 1;
static constexpr u32 RV = 0;
static constexpr u32 R0 = 0;
static constexpr u32 R1 = 1;

static constexpr u32 PROGRAM_ORG = 0;


static constexpr u32 GPR_SIZE = sizeof(u32);



static constexpr u32 OP_SIZE = sizeof(Opcode);


struct SymbolTable;



// IR SYSCALLS
static constexpr u32 SYSCALL_EXIT = 0x0;
static constexpr u32 SYSCALL_WRITE_STRING = SYSCALL_EXIT + 1;


static constexpr u32 STACK_SIZE = 32 * 1024;

enum class block_type
{   
    if_t,
    else_if_t,
    else_t,
    chain_cmp_t,
    for_t,
    while_t,
    body_t,
    case_t,
};

inline const char *block_names[] =
{
    "if",
    "else_if",
    "else",
    "chain_cmp",
    "for",
    "while",
    "body",
};



struct ListNode
{
    Opcode opcode;
    ListNode *next = nullptr;
    ListNode *prev = nullptr;
};

struct List
{
    // global list Arena allocator
    ArenaAllocator *allocator = nullptr;

    ListNode *start = nullptr;

    ListNode *end = nullptr;
};
List make_list(ArenaAllocator* allocator);

static constexpr u32 INVALID_BLOCK = 0xffff'ffff;

struct Block
{
    List list;

    block_type type;

    // what is the corresponding label for this block?
    LabelSlot label_slot;

    u32 loop_nesting = 0;

    Array<u32> exit;
};

struct IrEmitter
{
    Array<Block> program;

};

struct Function;

void emit(Function& func,op_type op, u32 imm);
void emit(Function& func,op_type op, SymSlot v1, SymSlot v2, u32 imm);
void emit(Function& func,op_type op, SymSlot v1, u32 imm);
void emit(Function& func,op_type op, SymSlot v1, u32 v2, u32 v3);
void emit(Function &func,op_type op,LabelSlot v1, SymSlot v2);

void emit(Function& func,op_type op, SymSlot v1 = {}, SymSlot v2 = {}, SymSlot v3 = {});
void emit_block(Function &func,BlockSlot block,op_type op, SymSlot v1 = {}, SymSlot v2 = {}, SymSlot v3 = {});
void emit_block(Function &func,BlockSlot block,op_type op,LabelSlot v1, SymSlot v2 = {});

void emit(Function& func,op_type op, LabelSlot v1);


SymSlot emit_res(Function& func, op_type op, SymSlot v2 = {}, SymSlot v3 = {});
SymSlot emit_res(Function& func, op_type op, SymSlot v2, u32 v3);

void destroy_emitter(IrEmitter& emitter);

void new_block(ArenaAllocator* list_allocator,Function& func,block_type type, u32 slot); 

void disass_opcode_sym(const Opcode &opcode, const SymbolTable& table);
void disass_opcode_raw(const Opcode &opcode);

inline u32 symbol(u32 s)
{
    return SYMBOL_START + s;
}
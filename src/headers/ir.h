#pragma once
#include <lib.h>
#include <alloc.h>

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
    load,

    addrof,

    save_regs,
    restore_regs,

    pool_addr,

    free_reg,

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
static constexpr u32 CONST_POOL = 1;


// what kind of opcode is this?
// NOTE: when we add indirect branches we need it under a seperate group
enum class op_group
{
    reg_t,
    regm_t,
    imm_t,
    load_t,
    store_t,
    implicit_t,
    branch_t,
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
    const char *name;
    u32 args;
    arg_type type[3];
};

extern const OpInfo OPCODE_TABLE[OPCODE_SIZE];

static constexpr u32 SYMBOL_NO_SLOT = 0xffffffff;
static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 UNALLOCATED_OFFSET = 0xe0000000;
static constexpr u32 PENDING_ALLOCATION = 0xf0000000;

static constexpr u32 LOCATION_MEM = 0xffffffff;



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

// standard symbols
static constexpr u32 SYMBOL_START = 0x80000000;


// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define some targets
static constexpr u32 MACHINE_REG_SIZE = 4;

static constexpr u32 SPECIAL_PURPOSE_REG_START = 0x7fffff00;

static constexpr u32 SP_IR = SPECIAL_PURPOSE_REG_START;
static constexpr u32 PC_IR = SPECIAL_PURPOSE_REG_START + 1;
static constexpr u32 RV_IR = SPECIAL_PURPOSE_REG_START + 2;
static constexpr u32 R0_IR = SPECIAL_PURPOSE_REG_START + 3;
static constexpr u32 R1_IR = SPECIAL_PURPOSE_REG_START + 4;

// dummy reg to tell compilier loads are not necessary for fixed arrays
static constexpr u32 ACCESS_FIXED_LEN_REG = SPECIAL_PURPOSE_REG_START + 5;

// dont perform any moves
static constexpr u32 NO_SLOT = SPECIAL_PURPOSE_REG_START + 6;


const char* SPECIAL_REG_NAMES[] = 
{
    "sp",
    "pc",
    "rv",
    "r0",
    "r1",
    "fixed_len",
    "null",
};

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

struct Symbol;
struct SymbolTable;
struct Label;
using SlotLookup = std::vector<Symbol>;
using LabelLookup = std::vector<Label>;


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
    body_t,
};

inline const char *block_names[] =
{
    "if",
    "else_if",
    "else",
    "chain_cmp",
    "for",
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


struct Block
{
    List list;

    block_type type;

    // is considered the last block in a set of control flow
    bool last;
};

struct IrEmitter
{
    std::vector<Block> program;
    std::vector<u32> block_slot;


    // how many registers used in this expression
    u32 reg_count;
};

void emit(IrEmitter &emitter,op_type op, u32 v1 = 0, u32 v2 = 0, u32 v3 = 0);
void new_block(ArenaAllocator* list_allocator,IrEmitter &emitter,block_type type, u32 slot = 0xffffffff); 

void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table);
void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table, const LabelLookup &label_lookup);
void disass_opcode_raw(const Opcode &opcode);

inline u32 symbol(u32 s)
{
    return SYMBOL_START + s;
}
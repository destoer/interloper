#pragma once
#include <lib.h>

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


    // DIRECTIVES
    // varabile on the stack is out of scope
    // so we can reclaim allocation on the stack
    alloc_slot,
    free_slot,

    // give a function call arg
    // how it will be passed will be decided in reg alloc
    push_arg,

    // perform cleanup after a function call
    // free the stack space for args
    clean_args,

    // directives to make sure registers get preserved correctly across calls
    save_regs,
    restore_regs,

    // branch to end of if statement chain
    exit_block,

    // used when the end of the block is read past in the optimiser
    placeholder,

    spill_rv,

    spill,
    load,

    addrof,

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


// what kind of opcode is this?
// NOTE: when we add indirect branches we need it under a seperate group
enum op_group
{
    reg_t,
    regm_t,
    imm_t,
    load_t,
    implicit_t,
    branch_t,
    slot_t,
};

struct OpInfo
{
    op_group group;
    const char *name;
    u32 args;
};

extern const OpInfo OPCODE_TABLE[OPCODE_SIZE];




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

using opcode_iterator_t = std::list<Opcode>::iterator;

// standard symbols
static constexpr u32 SYMBOL_START = 0x80000000;


inline u32 reg(u32 r)
{
    return r;
}

inline u32 symbol(u32 s)
{
    return SYMBOL_START + s;
}

inline u32 symbol_to_idx(u32 s)
{
    return s - SYMBOL_START;
}

inline bool is_symbol(u32 s)
{
    return s >= SYMBOL_START;
}


// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define some targets
static constexpr u32 MACHINE_REG_SIZE = 4;

static constexpr u32 SPECIAL_PURPOSE_REG_START = 0x07000000;

static constexpr u32 SP_IR = SPECIAL_PURPOSE_REG_START;
static constexpr u32 PC_IR = SPECIAL_PURPOSE_REG_START + 1;
static constexpr u32 RV_IR = SPECIAL_PURPOSE_REG_START + 2;

// for use in the interpretter
static constexpr u32 SP = MACHINE_REG_SIZE;
static constexpr u32 PC = MACHINE_REG_SIZE + 1;
static constexpr u32 RV = 0;


static constexpr u32 GPR_SIZE = sizeof(u32);



inline bool is_reg(u32 r)
{
    return r < SPECIAL_PURPOSE_REG_START;
}

inline bool is_special_reg(u32 r)
{
    return r >= SPECIAL_PURPOSE_REG_START && r < SYMBOL_START;
}

inline bool is_tmp(u32 r)
{
    return r < SPECIAL_PURPOSE_REG_START;
}

static constexpr u32 OP_SIZE = sizeof(Opcode);

struct Symbol;
struct SymbolTable;
struct Label;
using SlotLookup = std::vector<Symbol>;
using LabelLookup = std::vector<Label>;

void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table, const LabelLookup &label_lookup);
void disass_opcode_raw(const Opcode &opcode);


// IR SYSCALLS
static constexpr u32 SWI_EXIT = 0x0;


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


struct Block
{
    Block(block_type t) : type(t)
    {}

    std::list<Opcode> buf;

    block_type type;

    // is considered the last block in a set of control flow
    bool last = false;
};

struct IrEmitter
{
    std::vector<Block> program;
    std::vector<u32> block_slot;


    // how many registers used in this expression
    u32 reg_count;

    // how do we handle resolving labels?
    u32 pc;
};

void emit(IrEmitter &emitter,op_type op, u32 v1 = 0, u32 v2 = 0, u32 v3 = 0);
void new_block(IrEmitter &emitter,block_type type, u32 slot = 0xffffffff);
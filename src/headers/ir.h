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

    push,
    pop,

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
    free_slot_stack,

    // give a function call arg
    // how it will be passed will be decided in reg alloc
    push_arg,

    // perform cleanup after a function call
    // free the stack space for args
    clean_args,

    // directives to make sure registers get preserved correctly across calls
    save_reg,
    restore_reg,

    // branch to end of if statement chain
    exit_block,

    // used when the end of the block is read past in the optimiser
    placeholder,

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

static constexpr u32 OPCODE_SIZE = static_cast<uint32_t>(op_type::END)+1;


// what kind of opcode is this?
enum op_group
{
    reg_t,
    imm_t,
    load_t,
    implicit_t,
    branch_t,
};

struct OpInfo
{
    op_group group;
    const char *name;
    uint32_t args;
};

extern const OpInfo OPCODE_TABLE[OPCODE_SIZE];




struct Opcode
{
    Opcode() {}

    Opcode(op_type op, uint32_t v1, uint32_t v2, uint32_t v3)
    {
        this->op = op;
        this->v1 = v1;
        this->v2 = v2;
        this->v3 = v3;
    }

    op_type op; 

    // operands (either a register id)
    // or an immediate (depends implictly on opcode type)
    // or a label number
    uint32_t v1;
    uint32_t v2;
    uint32_t v3;
};

// standard symbols
static constexpr uint32_t SYMBOL_START = 0x80000000;

// function args
static constexpr uint32_t SYMBOL_ARG_START = 0xf0000000;

inline uint32_t reg(uint32_t r)
{
    return r;
}

// need a better way to mark an arg cause we want to index the slots similar to the rest
inline uint32_t arg(uint32_t s)
{
    return SYMBOL_ARG_START + s;
}

inline bool is_arg(u32 s)
{
    return s >= SYMBOL_ARG_START;
}

inline uint32_t symbol(uint32_t s)
{
    return SYMBOL_START + s;
}

inline uint32_t symbol_to_idx(uint32_t s)
{
    return s >= SYMBOL_ARG_START? s - SYMBOL_ARG_START : s - SYMBOL_START;
}

inline bool is_reg(uint32_t r)
{
    return r < SYMBOL_START;
}


// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define some targets
static constexpr uint32_t MACHINE_REG_SIZE = 4;

static constexpr uint32_t SP = MACHINE_REG_SIZE;
static constexpr uint32_t PC = MACHINE_REG_SIZE + 1;

// so we the "first" register reserved for returns
static constexpr uint32_t RV = 0;

static constexpr uint32_t OP_SIZE = sizeof(Opcode);

struct SymbolTable;
struct VarAlloc;
struct Label;

void disass_opcode_sym(const Opcode &opcode, const std::vector<VarAlloc> &table, const std::vector<Label> &label_lookup);
void disass_opcode_raw(const Opcode &opcode);


// IR SYSCALLS
static constexpr u32 SWI_EXIT = 0x0;

struct IrEmitter
{
    void emit(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);

    void new_block(u32 slot = 0xffffffff);

    std::vector<std::list<Opcode>> program;
    std::vector<u32> block_slot;


    // how many registers used in this expression
    uint32_t reg_count;

    // how do we handle resolving labels?
    uint32_t pc;
};
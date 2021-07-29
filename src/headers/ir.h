#pragma once
#include <lib.h>

enum class op_type
{
    mov_reg,
    add_reg,
    sub_reg,
    mul_reg,
    div_reg,
    
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

    lb,
    lh,
    lw,

    lsb,
    lsh,

    sb,
    sh,
    sw,

    push,

    call,
    ret,

    swi,

    // DIRECTIVES
    free_slot_stack
};

static constexpr uint32_t OPCODE_SIZE = 28;


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
static constexpr uint32_t MACHINE_REG_SIZE = 14;

static constexpr uint32_t RETURN_REGISTER = 0;


static constexpr uint32_t SP = 14;
static constexpr uint32_t PC = 15;

static constexpr uint32_t OP_SIZE = sizeof(Opcode);

struct SymbolTable;
struct VarAlloc;

void disass_opcode_sym(const Opcode &opcode, const std::vector<VarAlloc> &table, const std::vector<std::string> &label_lookup);
void disass_opcode_raw(const Opcode &opcode);


// IR SYSCALLS
static constexpr u32 SWI_EXIT = 0x0;

struct IrEmitter
{
    void emit(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);

    std::list<Opcode> program;
    
    // how many registers used in this expression
    uint32_t reg_count;

    // how do we handle resolving labels?
    uint32_t pc;
};
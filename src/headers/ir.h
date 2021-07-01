#pragma once
#include <lib.h>
#include <type.h>

enum class op_type
{
    mov_reg,
    add_reg,
    sub_reg,
    mul_reg,
    div_reg,
    
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

    ret,
};

static constexpr uint32_t OPCODE_SIZE = 20;


// what kind of opcode is this?
enum op_group
{
    reg_t,
    imm_t,
    load_t,
    implicit_t,
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

static constexpr uint32_t SYMBOL_START = 0x80000000;

inline uint32_t reg(uint32_t r)
{
    return r;
}

inline uint32_t symbol(uint32_t s)
{
    return SYMBOL_START + s;
}

inline uint32_t symbol_to_idx(uint32_t s)
{
    return s - SYMBOL_START;
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

static constexpr uint32_t SP_IR = 0x7fffffff;

static constexpr uint32_t SP = 14;
static constexpr uint32_t PC = 15;

static constexpr uint32_t OP_SIZE = sizeof(Opcode);

void disass_opcode_sym(const Opcode &opcode, const SymbolTable &table);
void disass_opcode_raw(const Opcode &opcode);

struct IrEmitter
{
    void emit(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);

    std::list<Opcode> program;
    
    // how many registers used in this expression
    uint32_t reg_count;

    // how do we handle resolving labels?
    uint32_t pc;
};
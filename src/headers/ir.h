
enum class op_type
{
    mov_reg,
    mov_imm,
    add_reg,
    sub_reg,
    mul_reg,
    div_reg,
    ret,
};


struct OpInfo
{
    op_type type;
    const char *name;
    uint32_t size;
};

static constexpr uint32_t OP_SIZE = 6;

struct Opcode
{
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

// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define some targets
static constexpr uint32_t MACHINE_REG_SIZE = 16;

static constexpr uint32_t RETURN_REGISTER = 0;
static constexpr uint32_t SP = 0xffffffff;

struct IrEmitter
{
    void emit(op_type op, uint32_t v1 = 0, uint32_t v2 = 0, uint32_t v3 = 0);

    std::vector<Opcode> program;
    
    // how many registers used in this expression
    uint32_t reg_count;

    uint32_t sym_count;

    // how do we handle resolving labels?
    uint32_t pc;
};
#pragma once
enum class slot_type
{
    symbol,
    tmp,
    spec,
    label,
    block,
    pool,
    func,
};

static constexpr u32 INVALID_HANDLE = 0xffff'ffff;

template<slot_type type>
struct Slot
{
    u32 handle;
};

using SymSlot = Slot<slot_type::symbol>;
using TmpSlot = Slot<slot_type::tmp>;
using LabelSlot = Slot<slot_type::label>;
using BlockSlot = Slot<slot_type::block>;
using PoolSlot = Slot<slot_type::pool>;

using lowered_reg = u32;


template<slot_type T>
b32 is_valid_slot(Slot<T> slot)
{
    return slot.handle != INVALID_HANDLE;
}

inline u32 u32_hash_func(u32 size, u32 v)
{
    // TODO: impl a better integer hash func
    const u32 hash = v;
    const u32 slot = hash & (size - 1); 

    return slot;
}


template<slot_type type>
u32 hash_slot(u32 size, Slot<type> v)
{
    return u32_hash_func(size,v.handle);
}

SymSlot sym_from_idx(u32 idx)
{
    return {idx};
}

template<slot_type type>
b32 operator==(const Slot<type> v1,const Slot<type> v2)
{
    return v1.handle == v2.handle;
}


enum class reg_type
{
    float_t,
    gpr_t,
};

enum class reg_kind
{
    sym,
    tmp,
    spec,
};

enum class reg_segment
{
    local,
    constant,
    global,
};

static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 LOCATION_GLOBAL = 0xfffffffe;
static constexpr u32 SPECIAL_REG_START = 0x7000'0000;

// These constants are large so they cant be confused with any legitimate register
enum class spec_reg
{
    // generic
    sp = SPECIAL_REG_START,
    pc = SPECIAL_REG_START + 1,
    rv_gpr = SPECIAL_REG_START + 2,
    rv_fpr = SPECIAL_REG_START + 3,

    // x86
    rax = SPECIAL_REG_START + 4,
    rcx = SPECIAL_REG_START + 5,
    rdx = SPECIAL_REG_START + 6,
    rdi = SPECIAL_REG_START + 7,
    rsi = SPECIAL_REG_START + 8,

    r8 = SPECIAL_REG_START + 9,
    r9 = SPECIAL_REG_START + 10,
    r10 = SPECIAL_REG_START + 11,

    // Return that requires a memory copy
    rv_struct = SPECIAL_REG_START + 12,

    // dont perform any moves
    null = SPECIAL_REG_START + 13,

    const_seg = SPECIAL_REG_START + 14,
    global_seg = SPECIAL_REG_START + 15,

    // args
    a1 = SPECIAL_REG_START + 16,
    a2 = SPECIAL_REG_START + 17,
};

static constexpr u32 SPECIAL_REG_SIZE = (u32(spec_reg::a2) - SPECIAL_REG_START) + 1;
static constexpr u32 SPECIAL_REG_END = (SPECIAL_REG_START + SPECIAL_REG_SIZE) - 1;


static constexpr u32 SPECIAL_REG_ARG_START = u32(spec_reg::a1);


static const String SPECIAL_REG_NAMES[] = 
{
    "sp",
    "pc",
    "rv_gpr",
    "rv_fpr",

    "rax",
    "rcx",
    "rdx",
    "rdi",
    "rsi",

    "r8",
    "r9",
    "r10",

    "rv_struct",
    "null_slot",

    "const",
    "global",

    "a1",
    "a2",
};

static_assert((sizeof(SPECIAL_REG_NAMES) / sizeof(String)) == SPECIAL_REG_SIZE);

b32 is_raw_special_reg(u32 reg)
{
    return reg >= SPECIAL_REG_START && reg <= SPECIAL_REG_END;
}



struct RegSlot
{
    union 
    {
        TmpSlot tmp_slot;
        SymSlot sym_slot = {INVALID_HANDLE};
        spec_reg spec;
    };

    reg_kind kind = reg_kind::sym;
};


inline bool operator == (const RegSlot& v1, const RegSlot &v2)
{
    if(v1.kind != v2.kind)
    {
        return false;
    }

    switch(v1.kind)
    {
        case reg_kind::sym: return v1.sym_slot == v2.sym_slot;
        case reg_kind::tmp: return v1.tmp_slot == v2.tmp_slot;
        case reg_kind::spec: return v1.spec == v2.spec;
    }

    assert(false);
}

u32 hash_slot(u32 size, RegSlot slot)
{
    switch(slot.kind)
    {
        case reg_kind::tmp: return hash_slot(size,slot.tmp_slot);
        case reg_kind::sym: return hash_slot(size,slot.sym_slot);
        case reg_kind::spec: return u32_hash_func(size,u32(slot.spec));
    }

    assert(false);
}

RegSlot make_sym_reg_slot(SymSlot slot)
{
    RegSlot handle;
    handle.sym_slot = slot;
    handle.kind = reg_kind::sym;

    return handle;
}

const RegSlot INVALID_SYM_REG_SLOT = make_sym_reg_slot({INVALID_HANDLE});

RegSlot make_tmp_reg_slot(TmpSlot slot)
{
    RegSlot handle;
    handle.tmp_slot = slot;
    handle.kind = reg_kind::tmp;

    return handle;
}

RegSlot make_spec_reg_slot(spec_reg reg)
{
    RegSlot handle;
    handle.spec = reg;
    handle.kind = reg_kind::spec;

    return handle;
}

struct RegSpan
{
    Span<RegSlot> dst;
    Span<RegSlot> src;
};

struct ConstRegSpan
{
    ConstRegSpan(const RegSpan& span)
    {   
        dst = span.dst;
        src = span.src;
    }

    ConstRegSpan()
    {

    }

    ConstSpan<RegSlot> dst;
    ConstSpan<RegSlot> src;
};

RegSpan make_reg_span(RegSlot* dst, RegSlot* src, size_t size)
{
    RegSpan regs;
    regs.dst = make_span(dst,0,size);
    regs.src = make_span(src,0,size);

    return regs;
}

struct Addr
{
    RegSlot base;
    RegSlot index = make_spec_reg_slot(spec_reg::null);
    u32 scale = 1;
    u32 offset = 0;
};

struct LoweredAddr
{
    lowered_reg base = 0;
    lowered_reg index = u32(spec_reg::null);
    u32 scale = 1;
    u32 offset = 0;
};

// Subtyped for passing to emitters to ensure context is correct.
struct PointerAddr
{
    Addr addr;
};

struct StructAddr
{
    Addr addr;
};

struct AddrSlot
{
    Addr addr;

    // slot is not a pointer and refers to an actual variable
    b32 struct_addr = false;
};

enum class cmp_sign_op
{
    lt,
    le,
    gt,
    ge,    
};

enum class cmp_eq_op
{
    eq,
    ne,    
};


enum class comparison_op
{
    lt,
    le,
    gt,
    ge,

    eq,
    ne,
};

static constexpr u32 COMPARISON_OP_SIZE = 6;

static const char* COMPARISON_NAMES[COMPARISON_OP_SIZE] = 
{
    "<",
    "<=",
    ">",
    ">=",

    "==",
    "!="
};

enum class boolean_logic_op
{
    and_t,
    or_t,
};

static const char* BOOLEAN_LOGIC_NAMES[] =
{
    "&&",
    "||"
};

enum class arith_unary_op
{
    add_t,
    sub_t,
    bitwise_not_t,
    logical_not_t,
};

static const char* ARITH_UNARY_NAMES[] =
{
    "+",
    "-",
    "~",
    "!",
};


enum class shift_op
{
    left,
    right,
};

static const char* SHIFT_NAMES[] = 
{
    "<<",
    ">>"
};



static constexpr u32 ARITH_BIN_FLAG_FLOAT_ENABLED = (1 << 0);
static constexpr u32 ARITH_BIN_FLAG_BOOL_ENABLED = (1 << 1);

enum class arith_bin_op
{
    add_t,
    sub_t,
    mul_t,
    mod_t,
    div_t,
    xor_t,
    and_t,
    or_t,
};

struct ArithBinInfo
{
    const char* name = nullptr;
    u32 flags = 0;
};

static constexpr u32 ARITH_BIN_OP_SIZE = 8;

static const ArithBinInfo ARITH_BIN_INFO[ARITH_BIN_OP_SIZE] = 
{
    {"+", ARITH_BIN_FLAG_FLOAT_ENABLED}, 
    {"-", ARITH_BIN_FLAG_FLOAT_ENABLED},
    {"*", ARITH_BIN_FLAG_FLOAT_ENABLED},
    {"%", 0},
    {"/", ARITH_BIN_FLAG_FLOAT_ENABLED},
    {"^", 0},
    {"&", ARITH_BIN_FLAG_BOOL_ENABLED},
    {"|",  ARITH_BIN_FLAG_BOOL_ENABLED}
};


enum class op_group
{
    implicit,
    branch_label,
    directive,
    mov_gpr_imm,
    arith_imm_three,
    lea,
    addrof,
    load,
    load_struct,
    store,
    store_struct,
    mov_reg,
};

enum class ir_reg_type
{
    src,
    dst,
    dst_src,
    directive,    
};

struct IrRegister
{
    union
    {
        RegSlot ir = {};
        lowered_reg reg;
    };
};

enum class directive_operand_type
{
    // Bottom of this must match ir_reg_type
    src,
    dst,
    dst_src,
    directive_reg,
    decimal,
    imm,
    pool,
    label,
};

struct DirectiveOperand
{
    union 
    {
        f64 decimal;
        u64 imm;
        RegSlot reg = {INVALID_SYM_REG_SLOT};
        LabelSlot label;
        PoolSlot pool;
    };

    directive_operand_type type = directive_operand_type::dst_src;
};

enum class directive_type
{
    push_arg,
    push_float_arg,
    reload_slot,
    spill_slot,
    clean_args,
    unlock_reg_set,
    lock_reg,
    pool_addr,
    alloc_stack,
};

struct Directive
{
    directive_type type;
    DirectiveOperand operand[3];
    u32 size = 0;
};

enum class implicit_type
{
    syscall,
    ret,
};

struct Implicit
{
    implicit_type type;
};

enum class branch_label_type
{
    call
};

struct BranchLabel
{
    branch_label_type type;
    LabelSlot label;
};

struct MovGprImm
{
    IrRegister dst;
    u64 imm;      
};


template<typename op_type,const bool IS_LOAD, const bool IS_STRUCT>
struct AddrOpcode
{
    op_type type;

    union
    {
        Addr addr_ir = {};
        LoweredAddr addr;
    };

    IrRegister v1;    
};

enum class load_type
{
    lb,
    lh,
    lw,
    ld,

    lsb,
    lsh,
    lsw,

    lf,
};

enum class store_type
{
    sb,
    sh,
    sw,
    sd,
    
    sf,
};

enum class take_addr
{
    lea,
    addrof,
};


using Load = AddrOpcode<load_type,true,false>;
using LoadStruct = AddrOpcode<load_type,true,true>;

using Store = AddrOpcode<store_type,false,false>;
using StoreStruct = AddrOpcode<store_type,false,true>;

using Lea = AddrOpcode<take_addr,true,false>;
using AddrOf = AddrOpcode<take_addr,true,true>;

template<typename op_type>
struct ImmThree
{
    op_type type;

    IrRegister dst;
    IrRegister src;

    u64 imm = 0;
};

using ArithImmThree = ImmThree<arith_bin_op>;

enum class mov_reg_type
{
    gpr,
    fpr
};

struct MovReg
{
    mov_reg_type type;

    IrRegister dst;
    IrRegister src;
};

struct Opcode
{
    op_group group = op_group::directive;

    union
    {
        Directive directive = {};
        BranchLabel branch_label;
        Implicit implicit;
        MovGprImm mov_gpr_imm;
        ArithImmThree arith_imm_three;
        MovReg mov_reg;
        Lea lea;
        AddrOf addrof;
        Load load;
        LoadStruct load_struct;
        Store store;
        StoreStruct store_struct;
    };

    bool lowered = false;
};

static constexpr u32 MAX_OPCODE_REGS = 8;
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

using lowered_reg_t = u32;


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
    fpr,
    gpr,
};

enum class reg_kind
{
    sym,
    tmp,
    spec,
};

enum class reg_arg_kind
{
    src,
    dst,
    dst_src
};

bool is_arg_dst(reg_arg_kind kind) 
{
    return kind == reg_arg_kind::dst || kind == reg_arg_kind::dst_src;
}


bool is_arg_src(reg_arg_kind kind) 
{
    return kind == reg_arg_kind::src || kind == reg_arg_kind::dst_src;
}


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

b32 is_raw_special_reg(lowered_reg_t reg)
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

static constexpr u32 MAX_OPCODE_REGS = 8;

struct IrRegSpanStorage
{
    RegSlot src[MAX_OPCODE_REGS] = {};
    RegSlot dst[MAX_OPCODE_REGS] = {};
    RegSlot dst_src[MAX_OPCODE_REGS] = {};
};

struct IrRegSpan
{
    Span<RegSlot> dst;
    Span<RegSlot> src;
    Span<RegSlot> dst_src;
};



struct ConstIrRegSpan
{
    ConstIrRegSpan(const IrRegSpan& span)
    {   
        dst = span.dst;
        src = span.src;
        dst_src = span.dst_src;
    }

    ConstIrRegSpan()
    {

    }

    ConstSpan<RegSlot> dst;
    ConstSpan<RegSlot> src;
    ConstSpan<RegSlot> dst_src;
};

IrRegSpan make_ir_reg_span(IrRegSpanStorage& storage)
{
    IrRegSpan regs;
    regs.dst = make_span(storage.dst,0,MAX_OPCODE_REGS);
    regs.src = make_span(storage.src,0,MAX_OPCODE_REGS);
    regs.dst_src = make_span(storage.dst_src,0,MAX_OPCODE_REGS);

    return regs;
}

struct LoweredRegSpanStorage
{
    lowered_reg_t src[MAX_OPCODE_REGS] = {};
    lowered_reg_t dst[MAX_OPCODE_REGS] = {};
    lowered_reg_t dst_src[MAX_OPCODE_REGS] = {};
};


struct LoweredRegSpan
{
    Span<lowered_reg_t> dst;
    Span<lowered_reg_t> src;
    Span<lowered_reg_t> dst_src; 
};

LoweredRegSpan make_lowered_reg_span(LoweredRegSpanStorage& storage)
{
    LoweredRegSpan regs;
    regs.dst = make_span(storage.dst,0,MAX_OPCODE_REGS);
    regs.src = make_span(storage.src,0,MAX_OPCODE_REGS);
    regs.dst_src = make_span(storage.dst_src,0,MAX_OPCODE_REGS);

    return regs;
}

struct ConstLoweredRegSpan
{
    ConstLoweredRegSpan(const LoweredRegSpan& span)
    {   
        dst = span.dst;
        src = span.src;
        dst_src = span.dst_src;
    }

    ConstLoweredRegSpan()
    {

    }

    ConstSpan<lowered_reg_t> dst;
    ConstSpan<lowered_reg_t> src;
    ConstSpan<lowered_reg_t> dst_src;
};



struct Addr
{
    RegSlot base;
    RegSlot index = make_spec_reg_slot(spec_reg::null);
    u32 scale = 1;
    u32 offset = 0;
};

struct LoweredAddr
{
    lowered_reg_t base = 0;
    lowered_reg_t index = u32(spec_reg::null);
    u32 scale = 1;
    u32 offset = 0;

    // Copy for struct addrs
    RegSlot base_ir = {INVALID_SYM_REG_SLOT};
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
    ult,
    ule,
    ugt,
    uge,   
    
    slt,
    sle,
    sgt,
    sge,  

    eq,
    ne, 
};

static constexpr u32 CMP_SIGN_OP_SIZE = 10;

static const char* CMP_SIGN_NAMES[CMP_SIGN_OP_SIZE] = 
{
    "cmpult",
    "cmpule",
    "cmpugt",
    "cmpuge",

    "cmpslt",
    "cmpsle",
    "cmpsgt",
    "cmpsge",

    "cmpeq",
    "cmpne"
};

static const char* SET_FROM_GPR_NAMES[CMP_SIGN_OP_SIZE] = 
{
    "setult",
    "setule",
    "setugt",
    "setuge",
    "setslt",
    "setsle",
    "setsgt",
    "setsge",
    "seteq",
    "setne"
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

static constexpr u32 CMP_OP_SIZE = 6;

static const char* CMP_NAMES[CMP_OP_SIZE] = 
{
    "<",
    "<=",
    ">",
    ">=",

    "==",
    "!="
};

static const char* CMP_FPR_NAMES[CMP_OP_SIZE] = 
{
    "cmpflt",
    "cmpfle",
    "cmpfgt",
    "cmpfge",

    "cmpfeq",
    "cmpfne"
};

static const char* SET_FROM_FPR_NAMES[CMP_OP_SIZE] = 
{
    "setflt",
    "setfle",
    "setfgt",
    "setfge",

    "setfeq",
    "setfne"
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


enum class shift_type
{
    left,
    right,
};

enum class shift_op
{
    lsr,
    asr,
    lsl
};

inline shift_op shift_type_to_op(shift_type type, bool sign)
{
    switch(type)
    {
        case shift_type::right: return sign? shift_op::asr : shift_op::lsr;
        case shift_type::left: return shift_op::lsl; 
    }

    assert(false);
}

static const char* SHIFT_NAMES[] = 
{
    "<<",
    ">>"
};

static const char* SHIFT_OP_NAMES[] = 
{
    "lsr",
    "asr",
    "lsl"
};


static constexpr u32 ARITH_BIN_FLAG_FLOAT_ENABLED = (1 << 0);
static constexpr u32 ARITH_BIN_FLAG_BOOL_ENABLED = (1 << 1);
static constexpr u32 ARITH_BIN_IMM_SUPPORT = (1 << 2);
static constexpr u32 ARITH_BIN_COMMUTATIVE = (1 << 3);

enum class arith_bin_type
{
    add_t,
    sub_t,
    mul_t,
    div_t,
    mod_t,
    xor_t,
    and_t,
    or_t,
};

enum class arith_bin_op
{
    add_t,
    sub_t,
    mul_t,
    umod_t,
    smod_t,
    sdiv_t,
    udiv_t,
    xor_t,
    and_t,
    or_t,
};

static const char* ARITH_NAMES[] =
{
    "add",
    "sub",
    "mul",
    "umod",
    "smod",
    "sdiv",
    "udiv",
    "xor",
    "and",
    "or"
};


static constexpr u64 ARITH_GPR_COMMUTATIVE = 
    true << 0 |
    true << 1 |
    true << 2 |
    false << 3 |
    false << 4 |
    false << 5 |
    false << 6 |
    true << 7 |
    true << 8 |
    true << 9;


enum class fpr_arith
{
    add_t,
    sub_t,
    mul_t,
    div_t
};

static constexpr u64 ARITH_FPR_COMMUTATIVE = 
    true << 0 |
    false << 1 |
    true << 2 |
    false << 3;


const char* FPR_ARITH_NAMES[] =
{
    "addf",
    "subf",
    "mulf",
    "divf"
};


inline fpr_arith arith_bin_to_fpr(arith_bin_type type)
{
    assert(type <= arith_bin_type::div_t);
    return fpr_arith(u32(type));
}

struct ArithBinInfo
{
    u32 flags = 0;
    arith_bin_op signed_form;
    arith_bin_op unsigned_form;
};

static constexpr u32 ARITH_BIN_TYPE_SIZE = 8;

static const ArithBinInfo ARITH_BIN_INFO[ARITH_BIN_TYPE_SIZE] = 
{
    {ARITH_BIN_FLAG_FLOAT_ENABLED | ARITH_BIN_IMM_SUPPORT | ARITH_BIN_COMMUTATIVE,arith_bin_op::add_t,arith_bin_op::add_t}, 
    {ARITH_BIN_FLAG_FLOAT_ENABLED | ARITH_BIN_IMM_SUPPORT,arith_bin_op::sub_t,arith_bin_op::sub_t},
    {ARITH_BIN_FLAG_FLOAT_ENABLED | ARITH_BIN_IMM_SUPPORT | ARITH_BIN_COMMUTATIVE,arith_bin_op::mul_t,arith_bin_op::mul_t},
    {ARITH_BIN_FLAG_FLOAT_ENABLED,arith_bin_op::sdiv_t,arith_bin_op::udiv_t},
    {0,arith_bin_op::smod_t,arith_bin_op::umod_t},
    {ARITH_BIN_IMM_SUPPORT | ARITH_BIN_COMMUTATIVE,arith_bin_op::xor_t,arith_bin_op::xor_t},
    {ARITH_BIN_FLAG_BOOL_ENABLED | ARITH_BIN_IMM_SUPPORT | ARITH_BIN_COMMUTATIVE,arith_bin_op::and_t,arith_bin_op::and_t},
    {ARITH_BIN_FLAG_BOOL_ENABLED | ARITH_BIN_COMMUTATIVE,arith_bin_op::or_t,arith_bin_op::or_t}
};

static const char* ARITH_BIN_NAMES[ARITH_BIN_TYPE_SIZE] =
{
    "+",
    "-",
    "*",
    "/",
    "%",
    "^",
    "&",
    "|",
};

inline arith_bin_op arith_type_to_op(arith_bin_type type, bool sign)
{
    const auto& arith_info = ARITH_BIN_INFO[u32(type)];
    return sign? arith_info.signed_form : arith_info.unsigned_form;
}

enum class op_group
{
    implicit,
    branch_label,
    branch_reg,
    branch_cond,
    branch_cond_flag,
    directive,
    mov_gpr_imm,
    mov_fpr_imm,
    arith_imm3,
    arith_imm2,
    arith_gpr3,
    arith_gpr2,
    arith_fpr3,
    arith_fpr2,
    shift_reg3,
    shift_reg2,
    lea,
    addrof,
    load,
    load_struct,
    store,
    store_struct,
    shift_imm3,
    shift_imm2,
    unary_reg2,
    unary_reg1,
    sign_extend,
    cmp_imm3,
    cmp_gpr3,
    cmp_fpr3,
    reg2_src,
    imm2_src,
    set_from_flag_gpr,
    set_from_flag_fpr,
    x86_fixed,
};

// TODO: May want to filter out calls.
inline bool is_group_branch(op_group group) 
{
    return group >= op_group::branch_label && group <= op_group::branch_cond_flag;
}

enum class ir_reg_type
{
    src,
    dst,
    dst_src,
    directive,    
};

struct DirectiveReg
{
    RegSlot slot;
    ir_reg_type type;
};

struct IrRegister
{
    union
    {
        RegSlot ir = {};
        lowered_reg_t reg;
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
    reg_set,
    pool,
    label,
    lowered_reg,
};

struct DirectiveOperand
{
    union 
    {
        f64 decimal;
        u64 imm;
        u64 reg_set;
        RegSlot ir_reg = {INVALID_SYM_REG_SLOT};
        LabelSlot label;
        PoolSlot pool;
        lowered_reg_t reg;
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
    lock_reg_set,
    unlock_reg_set,
    lock_reg,
    mov_unlock,
    pool_addr,
    alloc_stack,
    alloc_slot,
    load_func_addr,
    alloc_local_array,
    alloc_global_array,
    load_const_float,
    live_var,
    spill,
    load
};

static const char* DIRECTIVE_NAMES[] = 
{
    "push_arg",
    "push_float_arg",
    "reload_slot",
    "spill_slot",
    "clean_args",
    "lock_reg_set",
    "unlock_reg_set",
    "lock",
    "mov_unlock",
    "pool_addr",
    "alloc_stack",
    "alloc_slot",
    "load_func_addr",
    "alloc_local_array",
    "alloc_global_array",
    "load_const_float",
    "live_var",
    "spill",
    "load"
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
    spill_func_bounds
};

static const char* IMPLICIT_NAMES[] = 
{
    "syscall",
    "ret",
    "spill_func_bounds"
};

struct Implicit
{
    implicit_type type;
};



enum class branch_type
{
    call,
    branch,
};

static const char* BRANCH_NAMES[] = 
{
    "call",
    "b",
};

struct BranchLabel
{
    branch_type type;
    LabelSlot label;
};

struct BranchReg
{
    branch_type type;
    IrRegister src;
};

struct BranchCmp
{
    branch_type type;
    cmp_sign_op cmp_type;
    IrRegister v1;
    IrRegister V2;
    LabelSlot label;
};

enum class branch_cond_type
{
    eqz,
    nez,
};

// TODO: change these once we have confirmed a match
// beqz
// bnez
static const char* BRANCH_COND_NAMES[] =
{
    "bnc",
    "bc"
};

// TODO: change these once we have confirmed a match
static const char* JUMP_COND_NAMES[] =
{
    "je",
    "jne"
};

struct BranchCond
{
    branch_cond_type type;
    IrRegister src;
    LabelSlot label;
};

struct BranchCondFlag
{
    branch_cond_type type;
    LabelSlot label;
};


struct MovGprImm
{
    IrRegister dst;
    u64 imm;      
};

struct MovFprImm
{
    IrRegister dst;
    f64 imm;
};


template<typename op_type,const bool IS_LOAD, const bool IS_STRUCT, op_group group>
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

const char* LOAD_NAMES[] =
{
    "lb",
    "lh",
    "lw",
    "ld",

    "lsb",
    "lsh",
    "lsw",

    "lf",
};

const char* LOAD_STRUCT_NAMES[] =
{
    "load_struct_u8",
    "load_struct_u16",
    "load_struct_u32",
    "load_struct_u64",

    "load_struct_s8",
    "load_struct_s16",
    "load_struct_s32",

    "load_struct_f64",    
};

enum class store_type
{
    sb,
    sh,
    sw,
    sd,
    
    sf,
};

const char* STORE_NAMES[] = 
{
    "sb",
    "sh",
    "sw",
    "sd",

    "sf",
};


const char* STORE_STRUCT_NAMES[] =
{
    "store_struct_u8",
    "store_struct_u16",
    "store_struct_u32",
    "store_struct_u64",

    "store_struct_f64",    
};

enum class take_addr
{
    lea,
    addrof,
};


static const char* TAKE_ADDR_NAMES[] =
{
    "lea",
    "addrof"
};

using Load = AddrOpcode<load_type,true,false,op_group::load>;
using LoadStruct = AddrOpcode<load_type,true,true,op_group::load_struct>;

using Store = AddrOpcode<store_type,false,false,op_group::store>;
using StoreStruct = AddrOpcode<store_type,false,true,op_group::store_struct>;

using Lea = AddrOpcode<take_addr,true,false,op_group::lea>;
using AddrOf = AddrOpcode<take_addr,true,true,op_group::addrof>;

template<typename op_type, op_group group>
struct ImmThree
{
    op_type type;

    IrRegister dst;
    IrRegister src;

    u64 imm = 0;
};


template<typename op_type, op_group group>
struct ImmTwoDst
{
    op_type type;
    IrRegister dst;

    u64 imm = 0;
};

enum class imm_two_src
{
    cmp_flags_imm
};

static const char* IMM_TWO_SRC_NAMES[] =
{
    "cmp_flags_imm",
};

struct ImmTwoSrc
{
    imm_two_src type;
    IrRegister src;

    u64 imm = 0;
};

template<typename op_type, op_group group>
struct RegThree
{
    op_type type;

    IrRegister dst;
    IrRegister v1;
    IrRegister v2;
};

template<typename op_type, op_group group>
struct RegTwoDst
{
    op_type type;

    IrRegister dst;
    IrRegister src;
};


enum class x86_fixed_type
{
	lsl,
	asr,
	lsr,
	udiv,
	sdiv,
	umod,
	smod,
};

const char* X86_FIXED_NAMES[] = 
{
	"lsl_x86",
	"asr_x86",
	"lsr_x86",
	"udiv_x86",
	"sdiv_x86",
	"umod_x86",
	"smod_x86",
};

enum class reg_two_src
{
    cmp_flags_gpr,
    cmp_flags_fpr,
    test
};

static const char* REG_TWO_SRC_NAMES[] = 
{
    "cmp_flags",
    "cmp_flags_float",
    "test",
};


struct RegTwoSrc
{
    reg_two_src type;

    IrRegister v1;
    IrRegister v2;
};


using ShiftImm3 = ImmThree<shift_op,op_group::shift_imm3>;
using ShiftImm2 = ImmTwoDst<shift_op,op_group::shift_imm2>;
using ArithImm3 = ImmThree<arith_bin_op,op_group::arith_imm3>;
using ArithImm2 = ImmTwoDst<arith_bin_op,op_group::arith_imm2>;

using ArithGpr3 = RegThree<arith_bin_op,op_group::arith_gpr3>;
using ArithGpr2 = RegTwoDst<arith_bin_op,op_group::arith_gpr2>;
using ArithFpr3 = RegThree<fpr_arith,op_group::arith_fpr3>;
using ArithFpr2 = RegTwoDst<fpr_arith,op_group::arith_fpr2>;
using ShiftReg3 = RegThree<shift_op,op_group::shift_reg3>;
using ShiftReg2 = RegTwoDst<shift_op,op_group::shift_reg2>;

using X86Fixed = RegTwoDst<x86_fixed_type,op_group::x86_fixed>;


using CmpGpr3 = RegThree<cmp_sign_op,op_group::cmp_gpr3>;
using CmpFpr3 = RegThree<comparison_op,op_group::cmp_fpr3>;
using CmpImm3 = ImmThree<cmp_sign_op,op_group::cmp_imm3>;

enum class unary_reg2_op
{
    mov_gpr_reg,
    mov_fpr_reg,
    bitwise_not,
    cvt_if,
    cvt_fi
};

static const char* UNARY_REG_TWO_NAMES[] =
{
    "mov",
    "movf",
    "not",
    "cvtif",
    "cvtfi"
};

enum class unary_reg1_op
{
    bitwise_not,
};

static const char* UNARY_REG_ONE_NAMES[] = 
{
    "not",
};

enum class sign_extend_op
{
    sxb,
    sxh,
    sxw
};

const char* SIGN_EXTEND_NAMES[] =
{
    "sxb",
    "sxh",
    "sxw"
};

template<typename op_type,op_group group>
struct UnaryReg2
{
    op_type type;
    IrRegister dst;
    IrRegister src;
};


template<typename op_type, op_group group>
struct UnaryReg1
{
    op_type type;
    IrRegister dst;
};

using UnaryRegTwo = UnaryReg2<unary_reg2_op,op_group::unary_reg2>;
using UnaryRegOne = UnaryReg1<unary_reg1_op,op_group::unary_reg1>;
using SignExtend = UnaryReg2<sign_extend_op,op_group::sign_extend>;
using SetFromFlagGpr = UnaryReg1<cmp_sign_op,op_group::set_from_flag_gpr>;
using SetFromFlagFpr = UnaryReg1<comparison_op,op_group::set_from_flag_fpr>;


struct Opcode
{
    op_group group = op_group::directive;

    Opcode(const Directive& value) : group(op_group::directive), directive(value) {}
    Opcode(const BranchLabel& value) : group(op_group::branch_label), branch_label(value) {}
    Opcode(const BranchCond& value) : group(op_group::branch_cond), branch_cond(value) {}
    Opcode(const BranchCondFlag& value) : group(op_group::branch_cond_flag), branch_cond_flag(value) {}
    Opcode(const Implicit& value) : group(op_group::implicit), implicit(value) {}
    Opcode(const MovGprImm& value) : group(op_group::mov_gpr_imm), mov_gpr_imm(value) {} 
    Opcode(const MovFprImm& value) : group(op_group::mov_fpr_imm), mov_fpr_imm(value) {} 
    Opcode(const ArithImm3& value) : group(op_group::arith_imm3), arith_imm3(value) {}
    Opcode(const ArithImm2& value) : group(op_group::arith_imm2), arith_imm2(value) {}
    Opcode(const ArithGpr3& value) : group(op_group::arith_gpr3), arith_gpr3(value) {}
    Opcode(const ArithGpr2& value) : group(op_group::arith_gpr2), arith_gpr2(value) {}
    Opcode(const ArithFpr3& value) : group(op_group::arith_fpr3), arith_fpr3(value) {}
    Opcode(const ArithFpr2& value) : group(op_group::arith_fpr2), arith_fpr2(value) {}
    Opcode(const Lea& value) : group(op_group::lea), lea(value) {}
    Opcode(const AddrOf& value) : group(op_group::addrof), addrof(value) {}
    Opcode(const Load& value) : group(op_group::load), load(value) {}
    Opcode(const LoadStruct& value) : group(op_group::load_struct), load_struct(value) {}
    Opcode(const Store& value) : group(op_group::store), store(value) {}
    Opcode(const StoreStruct& value) : group(op_group::store_struct), store_struct(value) {}
    Opcode(const BranchReg& value) : group(op_group::branch_reg), branch_reg(value) {}
    Opcode(const ShiftImm3& value) : group(op_group::shift_imm3), shift_imm3(value) {}
    Opcode(const ShiftImm2& value) : group(op_group::shift_imm2), shift_imm2(value) {}
    Opcode(const ShiftReg3& value) : group(op_group::shift_reg3), shift_reg3(value) {}
    Opcode(const ShiftReg2& value) : group(op_group::shift_reg2), shift_reg2(value) {}
    Opcode(const UnaryRegTwo& value) : group(op_group::unary_reg2), unary_reg2(value) {}
    Opcode(const UnaryRegOne& value) : group(op_group::unary_reg1), unary_reg1(value) {}
    Opcode(const SignExtend& value) : group(op_group::sign_extend), sign_extend(value) {}
    Opcode(const CmpImm3& value) : group(op_group::cmp_imm3), cmp_imm3(value) {}
    Opcode(const CmpGpr3& value) : group(op_group::cmp_gpr3), cmp_gpr3(value) {}
    Opcode(const CmpFpr3& value) : group(op_group::cmp_fpr3), cmp_fpr3(value) {}
    Opcode(const RegTwoSrc& value) : group(op_group::reg2_src), reg2_src(value) {}
    Opcode(const ImmTwoSrc& value) : group(op_group::imm2_src), imm2_src(value) {}
    Opcode(const SetFromFlagFpr& value) : group(op_group::set_from_flag_fpr), set_from_flag_fpr(value) {}
    Opcode(const SetFromFlagGpr& value) : group(op_group::set_from_flag_gpr), set_from_flag_gpr(value) {}
    Opcode(const X86Fixed& value) : group(op_group::x86_fixed), x86_fixed(value) {}

    Opcode() {}

    union
    {
        Directive directive = {};
        BranchLabel branch_label;
        BranchCond branch_cond;
        BranchCondFlag branch_cond_flag;
        Implicit implicit;
        MovGprImm mov_gpr_imm;
        MovFprImm mov_fpr_imm;
        ArithImm3 arith_imm3;
        ArithImm2 arith_imm2;
        ArithGpr3 arith_gpr3;
        ArithGpr2 arith_gpr2;
        ArithFpr3 arith_fpr3;
        ArithFpr2 arith_fpr2;
        Lea lea;
        AddrOf addrof;
        Load load;
        LoadStruct load_struct;
        Store store;
        StoreStruct store_struct;
        BranchReg branch_reg;
        ShiftImm3 shift_imm3;
        ShiftImm2 shift_imm2;
        ShiftReg3 shift_reg3;
        ShiftReg2 shift_reg2;
        UnaryRegTwo unary_reg2;
        UnaryRegOne unary_reg1;
        SignExtend sign_extend;
        CmpImm3 cmp_imm3;
        CmpGpr3 cmp_gpr3;
        CmpFpr3 cmp_fpr3;
        RegTwoSrc reg2_src;
        ImmTwoSrc imm2_src;
        SetFromFlagFpr set_from_flag_fpr;
        SetFromFlagGpr set_from_flag_gpr;
        X86Fixed x86_fixed;
    };

    bool lowered = false;
};
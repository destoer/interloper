#pragma once
#include <destoer/destoer.h>
#include "list.inl"
#include "error.h"

enum class op_type
{
    mov_reg,
    mov_unlock,
    add_reg,
    sub_reg,
    mul_reg,
    udiv_reg,
    sdiv_reg,
    umod_reg,
    smod_reg,


    lsl_reg,
    asr_reg,
    lsr_reg,

    xor_reg,
    or_reg,
    and_reg,
    not_reg,
    

    sxb,
    sxh,
    sxw,

    mov_imm,
    add_imm,
    sub_imm,
    mul_imm,

    lsl_imm,
    lsr_imm,
    and_imm,
    xor_imm,

    add_reg2,
    sub_reg2,
    mul_reg2,
    udiv_reg2,
    sdiv_reg2,
    umod_reg2,
    smod_reg2,


    lsl_reg2,
    asr_reg2,
    lsr_reg2,

    xor_reg2,
    or_reg2,
    and_reg2,
    
    add_imm2,
    sub_imm2,
    mul_imm2,

    lsl_imm2,
    lsr_imm2,
    and_imm2,
    xor_imm2,

    cqo,
    udiv_x86,
    sdiv_x86,
    umod_x86,
    smod_x86,

    lsl_x86,
    lsr_x86,
    asr_x86,

    not_reg1,

    movf_imm,
    movf_reg,
    lf,
    sf,
    addf_reg,
    subf_reg,
    mulf_reg,
    divf_reg,

    cmpflt_reg,
    cmpfle_reg,
    cmpfgt_reg,
    cmpfge_reg,

    cmpfeq_reg,
    cmpfne_reg,

    cmp_flags_float,

    setflt,
    setfle,
    setfgt,
    setfge,

    setfeq,
    setfne,

    addf_reg2,
    subf_reg2,
    mulf_reg2,
    divf_reg2,
    cvt_fi,
    cvt_if,

    lb,
    lh,
    lw,
    ld,

    lsb,
    lsh,
    lsw,

    sb,
    sh,
    sw,
    sd,

    lea,

    push,
    pop,

    pushm,
    popm,

    call,
    call_reg,
    leave,
    ret,

    syscall,
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

    cmpeq_imm,
    cmpne_imm,

    // compare flags
    cmp_flags,
    cmp_flags_imm,

    // set signed
    setslt,
    setsle,
    setsgt,
    setsge,

    // set unsigned
    setult,
    setule,
    setugt,
    setuge,


    seteq,
    setne,

    bnc,
    bc,
    b,
    b_reg,

    // x86 branch
    test,
    je,
    jne,

    // DIRECTIVES
    // variable on the stack is out of scope
    // so we can reclaim allocation on the stack
    DIRECTIVE,
    alloc_slot,

    alloc_local_array,
    alloc_global_array,

    // give a function call arg
    // how it will be passed will be decided in reg alloc
    push_arg,
    push_float_arg,

    // perform cleanup after a function call
    // free the stack space for args
    alloc_stack,
    clean_args,

    // branch to end of if statement chain
    exit_block,

    lock_reg,
    lock_reg_set,
    unlock_reg,
    unlock_reg_set,

    spill,
    spill_all,
    spill_func_bounds,
    
    reload_slot,
    spill_slot,

    load,

    addrof,
    load_func_addr,

    load_struct_s8,
    load_struct_s16,
    load_struct_s32,

    load_struct_u8,
    load_struct_u16,
    load_struct_u32,
    load_struct_u64,
    load_struct_f64,

    store_struct_u8,
    store_struct_u16,
    store_struct_u32,
    store_struct_u64,
    store_struct_f64,

    pool_addr,

    load_const_float,

    live_var,

    none,

    // just c++ things not used
    END,
};

b32 is_directive(op_type type)
{
    return type >= op_type::DIRECTIVE;
}

// general operation defined for unsigned or unsigned ints
// will be converted to a specific counterpart in op_type
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

static constexpr u32 ARITH_BIN_OP_SIZE = 8;

static const char* ARITH_BIN_NAMES[] = 
{
    "+",
    "-",
    "*",
    "%",
    "/",
    "^",
    "&",
    "|"
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

using LabelSlot = Slot<slot_type::label>;

using BlockSlot = Slot<slot_type::block>;

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

struct TypedReg
{
    RegSlot slot;
    Type* type = nullptr;
};


using RegResult = destoer::Result<TypedReg,itl_error>;

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

const RegSlot INVALID_SYM_REG_SLOT = make_sym_reg_slot({INVALID_HANDLE});


struct Addr
{
    RegSlot base;
    RegSlot index = make_spec_reg_slot(spec_reg::null);
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

inline bool is_direct_addr(const AddrSlot& addr_slot)
{
    return addr_slot.struct_addr && addr_slot.addr.index == make_spec_reg_slot(spec_reg::null) && addr_slot.addr.offset == 0;
}


AddrSlot make_struct_addr(RegSlot slot, u32 offset);
AddrSlot make_pointer_addr(RegSlot slot, u32 offset);
Addr make_addr(RegSlot slot, u32 offset);

struct Opcode
{
    u32 foo;
};

static constexpr u32 SIGNED_FLAG = 1 << 0;
static constexpr u32 STORED_IN_MEM = 1 << 1;
static constexpr u32 ALIASED = 1 << 2;
static constexpr u32 PENDING_STACK_ALLOCATION = 1 << 3;
static constexpr u32 CONST = 1 << 4;
static constexpr u32 STACK_ARG = 1 << 5;
static constexpr u32 REG_FLOAT = 1 << 6;
static constexpr u32 STACK_ALLOCATED = 1 << 7;
static constexpr u32 GLOBALLY_ALLOCATED = 1 << 8;


// TODO:
// for now hardcode this to the limits of our vm
// we will move this into a struct as part of a config
// when we actually want to define multiple targets
static constexpr u32 MACHINE_REG_SIZE = 32;

// TAC wont function on less than 3 regs
static_assert(MACHINE_REG_SIZE >= 3);

static constexpr u32 REG_FREE = 0xffff'ffff;

struct Reg
{
    // Where is this register allocated
    reg_segment segment = reg_segment::local;

    // what slot does this symbol hold inside the ir?
    RegSlot slot;

    // how much memory does this thing use GPR_SIZE max (spilled into count if larger)
    // i.e this is for stack allocation to get actual var sizes use type_size();
    u32 size = 0;
    u32 count = 0;

    // intialized during register allocation

    // where is the current offset for its section?
    u32 offset = 0;

    // Where is this register globally allocated if at all
    u32 global_reg = REG_FREE;
    // Where does this register reside in the current block?
    u32 local_reg = REG_FREE;

    // Registers that it has fixed interactions with
    // Attempt to allocate register into here to avoid shuffles.
    u32 hint = 0;

    u32 cur_local_uses = 0;
    Array<u32> local_uses;

    u32 flags = 0;
};

struct Interloper;
struct Function;
struct Type;

Reg make_reg(const RegSlot& slot, u32 size, b32 is_signed, b32 is_float);
Reg make_reg(Interloper& itl, const RegSlot& slot, const Type* type);
void destroy_reg(Reg& ir_reg);
void print(const Reg& reg);


static constexpr u32 GPR_SIZE = sizeof(u64);



struct SymbolTable;


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


static constexpr u32 SPECIAL_PURPOSE_BLOCK_START_HANDLE = 0xffff'fff0;
static constexpr u32 INVALID_BLOCK_HANDLE = SPECIAL_PURPOSE_BLOCK_START_HANDLE + 0;
static constexpr u32 BLOCK_FUNC_EXIT_HANDLE = SPECIAL_PURPOSE_BLOCK_START_HANDLE + 1;

static constexpr BlockSlot INVALID_BLOCK = {INVALID_BLOCK_HANDLE};
static constexpr BlockSlot BLOCK_FUNC_EXIT = {BLOCK_FUNC_EXIT_HANDLE}; 


static constexpr u32 HAS_FUNC_EXIT = 1 << 0;
static constexpr u32 REACH_FUNC_EXIT = 1 << 1;
static constexpr u32 IN_LOOP = 1 << 2;

using OpcodeNode = ListNode<Opcode>;
using OpcodeList = List<Opcode>;

struct Block
{
    OpcodeList list;
    u32 branch_count = 0;

    u32 flags = 0;

    // what is the corresponding label for this block?
    LabelSlot label_slot;

    // What is our own slot?
    BlockSlot block_slot;

    // blocks that can enter
    Array<BlockSlot> entry;

    // block we exit to
    Array<BlockSlot> exit;

    Set<RegSlot> live_in;
    Set<RegSlot> live_out;
    Set<RegSlot> def;
    Set<RegSlot> use;

    // what blocks are reachable from this block?
    Array<BlockSlot> links;
};

void add_func_exit(Function& func, BlockSlot slot);

struct IrEmitter
{
    Array<Block> program;

};


struct ArrayAllocation
{
    SymSlot slot;
    u32 stack_offset = 0;
    u32 offset = 0;
    u32 size = 0;
    u32 count = 0;
};

struct GlobalAlloc
{
    u32 addr = 0;
    u32 count[4] = {0};
    u32 start[4] = {0};
    u32 size = 0;

    u64 base_vaddr = 0;

    b32 print_global = false;

    Array<ArrayAllocation> array_allocation;
};

void finalise_global_offset(Interloper& itl);

void sign_extend_byte(Interloper& itl, Function& func, RegSlot dst, RegSlot src);
void sign_extend_half(Interloper& itl, Function& func, RegSlot dst, RegSlot src);
void sign_extend_word(Interloper& itl, Function& func, RegSlot dst, RegSlot src);

void cvt_fi(Interloper& itl, Function& func, RegSlot dst, RegSlot v1);
void cvt_if(Interloper& itl, Function& func, RegSlot dst, RegSlot v1);


void mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src);

void and_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm);

void cmp_signed_gt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm);
void cmp_unsigned_gt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm);

void mov_imm(Interloper& itl, Function& func, RegSlot dst, u64 imm);

void spill_func_bounds(Interloper& itl, Function& func);

void spill_slot(Interloper& itl, Function& func, const Reg& reg);

BlockSlot block_from_idx(u32 v);
BlockSlot cur_block(Function& func);
Block& block_from_slot(Function& func, BlockSlot slot);


void destroy_emitter(IrEmitter& emitter);

struct TypedAddr
{
    AddrSlot addr_slot;
    Type* type = nullptr;
};

using AddrResult = Result<AddrSlot,itl_error>;
using TypedAddrResult = Result<TypedAddr,itl_error>;

// intrin
void ir_memcpy(Interloper&itl, Function& func, AddrSlot dst_addr, AddrSlot src_addr, u32 size);

enum class arch_target
{
    x86_64_t,
};

static constexpr u32 ARCH_SIZE = 1;

enum class os_target
{
    linux_t,
};


void disass_opcode_sym(const Opcode &opcode, const SymbolTable& table,arch_target arch);
void disass_opcode_raw(const Opcode &opcode,arch_target arch);
void dump_ir_sym(Interloper& itl,Function &func,SymbolTable& table);

struct AsmFunc
{
    Function* ir_func = nullptr;
    u32 offset;
    u32 size;
};

struct LinkOpcode
{
    Opcode opcode;
    u32 offset;
};

struct AsmEmitter
{
    Array<u8> buffer;

    Array<AsmFunc> func;

    Array<LinkOpcode> link;
    u64 base_vaddr = 0;
    u64 base_offset = 0;
};

namespace x86
{

void emit_asm(Interloper& itl);

}

enum x86_reg : u64
{
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,

    // need upper encoding to access
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15,


    // sse regs
    xmm0,
    xmm1,
    xmm2,
    xmm3,
    xmm4,
    xmm5,
    xmm6,
    xmm7,
    xmm8,

    xmm9,
    xmm10,
    xmm11,
    xmm12,
    xmm13,
    xmm14,
    xmm15,

    // special regs (needs special encoding to access)
    rip,
};

static constexpr u32 X86_REG_SIZE = 33;
static constexpr u32 X86_GPR_SIZE = 16;
static constexpr u32 X86_FPR_SIZE = 16;

static const char* X86_NAMES[X86_REG_SIZE] =
{
    "rax",
    "rcx",
    "rdx",
    "rbx",
    "rsp",
    "rbp",
    "rsi",
    "rdi",

    "r8",
    "r9",
    "r10",
    "r11",
    "r12",
    "r13",
    "r14",
    "r15",

    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
    "xmm8",

    "xmm9",
    "xmm10",
    "xmm11",
    "xmm12",
    "xmm13",
    "xmm14",
    "xmm15",      

    "rip",
};

/*
enum class ir_pass
{
    optimize
    arch_rewrite1,
    reg_alloc,
}
*/

// intrin
static constexpr u32 INTRIN_TABLE_SIZE = 2;


struct FuncCallNode;
using INTRIN_EMIT_FUNC = void (*)(Interloper &itl,Function &func,FuncCallNode *func_call, RegSlot dst_slot);
using INTRIN_TYPE_FUNC = TypeResult (*)(Interloper &itl,FuncCallNode *func_call);

struct IntrinHandler
{
    INTRIN_EMIT_FUNC emit = nullptr;
    INTRIN_TYPE_FUNC type_check = nullptr;
};

extern const HashNode<String,IntrinHandler> INTRIN_TABLE[INTRIN_TABLE_SIZE];
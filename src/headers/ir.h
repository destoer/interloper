#pragma once
#include <destoer/destoer.h>

enum class op_type
{
    mov_reg,
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
    mul_x86,

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
    // varabile on the stack is out of scope
    // so we can reclaim allocation on the stack
    DIRECTIVE,
    alloc_slot,
    free_slot,

    alloc_local_array,
    alloc_global_array,
    free_fixed_array,
    alloc_vla,

    // allocate and store offset inside a var
    // used for runtime array alloc
    buf_alloc,
    state_dump,

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

    // used when the end of the block is read past in the optimiser
    placeholder,

    lock_reg,
    unlock_reg,


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

    // just c++ things not used
    END,
};

b32 is_directive(op_type type)
{
    return type >= op_type::DIRECTIVE;
}

// general operation defined for unsigned or unsigned ints
// will be convered to a specific conterpart in op_type
enum class logic_op
{
    cmplt_reg,
    cmple_reg,
    cmpgt_reg,
    cmpge_reg,

    cmpeq_reg,
    cmpne_reg,

    and_reg,
    or_reg,
};

static constexpr u32 LOGIC_OP_SIZE = 8;

static constexpr u32 OPCODE_SIZE = static_cast<u32>(op_type::END)+1;


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

inline bool is_group_branch(op_group group) 
{
    return group == op_group::branch_t || group == op_group::branch_reg_t;
}

enum class arg_type
{
    // NOTE: these 3 must be first
    src_reg,
    src_float,

    dst_reg,
    dst_float,

    // double duty, used in reg2 opcodes
    // i.e add dst, v1
    dst_src_float,
    dst_src_reg,


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

template<slot_type T>
b32 is_valid_slot(Slot<T> slot)
{
    return slot.handle != INVALID_HANDLE;
}


template<slot_type type>
u32 hash_slot(u32 size, Slot<type> v)
{
    // TODO: impl a better integer hash func
    const u32 hash = v.handle;
    const u32 slot = hash & (size - 1); 

    return slot;
}


using SymSlot = Slot<slot_type::symbol>;
using TmpSlot = Slot<slot_type::tmp>;
using SpecSlot = Slot<slot_type::spec>;

SymSlot sym_from_idx(u32 idx)
{
    return {idx};
}

static constexpr SymSlot SYMBOL_NO_SLOT = {0xffff'fffe};
static constexpr SymSlot SYM_ERROR = {0xffff'ffff};

template<slot_type type>
b32 operator==(const Slot<type> v1,const Slot<type> v2)
{
    return v1.handle == v2.handle;
}

using LabelSlot = Slot<slot_type::label>;

using BlockSlot = Slot<slot_type::block>;

extern const OpInfo OPCODE_TABLE[OPCODE_SIZE];

static constexpr u32 NON_ARG = 0xffffffff;

static constexpr u32 UNALLOCATED_OFFSET = 0xffff'ffff;

static constexpr u32 LOCATION_MEM = 0xffffffff;
static constexpr u32 LOCATION_GLOBAL = 0xfffffffe;


b32 is_var(SymSlot slot);

enum class operand_type
{
    decimal,
    imm,
    spec,
    reg,
    label,
    // Post rewrite raw operaend
    raw,
};


const String SPECIAL_REG_NAMES[16] = 
{
    "sp",
    "pc",
    "rv",
    "rv_float",
    "rax",
    "rcx",
    "rdx",
    "rdi",
    "rsi",
    "r8",
    "r9",
    "r10",
    "fixed_len",
    "null_slot",
    "const",
    "global",
};

static constexpr u32 SP_NAME_IDX = 0;
static constexpr u32 PC_NAME_IDX = 1;

static constexpr u32 SPECIAL_REG_SIZE = sizeof(SPECIAL_REG_NAMES) / sizeof(SPECIAL_REG_NAMES[0]);

enum class spec_reg
{
    // generic
    sp,
    pc,
    rv,
    rv_float,

    // x86
    rax,
    rcx,
    rdx,
    rdi,
    rsi,
    r8,
    r9,
    r10,

    // dummy reg to tell compilier loads are not necessary for fixed arrays
    access_fixed_len_reg,

    // dont perform any moves
    no_move,

    const_seg,
    global_seg,
};

enum class reg_kind
{
    local,
    global,
    constant,
    tmp,
    spec,
};

struct RegSlot
{
    union 
    {
        TmpSlot tmp_slot;
        SymSlot sym_slot = {SYMBOL_NO_SLOT};
        spec_reg spec;
    };

    reg_kind kind = reg_kind::local;
};

RegSlot make_sym_reg_slot(SymSlot slot, reg_kind kind)
{
    RegSlot handle;
    handle.sym_slot = slot;
    handle.kind = kind;

    return handle;
}

RegSlot make_tmp_reg_slot(TmpSlot slot)
{
    RegSlot handle;
    handle.tmp_slot = slot;
    handle.kind = reg_kind::tmp;

    return handle;
}

struct Operand
{
    union 
    {
        f64 decimal;
        u64 imm;
        u64 raw = 0;
        RegSlot reg;
        LabelSlot label;
    };

    operand_type type = operand_type::raw;
};

struct Opcode
{
    op_type op; 
    Operand v[3];
};

static constexpr u32 SIGNED_FLAG = 1 << 0;
static constexpr u32 STORED_IN_MEM = 1 << 1;
static constexpr u32 ALIASED = 1 << 2;
static constexpr u32 PENDING_STACK_ALLOCATION = 1 << 3;
static constexpr u32 CONST = 1 << 4;
static constexpr u32 FUNC_ARG = 1 << 5;
static constexpr u32 REG_FLOAT = 1 << 6;


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
    // what slot does this symbol hold inside the ir?
    RegSlot slot;

    // how much memory does this thing use GPR_SIZE max (spilled into count if larger)
    // i.e this is for stack allocation to get actual var sizes use type_size();
    u32 size = 0;
    u32 count = 0;

    // intialized during register allocation

    // where is the current offset for its section?
    u32 offset = UNALLOCATED_OFFSET;

    // Where is this register globally allocated if at all
    u32 global_reg = REG_FREE;
    // Where does this register reside in the current block?
    u32 local_reg = REG_FREE;

    u32 cur_local_uses = 0;
    Array<u32> local_uses;

    u32 flags = 0;
};

struct Interloper;
struct Function;
struct Type;

Reg make_reg(const RegSlot& slot, u32 size, b32 is_signed, b32 is_float);
Reg make_reg(Interloper& itl, const RegSlot& slot, const Type* type);
b32 is_special_reg(SymSlot r);
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



struct ListNode
{
    Opcode opcode;
    ListNode *next = nullptr;
    ListNode *prev = nullptr;
};

struct ListIterator
{

    ListIterator(ListNode* node)
    {
        this->node = node;
    }

    bool operator==(const ListIterator& it) const 
    {
        return node == it.node;
    }

    ListIterator& operator++()
    {
        node = node->next;
        return *this;
    }

    ListNode& operator*()
    {
        return *node;
    }

    const ListNode& operator*() const
    {
        return *node;
    }

    ListNode* node = nullptr;
};

struct List
{
    // global list Arena allocator
    ArenaAllocator *allocator = nullptr;

    ListNode *start = nullptr;

    ListNode *finish = nullptr;

    ListIterator begin()
    {
        return ListIterator(start);
    }

    ListIterator end()
    {
        return ListIterator(nullptr);
    }

    const ListIterator begin() const
    {
        return ListIterator(start);
    }

    const ListIterator end() const
    {
        return ListIterator(nullptr);
    }
};

enum class insertion_type
{
    after,
    before,
};

List make_list(ArenaAllocator* allocator);


static constexpr u32 SPECIAL_PURPOSE_BLOCK_START_HANDLE = 0xffff'fff0;
static constexpr u32 INVALID_BLOCK_HANDLE = SPECIAL_PURPOSE_BLOCK_START_HANDLE + 0;
static constexpr u32 BLOCK_FUNC_EXIT_HANDLE = SPECIAL_PURPOSE_BLOCK_START_HANDLE + 1;

static constexpr BlockSlot INVALID_BLOCK = {INVALID_BLOCK_HANDLE};
static constexpr BlockSlot BLOCK_FUNC_EXIT = {BLOCK_FUNC_EXIT_HANDLE}; 


static constexpr u32 HAS_FUNC_EXIT = 1 << 0;
static constexpr u32 REACH_FUNC_EXIT = 1 << 1;
static constexpr u32 IN_LOOP = 1 << 2;

struct Block
{
    List list;

    u32 flags = 0;

    // what is the corresponding label for this block?
    LabelSlot label_slot;

    // What is our own slot?
    BlockSlot block_slot;

    // blocks that can enter
    Array<BlockSlot> entry;

    // block we exit to
    Array<BlockSlot> exit;

    Set<SymSlot> live_in;
    Set<SymSlot> live_out;
    Set<SymSlot> def;
    Set<SymSlot> use;

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

void free_fixed_array(Interloper& itl,Function& func,RegSlot src,u32 size,u32 count);
void free_slot(Interloper& itl,Function& func, RegSlot slot);


BlockSlot block_from_idx(u32 v);
BlockSlot cur_block(Function& func);
Block& block_from_slot(Function& func, BlockSlot slot);


void destroy_emitter(IrEmitter& emitter);

b32 is_tmp(SymSlot s);


struct AddrSlot
{
    SymSlot slot = {SYMBOL_NO_SLOT};
    u32 offset = 0;

    // slot is not a pointer and refers to an actual variable
    b32 struct_addr = false;
};

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
    rdp,
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
    "rdp",
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
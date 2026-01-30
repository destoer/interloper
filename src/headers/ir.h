#pragma once
#include <destoer/destoer.h>
#include "list.inl"
#include "error.h"
#include "opcode.h"


struct TypedReg
{
    RegSlot slot;
    Type* type = nullptr;
};


using RegResult = destoer::Result<TypedReg,itl_error>;

inline bool is_direct_addr(const AddrSlot& addr_slot)
{
    return addr_slot.struct_addr && addr_slot.addr.index == make_spec_reg_slot(spec_reg::null) && addr_slot.addr.offset == 0;
}


AddrSlot make_struct_addr(RegSlot slot, u32 offset);
AddrSlot make_pointer_addr(RegSlot slot, u32 offset);
Addr make_addr(RegSlot slot, u32 offset);


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
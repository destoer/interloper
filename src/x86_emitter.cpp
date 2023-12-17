
struct X86Emitter
{
    Array<u8> buffer;
};

void destroy_emitter(X86Emitter& emitter)
{
    destroy_arr(emitter.buffer);
}

enum x86_reg
{
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rdp,
    rsi,
    rdi,
};

template<typename T>
void push(X86Emitter& emitter, T v)
{
    push_var(emitter.buffer,v);
}

void push_u8(X86Emitter& emitter, u8 v)
{
    push(emitter,v);
}

void push_u16(X86Emitter& emitter, u16 v)
{
    push(emitter,v);
}

void push_u32(X86Emitter& emitter, u32 v)
{
    push(emitter,v);
}

void push_u64(X86Emitter& emitter, u64 v)
{
    push(emitter,v);
}

// base rex
static constexpr u8 REX = 0x40;

// 64 bit operand
static constexpr u8 REX_W = REX | (1 << 3);

// extend reg
static constexpr u8 REX_R = REX | (1 << 2);

// extend sib index
static constexpr u8 REX_X = REX | (1 << 1);

// extend sib base / modrm
static constexpr u8 REX_B = REX | (1 << 0);


u8 mod_reg(x86_reg reg)
{
    return (0b11 << 6) | (u32(reg) << 3);
}

void mov_imm(X86Emitter& emitter, x86_reg reg, u64 imm)
{
    const u8 opcode = 0xb8 + u32(reg);

    // mov r64, imm64
    push_u16(emitter,(opcode << 8) | REX_W);

    // push the immediate
    push_u64(emitter,imm);
}

/*
void add(X86Emitter& emitter, x86_reg dst, x86_reg v1)
{
    // add dst, v1, v2 -> lea dst,[v1 + v2]
}

void sub(X86Emitter& emitter, x86_reg dst, x86_reg v1)
{
    // sub dst
}
*/


void syscall(X86Emitter& emitter)
{
    push_u16(emitter,0x05'0f);
}
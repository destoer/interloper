namespace x86
{
// https://wheremyfoodat.github.io/floating-point-arithmetic-in-x86/
// https://wiki.osdev.org/X86_Instruction_Encoding


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


u32 mask_reg(x86_reg reg)
{
    return u32(reg) & 0x7;
}

void zxb(AsmEmitter& emitter, x86_reg dst);

u8 mod_rm(x86_reg r, x86_reg m)
{
    // reg, reg
    return (0b11 << 6) | (mask_reg(r) << 3) | (mask_reg(m) << 0);
}

u8 mod_mr(x86_reg m, x86_reg r)
{
    return mod_rm(r,m);
}

u8 mod_mo(x86_reg m,u32 o)
{
    return (0b11 << 6) | (o << 3) | (mask_reg(m) << 0);
}

u16 reg_base_disp_32(x86_reg reg,x86_reg base)
{
    // reg, [base + disp32]
    const u8 mod = (0b10 << 6) | (mask_reg(reg) << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}

u16 reg_base_disp_8(x86_reg reg,x86_reg base)
{
    // reg, [base + disp8]
    const u8 mod = (0b01 << 6) | (mask_reg(reg) << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}


u16 reg_base(x86_reg reg,x86_reg base)
{
    // reg, [base]
    const u8 mod = (0b00 << 6) | (mask_reg(reg) << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}

u16 reg_base_index(x86_reg dst, x86_reg base, x86_reg index, u32 scale)
{
    const u32 log_scale = log2(scale);
    const u8 mod = (0b00 << 6) | (mask_reg(dst) << 3) | (0b100 << 0);
    const u8 sib = (log_scale << 6) | (mask_reg(index) << 3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}

u16 reg_base_index_disp_8(x86_reg dst, x86_reg base, x86_reg index, u32 scale)
{
    const u32 log_scale = log2(scale);
    const u8 mod = (0b01 << 6) | (mask_reg(dst) << 3) | (0b100 << 0);
    const u8 sib = (log_scale << 6) | (mask_reg(index) << 3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}

u16 reg_base_index_disp_32(x86_reg dst, x86_reg base, x86_reg index, u32 scale)
{
    const u32 log_scale = log2(scale);
    const u8 mod = (0b10 << 6) | (mask_reg(dst) << 3) | (0b100 << 0);
    const u8 sib = (log_scale << 6) | (mask_reg(index) << 3) | (mask_reg(base) << 0);

    return ((sib << 8) | (mod << 0));
}


void prefix_u16_reg(AsmEmitter& emitter)
{
    // 16 bit override
    push_u8(emitter,0x66);
}

b32 is_extended_reg(x86_reg reg)
{
    return (reg >= x86_reg::r8 && reg <= x86_reg::r15) || (reg >= x86_reg::xmm8 && reg <= x86_reg::xmm15);
}

u8 rex_rm(x86_reg r, x86_reg m)
{
    u8 rex = 0;

    if(is_extended_reg(r))
    {
        rex |= REX_R;
    }

    if(is_extended_reg(m))
    {
        rex |= REX_B;
    }

    return rex;
}

u8 rex_rbi(x86_reg r, x86_reg b, x86_reg i)
{
    u8 rex = 0;

    if(is_extended_reg(r))
    {
        rex |= REX_R;
    }

    if(is_extended_reg(i))
    {
        rex |= REX_X;
    }

    if(is_extended_reg(b))
    {
        rex |= REX_B;
    }

    return rex;
}


u8 rex_mr(x86_reg m, x86_reg r)
{
    return rex_rm(r,m);
}

u8 rex_r(x86_reg r)
{
    return is_extended_reg(r)? REX_R : 0;
}

u8 rex_m(x86_reg r)
{
    return is_extended_reg(r)? REX_B : 0;
}

u8 rex_r64(x86_reg r)
{
    return REX_W | rex_r(r);
}

u8 rex_m64(x86_reg r)
{
    return REX_W | rex_m(r);
}

u8 rex_rm64(x86_reg r, x86_reg m)
{
    return REX_W | rex_rm(r,m);
}

u8 rex_rbi64(x86_reg r, x86_reg b, x86_reg i)
{
    return REX_W | rex_rbi(r,b,i);
}



u8 rex_mr64(x86_reg r, x86_reg m)
{
    return REX_W | rex_mr(r,m);
}

void prefix_u8_data_m(AsmEmitter& emitter, x86_reg m)
{
    if(is_extended_reg(m))
    {
        push_u8(emitter,rex_m(m));
    }

    // index or data reg, used must prefix with rex
    // as it is not possible to access 8 bits on older x86 versions
    else if(m > x86_reg::rdx)
    {
        push_u8(emitter,REX);
    }
}

void prefix_u8_data_rm(AsmEmitter& emitter, x86_reg r, x86_reg m)
{
    if(is_extended_reg(r) || is_extended_reg(m))
    {
        push_u8(emitter,rex_rm(r,m));
    }

    // index or data reg, used must prefix with rex
    else if(r > x86_reg::rdx)
    {
        push_u8(emitter,REX);
    }
}

void prefix_u8_data_rbi(AsmEmitter& emitter, x86_reg r, x86_reg b, x86_reg i)
{
    if(is_extended_reg(r) || is_extended_reg(b) || is_extended_reg(i))
    {
        push_u8(emitter,rex_rbi(r,b,i));
    }

    // index or data reg, used must prefix with rex
    else if(r > x86_reg::rdx)
    {
        push_u8(emitter,REX);
    }
}



void push_reg_base_disp(AsmEmitter& emitter,x86_reg reg, x86_reg base, u64 imm)
{
    // handle special regs
    if(base == x86_reg::rip)
    {
        // reg [rip + disp32]
        const u8 mod = (0b00 << 6) | (mask_reg(reg) << 3) | (0b101 << 0);
        push_u8(emitter,mod);
        push_u32(emitter,imm);
    }

    else if(imm == 0)
    {
        // rbp is reserved for RIP encoding, and r13 is reserved also
        // when no base is used we have to use disp 8 anyways
        if(base == x86_reg::rbp || base == x86_reg::r13)
        {
            push_u16(emitter,reg_base_disp_8(reg,base));
            push_u8(emitter,0);     
        }

        else
        {
            push_u16(emitter,reg_base(reg,base));
        }
    }

    else if(fit_into_s8(imm))
    {
        push_u16(emitter,reg_base_disp_8(reg,base));
        push_u8(emitter,s8(imm));
    }

    // use 32 bit
    else if(fit_into_s32(imm))
    {
        push_u16(emitter,reg_base_disp_32(reg,base));
        push_u32(emitter,imm);
    }

    // cannot fit 
    else
    {
        crash_and_burn("Cannot fit base disp imm %lx",imm);
    }
}


void push_reg_base_index_disp(AsmEmitter& emitter,x86_reg reg, x86_reg base, x86_reg index, u64 scale, u64 imm)
{
    // Cannot do indexed RIP relative addressing.
    // If we have emitted these instructions we have messed up.
    assert(base != x86_reg::rip);

    if(imm == 0)
    {
        // rbp is reserved for RIP encoding, and r13 is reserved also
        // when no base is used we have to use disp 8 anyways
        if(base == x86_reg::rbp || base == x86_reg::r13)
        {
            push_u16(emitter,reg_base_index_disp_8(reg,base,index,scale));
            push_u8(emitter,0);     
        }

        else
        {
            push_u16(emitter,reg_base_index(reg,base,index,scale));
        }
    }

    else if(fit_into_s8(imm))
    {
        push_u16(emitter,reg_base_index_disp_8(reg,base,index,scale));
        push_u8(emitter,s8(imm));
    }

    // use 32 bit
    else if(fit_into_s32(imm))
    {
        push_u16(emitter,reg_base_index_disp_32(reg,base,index,scale));
        push_u32(emitter,imm);
    }

    // cannot fit
    else
    {
        crash_and_burn("Cannot fit index disp imm %lx",imm);
    }
}

void push_reg_base_index(AsmEmitter& emitter, x86_reg reg, x86_reg base, x86_reg index)
{
    push_reg_base_index_disp(emitter,reg,base,index,1,0);
}

void push_rex_opt(AsmEmitter& emitter, u8 rex)
{
    if(rex != 0)
    {
        push_u8(emitter,rex);
    }
}

// NOTE: this is just for the override
// not a size ext that has to happen separately
void emit_rex_rm_opt(AsmEmitter& emitter,x86_reg r, x86_reg m)
{
    const u8 rex = rex_rm(r,m);
    push_rex_opt(emitter,rex);
}

void emit_rex_m_opt(AsmEmitter& emitter,x86_reg m)
{
    const u8 rex = rex_m(m);
    push_rex_opt(emitter,rex);
}

// NOTE: this is just for the override
// not a size ext that has to happen separately
void emit_rex_rbi_opt(AsmEmitter& emitter,x86_reg r, x86_reg b, x86_reg i)
{
    const u8 rex = rex_rbi(r,b,i);
    push_rex_opt(emitter,rex);
}


enum class prefix_t
{
    rm8,
    rm16,
    rm32,
    rm64,
};

template<prefix_t prefix>
void emit_rm_prefix(AsmEmitter& emitter, x86_reg r, x86_reg m)
{
    if constexpr(prefix == prefix_t::rm8)
    {
        prefix_u8_data_rm(emitter,r,m);
    }

    else if constexpr(prefix == prefix_t::rm16)
    {
        prefix_u16_reg(emitter);
        emit_rex_rm_opt(emitter,r,m);
    }

    if constexpr(prefix == prefix_t::rm32)
    {
        emit_rex_rm_opt(emitter,r,m);
    }

    else if constexpr(prefix == prefix_t::rm64)
    {
        push_u8(emitter,rex_rm64(r,m));
    }
}

template<prefix_t prefix>
void emit_rbi_prefix(AsmEmitter& emitter, x86_reg r, x86_reg b, x86_reg i)
{
    if constexpr(prefix == prefix_t::rm8)
    {
        prefix_u8_data_rbi(emitter,r,b,i);
    }

    else if constexpr(prefix == prefix_t::rm16)
    {
        prefix_u16_reg(emitter);
        emit_rex_rbi_opt(emitter,r,b,i);
    }

    if constexpr(prefix == prefix_t::rm32)
    {
        emit_rex_rbi_opt(emitter,r,b,i);
    }

    else if constexpr(prefix == prefix_t::rm64)
    {
        push_u8(emitter,rex_rbi64(r,b,i));
    }
}



template<typename T,prefix_t prefix>
void emit_mem_op(AsmEmitter& emitter,T opcode,x86_reg reg, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    // [base + offset]
    if(!index)
    {
        // handle storage size
        emit_rm_prefix<prefix>(emitter,reg,base);
        push(emitter,opcode);
        push_reg_base_disp(emitter,reg,base,imm);
    }

    else
    {
        // handle storage size
        emit_rbi_prefix<prefix>(emitter,reg,base,*index);
        push(emitter,opcode);
        push_reg_base_index_disp(emitter,reg,base,*index,scale,imm);
    }
}

// don't bother specifying types on most loads
// because we just want to sign extend
void emit_load32_op8(AsmEmitter& emitter,u8 opcode,x86_reg dst, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm32>(emitter,opcode,dst,base,index,scale,imm);
}

void emit_load_op8(AsmEmitter& emitter,u8 opcode,x86_reg dst, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm64>(emitter,opcode,dst,base,index,scale,imm);
}
void emit_load_op16(AsmEmitter& emitter,u16 opcode,x86_reg dst, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u16,prefix_t::rm64>(emitter,opcode,dst,base,index,scale,imm);
}

void emit_store8_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm8>(emitter,opcode,src,base,index,scale,imm);
}

void emit_store16_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm16>(emitter,opcode,src,base,index,scale,imm);
}

void emit_store32_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm32>(emitter,opcode,src,base,index,scale,imm);
}

void emit_store64_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base,  Option<x86_reg> index, u64 scale, u64 imm)
{
    emit_mem_op<u8,prefix_t::rm64>(emitter,opcode,src,base,index,scale,imm);
}

template<prefix_t prefix>
void emit_reg2_rm_extended(AsmEmitter& emitter, const u16 opcode, x86_reg r, x86_reg m)
{
    emit_rm_prefix<prefix>(emitter,r,m);

    // opcode r32, r32
    push_u16(emitter,opcode);
    push_u8(emitter,mod_rm(r,m));
}

void emit_reg2_rm_extended_32(AsmEmitter& emitter, const u16 opcode, x86_reg r, x86_reg m)
{
    emit_reg2_rm_extended<prefix_t::rm32>(emitter,opcode,r,m);
}

void emit_reg2_rm_extended_64(AsmEmitter& emitter, const u16 opcode, x86_reg r, x86_reg m)
{
    emit_reg2_rm_extended<prefix_t::rm64>(emitter,opcode,r,m);
}

void emit_reg2_rm_32(AsmEmitter& emitter, const u8 opcode, x86_reg r, x86_reg m)
{
    emit_rex_rm_opt(emitter,r,m);

    // opcode r1, r2
    push_u16(emitter,(mod_rm(r,m) << 8) | (opcode << 0));
}


void emit_reg2_rm_64(AsmEmitter& emitter, const u8 opcode, x86_reg r, x86_reg m)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | rex_rm64(r,m));
    push_u8(emitter,mod_rm(r,m));
}

void emit_reg2_mr_32(AsmEmitter& emitter, const u8 opcode, x86_reg m, x86_reg r)
{
    emit_reg2_rm_32(emitter,opcode,r,m);
}

void emit_reg2_mr_64(AsmEmitter& emitter, const u8 opcode, x86_reg m, x86_reg r)
{
    emit_reg2_rm_64(emitter,opcode,r,m);
}

void emit_plus_reg_opcode(AsmEmitter& emitter,x86_reg dst, const u8 opcode)
{
    emit_rex_m_opt(emitter,dst);

    // pop +r64
    push_u8(emitter,opcode + mask_reg(dst));    
}

u32 emit_cond_jump(AsmEmitter& emitter, u16 opcode)
{
    push_u16(emitter,opcode);

    const u32 offset = emitter.buffer.size;

    // dummy value
    push_u32(emitter,0);

    return offset;
}


void push_imm64_ext_op(AsmEmitter& emitter, x86_reg dst, u32 opcode, u32 opcode_ext)
{
    push_u16(emitter,(opcode << 8) | rex_m64(dst));

    // opcode extenstion required
    push_u8(emitter,mod_mo(dst,opcode_ext));
}

void emit_arith_imm(AsmEmitter& emitter, x86_reg dst, s64 v1, u32 opcode_ext)
{
    // add r64, imm8
    if(fit_into_s8(v1))
    {
        push_imm64_ext_op(emitter,dst,0x83,opcode_ext);
        push_u8(emitter,s8(v1));
    }

    // add r64, imm32
    else if(fit_into_s32(v1))
    {
        push_imm64_ext_op(emitter,dst,0x81,opcode_ext);
        push_u32(emitter,v1);
    }    

    // imm64 required
    // NOTE: this should not trip
    // our upper level IR should convert this to
    // add dst, imm64
    // -> mov t0, imm64
    // -> add dst, t0
    else
    {
        crash_and_burn("Cannot fit arith imm %lx",v1);
    }
}

void emit_shift(AsmEmitter& emitter, x86_reg src, u32 op)
{
    const u8 opcode = 0xd3;
    push_u16(emitter,(opcode << 8) | rex_m64(src));

    push_u8(emitter,mod_mo(src,op));   
}

void emit_shift_imm(AsmEmitter& emitter, x86_reg dst, u8 v1, u32 opcode_ext)
{
    // shl r64, imm8
    push_imm64_ext_op(emitter,dst,0xc1,opcode_ext);
    push_u8(emitter,v1);
}

void emit_set_flag(AsmEmitter& emitter, x86_reg dst, u8 op)
{   
    // set<x> dst
    prefix_u8_data_m(emitter,dst);

    push_u16(emitter,(op << 8) | (0xf << 0));
    push_u8(emitter,mod_mo(dst,0));

    // now zero extend the 8 bit quantity
    // as set<x> does not 
    zxb(emitter,dst);
}

void emit_arith_fixed(AsmEmitter& emitter, x86_reg src, u32 ext)
{
    // udiv r64
    const u8 opcode = 0xf7;
    push_u16(emitter,(opcode << 8) | rex_m64(src));

    push_u8(emitter,mod_mo(src,ext));
}

void emit_branch_reg(AsmEmitter& emitter, x86_reg src, u32 ext)
{
    emit_rex_m_opt(emitter,src);

    // call r64
    const u8 opcode = 0xff;
    push_u8(emitter,opcode);

    push_u8(emitter,mod_mo(src,ext));
}

u32 emit_branch_rel(AsmEmitter& emitter, u8 opcode)
{
    // jmp rel32
    push_u8(emitter,opcode);

    const u32 offset = emitter.buffer.size;

    // dummy value
    push_u32(emitter,0);

    return offset;  
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // add r64, r64
    emit_reg2_rm_64(emitter,0x3,dst,v1);
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1, x86_reg v2)
{
    // lea r64, [r64 + r64]
    const u8 opcode = 0x8d;
    push_u16(emitter,(opcode << 8) |  rex_rbi64(dst,v1,v2));
    push_reg_base_index(emitter,dst,v1,v2);
}

void bitwise_and(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // and r64, r64
    emit_reg2_rm_64(emitter,0x23,dst,v1);
}

void bitwise_or(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // or r64, r64
    emit_reg2_rm_64(emitter,0xB,dst,v1);
}

void bitwise_xor(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // xor r64, r64
    emit_reg2_rm_64(emitter,0x33,dst,v1);
}

void bitwise_not(AsmEmitter& emitter, x86_reg dst)
{
    // not r64
    const u8 opcode = 0xf7;
    push_u16(emitter,(opcode << 8) | rex_m64(dst));

    push_u8(emitter,mod_mo(dst,2));
}

void sub(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // sub r64, r64
    emit_reg2_rm_64(emitter,0x2B,dst,v1);
}

void cmp_gpr(AsmEmitter& emitter, x86_reg v1, x86_reg v2)
{
    // cmp r64, r64
    emit_reg2_rm_64(emitter,0x3B,v1,v2);
}

void test(AsmEmitter& emitter, x86_reg v1, x86_reg v2)
{
    // TODO: do we even care about 64 bit here ever?
    // test r64, r64
    emit_reg2_rm_64(emitter,0x85,v1,v2);
}

void zero_reg(AsmEmitter& emitter, x86_reg dst)
{
    // xor reg, reg 
    // NOTE: we use 32 version as it sign extends
    // and is shorter
    emit_reg2_rm_32(emitter,0x31,dst,dst);
}

void mov_imm(AsmEmitter& emitter, x86_reg reg, u64 imm)
{
    //NOTE: this move does not sign extend
    const u8 opcode = 0xb8 + mask_reg(reg);

    // requires 64 bit mov
    if(imm > 0xffff'ffff)
    {
        // mov +r64, imm64
        push_u16(emitter,(opcode << 8) | rex_m64(reg));

        // push the immediate
        push_u64(emitter,imm);
    }


    // special case zero
    else if(imm == 0)
    {
        zero_reg(emitter,reg);
    }

    // 32 bit move
    else
    {
        emit_rex_m_opt(emitter,reg);

        push_u8(emitter,opcode);
        push_u32(emitter,u32(imm));
    }
}

void add_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    emit_arith_imm(emitter,dst,v1,0);
}

void sub_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    emit_arith_imm(emitter,dst,v1,5);
}

void xor_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    emit_arith_imm(emitter,dst,v1,6);
}

void zxb(AsmEmitter& emitter, x86_reg dst)
{
    // movzx r64, r8
    if(dst <= x86_reg::rbx)
    {
        emit_reg2_rm_extended_32(emitter,0xb6'0f,dst,dst);
    }

    // need to use 64 bit for the upper registers
    else
    {
        emit_reg2_rm_extended_64(emitter,0xb6'0f,dst,dst);
    }  
}

void and_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    // special case type clipping
    if(v1 == 0xff)
    {
        zxb(emitter,dst);
    }

    else if(v1 == 0xffff)
    {
        // movzx r64, r16
        emit_reg2_rm_extended_32(emitter,0xb7'0f,dst,dst);
    }

    else if(v1 == 0xffff'ffff)
    {
        // rely on 32 bit zero extension
        // NOTE: this may generate weird looking instructions
        // like mov eax, eax
        // mov r32, r32
        emit_reg2_mr_32(emitter,0x89,dst,dst);
    }

    // special case for u32
    // as any 32 bit value will remove
    // the top bits when and happens a zero extend from eax is fine!
    else if(fit_into_u32(v1))
    {
        emit_rex_m_opt(emitter,dst);

        // and r32, u32
        const u32 opcode = 0x81;
        push_u16(emitter,(mod_mo(dst,4) << 8) | (opcode << 0));

        push_u32(emitter,v1);   
    }

    else
    {
        emit_arith_imm(emitter,dst,v1,4);
    }
}


void lsl_imm(AsmEmitter& emitter, x86_reg dst, u32 v1)
{
    emit_shift_imm(emitter,dst,v1,4);
}

void lsr_imm(AsmEmitter& emitter, x86_reg dst, u32 v1)
{
    emit_shift_imm(emitter,dst,v1,5);
}

void asr_imm(AsmEmitter& emitter, x86_reg dst, u32 v1)
{
    emit_shift_imm(emitter,dst,v1,7);
}


void cmp_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    emit_arith_imm(emitter,dst,v1,7);
}

void mov(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // mov r64, r64
    emit_reg2_mr_64(emitter,0x89,dst,v1);
}

void leave(AsmEmitter& emitter)
{
    push_u8(emitter,0xC9);
}

void ret(AsmEmitter& emitter)
{
    push_u8(emitter,0xC3);
}

void sb(AsmEmitter& emitter, x86_reg src, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov r/m8, r8
    emit_store8_op8(emitter,0x88,src,base,index,scale,imm);
}

void lb(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // movzx r64, r/m8,
    emit_load_op16(emitter,0xb6'0f,dst,base,index,scale,imm);
}

void lh(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // movzx r64, r/m16,
    emit_load_op16(emitter,0xb7'0f,dst,base,index,scale,imm);
}

void sh(AsmEmitter& emitter, x86_reg src, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov r/m16, r16
    emit_store16_op8(emitter,0x89,src,base,index,scale,imm);
}


void lsw(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // movsxd r64, r/m16
    emit_load_op8(emitter,0x63,dst,base,index,scale,imm);
}

void lsb(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // movsx r64, r/m8
    emit_load_op16(emitter,0xbe'0f,dst,base,index,scale,imm);
}

void lsh(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // movsx r64, r/m8
    emit_load_op16(emitter,0xbf'0f,dst,base,index,scale,imm);
}

void lw(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov r32, m/r32
    emit_load32_op8(emitter,0x8b,dst,base,index,scale,imm);
}

void sw(AsmEmitter& emitter, x86_reg src, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov r/m32, r32
    emit_store8_op8(emitter,0x89,src,base,index,scale,imm);
}


void ld(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov r64, m/r64
    emit_load_op8(emitter,0x8b,dst,base,index,scale,imm);
}


void sd(AsmEmitter& emitter, x86_reg src, x86_reg base, Option<x86_reg> index, u32 scale,u64 imm)
{
    // mov m/r64, r64
    emit_store64_op8(emitter,0x89,src,base,index,scale,imm);
}

void sxb(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsx r64, r8
    emit_reg2_rm_extended_64(emitter,0xbe'0f,dst,v1);
}


void sxh(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsx r64, r16
    emit_reg2_rm_extended_64(emitter,0xbf'0f,dst,v1);
}

void sxw(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsxd r64, r32
    emit_reg2_rm_64(emitter,0x63,dst,v1);
}

u32 je(AsmEmitter& emitter)
{
    return emit_cond_jump(emitter,0x84'0f);
}

u32 jne(AsmEmitter& emitter)
{
    return emit_cond_jump(emitter,0x85'0f);
}

u32 branch(AsmEmitter& emitter)
{
    // jmp rel32
    return emit_branch_rel(emitter,0xe9);
}

u32 call(AsmEmitter& emitter)
{
    // TODO: we need a way to encode far calls
    // in the upper IR eventually

    // call rel32
    return emit_branch_rel(emitter,0xe8);
}

void call_reg(AsmEmitter& emitter, x86_reg src)
{
    // call r64
    emit_branch_reg(emitter,src,2);
}


void branch_reg(AsmEmitter& emitter, x86_reg src)
{
    // jmp r64
    emit_branch_reg(emitter,src,4);
}


void setsgt(AsmEmitter& emitter, x86_reg dst)
{
    // setg
    emit_set_flag(emitter,dst,0x9f);
}

void setsge(AsmEmitter& emitter, x86_reg dst)
{
    // setge
    emit_set_flag(emitter,dst,0x9d);
}

void setslt(AsmEmitter& emitter, x86_reg dst)
{
    // setl
    emit_set_flag(emitter,dst,0x9c);
}

void setsle(AsmEmitter& emitter, x86_reg dst)
{
    // setle
    emit_set_flag(emitter,dst,0x9e);
}


void setugt(AsmEmitter& emitter, x86_reg dst)
{
    // seta
    emit_set_flag(emitter,dst,0x97);
}

void setuge(AsmEmitter& emitter, x86_reg dst)
{
    // setae
    emit_set_flag(emitter,dst,0x93);
}


void setult(AsmEmitter& emitter, x86_reg dst)
{
    // setb
    emit_set_flag(emitter,dst,0x92);
}

void setule(AsmEmitter& emitter, x86_reg dst)
{
    // setbe
    emit_set_flag(emitter,dst,0x96);
}


void seteq(AsmEmitter& emitter, x86_reg dst)
{
    emit_set_flag(emitter,dst,0x94);
}

void setne(AsmEmitter& emitter, x86_reg dst)
{
    emit_set_flag(emitter,dst,0x95);
}


void push(AsmEmitter& emitter, x86_reg src)
{
    // push +r64
    emit_plus_reg_opcode(emitter,src,0x50);
}

void pop(AsmEmitter& emitter, x86_reg src)
{
    // pop +r64
    emit_plus_reg_opcode(emitter,src,0x58);
}

void syscall(AsmEmitter& emitter)
{
    push_u16(emitter,0x05'0f);
}

void cqo(AsmEmitter& emitter)
{
    const u8 opcode = 0x99;
    push_u16(emitter,(opcode << 8) | REX_W);
}

void udiv_x86(AsmEmitter& emitter, x86_reg src)
{
    // udiv r64
    emit_arith_fixed(emitter,src,6);
}

void sdiv_x86(AsmEmitter& emitter, x86_reg src)
{
    // idiv r64
    emit_arith_fixed(emitter,src,7);
}

void lsl_x86(AsmEmitter& emitter, x86_reg src)
{
    // lsl r64, cl
    emit_shift(emitter,src,4);
}

void lsr_x86(AsmEmitter& emitter, x86_reg src)
{
    // lsr r64, cl
    emit_shift(emitter,src,5);
}

void asr_x86(AsmEmitter& emitter, x86_reg src)
{
    // asr r64, cl;
    emit_shift(emitter,src,7);
}

void add_imm(AsmEmitter& emitter, x86_reg dst, x86_reg v1, u64 imm)
{
    // lea r64, [r64 + disp]
    const u8 opcode = 0x8d;
    push_u16(emitter,(opcode << 8) |  rex_rm64(dst,v1));

    push_reg_base_disp(emitter,dst,v1,imm);
}

void mul(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // imul r64, m64
    const u16 opcode = 0xAF0F;
    emit_reg2_rm_extended_64(emitter,opcode,dst,v1);
}

void lea(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u64 scale, u64 imm)
{
    if(!index)
    {
        add_imm(emitter,dst,base,imm);
    }

    else
    {
        const u8 opcode = 0x8d;
        push_u16(emitter,(opcode << 8) |  rex_rbi64(dst,base,*index));

        push_reg_base_index_disp(emitter,dst,base,*index,scale,imm);
    }
}

void add_rip_rel_link(AsmEmitter& emitter, const Opcode& opcode)
{
    // NOTE: relies on emitter disp always being u32
    // for RIP relative
    const u32 offset = emitter.buffer.size - 4;

    add_link(emitter,opcode,offset);
}

template<typename T, typename FUNC_PTR>
void emit_load_store(AsmEmitter& emitter, const Opcode& opcode, const T& addr_op, FUNC_PTR func)
{
    const auto dst = x86_reg(addr_op.v1.reg);
    auto addr = x86_reg(addr_op.addr.base);

    Option<x86_reg> index = option::none; 
    auto raw_index = u32(addr_op.addr.index);
    
    if(raw_index != u32(spec_reg::null))
    {
        index = x86_reg(raw_index);
    }
    
    s64 offset = s64(addr_op.addr.offset);
    const s64 scale = s64(addr_op.addr.scale);

    const bool is_data_sect = (addr == u32(spec_reg::const_seg) || addr == u32(spec_reg::global_seg));

    if(is_data_sect)
    {
        addr = x86_reg::rip;
        offset = 0;
    }

    func(emitter,dst,addr,index,scale,offset);

    if(is_data_sect)
    {
        add_rip_rel_link(emitter,opcode);
    }
}

void push_xmm_f2(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // push rex
    const u8 rex = rex_rm64(dst,src);
    push_u16(emitter,(rex << 8) | 0xf2);
}

void push_xmm_rbi(AsmEmitter& emitter, x86_reg dst, x86_reg base, x86_reg index)
{
    // push rex
    const u8 rex = rex_rbi64(dst,base,index);
    push_u16(emitter,(rex << 8) | 0xf2);
}


void push_xmm_66(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // push rex
    const u8 rex = rex_rm64(dst,src);
    push_u16(emitter,(rex << 8) | 0x66);
}

void lf(AsmEmitter& emitter, x86_reg dst, x86_reg base, Option<x86_reg> index, u64 scale, u64 imm)
{
    if(!index)
    {
        // movsd r, [m]
        push_xmm_f2(emitter,dst,base);
        push_u16(emitter,0x10'0f);

        push_reg_base_disp(emitter,dst,base,imm);
    }

    else 
    {
        push_xmm_rbi(emitter,dst,base,*index);

        push_u16(emitter,0x10'0f);
        push_reg_base_index_disp(emitter,dst,base,*index,scale,imm);
    }
}

void sf(AsmEmitter& emitter, x86_reg src, x86_reg base, Option<x86_reg> index, u64 scale, u64 imm)
{
    if(!index)
    {
        // movsd [m], r
        push_xmm_f2(emitter,src,base);
        push_u16(emitter,0x11'0f);

        push_reg_base_disp(emitter,src,base,imm);
    }

    else
    {
        push_xmm_rbi(emitter,src,base,*index);

        push_u16(emitter,0x11'0f);
        push_reg_base_index_disp(emitter,src,base,*index,scale,imm);     
    }
}

void movf(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // movsd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x10'0f);

    push_u8(emitter,mod_rm(dst,src));
}


void addf(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // addsd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x58'0f);

    push_u8(emitter,mod_rm(dst,src));
}

void subf(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // subsd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x5C'0f);

    push_u8(emitter,mod_rm(dst,src));
}

void mulf(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // mulsd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x59'0f);

    push_u8(emitter,mod_rm(dst,src));
}

void divf(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // divsd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x5E'0f);

    push_u8(emitter,mod_rm(dst,src));
}

void cvt_fi(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // cvttsd2si r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x2c'0f);

    push_u8(emitter,mod_rm(dst,src));
}



void cvt_if(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // cvtsi2sd r, m
    push_xmm_f2(emitter,dst,src);
    push_u16(emitter,0x2a'0f);

    push_u8(emitter,mod_rm(dst,src));
}

void cmp_fpr(AsmEmitter& emitter, x86_reg dst, x86_reg src)
{
    // ucomisd r, m
    push_xmm_66(emitter,dst,src);
    push_u16(emitter,0x2e'0f);

    push_u8(emitter,mod_rm(dst,src));
}

// these all happen to use the same flags as their unsigned counterparts
// put we still want to name them properly externally
void setflt(AsmEmitter& emitter, x86_reg dst)
{
    setult(emitter,dst);
}

void setfle(AsmEmitter& emitter, x86_reg dst)
{
    setule(emitter,dst);
}

void setfgt(AsmEmitter& emitter, x86_reg dst)
{
    setugt(emitter,dst);
}

void setfge(AsmEmitter& emitter, x86_reg dst)
{
    setuge(emitter,dst);
}

void setfeq(AsmEmitter& emitter, x86_reg dst)
{
    seteq(emitter,dst);
}

void setfne(AsmEmitter& emitter, x86_reg dst)
{
    setne(emitter,dst);
}


void emit_reg1_src(AsmEmitter& emitter, const RegOneSrc& reg1_src)
{
    const auto src = x86_reg(reg1_src.src.reg);

    switch(reg1_src.type)
    {
        case reg1_src_type::push: push(emitter,src); break;
    }
}

void panic_lowered(const char* name)
{
    crash_and_burn("Opcode: %s should have been lowered",name);
}


void emit_sign_extend(AsmEmitter& emitter, const SignExtend& sign_extend)
{
    const auto dst = x86_reg(sign_extend.dst.reg);
    const auto src = x86_reg(sign_extend.src.reg);

    switch(sign_extend.type)
    {
        case sign_extend_op::sxb: sxb(emitter,dst,src); break;
        case sign_extend_op::sxh: sxh(emitter,dst,src); break;
        case sign_extend_op::sxw: sxw(emitter,dst,src); break;
    }
}


void emit_unary_reg2(AsmEmitter& emitter, const UnaryRegTwo& unary_reg2)
{
    const auto dst = x86_reg(unary_reg2.dst.reg);
    const auto src = x86_reg(unary_reg2.src.reg);

    switch(unary_reg2.type)
    {
        case unary_reg2_op::mov_gpr_reg: mov(emitter,dst,src); break;
        case unary_reg2_op::mov_fpr_reg: movf(emitter,dst,src); break;
        case unary_reg2_op::bitwise_not: panic_lowered("not_unary_reg2"); break;
        case unary_reg2_op::cvt_if: cvt_if(emitter,dst,src); break;
        case unary_reg2_op::cvt_fi: cvt_fi(emitter,dst,src); break;
    }
}

void emit_unary_reg1(AsmEmitter& emitter, const UnaryRegOne& unary_reg1)
{
    const auto dst = x86_reg(unary_reg1.dst.reg);

    switch(unary_reg1.type)
    {
        case unary_reg1_op::bitwise_not: bitwise_not(emitter,dst);
    }
}


void emit_mov_imm(AsmEmitter& emitter, const MovGprImm& mov)
{
    mov_imm(emitter,x86_reg(mov.dst.reg),mov.imm);
}

void emit_load(AsmEmitter& emitter, const Opcode& opcode, const Load& load)
{
    switch(load.type)
    {
        case load_type::lb: emit_load_store(emitter,opcode,load,lb); break;
        case load_type::lh: emit_load_store(emitter,opcode,load,lh); break;
        case load_type::lw: emit_load_store(emitter,opcode,load,lw); break;
        case load_type::ld: emit_load_store(emitter,opcode,load,ld); break;

        case load_type::lsb: emit_load_store(emitter,opcode,load,lsb); break;
        case load_type::lsh: emit_load_store(emitter,opcode,load,lsh); break;
        case load_type::lsw: emit_load_store(emitter,opcode,load,lsw); break;

        case load_type::lf: emit_load_store(emitter,opcode,load,lf); break;
    }
}

void emit_store(AsmEmitter& emitter, const Opcode& opcode, const Store& store)
{
    switch(store.type)
    {
        case store_type::sb: emit_load_store(emitter,opcode,store,sb); break;
        case store_type::sh: emit_load_store(emitter,opcode,store,sh); break;
        case store_type::sw: emit_load_store(emitter,opcode,store,sw); break;
        case store_type::sd: emit_load_store(emitter,opcode,store,sd); break;
        
        case store_type::sf: emit_load_store(emitter,opcode,store,sf); break;
    }
}

void emit_branch_label(AsmEmitter& emitter, const Opcode& opcode)
{
    u32 offset = 0;

    switch(opcode.branch_label.type)
    {
        case branch_type::branch: offset = branch(emitter); break;
        case branch_type::call: offset = call(emitter); break;
    }
    
    add_link(emitter,opcode,offset);
}

void emit_branch_cond_flag(AsmEmitter& emitter, const Opcode& opcode)
{
    const auto& branch = opcode.branch_cond_flag;
    u32 offset = 0;

    switch(branch.type)
    {
        case branch_cond_type::eqz: offset = je(emitter); break;
        case branch_cond_type::nez: offset = jne(emitter); break;
    }

    add_link(emitter,opcode,offset);
}

void emit_implicit(AsmEmitter& emitter, const Implicit& implicit)
{
    switch(implicit.type)
    {
        case implicit_type::syscall: syscall(emitter); break;
        case implicit_type::ret: ret(emitter); break;
        case implicit_type::cqo: cqo(emitter); break;
        case implicit_type::leave: leave(emitter); break;        
    }
}

void emit_arith_imm2(AsmEmitter& emitter, const ArithImm2& arith)
{
    const auto dst = x86_reg(arith.dst.reg);
    const auto imm = arith.imm;

    switch(arith.type)
    {
        case arith_bin_op::add_t: add_imm(emitter,dst,imm); break;
        case arith_bin_op::sub_t: sub_imm(emitter,dst,imm); break;
        case arith_bin_op::xor_t: xor_imm(emitter,dst,imm); break;
        case arith_bin_op::and_t: and_imm(emitter,dst,imm); break;
        default: panic_lowered(ARITH_BIN_NAMES[u32(arith.type)]);
    }
}

void emit_arith_imm3(AsmEmitter& emitter, const ArithImm3& arith)
{
    const auto dst = x86_reg(arith.dst.reg);
    const auto src = x86_reg(arith.src.reg);
    const auto imm = arith.imm;

    switch(arith.type)
    {
        case arith_bin_op::add_t: add_imm(emitter,dst,src,imm); break;
        default: panic_lowered(ARITH_BIN_NAMES[u32(arith.type)]);
    }
}

void emit_reg1_dst(AsmEmitter& emitter, const RegOneDst& reg)
{
    const auto dst = x86_reg(reg.dst.reg);

    switch(reg.type)
    {
        case reg1_dst_type::pop: pop(emitter,dst); break;
    }
}

void emit_directive(AsmEmitter& emitter, const Opcode& opcode, const Directive& directive)
{
    switch(directive.type)
    {
        case directive_type::pool_addr:
        {
            const auto dst = x86_reg(directive.operand[0].reg);

            lea(emitter,dst,x86_reg::rip,option::none,1,0);
            add_rip_rel_link(emitter,opcode);
            break;
        }

        case directive_type::load_func_addr:
        {
            const auto dst = x86_reg(directive.operand[0].reg);

            lea(emitter,dst,x86_reg::rip,option::none,1,0);
            add_rip_rel_link(emitter,opcode);
            break;
        }

        default: panic_lowered(DIRECTIVE_NAMES[u32(directive.type)]);
    }
}

void emit_reg2_src(AsmEmitter& emitter, const RegTwoSrc& reg2_src)
{
    const auto v1 = x86_reg(reg2_src.v1.reg);
    const auto v2 = x86_reg(reg2_src.v2.reg);

    switch(reg2_src.type)
    {
        case reg_two_src::cmp_flags_gpr: cmp_gpr(emitter,v1,v2); break;
        case reg_two_src::cmp_flags_fpr: cmp_fpr(emitter,v1,v2); break;
        case reg_two_src::test: test(emitter,v1,v2); break;
    }
}


void emit_arith_fpr2(AsmEmitter& emitter, const ArithFpr2& arith)
{
    const auto dst = x86_reg(arith.dst.reg);
    const auto src = x86_reg(arith.src.reg);

    switch(arith.type)
    {
        case fpr_arith::add_t: addf(emitter,dst,src); break;
        case fpr_arith::sub_t: subf(emitter,dst,src); break;
        case fpr_arith::mul_t: mulf(emitter,dst,src); break;
        case fpr_arith::div_t: divf(emitter,dst,src); break;
    }
}

void emit_arith_gpr2(AsmEmitter& emitter, const ArithGpr2& arith)
{
    const auto dst = x86_reg(arith.dst.reg);
    const auto src = x86_reg(arith.src.reg);

    switch(arith.type)
    {
        case arith_bin_op::add_t: add(emitter,dst,src); break;
        case arith_bin_op::sub_t: sub(emitter,dst,src); break;
        case arith_bin_op::mul_t: mul(emitter,dst,src); break;
        case arith_bin_op::xor_t: bitwise_xor(emitter,dst,src); break;
        case arith_bin_op::or_t: bitwise_or(emitter,dst,src); break;
        case arith_bin_op::and_t: bitwise_and(emitter,dst,src); break;
        
        default: panic_lowered(ARITH_BIN_NAMES[u32(arith.type)]);
    }
}

void emit_arith_gpr3(AsmEmitter& emitter, const ArithGpr3& arith)
{
    const auto dst = x86_reg(arith.dst.reg);
    const auto v1 = x86_reg(arith.v1.reg);
    const auto v2 = x86_reg(arith.v2.reg);


    switch(arith.type)
    {
        case arith_bin_op::add_t: add(emitter,dst,v1,v2); break;
        default: panic_lowered(ARITH_BIN_NAMES[u32(arith.type)]);
    }
}


void emit_shift_imm2(AsmEmitter& emitter, const ShiftImm2& shift)
{
    const auto dst = x86_reg(shift.dst.reg);
    const auto imm = shift.imm;
    
    switch(shift.type)
    {
        case shift_op::lsr: lsr_imm(emitter,dst,imm); break;
        case shift_op::asr: asr_imm(emitter,dst,imm); break;
        case shift_op::lsl: lsl_imm(emitter,dst,imm); break;
    }
}


void emit_set_from_flag_fpr(AsmEmitter& emitter, const SetFromFlagFpr& set_flag)
{
    const auto dst = x86_reg(set_flag.dst.reg);

    switch(set_flag.type)
    {
        case comparison_op::lt: setflt(emitter,dst); break;
        case comparison_op::le: setfle(emitter,dst); break;
        case comparison_op::gt: setfgt(emitter,dst); break;
        case comparison_op::ge: setfge(emitter,dst); break;

        case comparison_op::eq: setfeq(emitter,dst); break;
        case comparison_op::ne: setfne(emitter,dst); break;
    }
}

void emit_set_from_flag_gpr(AsmEmitter& emitter, const SetFromFlagGpr& set_flag)
{
    const auto dst = x86_reg(set_flag.dst.reg);

    switch(set_flag.type)
    {
        case cmp_sign_op::ult: setult(emitter,dst); break;
        case cmp_sign_op::ule: setule(emitter,dst); break;
        case cmp_sign_op::ugt: setugt(emitter,dst); break;
        case cmp_sign_op::uge: setuge(emitter,dst); break;   
        
        case cmp_sign_op::slt: setslt(emitter,dst); break;
        case cmp_sign_op::sle: setsle(emitter,dst); break;
        case cmp_sign_op::sgt: setsgt(emitter,dst); break;
        case cmp_sign_op::sge: setsge(emitter,dst); break;  

        case cmp_sign_op::eq: seteq(emitter,dst); break;
        case cmp_sign_op::ne: setne(emitter,dst); break; 
    }
}

void emit_imm2_src(AsmEmitter& emitter, const ImmTwoSrc& imm_two)
{
    const auto src = x86_reg(imm_two.src.reg);
    const auto imm = imm_two.imm;

    switch(imm_two.type)
    {
        case imm_two_src::cmp_flags_imm: cmp_imm(emitter,src,imm); break;
    }
}

void emit_branch_reg(AsmEmitter& emitter, const BranchReg& branch)
{
    const auto src = x86_reg(branch.src.reg);

    switch(branch.type)
    {
        case branch_type::branch:  branch_reg(emitter,src); break;
        case branch_type::call: call_reg(emitter,src); break;
    }
}

void emit_x86_fixed(AsmEmitter& emitter, X86Fixed x86_fixed)
{
    const auto dst = x86_reg(x86_fixed.dst.reg);
    const auto src = x86_reg(x86_fixed.src.reg);

    switch(x86_fixed.type)
    {
        case x86_fixed_type::lsl: lsl_x86(emitter,dst); break;
        case x86_fixed_type::asr: asr_x86(emitter,dst); break;
        case x86_fixed_type::lsr: lsr_x86(emitter,dst); break;
        case x86_fixed_type::udiv: udiv_x86(emitter,src); break;
        case x86_fixed_type::sdiv: sdiv_x86(emitter,src); break;

        // Same instruction different result taken from the pair
        case x86_fixed_type::umod: udiv_x86(emitter,src); break;
        case x86_fixed_type::smod: sdiv_x86(emitter,src); break;
    }
}

void emit_opcode(AsmEmitter& emitter, const Opcode& opcode)
{  
    switch(opcode.group)
    {
        case op_group::x86_fixed:
        {
            emit_x86_fixed(emitter,opcode.x86_fixed);
            break;
        }

        case op_group::set_from_flag_gpr:
        {
            emit_set_from_flag_gpr(emitter,opcode.set_from_flag_gpr);
            break;
        }

        case op_group::set_from_flag_fpr:
        {
            emit_set_from_flag_fpr(emitter,opcode.set_from_flag_fpr);
            break;
        }

        case op_group::arith_imm2:
        {
            emit_arith_imm2(emitter,opcode.arith_imm2);
            break;
        }

        case op_group::arith_imm3:
        {
            emit_arith_imm3(emitter,opcode.arith_imm3);
            break;
        }

        case op_group::arith_gpr2:
        {
            emit_arith_gpr2(emitter,opcode.arith_gpr2);
            break;
        }

        case op_group::arith_fpr2:
        {
            emit_arith_fpr2(emitter,opcode.arith_fpr2);
            break;
        }

        case op_group::arith_gpr3:
        {
            emit_arith_gpr3(emitter,opcode.arith_gpr3);
            break;
        }

        case op_group::shift_imm2:
        {
            emit_shift_imm2(emitter,opcode.shift_imm2);
            break;
        }

        case op_group::imm2_src:
        {
            emit_imm2_src(emitter,opcode.imm2_src);
            break;
        }

        case op_group::reg2_src:
        {
            emit_reg2_src(emitter,opcode.reg2_src);
            break;
        }

        case op_group::reg1_dst:
        {
            emit_reg1_dst(emitter,opcode.reg1_dst);
            break;
        }

        case op_group::implicit:
        {
            emit_implicit(emitter,opcode.implicit);
            break;
        }

        case op_group::mov_gpr_imm: 
        {
            emit_mov_imm(emitter,opcode.mov_gpr_imm); 
            break;
        }

        case op_group::reg1_src:
        {
            emit_reg1_src(emitter,opcode.reg1_src);
            break;
        }

        case op_group::unary_reg1:
        {
            emit_unary_reg1(emitter,opcode.unary_reg1);
            break;
        }

        case op_group::unary_reg2:
        {
            emit_unary_reg2(emitter,opcode.unary_reg2);
            break;
        }

        case op_group::sign_extend:
        {
            emit_sign_extend(emitter,opcode.sign_extend);
            break;
        }

        case op_group::lea:
        {
            emit_load_store(emitter,opcode,opcode.lea,lea);
            break;
        }

        case op_group::load:
        {
            emit_load(emitter,opcode,opcode.load);
            break;
        }

        case op_group::store:
        {
            emit_store(emitter,opcode,opcode.store);
            break;
        }

        case op_group::branch_label:
        {
            emit_branch_label(emitter,opcode);
            break;
        }

        case op_group::branch_cond_flag:
        {
            emit_branch_cond_flag(emitter,opcode);
            break;
        }

        case op_group::branch_reg:
        {
            emit_branch_reg(emitter,opcode.branch_reg);
            break;
        }

        case op_group::directive:
        {
            emit_directive(emitter,opcode,opcode.directive);
            break;
        }

        default:
        {
            unimplemented("[X86 EMITTER]: Invalid group: %s",OP_GROUP_NAMES[u32(opcode.group)]);
        }
    }
}

void emit_func(Interloper& itl, Function& func)
{
    const u32 func_idx = add_func(itl.asm_emitter,func);

    for(auto& block : func.emitter.program)
    {
        // store cur relative offset to finalise later
        write_cur_rel_offset(itl,block.label_slot);

        for(const OpcodeNode &node : block.list)
        {
            emit_opcode(itl.asm_emitter,node.value);
        }
    }

    end_func(itl.asm_emitter,func_idx);
}

void emit_asm(Interloper& itl)
{
    for(auto& func : itl.func_table.used)
    {
        emit_func(itl,*func);
    }
}

}
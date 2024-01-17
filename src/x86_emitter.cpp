namespace x86
{


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

void prefix_u16_reg(AsmEmitter& emitter)
{
    // 16 bit override
    push_u8(emitter,0x66);
}

b32 is_extended_reg(x86_reg reg)
{
    return reg >= x86_reg::r8 && reg <= x86_reg::r15;
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


void push_reg_base_disp(AsmEmitter& emitter,x86_reg reg, x86_reg base, s32 imm)
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
        // rbp is reserved for RIP encoding
        // when no base is used we have to use disp 8 anyways
        if(base == x86_reg::rdp)
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
        assert(false);
    }
}


void push_rex_opt(AsmEmitter& emitter, u8 rex)
{
    if(rex != 0)
    {
        push_u8(emitter,rex);
    }
}

// NOTE: this is just for the overide
// not a size ext that has to happen seperately
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

template<typename T,prefix_t prefix>
void emit_mem_op(AsmEmitter& emitter,T opcode,x86_reg reg, x86_reg base, s32 imm)
{
    // handle storage size
    emit_rm_prefix<prefix>(emitter,reg,base);
    push(emitter,opcode);

    push_reg_base_disp(emitter,reg,base,imm);
}

// don't bother specifing types on most loads
// because we just want to sign exend
void emit_load32_op8(AsmEmitter& emitter,u8 opcode,x86_reg dst, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm32>(emitter,opcode,dst,base,imm);
}

void emit_load_op8(AsmEmitter& emitter,u8 opcode,x86_reg dst, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm64>(emitter,opcode,dst,base,imm);
}
void emit_load_op16(AsmEmitter& emitter,u16 opcode,x86_reg dst, x86_reg base, s32 imm)
{
    emit_mem_op<u16,prefix_t::rm64>(emitter,opcode,dst,base,imm);
}

void emit_store8_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm8>(emitter,opcode,src,base,imm);
}

void emit_store16_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm16>(emitter,opcode,src,base,imm);
}

void emit_store32_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm32>(emitter,opcode,src,base,imm);
}

void emit_store64_op8(AsmEmitter& emitter,u8 opcode,x86_reg src, x86_reg base, s32 imm)
{
    emit_mem_op<u8,prefix_t::rm64>(emitter,opcode,src,base,imm);
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
        assert(false);
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

void cmp(AsmEmitter& emitter, x86_reg v1, x86_reg v2)
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
        // rely on 32 bit zero extenstion
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

void cmp_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    emit_arith_imm(emitter,dst,v1,7);
}

void mov(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // mov r64, r64
    emit_reg2_mr_64(emitter,0x89,dst,v1);
}

void ret(AsmEmitter& emitter)
{
    push_u8(emitter,0xC3);
}

void sb(AsmEmitter& emitter, x86_reg src, x86_reg addr,s32 imm)
{
    // mov r/m8, r8
    emit_store8_op8(emitter,0x88,src,addr,imm);
}

void lb(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // movzx r64, r/m8,
    emit_load_op16(emitter,0xb6'0f,dst,addr,imm);
}

void lh(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // movzx r64, r/m16,
    emit_load_op16(emitter,0xb7'0f,dst,addr,imm);
}

void sh(AsmEmitter& emitter, x86_reg src, x86_reg addr, s32 imm)
{
    // mov r/m16, r16
    emit_store16_op8(emitter,0x89,src,addr,imm);
}


void lsw(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // movsxd r64, r/m16
    emit_load_op8(emitter,0x63,dst,addr,imm);
}

void lsb(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // movsx r64, r/m8
    emit_load_op16(emitter,0xbe'0f,dst,addr,imm);
}

void lsh(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // movsx r64, r/m8
    emit_load_op16(emitter,0xbf'0f,dst,addr,imm);
}

void lw(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // mov r32, m/r32
    emit_load32_op8(emitter,0x8b,dst,addr,imm);
}

void sw(AsmEmitter& emitter, x86_reg src, x86_reg addr, s32 imm)
{
    // mov r/m32, r32
    emit_store8_op8(emitter,0x89,src,addr,imm);
}


void ld(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s32 imm)
{
    // mov r64, m/r64
    emit_load_op8(emitter,0x8b,dst,addr,imm);
}


void sd(AsmEmitter& emitter, x86_reg src, x86_reg addr, s32 imm)
{
    // mov m/r64, r64
    emit_store64_op8(emitter,0x89,src,addr,imm);
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

void mul_x86(AsmEmitter& emitter, x86_reg src)
{
    // mul r64
    emit_arith_fixed(emitter,src,4);
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

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1, s64 imm)
{
    // lea r64, [r64 + disp]
    const u8 opcode = 0x8d;
    push_u16(emitter,(opcode << 8) |  rex_rm64(dst,v1));

    push_reg_base_disp(emitter,dst,v1,imm);
}

void lea(AsmEmitter& emitter, x86_reg dst, x86_reg addr, s64 imm)
{
    add(emitter,dst,addr,imm);
}

void add_rip_rel_link(AsmEmitter& emitter, const Opcode& opcode)
{
    // NOTE: relies on emitter disp allwayys being u32
    // for RIP relative
    const u32 offset = emitter.buffer.size - 4;

    add_link(emitter,opcode,offset);
}

template<typename FUNC_PTR>
void emit_load_store(AsmEmitter& emitter, const Opcode& opcode, FUNC_PTR func)
{
    const auto dst = x86_reg(opcode.v[0]);
    auto addr = x86_reg(opcode.v[1]);
    s64 offset = s64(opcode.v[2]);

    const bool is_data_sect = (addr == CONST_IR || addr == GP_IR);

    if(is_data_sect)
    {
        addr = x86_reg::rip;
        offset = 0;
    }

    func(emitter,dst,addr,offset);

    if(is_data_sect)
    {
        add_rip_rel_link(emitter,opcode);
    }
}

void emit_opcode(AsmEmitter& emitter, const Opcode& opcode)
{
    UNUSED(emitter);

    const auto dst = x86_reg(opcode.v[0]);
    const auto v1 = x86_reg(opcode.v[1]);
    const auto v2 = x86_reg(opcode.v[2]);

    UNUSED(v2);

    switch(opcode.op)
    {
        case op_type::mov_imm:
        {
            mov_imm(emitter,dst,u64(v1));
            break;
        }

        case op_type::add_reg2: 
        {
            add(emitter,dst,v1);
            break;
        }

        case op_type::not_reg1: 
        {
            bitwise_not(emitter,dst);
            break;
        }

        case op_type::xor_reg2: 
        {
            bitwise_xor(emitter,dst,v1);
            break;
        }

        case op_type::xor_imm2:
        {
            xor_imm(emitter,dst,s64(v1));
            break;
        }

        case op_type::and_reg2: 
        {
            bitwise_and(emitter,dst,v1);
            break;
        }

        case op_type::and_imm2:
        {
            and_imm(emitter,dst,s64(v1));
            break;
        }

        case op_type::or_reg2: 
        {
            bitwise_or(emitter,dst,v1);
            break;
        }

        case op_type::sub_reg2: 
        {
            sub(emitter,dst,v1);
            break;
        }

        case op_type::cmp_flags:
        {
            cmp(emitter,dst,v1);
            break;
        }


        case op_type::test:
        {
            test(emitter,dst,v1);
            break;
        }

        case op_type::cmp_flags_imm:
        {
            cmp_imm(emitter,dst,v1);
            break;
        }      

        case op_type::setsgt:
        {
            setsgt(emitter,dst);
            break;
        }

        case op_type::setsge:
        {
            setsge(emitter,dst);
            break;
        }

        case op_type::setslt:
        {
            setslt(emitter,dst);
            break;
        }

        case op_type::setsle:
        {
            setsle(emitter,dst);
            break;
        }

        case op_type::setugt:
        {
            setugt(emitter,dst);
            break;
        }

        case op_type::setuge:
        {
            setuge(emitter,dst);
            break;
        }

        case op_type::setult:
        {
            setult(emitter,dst);
            break;
        }

        case op_type::setule:
        {
            setule(emitter,dst);
            break;
        }

        case op_type::seteq:
        {
            seteq(emitter,dst);
            break;
        }

        case op_type::setne:
        {
            setne(emitter,dst);
            break;
        }

        case op_type::add_imm2:
        {
            add_imm(emitter,dst,s32(v1));
            break;
        }

        case op_type::sub_imm2:
        {
            sub_imm(emitter,dst,s32(v1));
            break;
        }

        case op_type::mov_reg:
        {
            mov(emitter,dst,v1);
            break;
        }

        case op_type::ret:
        {
            ret(emitter);
            break;
        }

        case op_type::call:
        {
            const u32 offset = call(emitter);

            add_link(emitter,opcode,offset);
            break;
        }

        case op_type::pool_addr:
        {
            lea(emitter,dst,x86_reg::rip,0);
            add_rip_rel_link(emitter,opcode);
            break;
        }

        case op_type::je:
        {
            const u32 offset = je(emitter);

            add_link(emitter,opcode,offset);
            break;
        }

        case op_type::jne:
        {
            const u32 offset = jne(emitter);

            add_link(emitter,opcode,offset);
            break;
        }


        case op_type::b:
        {
            const u32 offset = branch(emitter);

            add_link(emitter,opcode,offset);
            break;
        }

        case op_type::call_reg:
        {
            call_reg(emitter,dst);
            break;
        }

        case op_type::b_reg:
        {
            branch_reg(emitter,dst);
            break;
        }

        case op_type::push:
        {
            push(emitter,dst);
            break;
        }

        case op_type::pop:
        {
            pop(emitter,dst);
            break;
        }

        case op_type::syscall:
        {
            syscall(emitter);
            break;
        }

        case op_type::cqo:
        {
            cqo(emitter);
            break;
        }

        case op_type::sdiv_x86:
        {
            sdiv_x86(emitter,v1);
            break;
        }

        case op_type::udiv_x86:
        {
            udiv_x86(emitter,v1);
            break;
        }

        case op_type::umod_x86:
        {
            // same as div just different result reg used
            udiv_x86(emitter,v1);
            break;
        }

        case op_type::smod_x86:
        {
            // same as div just different result reg used
            sdiv_x86(emitter,v1);
            break;
        }

        case op_type::mul_x86:
        {
            mul_x86(emitter,v1);
            break;
        }

        case op_type::lsl_x86:
        {
            lsl_x86(emitter,dst);
            break;
        }

        case op_type::lsl_imm2:
        {
            lsl_imm(emitter,dst,u32(v1));
            break;
        }

        case op_type::lsr_x86:
        {
            lsr_x86(emitter,dst);
            break;
        }

        case op_type::asr_x86:
        {
            asr_x86(emitter,dst);
            break;
        }

        case op_type::lb:
        {
            emit_load_store(emitter,opcode,lb);
            break;
        }

        case op_type::sb:
        {
            emit_load_store(emitter,opcode,sb);
            break;
        }

        case op_type::lh:
        {
            emit_load_store(emitter,opcode,lh);
            break;        
        }

        case op_type::sh:
        {
            emit_load_store(emitter,opcode,sh);
            break;        
        }

        case op_type::lw:
        {
            emit_load_store(emitter,opcode,lw);
            break;
        }

        case op_type::sw:
        {
            emit_load_store(emitter,opcode,sw);
            break;
        }

        case op_type::lsb:
        {
            emit_load_store(emitter,opcode,lsb);
            break;
        }

        case op_type::lsh:
        {
            emit_load_store(emitter,opcode,lsh);
            break;
        }

        case op_type::lsw:
        {
            emit_load_store(emitter,opcode,lsw);
            break;
        }

        case op_type::ld:
        {
            emit_load_store(emitter,opcode,ld);
            break;
        }

        case op_type::sd:
        {
            emit_load_store(emitter,opcode,sd);;
            break;
        }

        case op_type::lea:
        {
            emit_load_store(emitter,opcode,lea);
            break;
        }

        case op_type::load_func_addr:
        {
            lea(emitter,dst,x86_reg::rip,0);
            add_rip_rel_link(emitter,opcode);
            break;
        }

        case op_type::sxb:
        {
            sxb(emitter,dst,v1);
            break;
        }

        case op_type::sxh:
        {
            sxh(emitter,dst,v1);
            break;
        }

        case op_type::sxw:
        {
            sxw(emitter,dst,v1);
            break;
        }


        default:
        {
            auto& info = info_from_op(opcode);
            printf("[EMIT X86]: unknown opcode: %s\n",info.fmt_string.buf);
            assert(false);
            break;
        } 
    }
}

void emit_func(Interloper& itl, Function& func)
{
    const u32 func_idx = add_func(itl.asm_emitter,func);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        ListNode* node = block.list.start;

        // store cur relative offset to finalise later
        write_cur_rel_offset(itl,block.label_slot);

        while(node)
        {
            emit_opcode(itl.asm_emitter,node->opcode);
            node = node->next;
        }
    }

    end_func(itl.asm_emitter,func_idx);
}

void emit_asm(Interloper& itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        emit_func(itl,func);
    }
}

}
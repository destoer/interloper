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

// NOTE: the order depends on format
// r16/32/64 	r/m16/32/64 (should be v1 = src, v2 = dst)
// r/m16/32/64 	r16/32/64 (should be v1 = dst, v2 = src)
u8 mod_reg(x86_reg v1, x86_reg v2)
{
    return (0b11 << 6) | (u32(v2) << 3) | (u32(v1) << 0);
}

u8 mod_opcode_reg(x86_reg v1, u8 ext)
{
    return (0b11 << 6) | (ext << 3) | (u32(v1) << 0);
}

// NOTE: the order of operands depends on format of opcode
// r16/32/64 	r/m16/32/64  -> // reg, [base + disp32]
// r/m16/32/64 	r16/32/64  -> // [base + disp32], reg

u16 mod_base_disp_32(x86_reg dst,x86_reg src)
{
    // reg, [base + disp32]
    const u8 mod = (0b10 << 6) | (dst << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (u32(src) << 0);

    return ((sib << 8) | (mod << 0));
}

u16 mod_base_disp_8(x86_reg dst,x86_reg src)
{
    // reg, [base + disp8]
    const u8 mod = (0b01 << 6) | (dst << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (u32(src) << 0);

    return ((sib << 8) | (mod << 0));
}


u16 mod_base(x86_reg dst,x86_reg src)
{
    // reg, [base]
    const u8 mod = (0b00 << 6) | (dst << 3) | (0b100 << 0);
    const u8 sib = (0b00 << 6) | (0b100 <<  3) | (u32(src) << 0);

    return ((sib << 8) | (mod << 0));
}


void push_base_disp(AsmEmitter& emitter,x86_reg dst, x86_reg src, s32 imm)
{
    // handle special regs
    if(src == x86_reg::rip)
    {
        // reg [rip + disp32]
        const u8 mod = (0b00 << 6) | (dst << 3) | (0b101 << 0);
        push_u8(emitter,mod);
        push_u32(emitter,imm);
    }

    else if(imm == 0)
    {
        // rbp is reserved for RIP encoding
        // when no base is used we have to use disp 8 anyways
        if(src == x86_reg::rdp)
        {
            push_u16(emitter,mod_base_disp_8(dst,src));
            push_u8(emitter,0);     
        }

        push_u16(emitter,mod_base(dst,src));
    }

    else if(fit_into_s8(imm))
    {
        push_u16(emitter,mod_base_disp_8(dst,src));
        push_u8(emitter,s8(imm));
    }

    // use 32 bit
    else if(fit_into_s32(imm))
    {
        push_u16(emitter,mod_base_disp_32(dst,src));
        push_u32(emitter,imm);
    }

    // cannot fit
    else
    {
        assert(false);
    }
}

void emit_reg2_rm_extended_32(AsmEmitter& emitter, const u16 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r32, r2
    push_u16(emitter,(opcode));
    push_u8(emitter,mod_reg(v1,dst));
}

void emit_reg2_rm_extended(AsmEmitter& emitter, const u16 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r64, r2
    // override to r64
    push_u8(emitter,REX_W);
    emit_reg2_rm_extended_32(emitter,opcode,dst,v1);
}

void emit_reg2_rm_32(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(mod_reg(v1,dst) << 8) | (opcode << 0));
}


void emit_reg2_rm(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | REX_W);
    push_u8(emitter,mod_reg(v1,dst));
}

void emit_reg2_mr_32(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(mod_reg(dst,v1) << 8) | (opcode << 0));
}

void emit_reg2_mr(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | REX_W);
    push_u8(emitter,mod_reg(dst,v1));
}

void prefix_u8_data_reg(AsmEmitter& emitter, x86_reg dst)
{
    // index or data reg, used must prefix with rex
    if(dst > x86_reg::rdx)
    {
        push_u8(emitter,REX);
    }
}

void prefix_u16_reg(AsmEmitter& emitter)
{
    // 16 bit override
    push_u8(emitter,0x66);
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // add r64, r64
    emit_reg2_rm(emitter,0x3,dst,v1);
}

void bitwise_and(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // and r64, r64
    emit_reg2_rm(emitter,0x23,dst,v1);
}

void bitwise_or(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // or r64, r64
    emit_reg2_rm(emitter,0xB,dst,v1);
}

void bitwise_xor(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // xor r64, r64
    emit_reg2_rm(emitter,0x33,dst,v1);
}

void bitwise_not(AsmEmitter& emitter, x86_reg dst)
{
    // not r64
    const u8 opcode = 0xf7;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(dst,2));
}

void sub(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // sub r64, r64
    emit_reg2_rm(emitter,0x2B,dst,v1);
}

void cmp(AsmEmitter& emitter, x86_reg v1, x86_reg v2)
{
    // cmp r64, r64
    emit_reg2_rm(emitter,0x3B,v1,v2);
}

void test(AsmEmitter& emitter, x86_reg v1, x86_reg v2)
{
    // TODO: do we even care about 64 bit here ever?
    // test r64, r64
    emit_reg2_rm(emitter,0x85,v1,v2);
}

void zero_reg(AsmEmitter& emitter, x86_reg dst)
{
    // xor reg, reg 
    // NOTE: we use 32 version as it sign extends
    // and is shorter
    push_u8(emitter,0x31);
    push_u8(emitter,mod_reg(dst,dst));
}

void mov_imm(AsmEmitter& emitter, x86_reg reg, u64 imm)
{
    //NOTE: this move does not sign extend
    const u8 opcode = 0xb8 + u32(reg);

    // requires 64 bit mov
    if(imm > 0xffff'ffff)
    {
        // mov +r64, imm64
        push_u16(emitter,(opcode << 8) | REX_W);

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
        push_u8(emitter,opcode);
        push_u32(emitter,u32(imm));
    }
}


void arith_imm(AsmEmitter& emitter, x86_reg dst, s64 v1, u32 opcode_ext)
{
    // add r64, imm8
    if(fit_into_s8(v1))
    {
        const u8 opcode = 0x83;
        push_u16(emitter,(opcode << 8) | REX_W);

        // opcode extenstion required
        push_u8(emitter,mod_opcode_reg(dst,opcode_ext));

        push_u8(emitter,s8(v1));
    }

    // add r64, imm32
    else if(fit_into_s32(v1))
    {
        const u8 opcode = 0x81;
        push_u16(emitter,(opcode << 8) | REX_W);

        // opcode extenstion required
        push_u8(emitter,mod_opcode_reg(dst,opcode_ext));

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

void add_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    arith_imm(emitter,dst,v1,0);
}

void sub_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    arith_imm(emitter,dst,v1,5);
}

void xor_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    arith_imm(emitter,dst,v1,6);
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
        emit_reg2_rm_extended(emitter,0xb6'0f,dst,dst);
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
        // and r32, u32
        const u32 opcode = 0x81;
        push_u16(emitter,(mod_opcode_reg(dst,4) << 8) | (opcode << 0));

        push_u32(emitter,v1);   
    }

    else
    {
        arith_imm(emitter,dst,v1,4);
    }
}


void cmp_imm(AsmEmitter& emitter, x86_reg dst, s64 v1)
{
    arith_imm(emitter,dst,v1,7);
}

void mov(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // mov r64, r64
    emit_reg2_mr(emitter,0x89,dst,v1);
}

void ret(AsmEmitter& emitter)
{
    push_u8(emitter,0xC3);
}

void sb(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    prefix_u8_data_reg(emitter,src);

    // mov r/m8, r8
    const u8 opcode = 0x88;
    push_u8(emitter,opcode);

    push_base_disp(emitter,src,v1,imm);
}

void lb(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // movzx r64, r/m8,
    
    push_u8(emitter,REX_W);
    push_u16(emitter,0xb6'0f);;

    push_base_disp(emitter,src,v1,imm);
}

void lh(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // movzx r64, r/m16,
    
    push_u8(emitter,REX_W);
    push_u16(emitter,0xb7'0f);;

    push_base_disp(emitter,src,v1,imm);
}

void sh(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    prefix_u16_reg(emitter);

    // mov r/m16, r16
    const u8 opcode = 0x89;
    push_u8(emitter,opcode);

    push_base_disp(emitter,src,v1,imm);
}


void lsw(AsmEmitter& emitter, x86_reg dst, x86_reg v1, s32 imm)
{
    // movsxd r64, r/m16
    const u8 opcode = 0x63;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_base_disp(emitter,dst,v1,imm);
}


void lw(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // mov r32, m/r32
    const u8 opcode = 0x8b;
    push_u8(emitter,opcode);

    push_base_disp(emitter,src,v1,imm);
}

void sw(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // mov r/m32, r32
    const u8 opcode = 0x89;
    push_u8(emitter,opcode);

    push_base_disp(emitter,src,v1,imm);
}



void ld(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // mov r64, m/r64
    const u8 opcode = 0x8b;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_base_disp(emitter,src,v1,imm);
}


void sd(AsmEmitter& emitter, x86_reg src, x86_reg v1, s32 imm)
{
    // mov m/r64, r64
    const u8 opcode = 0x89;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_base_disp(emitter,src,v1,imm);
}

void sxb(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsx r64, r8
    emit_reg2_rm_extended(emitter,0xbe'0f,dst,v1);
}


void sxh(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsx r64, r16
    emit_reg2_rm_extended(emitter,0xbf'0f,dst,v1);
}

void sxw(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // movsxd r64, r16
    emit_reg2_rm(emitter,0x63,dst,v1);
}

u32 emit_cond_jump(AsmEmitter& emitter, u16 opcode)
{
    push_u16(emitter,opcode);

    const u32 offset = emitter.buffer.size;

    // dummy value
    push_u32(emitter,0);

    return offset;
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
    push_u8(emitter,0xe9);

    const u32 offset = emitter.buffer.size;

    // dummy value
    push_u32(emitter,0);

    return offset;
}

u32 call(AsmEmitter& emitter)
{
    // TODO: we need a way to encode far calls
    // in the upper IR eventually

    // call rel32
    push_u8(emitter,0xe8);

    const u32 offset = emitter.buffer.size;
    
    push_u32(emitter,0);

    return offset;
}

void call_reg(AsmEmitter& emitter, x86_reg src)
{
    // call r64
    const u8 opcode = 0xff;
    push_u8(emitter,opcode);

    push_u8(emitter,mod_opcode_reg(src,2));
}


void branch_reg(AsmEmitter& emitter, x86_reg src)
{
    // jmp r64
    const u8 opcode = 0xff;
    push_u8(emitter,opcode);

    push_u8(emitter,mod_opcode_reg(src,4));
}


void emit_set_flag(AsmEmitter& emitter, x86_reg dst, u8 op)
{   
    // set<x> dst
    prefix_u8_data_reg(emitter,dst);

    push_u16(emitter,(op << 8) | (0xf << 0));
    push_u8(emitter,mod_opcode_reg(dst,0));

    // now zero extend the 8 bit quantity
    // as set<x> does not 
    zxb(emitter,dst);
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
    push_u8(emitter,0x50 + u32(src));
}

void pop(AsmEmitter& emitter, x86_reg src)
{
    // pop +r64
    push_u8(emitter,0x58 + u32(src));
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

void div_x86(AsmEmitter& emitter, x86_reg src)
{
    // div r64
    const u8 opcode = 0xf7;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(src,6));
}

void mul_x86(AsmEmitter& emitter, x86_reg src)
{
    // mul r64
    const u8 opcode = 0xf7;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(src,4));
}


void lsl_x86(AsmEmitter& emitter, x86_reg src)
{
    // lsl r64, cl
    const u8 opcode = 0xd3;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(src,4));
}

void lsr_x86(AsmEmitter& emitter, x86_reg src)
{
    // lsr r64, cl
    const u8 opcode = 0xd3;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(src,5));
}

void asr_x86(AsmEmitter& emitter, x86_reg src)
{
    // asr r64, c;
    const u8 opcode = 0xd3;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_u8(emitter,mod_opcode_reg(src,7));
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1, s64 imm)
{
    // lea r64, [r64 + disp]
    const u8 opcode = 0x8d;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_base_disp(emitter,dst,v1,imm);
}

void lea(AsmEmitter& emitter, x86_reg dst, x86_reg v1, s64 imm)
{
    add(emitter,dst,v1,imm);
}

void add_lea_rel_link(AsmEmitter& emitter, const Opcode& opcode)
{
    // NOTE: relies on emitter disp allwayys being u32
    // for RIP relative
    const u32 offset = emitter.buffer.size - 4;

    add_link(emitter,opcode,offset);
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

        case op_type::div_x86:
        {
            div_x86(emitter,v1);
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
            lb(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::sb:
        {
            sb(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::lh:
        {
            lh(emitter,dst,v1,s64(v2));
            break;        
        }

        case op_type::sh:
        {
            sh(emitter,dst,v1,s64(v2));
            break;        
        }

        case op_type::lw:
        {
            lw(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::sw:
        {
            sw(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::lsw:
        {
            lsw(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::ld:
        {
            ld(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::sd:
        {
            sd(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::lea:
        {
            lea(emitter,dst,v1,s64(v2));
            break;
        }

        case op_type::load_func_addr:
        {
            lea(emitter,dst,x86_reg::rip,0);
            add_lea_rel_link(emitter,opcode);
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
    for(u32 idx = 0; idx != count(itl.used_func); idx++)
    {
        auto& func = *lookup(itl.function_table,itl.used_func[idx]);
        emit_func(itl,func);
    }
}

}
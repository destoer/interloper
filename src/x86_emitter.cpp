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
    if(imm == 0)
    {
        push_u16(emitter,mod_base(dst,src));
    }

    else if(fit_into_s8(imm))
    {
        push_u16(emitter,mod_base_disp_8(dst,src));
        push_u8(emitter,s8(imm));
    }

    // use 32 bit
    else
    {
        push_u16(emitter,mod_base_disp_32(dst,src));
        push_u32(emitter,imm);
    }
}

void emit_reg2_rm(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | REX_W);
    push_u8(emitter,mod_reg(v1,dst));
}

void emit_reg2_mr(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | REX_W);
    push_u8(emitter,mod_reg(dst,v1));
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // add r64, r64
    emit_reg2_rm(emitter,0x3,dst,v1);
}

void sub(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // sub r64, r64
    emit_reg2_rm(emitter,0x2B,dst,v1);
}

void bitwise_xor(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // add r64, r64
    emit_reg2_rm(emitter,0x31,dst,v1);
}

void mov_imm(AsmEmitter& emitter, x86_reg reg, u64 imm)
{
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
        // xor reg, reg 
        // NOTE: we use 32 version as it sign extends
        // and is shorter
        push_u8(emitter,0x31);
        push_u8(emitter,mod_reg(reg,reg));
    }

    // 32 bit move
    else
    {
        push_u8(emitter,opcode);
        push_u32(emitter,u32(imm));
    }
}


void arith_imm(AsmEmitter& emitter, x86_reg dst, s32 v1, u32 opcode_ext)
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
    else
    {
        const u8 opcode = 0x81;
        push_u16(emitter,(opcode << 8) | REX_W);

        // opcode extenstion required
        push_u8(emitter,mod_opcode_reg(dst,opcode));

        push_u32(emitter,v1);
    }    
}

void add_imm(AsmEmitter& emitter, x86_reg dst, s32 v1)
{
    arith_imm(emitter,dst,v1,0);
}

void sub_imm(AsmEmitter& emitter, x86_reg dst, s32 v1)
{
    arith_imm(emitter,dst,v1,5);
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

void lsw(AsmEmitter& emitter, x86_reg dst, x86_reg v1, s32 imm)
{
    // movsxd r64, r/m16
    const u8 opcode = 0x63;
    push_u16(emitter,(opcode << 8) | REX_W);

    push_base_disp(emitter,dst,v1,imm);
}

// TODO: this wont leave any useful linking information yet
u32 call(AsmEmitter& emitter,LabelSlot addr)
{
    UNUSED(addr);

    // TODO: we need a way to encode far calls
    // in the upper IR eventually

    // call rel32
    push_u8(emitter,0xe8);

    const u32 offset = emitter.buffer.size;
    
    push_u32(emitter,0);

    return offset;
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


/*
void sub(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    // sub dst
}
*/


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

        case op_type::sub_reg2: 
        {
            sub(emitter,dst,v1);
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
            LabelSlot label = label_from_idx(u32(dst));
            const u32 offset = call(emitter,label);

            add_link(emitter,opcode,offset);
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
            div_x86(emitter,dst);
            break;
        }

        case op_type::mul_x86:
        {
            mul_x86(emitter,dst);
            break;
        }


        case op_type::lsw:
        {
            lsw(emitter,dst,v1,s32(v2));
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
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            emit_func(itl,func);
        }
    }
}

}
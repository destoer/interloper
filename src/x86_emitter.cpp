namespace x86
{

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
};


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


u8 mod_reg(x86_reg v1, x86_reg v2)
{
    return (0b11 << 6) | (u32(v1) << 3) | (v2 << 0);
}

void mov_imm(AsmEmitter& emitter, x86_reg reg, u64 imm)
{
    const u8 opcode = 0xb8 + u32(reg);

    // mov r64, imm64
    push_u16(emitter,(opcode << 8) | REX_W);

    // push the immediate
    push_u64(emitter,imm);
}

void emit_reg2(AsmEmitter& emitter, const u8 opcode, x86_reg dst, x86_reg v1)
{
    // opcode r1, r2
    push_u16(emitter,(opcode << 8) | REX_W);
    push_u8(emitter,mod_reg(dst,v1));
}

void add(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    emit_reg2(emitter,0x3,dst,v1);
}

void mov(AsmEmitter& emitter, x86_reg dst, x86_reg v1)
{
    emit_reg2(emitter,0x89,dst,v1);
}

void ret(AsmEmitter& emitter)
{
    push_u8(emitter,0xC3);
}

// TODO: this wont leave any useful linking information yet
void call(AsmEmitter& emitter,LabelSlot addr)
{
    UNUSED(addr);

    // TODO: we need a way to encode far calls
    // in the upper IR eventually
    push_u8(emitter,0xe8);
    push_u32(emitter,0);
}

void push(AsmEmitter& emitter, x86_reg src)
{
    // push r64
    push_u8(emitter,0x50 + u32(src));
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
            call(emitter,label);
            break;
        }

        case op_type::push:
        {
            push(emitter,dst);
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

void end_func(AsmEmitter& emitter, Function& func)
{
    auto& asm_func = *lookup(emitter.func,func.name);
    asm_func.size = emitter.buffer.size - asm_func.offset; 
}

void emit_func(Interloper& itl, Function& func)
{
    add_func(itl.asm_emitter,func);

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

    end_func(itl.asm_emitter,func);
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
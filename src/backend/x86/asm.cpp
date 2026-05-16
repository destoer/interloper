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



void push(AsmEmitter& emitter, x86_reg src)
{
    // push +r64
    emit_plus_reg_opcode(emitter,src,0x50);
}

void emit_x86_reg1_src(AsmEmitter& emitter, const RegOneSrc& reg1_src)
{
    const auto src = x86_reg(reg1_src.src.reg);

    switch(reg1_src.type)
    {
        case reg1_src_type::push: push(emitter,src); break;
    }
}

void emit_x86_opcode(AsmEmitter& emitter, const Opcode& opcode)
{  
    UNUSED(emitter);

    switch(opcode.group)
    {
        case op_group::reg1_src:
        {
            emit_x86_reg1_src(emitter,opcode.reg1_src);
            break;
        }

        default:
        {
            unimplemented("[X86 EMITTER]: Invalid group :%d",opcode.group);
        }
    }
}

void emit_x86_func(Interloper& itl, Function& func)
{
    const u32 func_idx = add_func(itl.asm_emitter,func);

    for(auto& block : func.emitter.program)
    {
        // store cur relative offset to finalise later
        write_cur_rel_offset(itl,block.label_slot);

        for(const OpcodeNode &node : block.list)
        {
            emit_x86_opcode(itl.asm_emitter,node.value);
        }
    }

    end_func(itl.asm_emitter,func_idx);
}

void emit_x86_asm(Interloper& itl)
{
    for(auto& func : itl.func_table.used)
    {
        emit_x86_func(itl,*func);
    }
}
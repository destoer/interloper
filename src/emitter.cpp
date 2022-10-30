
List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).end;    
}



Opcode store_ptr(u32 dst_slot, u32 addr_slot, u32 size, u32 offset)
{
    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
    return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);
}

// this function only supports up to 32 bit reads atm
static_assert(GPR_SIZE == sizeof(u32));

Opcode load_ptr(u32 dst_slot, u32 addr_slot,u32 offset, u32 size, b32 is_signed)
{
    if(is_signed)
    {
        // word is register size (we dont need to extend it)
        static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);       
    }

    // "plain data"
    // just move by size
    else
    {
        static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);
    }
}

u32 gpr_count(u32 size)
{
    return size / GPR_SIZE;
}

void alloc_slot(Function& func, const Reg& reg)
{
    emit(func,op_type::alloc_slot,reg.slot,reg.size,reg.count);
}

void free_slot(Function& func, const Reg& reg)
{
    emit(func,op_type::free_slot,reg.slot,reg.size,reg.count);
}


void emit(Function& func,const Opcode& opcode)
{
    auto &list = get_cur_list(func.emitter);
    append(list,opcode);
}



// get back a longer lived tmp
// stored internally as a symbol
u32 new_tmp(Function& func, u32 size)
{
    const u32 slot = count(func.registers);

    const auto reg = make_reg(reg_kind::tmp,size,slot,false);
    push_var(func.registers,reg);

    return slot;
}

u32 new_tmp_ptr(Function &func)
{
    return new_tmp(func,GPR_SIZE);
}


// emit an opcode, and give back a new dst as a tmp
u32 emit_res(Function& func, op_type op, u32 v2, u32 v3)
{
    const u32 tmp = new_tmp(func,GPR_SIZE);
    emit(func,op,tmp,v2,v3);

    return tmp;
}



static constexpr u32 REG_FREE = SPECIAL_PURPOSE_REG_START - 1;
static constexpr u32 REG_TMP_START = 0x00000000;

b32 is_sym(u32 s)
{
    return s >= SYMBOL_START;
}

u32 tmp(u32 ir_reg)
{
    return ir_reg + REG_TMP_START;
}

// dont correct special regs
b32 is_reg(u32 r)
{
    return r < MACHINE_REG_SIZE;
}

b32 is_special_reg(u32 r)
{
    return r >= SPECIAL_PURPOSE_REG_START && r <= SPECIAL_PURPOSE_REG_START + SPECIAL_REG_SIZE;
}

b32 is_tmp(u32 r)
{
    return r < SYMBOL_START;
}


u32 slot_to_idx(u32 slot)
{
    return is_sym(slot)? sym_to_idx(slot) : slot;
}

Reg make_reg(reg_kind kind,u32 size, u32 slot, b32 is_signed)
{
    Reg reg;
    reg.kind = kind;

    if(size > GPR_SIZE)
    {
        reg.size = GPR_SIZE;
        reg.count = gpr_count(size);
    }

    else
    {
        reg.count = 1; 
        reg.size = size;
    }


    if(is_signed)
    {
        reg.flags |= SIGNED_FLAG;
    }

    reg.slot = slot;

    return reg;
}

void print(const Reg& reg)
{
    printf("offset: %x\n",reg.offset);
    printf("location: %x\n\n",reg.location);    
    printf("slot: %x\n",reg.slot);
}

void emit_block(Function& func, u32 block, op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto &list = func.emitter.program[block].list;
    append(list,opcode);    
}


u32 cur_block(Function& func)
{
    return count(func.emitter.program) - 1;
}


void emit(Function& func,op_type op, u32 v1, u32 v2, u32 v3)
{
    emit_block(func,cur_block(func),op,v1,v2,v3);
}

u32 addrof(Function& func,const Reg& reg)
{
    const u32 dst = emit_res(func,op_type::addrof,reg.slot);

    return dst;
}

u32 load_arr_data(Function& func,const Reg& reg)
{
    const u32 dst = emit_res(func,op_type::load_arr_data,reg.slot);

    return dst;
}

u32 mov_imm(Function& func,u32 v)
{
    const u32 dst = emit_res(func,op_type::mov_imm,v);

    return dst;
}
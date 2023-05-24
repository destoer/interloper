
List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).end;    
}



Opcode store_ptr(SymSlot dst_slot, SymSlot addr_slot, u32 size, u32 offset)
{
    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
    return Opcode(instr[size >> 1],dst_slot.handle,addr_slot.handle,offset);
}

// this function only supports up to 32 bit reads atm
static_assert(GPR_SIZE == sizeof(u32));

Opcode load_ptr(SymSlot dst_slot, SymSlot addr_slot,u32 offset, u32 size, b32 is_signed)
{
    if(is_signed)
    {
        // word is register size (we dont need to extend it)
        static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot.handle,addr_slot.handle,offset);       
    }

    // "plain data"
    // just move by size
    else
    {
        static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot.handle,addr_slot.handle,offset);
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
SymSlot new_tmp(Function& func, u32 size)
{
    const u32 slot = count(func.registers);

    const auto reg = make_reg(reg_kind::tmp,size,slot,false);
    push_var(func.registers,reg);

    return sym_from_idx(slot);
}

SymSlot new_tmp_ptr(Function &func)
{
    return new_tmp(func,GPR_SIZE);
}



static constexpr u32 REG_FREE = SPECIAL_PURPOSE_REG_START - 1;
static constexpr u32 TMP_END = REG_FREE - 1;
static constexpr u32 REG_TMP_START = 0x00000000;

b32 is_sym(SymSlot s)
{
    return s.handle >= SYMBOL_START;
}

u32 tmp(u32 ir_reg)
{
    return ir_reg + REG_TMP_START;
}

// dont correct special regs
b32 is_reg(SymSlot r)
{
    return r.handle < MACHINE_REG_SIZE;
}

b32 is_special_reg(SymSlot r)
{
    return r.handle >= SPECIAL_PURPOSE_REG_START && r.handle <= SPECIAL_PURPOSE_REG_START + SPECIAL_REG_SIZE;
}

b32 is_tmp(SymSlot s)
{
    return s.handle < TMP_END;
}


u32 slot_to_idx(SymSlot slot)
{
    return is_sym(slot)? sym_to_idx(slot) : slot.handle;
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

    reg.slot = {slot};

    return reg;
}

void print(const Reg& reg)
{
    printf("offset: %x\n",reg.offset);
    printf("location: %x\n\n",reg.location);    
    printf("slot: %x\n",reg.slot.handle);
}

BlockSlot block_from_idx(u32 v)
{
    BlockSlot slot;

    slot.handle = v;

    return slot;
}

BlockSlot cur_block(Function& func)
{
    return block_from_idx(count(func.emitter.program) - 1);
}

Block& block_from_slot(Function& func, BlockSlot slot)
{
    return func.emitter.program[slot.handle];
}

// Emitter overloads
void emit_block_internal(Function& func, BlockSlot block_slot, op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto& block = block_from_slot(func,block_slot);
    auto &list = block.list;
    append(list,opcode);    
}

void emit_internal(Function& func,op_type op, u32 v1, u32 v2, u32 v3)
{
    emit_block_internal(func,cur_block(func),op,v1,v2,v3);
}


void emit(Function& func,op_type op, SymSlot v1, SymSlot v2, SymSlot v3)
{
    emit_internal(func,op,v1.handle,v2.handle,v3.handle);
}


void emit(Function& func,op_type op, u32 imm)
{
    emit_internal(func,op,imm,0,0);
}

void emit(Function& func,op_type op, SymSlot v1, SymSlot v2, u32 imm)
{
    emit_internal(func,op,v1.handle,v2.handle,imm);
}

void emit(Function& func,op_type op, SymSlot v1, u32 imm)
{
    emit_internal(func,op,v1.handle,imm,0);
}

void emit(Function& func,op_type op, SymSlot v1, u32 v2, u32 v3)
{
    emit_internal(func,op,v1.handle,v2,v3);
}

void emit_block(Function &func,BlockSlot block,op_type op, SymSlot v1, SymSlot v2, SymSlot v3)
{
    emit_block_internal(func,block,op,v1.handle,v2.handle,v3.handle);
}

// emit an opcode, and give back a new dst as a tmp
SymSlot emit_res(Function& func, op_type op, SymSlot v2, u32 v3)
{
    const SymSlot tmp = new_tmp(func,GPR_SIZE);
    emit(func,op,tmp,v2,v3);

    return tmp;
}


SymSlot emit_res(Function& func, op_type op, SymSlot v2, SymSlot v3)
{
    const SymSlot tmp = new_tmp(func,GPR_SIZE);
    emit(func,op,tmp,v2,v3);

    return tmp;
}

SymSlot emit_res(Function& func, op_type op, u32 v2)
{
    const SymSlot tmp = new_tmp(func,GPR_SIZE);
    emit(func,op,tmp,v2);

    return tmp;
}

SymSlot addrof(Function& func,const Reg& reg)
{
    return emit_res(func,op_type::addrof,reg.slot);
}

SymSlot load_arr_data(Function& func,const Reg& reg)
{
    return emit_res(func,op_type::load_arr_data,reg.slot);
}

SymSlot mov_imm(Function& func,u32 v)
{
    return emit_res(func,op_type::mov_imm,v);
}


ListNode* emit_directive_internal(Interloper& itl,Function& func, op_type type, Operand v1 = BLANK_OPERAND, Operand v2 = BLANK_OPERAND, Operand v3 = BLANK_OPERAND)
{
    UNUSED(itl);

    const auto info = opcode_three_info(type);

    // Rewrite these to ignore the rewrites
    if(info.args >= 1 && info.type[0] == arg_type::directive && v1.type == operand_type::reg)
    {
        v1.type = operand_type::directive_reg;
    }
    
    if(info.args >= 2 && info.type[1] == arg_type::directive && v2.type == operand_type::reg)
    {
        v2.type = operand_type::directive_reg;
    }

    if(info.args >= 3 && info.type[2] == arg_type::directive && v3.type == operand_type::reg)
    {
        v3.type = operand_type::directive_reg;
    }

    return emit_block_internal(func,cur_block(func),type,v1,v2,v3);
}

void addrof(Interloper& itl,Function& func, RegSlot dst, RegSlot src, u32 offset = 0)
{
    // mark reg as aliased
    auto& reg = reg_from_slot(itl,func,src);
    reg.flags |= ALIASED;

    emit_directive_internal(itl,func,op_type::addrof,make_reg_operand(dst),make_reg_operand(src),make_imm_operand(offset));
}

RegSlot addrof_res(Interloper& itl, Function& func, RegSlot src, u32 offset = 0)
{
    const auto tmp = new_tmp_ptr(func);
    addrof(itl,func,tmp,src,offset);

    return tmp;
}

void clean_args(Interloper& itl, Function& func, u32 v)
{
    emit_imm0<op_type::clean_args>(itl,func,v);
}

void spill_all(Interloper& itl, Function& func)
{
    emit_implicit<op_type::spill_all>(itl,func);
}

void spill_func_bounds(Interloper& itl, Function& func)
{
    emit_implicit<op_type::spill_func_bounds>(itl,func);
}


void emit_exit_block(Interloper& itl, Function& func)
{
    emit_implicit<op_type::exit_block>(itl,func);
}

void pool_addr(Interloper& itl, Function& func, RegSlot dst_slot, PoolSlot pool_slot, u32 offset)
{
    emit_directive_internal(itl,func,op_type::addrof,make_reg_operand(dst_slot),make_raw_operand(pool_slot.handle),make_imm_operand(offset));
}

RegSlot pool_addr_res(Interloper& itl, Function& func, PoolSlot pool_slot, u32 offset)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    pool_addr(itl,func,tmp,pool_slot,offset);

    return tmp;
}

void free_slot(Interloper& itl,Function& func, RegSlot slot)
{
    emit_directive_internal(itl,func,op_type::free_slot,make_reg_operand(slot));
}

void free_fixed_array(Interloper& itl,Function& func,RegSlot src,u32 size,u32 count)
{
    emit_directive_internal(itl,func,op_type::free_fixed_array,make_reg_operand(src),make_raw_operand(size),make_raw_operand(count));
}

ListNode* alloc_slot(Interloper& itl,Function& func, const RegSlot slot, b32 force_alloc)
{
    return emit_directive_internal(itl,func,op_type::alloc_slot,make_reg_operand(slot),make_raw_operand(force_alloc));
}

ListNode* alloc_stack(Interloper& itl, Function& func, u32 size)
{
    return emit_directive_internal(itl,func,op_type::alloc_stack,make_imm_operand(size));    
}


void reload_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(!stored_in_mem(reg))
    {
        emit_directive_internal(itl,func,op_type::reload_slot,make_reg_operand(reg.slot));
    }
}

void spill_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(!stored_in_mem(reg))
    {
        emit_directive_internal(itl,func,op_type::spill_slot,make_reg_operand(reg.slot));
    }
}

void load_func_addr(Interloper& itl, Function& func, RegSlot dst, LabelSlot label)
{
    emit_directive_internal(itl,func,op_type::load_func_addr,make_reg_operand(dst),make_label_operand(label));
}

void load_struct_internal(Interloper& itl, Function& func, op_type type,RegSlot dst, AddrSlot addr_slot)
{
    emit_directive_internal(itl,func,type,make_reg_operand(dst),make_reg_operand(addr_slot.slot),make_imm_operand(addr_slot.offset));
}

void store_struct_internal(Interloper& itl, Function& func, op_type type,RegSlot src, AddrSlot addr_slot)
{
    emit_directive_internal(itl,func,type,make_reg_operand(src),make_reg_operand(addr_slot.slot),make_imm_operand(addr_slot.offset));
}


void load_struct_u64(Interloper& itl, Function& func, RegSlot dst, AddrSlot addr_slot)
{
    load_struct_internal(itl,func,op_type::load_struct_u64,dst,addr_slot);
}

RegSlot load_struct_u64_res(Interloper& itl, Function& func, AddrSlot addr_slot)
{
    const auto dst = new_tmp(func,GPR_SIZE);
    load_struct_u64(itl,func,dst,addr_slot);

    return dst;
}

void lock_reg(Interloper& itl, Function& func, RegSlot reg)
{
    assert(reg.kind == reg_kind::spec);
    const u32 machine_reg = special_reg_to_reg(itl.arch,reg.spec);
    func.locked_set |= (1 << machine_reg);

    emit_directive_internal(itl,func,op_type::lock_reg,make_reg_operand(reg));
}

void unlock_reg(Interloper& itl, Function& func, RegSlot reg)
{
   emit_directive_internal(itl,func,op_type::unlock_reg,make_reg_operand(reg));
}
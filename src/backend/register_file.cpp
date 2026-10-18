
RegisterFile& get_register_file(GraphAlloc& alloc, reg_type rtype)
{
    switch(rtype)
    {
        case reg_type::fpr:
        {
            return alloc.fpr;
        }

        case reg_type::gpr:
        {
            return alloc.gpr;
        }
    }

    assert(false);
}

RegisterFile& get_register_file(GraphAlloc& alloc, Reg& reg)
{
    return get_register_file(alloc,ir_rtype(reg));
}


void lock_reg(RegisterFile& regs, u32 reg)
{
    regs.locked_set = set_bit(regs.locked_set,reg);
}

void unlock_reg(RegisterFile& regs, u32 reg)
{
    regs.locked_set = deset_bit(regs.locked_set,reg);
}

bool is_locked(RegisterFile& regs,u32 reg)
{
    return is_set(regs.locked_set,reg);
}

void remove_reg(RegisterFile& regs, u32 reg)
{
    regs.free_set = deset_bit(regs.free_set,reg);
}

void free_reg(RegisterFile& regs,u32 reg)
{
    regs.free_set = set_bit(regs.free_set,reg);
}

void add_reg(RegisterFile& regs, x86_reg reg)
{
    free_reg(regs,u32(reg));
    unlock_reg(regs,u32(reg));
}

void mark_used(RegisterFile& regs, u32 reg)
{
    regs.used_set = set_bit(regs.used_set,reg);
}


void reset_stack_only_registers(GraphAlloc& alloc)
{
    // just add the scratch registers
    add_reg(alloc.gpr,x86_reg::r11);
    add_reg(alloc.gpr,x86_reg::r12);
    add_reg(alloc.gpr,x86_reg::r13);

    add_reg(alloc.fpr,x86_reg::xmm5);
    add_reg(alloc.fpr,x86_reg::xmm6);
    add_reg(alloc.fpr,x86_reg::xmm7);
}

void init_regs(GraphAlloc& alloc)
{
    if(alloc.stack_only)
    {
        reset_stack_only_registers(alloc);
        return;
    }

    // add grps
    add_reg(alloc.gpr,x86_reg::rax);
    add_reg(alloc.gpr,x86_reg::rcx);
    add_reg(alloc.gpr,x86_reg::rdx);
    add_reg(alloc.gpr,x86_reg::rbx);

    // Frame pointer is reserved in debug
    if(!alloc.debug)
    {
        add_reg(alloc.gpr,x86_reg::rbp);
    }

    add_reg(alloc.gpr,x86_reg::rsi);
    add_reg(alloc.gpr,x86_reg::rdi);

    add_reg(alloc.gpr,x86_reg::r8);
    add_reg(alloc.gpr,x86_reg::r9);
    add_reg(alloc.gpr,x86_reg::r10);
    add_reg(alloc.gpr,x86_reg::r11);
    add_reg(alloc.gpr,x86_reg::r12);
    add_reg(alloc.gpr,x86_reg::r13);
    add_reg(alloc.gpr,x86_reg::r14);
    add_reg(alloc.gpr,x86_reg::r15);

    // add sse regs
    add_reg(alloc.fpr,x86_reg::xmm0);
    add_reg(alloc.fpr,x86_reg::xmm1);
    add_reg(alloc.fpr,x86_reg::xmm2);
    add_reg(alloc.fpr,x86_reg::xmm3);
    add_reg(alloc.fpr,x86_reg::xmm4);
    add_reg(alloc.fpr,x86_reg::xmm5);
    add_reg(alloc.fpr,x86_reg::xmm6);
    add_reg(alloc.fpr,x86_reg::xmm7);
}

void reset_register_file(RegisterFile& file)
{
    file.free_set = file.free_set_full;
}

void reset_register_files(GraphAlloc& alloc)
{
    reset_register_file(alloc.gpr);
    reset_register_file(alloc.fpr);
}

u32 find_pref_register(u32 set, u32 pref)
{
    return destoer::ffs(set & pref);
}

u32 find_free_register(u32 set, u32 used, u32 pref)
{
    // ATTEMPT passed pref first
    u32 reg = find_pref_register(set,pref);

    if(reg == FFS_EMPTY)
    {
        reg = find_pref_register(set,used);
    }

    // get any reg we can
    if(reg == FFS_EMPTY)
    {
        reg = destoer::ffs(set);
    }

    return reg;
}


u32 alloc_reg(RegisterFile& regs, u32 pref)
{
    const u32 reg = find_free_register(regs.free_set & ~regs.locked_set,regs.used_set,pref);

    if(reg != FFS_EMPTY)
    {
        remove_reg(regs,reg);
        mark_used(regs,reg);
    }

    return reg;
}

lowered_reg_t reload_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg,insertion_type type)
{
    auto& ir_reg = reg_from_slot(alloc,slot);
    const auto opcode = make_load(reg,slot,alloc.stack_alloc.stack_offset);
    log_reg(alloc,"reload %r to %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);

    insert_node(block.list,node,opcode,type);

    assert(!is_mem_unallocated(ir_reg));

    return reg;
}

void reserve_offset(GraphAlloc& alloc, Reg& ir_reg,u32 reg)
{
    log_reg(alloc,"reserve offset for %r in %s\n",ir_reg.reg_slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}


// Spill a register to memory
void spill_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg, insertion_type type)
{
    auto& ir_reg = reg_from_slot(alloc,slot);
    auto& regs = get_register_file(alloc,ir_reg);

    const u32 size = ir_reg.size * ir_reg.count;

    if(ir_reg.segment == reg_segment::constant)
    {
        log_reg(alloc,"Constant %r deallocated from %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);
        free_reg(regs,reg);
        return;
    }

    // TODO: handle if structs aren't always in memory
    assert(size <= GPR_SIZE);

    // we have not spilled this value on the stack yet we need to actually allocate its position

    if(is_mem_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,reg);
    }

    log_reg(alloc,"spill %r from %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);


    const auto opcode = make_spill(reg,slot,alloc.stack_alloc.stack_offset);
    insert_node(block.list,node,opcode,type);

    // Mark the register as freed
    if(!alloc.stack_only)
    {
        free_reg(regs,reg);
    }
}

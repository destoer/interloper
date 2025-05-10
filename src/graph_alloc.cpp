struct RegisterFile
{
    // what registers have we used total for this function?
    u32 used_set = 0;

    // what registers are we allowed to use?
    u32 free_set = 0;

    // which regs are currently unusable
    u32 locked_set = 0;

    // What slot is being used by a register?
    RegSlot allocated[MACHINE_REG_SIZE];

    // What registers to use for loading from the stack
    u32 stack_scratch_registers[3];
};


struct GraphAlloc
{
    arch_target arch;

    // allcation info of tmp's for current function
    // NOTE: this is owned by the func and we dont have to free it
    Array<Reg> tmp_regs;
    SymbolTable* table;

    b32 print = false;

    RegisterFile gpr;
    RegisterFile fpr;

    StackAlloc stack_alloc;

    b32 stack_only = false;
};

void init_regs(GraphAlloc& alloc);

GraphAlloc make_graph_alloc(b32 print_reg,b32 print_stack, b32 stack_only,Array<Reg> registers, SymbolTable* table,arch_target arch)
{
    GraphAlloc alloc;

    alloc.print = print_reg;
    alloc.stack_alloc = make_stack_alloc(print_stack);

    alloc.arch = arch;
    alloc.tmp_regs = registers;
    alloc.table = table;
    alloc.stack_only = stack_only;

    init_regs(alloc);

    return alloc;
}

void destroy_graph_alloc(GraphAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}

bool is_reg_free(RegisterFile& regs,u32 reg)
{
    return is_set(regs.free_set & ~regs.locked_set,reg);
}

RegisterFile& get_register_file(GraphAlloc& alloc, Reg& reg)
{
    return (reg.flags & REG_FLOAT)? alloc.fpr : alloc.gpr;
}

void print_reg_alloc(GraphAlloc& alloc)
{
    const u32 free_regs = popcount(alloc.gpr.free_set);
    const u32 used_regs = popcount(alloc.gpr.used_set);

    printf("free gpr registers: %d\n",free_regs);
    printf("total used gpr registers: %d\n",used_regs);

    for(u32 i = 0; i < X86_GPR_SIZE; i++)
    {
        const RegSlot slot = alloc.gpr.allocated[i];

        if(is_reg_free(alloc.gpr,i))
        {
            continue;
        }

        log_reg(alloc.print,*alloc.table,"reg %s -> %r\n",reg_name(alloc.arch,i),slot);
    }

    putchar('\n');
}

void mark_used(RegisterFile& regs, u32 reg)
{
    regs.used_set = set_bit(regs.used_set,reg);
}

void free_reg(RegisterFile& regs,u32 reg)
{
    regs.free_set = set_bit(regs.free_set,reg);
    regs.allocated[reg] = make_spec_reg_slot(spec_reg::null);
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

void add_reg(RegisterFile& regs, x86_reg reg)
{
    free_reg(regs,u32(reg));
    regs.locked_set = deset_bit(regs.locked_set,u32(reg));
}

// Lock register and prevent anyone else from using it!
void claim_register(RegisterFile& regs, u32 reg)
{
    mark_used(regs,reg);
    lock_reg(regs,reg);
    free_reg(regs,reg);
}

// Allow others to use a register again
void release_register(RegisterFile& regs, u32 reg)
{
    unlock_reg(regs,reg);
    free_reg(regs,reg);
}

u32 find_free_register(u32 set,u32 used)
{
    u32 reg = FFS_EMPTY;

    // prefer used regs
    reg = destoer::ffs(set & used);

    // get any reg we can
    if(reg == FFS_EMPTY)
    {
        reg = destoer::ffs(set);
    }

    return reg;
}

u32 alloc_reg(RegisterFile& regs)
{
    const u32 reg = find_free_register(regs.free_set & ~regs.locked_set,regs.used_set);

    if(reg != FFS_EMPTY)
    {
        remove_reg(regs,reg);
        mark_used(regs,reg);
    }

    return reg;
}


// TODO: we need to dynamically lock the registers for each function
void init_regs(GraphAlloc& alloc)
{
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.gpr.allocated[i] = make_spec_reg_slot(spec_reg::null);
        alloc.fpr.allocated[i] = make_spec_reg_slot(spec_reg::null);
    }

    // All regs free (though this does not imply they are usable)
    alloc.gpr.free_set = 0xffff'ffff;
    alloc.fpr.free_set = 0xffff'ffff;

    // reg is locked until added
    alloc.gpr.locked_set = 0xffff'ffff;
    alloc.fpr.locked_set = 0xffff'ffff;
    
    // add grps
    add_reg(alloc.gpr,x86_reg::rax);
    add_reg(alloc.gpr,x86_reg::rcx);
    add_reg(alloc.gpr,x86_reg::rdx);
    add_reg(alloc.gpr,x86_reg::rbx);
    add_reg(alloc.gpr,x86_reg::rdp);
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

    alloc.gpr.stack_scratch_registers[0] = x86_reg::r11;
    alloc.gpr.stack_scratch_registers[1] = x86_reg::r12;
    alloc.gpr.stack_scratch_registers[2] = x86_reg::r13;


    // add sse regs
    add_reg(alloc.fpr,x86_reg::xmm0);
    add_reg(alloc.fpr,x86_reg::xmm1);
    add_reg(alloc.fpr,x86_reg::xmm2);
    add_reg(alloc.fpr,x86_reg::xmm3);
    add_reg(alloc.fpr,x86_reg::xmm4);
    add_reg(alloc.fpr,x86_reg::xmm5);
    add_reg(alloc.fpr,x86_reg::xmm6);
    add_reg(alloc.fpr,x86_reg::xmm7);

    alloc.fpr.stack_scratch_registers[0] = x86_reg::xmm5;
    alloc.fpr.stack_scratch_registers[1] = x86_reg::xmm6;
    alloc.fpr.stack_scratch_registers[2] = x86_reg::xmm7;

}

// NOTE: this relieso on pow2
static_assert(MACHINE_REG_SIZE == 32);

Reg& reg_from_slot(RegSlot slot, GraphAlloc& alloc)
{
    return reg_from_slot(*alloc.table,alloc.tmp_regs,slot);
}

void reload_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg,insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    const auto opcode = make_op(op_type::load,make_raw_operand(reg),make_directive_reg(slot),make_raw_operand(alloc.stack_alloc.stack_offset));
    log_reg(alloc.print,*alloc.table,"reload %r to %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);

    insert_node(block.list,node,opcode,type);

    assert(!is_stack_unallocated(ir_reg));
}


// Force a reload of a allocated register to deal with aliasing.
// TODO: Don't know how these are dealt with yet.

void reload_slot(GraphAlloc& alloc,Block& block, OpcodeNode* node, RegSlot slot)
{
    UNUSED(alloc); UNUSED(block); UNUSED(node); UNUSED(slot);
    if(!alloc.stack_only)
    {
        assert(false);
    }
}


void reserve_offset(GraphAlloc& alloc, Reg& ir_reg,u32 reg)
{
    log_reg(alloc.print,*alloc.table,"reserve offset for %r in %s\n",ir_reg.slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}

// Spill a register to memory
void spill_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg, insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    const u32 size = ir_reg.size * ir_reg.count;

    if(ir_reg.segment == reg_segment::constant)
    {
        log_reg(alloc.print,*alloc.table,"Constant %r deallocated from %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);
        return;
    }

    // TODO: handle if structs aernt always in memory
    assert(size <= GPR_SIZE);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,reg);
    }

    log_reg(alloc.print,*alloc.table,"spill %r from %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);


    const auto opcode = make_op(op_type::spill,make_raw_operand(reg),make_directive_reg(slot),make_raw_operand(alloc.stack_alloc.stack_offset));

    insert_node(block.list,node,opcode,type);
}

// Spill a slot to memory
void spill(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, insertion_type type)
{
    UNUSED(block); UNUSED(node); UNUSED(type);

    auto& ir_reg = reg_from_slot(slot,alloc);

    // is actually in a reg and not immediatly spilled
    if(!alloc.stack_only)
    {
        assert(false);
    }

    // reserve a space for this for a later spill
    else if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,LOCATION_MEM);
    }   
}

void allocate_and_rewrite_var_stack(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
{
    const auto opcode = node->value;
    const auto info = info_from_op(opcode);

    const b32 is_dst = is_arg_dst(info.type[reg]);
    const b32 is_src = is_arg_src(info.type[reg]);

    auto& ir_reg = reg_from_slot(slot,alloc);
    auto& reg_file = get_register_file(alloc,ir_reg);

    const u32 scratch_reg = reg_file.stack_scratch_registers[reg];

    // rewrite in the register
    node->value.v[reg] = make_raw_operand(scratch_reg);

    // src do a reload
    if(is_src) 
    {
        // issue a load
        reload_reg(alloc,block,node,slot,scratch_reg,insertion_type::before);

        // do the writeback as well
        if(is_dst)
        {
            spill_reg(alloc,block,node,slot,scratch_reg,insertion_type::after);
        }
    }

    // just a dst
    else if(is_dst)
    {
        spill_reg(alloc,block,node,slot,scratch_reg,insertion_type::after);      
    }
}

void allocate_and_rewrite_var(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
{
    // just rewrite the register simply
    if(alloc.stack_only)
    {
        allocate_and_rewrite_var_stack(alloc,block,node,slot,reg);
        return;
    }

    assert(false);
}

void rewrite_special_reg(GraphAlloc& alloc, OpcodeNode* node, spec_reg spec, u32 reg)
{
    const u32 location = special_reg_to_reg(alloc.arch,spec);
    const b32 is_float = is_special_reg_fpr(spec);
    auto& reg_file = is_float? alloc.fpr : alloc.gpr;

    // make sure the spec regs are marked as used
    mark_used(reg_file,location);
    node->value.v[reg] = make_raw_operand(location);
}

void allocate_and_rewrite_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
{
    switch(slot.kind)
    {
        case reg_kind::sym:
        case reg_kind::tmp:
        {
            allocate_and_rewrite_var(alloc,block,node,slot,reg);
            break;
        }

        case reg_kind::spec:
        {
            rewrite_special_reg(alloc,node,slot.spec,reg);
            break;
        }
    }
}

void allocate_and_rewrite(GraphAlloc& alloc, Block& block, OpcodeNode* node, u32 reg)
{
    Operand &operand = node->value.v[reg];

    switch(operand.type)
    {
        case operand_type::decimal:
        {
            operand.raw = bit_cast_from_f64(operand.decimal);
            operand.type = operand_type::raw;
            break;
        }

        case operand_type::imm: 
        {
            operand.raw = operand.imm;
            operand.type = operand_type::raw;
            break;
        }

        case operand_type::reg: 
        {
            allocate_and_rewrite_reg(alloc,block,node,operand.reg,reg);
            break;
        }

        // allready written
        case operand_type::raw: break;
        
        // rewrote on subsequent pass
        case operand_type::label: break;
        case operand_type::directive_reg: break;
    }
}

void rewrite_opcode(GraphAlloc& alloc,Block& block,OpcodeNode* node)
{
    const auto opcode = node->value;
    const auto info = info_from_op(opcode);

    // rewrite all
    for(u32 a = 0; a < info.args; a++)
    {
        allocate_and_rewrite(alloc,block,node,a);
    }
}

void finish_alloc(Reg& reg,GraphAlloc& alloc)
{
    assert(pending_stack_allocation(reg));

    finalise_offset(alloc.stack_alloc,reg);

    const auto print = alloc.stack_alloc.print;

    log_reg(print,*alloc.table,"final offset %r = [%x,%x] -> 0x%x\n",reg.slot,reg.size,reg.count,reg.offset);
}

void finish_stack_alloc(GraphAlloc& alloc)
{
    calc_allocation(alloc.stack_alloc);

    auto& stack_alloc = alloc.stack_alloc;

    for(const RegSlot slot : stack_alloc.pending_allocation)
    {
        auto& ir_reg = reg_from_slot(slot,alloc);
        finish_alloc(ir_reg,alloc);
    }
}

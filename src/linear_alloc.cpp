// TODO: atm this is just a basic stack spill allocator

struct LinearRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    SymSlot slot = {SYMBOL_NO_SLOT}; 
};

// http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
struct LinearAlloc
{
    arch_target arch;

    // what instruction are we on?
    u32 pc = 0;

    // allcation info of tmp's for current function
    // NOTE: this is owned by the func and we dont have to free it
    Array<Reg> tmp_regs;

    HashTable<SymSlot,u32> location;

    Array<LinearRange> range;

    b32 print = false;

    // need's to be as large as the code size
    // i.e 2 on x86 3 on mips
    // NOTE: we dont have to mark these as used
    // as by definition we dont care about their values
    u32 scratch_regs[3];

    // what registers have we used total for this function?
    u32 used_set = 0;

    // what registers are we allowed to use?
    u32 free_set = 0;

    StackAlloc stack_alloc;
};


void print_reg_alloc(LinearAlloc& alloc)
{
    UNUSED(alloc);
}

void mark_used(LinearAlloc& alloc, u32 reg)
{
    alloc.used_set = set_bit(alloc.used_set,reg);
}

LinearAlloc make_linear_alloc(b32 print_reg,b32 print_stack,Array<Reg> registers,arch_target arch)
{
    LinearAlloc alloc;

    alloc.print = print_reg;
    alloc.stack_alloc = make_stack_alloc(print_stack);

    alloc.arch = arch;
    alloc.tmp_regs = registers;

    switch(arch)
    {
        case arch_target::x86_64_t:
        {
            // setup our two scratch regs
            alloc.scratch_regs[0] = u32(x86_reg::r11);
            alloc.scratch_regs[1] = u32(x86_reg::r12);
            alloc.scratch_regs[2] = REG_FREE;

            break;
        }
    }

    alloc.location = make_table<SymSlot,u32>();

    return alloc;
}

void destroy_linear_alloc(LinearAlloc& alloc)
{
    destroy_table(alloc.location);
    destroy_arr(alloc.range);
}

Reg& reg_from_slot(SymSlot slot, SymbolTable& table, LinearAlloc& alloc)
{
    return reg_from_slot(table,alloc.tmp_regs,slot);
}

// TODO: for now we will be operating everything off the stack
// and not actually making use of location
// we should later add a flag that renables this mode of operation
// as a debugging tool!

void reload_reg(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, u32 reg)
{
    const auto opcode = make_op(op_type::load,reg,slot.handle,alloc.stack_alloc.stack_offset);

    auto& ir_reg = reg_from_slot(slot,table,alloc);
    log_reg(alloc.print,table,"reload %r to %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);

    insert_at(block.list,node,opcode);
}


void reload_slot(LinearAlloc& alloc, SymbolTable& table,Block& block, ListNode* node, SymSlot slot)
{
    const u32* reg_opt = lookup(alloc.location,slot);

    // is actually in a reg and not immediatly spilled
    if(reg_opt)
    {
        const u32 reg = *reg_opt;

        reload_reg(alloc,table,block,node,slot,reg);
    }
}


void reserve_offset(LinearAlloc& alloc,SymbolTable& table, Reg& ir_reg,u32 reg)
{
    log_reg(alloc.print,table,"reserve offset for %r in %s\n",ir_reg.slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}


void spill_reg(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, u32 reg, bool after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);
    const u32 size = ir_reg.size * ir_reg.count;

    // TODO: handle if structs aernt always in emory
    assert(size <= GPR_SIZE);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,table,ir_reg,reg);
    }

    log_reg(alloc.print,table,"spill %r from %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);


    const auto opcode = make_op(op_type::spill,reg,slot.handle,alloc.stack_alloc.stack_offset);

    if(!after)
    {
        insert_at(block.list,node,opcode); 
    }

    else 
    {
        insert_after(block.list,node,opcode);
    }
}

void spill(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, bool after)
{
    const u32* reg_opt = lookup(alloc.location,slot);

    // is actually in a reg and not immediatly spilled
    if(reg_opt)
    {
        const u32 reg = *reg_opt;

        spill_reg(alloc,table,block,node,slot,reg,after);
    }
}


void allocate_and_rewrite(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, u32 reg)
{
    const auto opcode = node->opcode;
    const auto info = info_from_op(opcode);

    const b32 is_dst = is_arg_dst(info.type[reg]);
    const b32 is_src = is_arg_src(info.type[reg]);

    // save slot so we can use it if it gets rewritten
    SymSlot slot = sym_from_idx(node->opcode.v[reg]);

    // check we are dealing with a var
    if(is_var(slot))
    {
        // src do a reload
        if(is_src) 
        {
            // issue a load
            reload_reg(alloc,table,block,node,slot,alloc.scratch_regs[reg]);

            // rewrite in the register
            node->opcode.v[reg] = alloc.scratch_regs[reg];

            // do the writeback as well
            if(is_dst)
            {
                spill_reg(alloc,table,block,node,slot,node->opcode.v[reg],true);
            }
        }

        // just a dst
        else if(is_dst)
        {
            // rewrite in the register
            node->opcode.v[reg] = alloc.scratch_regs[reg];

            spill_reg(alloc,table,block,node,slot,node->opcode.v[reg],true);      
        }
    }

    // special reg just rewrite it to whatever it wants
    else if(is_special_reg(slot))
    {
        node->opcode.v[reg] = special_reg_to_reg(alloc.arch,slot);
    }
}

void rewrite_opcode(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node)
{
    const auto opcode = node->opcode;
    const auto info = info_from_op(opcode);

    // rewrite each arguement
    for(u32 a = 0; a < info.args; a++)
    {
        allocate_and_rewrite(alloc,table,block,node,a);
    }
}

void finish_alloc(Reg& reg,SymbolTable& table,LinearAlloc& alloc)
{
    assert(pending_stack_allocation(reg));

    finalise_offset(alloc.stack_alloc,reg);

    const auto print = alloc.stack_alloc.print;

    log_reg(print,table,"final offset %r = [%x,%x] -> %x\n",reg.slot,reg.size,reg.count,reg.offset);
}

void finish_stack_alloc(SymbolTable& table, LinearAlloc& alloc)
{
    calc_allocation(alloc.stack_alloc);

    auto& stack_alloc = alloc.stack_alloc;

    for(u32 r = 0; r < count(stack_alloc.pending_allocation); r++)
    {
        const SymSlot slot = stack_alloc.pending_allocation[r];
        auto& ir_reg = reg_from_slot(slot,table,alloc);

        finish_alloc(ir_reg,table,alloc);
    }
}
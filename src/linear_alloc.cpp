struct LinearRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    RegSlot slot = {INVALID_HANDLE};
    u32 global_reg = REG_FREE;
    OpcodeNode* node = nullptr;
    BlockSlot block_slot = {INVALID_HANDLE};
};

bool is_reg_locally_allocated(const Reg& reg)
{
    return reg.local_reg != REG_FREE;
}

bool is_reg_globally_allocated(const Reg& reg)
{
    return reg.global_reg != REG_FREE;
}

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

// http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
struct LinearAlloc
{
    arch_target arch;

    // what instruction are we on?
    u32 pc = 0;

    // allcation info of tmp's for current function
    // NOTE: this is owned by the func and we dont have to free it
    Array<Reg> tmp_regs;
    SymbolTable* table;

    b32 print = false;

    RegisterFile gpr;
    RegisterFile fpr;

    // Registers marked for expiry
    RegSlot dead_slot[3];
    u32 dead_count = 0;
    u32 total_misplaced = 0;

    StackAlloc stack_alloc;

    b32 stack_only = false;
    b32 debug = false;
};


bool marked_for_exipiry(LinearAlloc& alloc, RegSlot slot);

bool is_reg_free(RegisterFile& regs,u32 reg)
{
    return is_set(regs.free_set & ~regs.locked_set,reg);
}

RegisterFile& get_register_file(LinearAlloc& alloc, reg_file_kind kind)
{
    switch(kind)
    {
        case reg_file_kind::fpr:
        {
            return alloc.fpr;
        }

        case reg_file_kind::gpr:
        {
            return alloc.gpr;
        }
    }

    assert(false);
}

RegisterFile& get_register_file(LinearAlloc& alloc, Reg& reg)
{
    return get_register_file(alloc,find_reg_set(reg));
}

void print_reg_alloc(LinearAlloc& alloc)
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

LinearAlloc make_linear_alloc(b32 print_reg,b32 print_stack, b32 stack_only, b32 debug, Array<Reg> registers, SymbolTable* table,arch_target arch)
{
    LinearAlloc alloc;

    alloc.print = print_reg;
    alloc.stack_alloc = make_stack_alloc(print_stack,debug);

    alloc.arch = arch;
    alloc.tmp_regs = registers;
    alloc.table = table;
    alloc.stack_only = stack_only;
    alloc.debug = debug;

    return alloc;
}

void update_range(Interloper& itl, Function& func,HashTable<RegSlot,LinearRange> &table, RegSlot slot,Block& block, OpcodeNode* node,u32 pc)
{
    auto& ir_reg = reg_from_slot(itl,func,slot);

    // if this register has side effects we aint
    // interested in it
    if(!is_local_reg(ir_reg))
    {
        return;
    }

    auto range_opt = lookup(table,slot);

    // add the intial entry
    if(!range_opt)
    {
        LinearRange range;
        range.slot = slot;
        range_opt = add(table,slot,range);
    }

    auto& range = *range_opt;

    if(pc < range.start)
    {
        range.block_slot = block.block_slot;
        range.node = node;
        range.start = pc;
    }

    range.end = std::max(range.end,pc);
}

Array<LinearRange> find_range(Interloper& itl, Function& func)
{

    auto table = make_table<RegSlot,LinearRange>();

    u32 pc = 0;

    // for each block
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        OpcodeNode* node = block.list.start;

        // If we are live in on a block as a arg (for the first time) we have to force it live from block start
        // This is not a problem for locals because they will actually defined and assigned before use.
        // Function args on the other hand are going to be in memory from being passed, and not a reg they may be put in.
        // We can return to this when we have the ablitly to setup more granular ranges for blocks via block args.
        /*
        live_in: ptr in rax
            lock rax
            mov rax, ptr <-- This is not loaded until here, but the register has already been locked we have a problem
        */

        for(const RegSlot slot : block.live_in)
        {
            if(is_special_reg(slot))
            {
                continue;
            }

            auto& ir_reg = reg_from_slot(itl,func,slot);

            if(is_arg(ir_reg) && !contains(table,slot))
            {
                const auto live_op = make_op(op_type::live_var,make_reg_operand(slot));
                node = insert_at(block.list,node,live_op);
                update_range(itl,func,table,slot,block,node,pc);
            }
        }

        // for each opcode
        while(node)
        {
            // scan each opcode for any vars
            const auto opcode = node->value;
            const auto info = info_from_op(opcode); 

            switch(opcode.op)
            {
                case op_type::mov_unlock:
                {
                    const auto src = opcode.v[1].reg;
                    const auto dst = opcode.v[0].reg;
                    const u32 location = special_reg_to_reg(itl.arch,src.spec);
                    auto& ir_reg = reg_from_slot(itl,func,dst);
                    ir_reg.hint |= (1 << location);
                }

                default: break;
            }

            for(s32 a = info.args - 1; a >= 0; a--)
            {
                const auto operand = opcode.v[a];

                if(!is_arg_reg(info.type[a]))
                {
                    continue;
                }

                const auto slot = operand.reg;

                if(!is_special_reg(slot))
                {
                    // dst only has a later lifetime
                    if(info.type[a] == arg_type::dst_reg)
                    {
                        pc += 1;
                        update_range(itl,func,table,slot,block,node,pc);
                    }

                    else
                    {
                        update_range(itl,func,table,slot,block,node,pc);
                    }
                }
            }

            // next opcode
            pc += 1;
            node = node->next;
        }
        
        // live out has a higher start point
        pc += 1;

        OpcodeNode* last = block.list.finish;

        // if its live out then consider it allocated till the end of the block
        for(const RegSlot slot : block.live_out)
        {
            update_range(itl,func,table,slot,block,last,pc);
        }
    }

    // okay we have all the ranges now flatten this into a sorted array
    Array<LinearRange> range;

    // first copy the hash table contents into the array
    for(const auto& hash_node : table)
    {
        const LinearRange linear_range = hash_node.v;
        push_var(range,linear_range);
    }

    // finally sort it
    heap_sort(range,[](const LinearRange& v1, const LinearRange& v2)
    {
        return v1.start > v2.start;
    });

    destroy_table(table); 
    return range;
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

void assign_local_reg(RegisterFile& regs,Reg& ir_reg, u32 reg)
{
    ir_reg.local_reg = reg;
    regs.allocated[ir_reg.local_reg] = ir_reg.slot;
}

void take_local_reg(RegisterFile& reg_file,Reg& ir_reg, u32 reg)
{
    assign_local_reg(reg_file,ir_reg,reg);
    remove_reg(reg_file, ir_reg.local_reg);
    mark_used(reg_file,ir_reg.local_reg);   
}

bool alloc_ir_reg(RegisterFile& regs, Reg& ir_reg)
{
    // If global register is free prefer its allocation
    if(is_reg_free(regs,ir_reg.global_reg))
    {
        take_local_reg(regs,ir_reg,ir_reg.global_reg);
        return true;
    }

    const u32 reg = alloc_reg(regs,ir_reg.hint);

    if(reg != FFS_EMPTY)
    {
        assign_local_reg(regs,ir_reg,reg);
        return true;
    }

    return false;
}

Reg& reg_from_slot(RegSlot slot, LinearAlloc& alloc)
{
    return reg_from_slot(*alloc.table,alloc.tmp_regs,slot);
}

void reserve_offset(LinearAlloc& alloc, Reg& ir_reg,u32 reg)
{
    log_reg(alloc.print,*alloc.table,"reserve offset for %r in %s\n",ir_reg.slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}


void free_ir_reg(Reg& ir_reg,RegisterFile& regs)
{
    free_reg(regs, ir_reg.local_reg);
    ir_reg.local_reg = REG_FREE;
}

// Spill a register to memory
void spill_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg, insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    const u32 size = ir_reg.size * ir_reg.count;

    if(ir_reg.segment == reg_segment::constant)
    {
        log_reg(alloc.print,*alloc.table,"Constant %r deallocated from %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);
        free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
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

    // Mark the register as freed
    if(!alloc.stack_only)
    {
        assert(is_reg_locally_allocated(ir_reg));
        free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
    }
}


// Spill a slot to memory
void spill(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);

    // is actually in a reg and not immediatly spilled
    if(is_reg_locally_allocated(ir_reg))
    {
        spill_reg(alloc,block,node,slot,ir_reg.local_reg,type);
    }

    // reserve a space for this for a later spill
    else if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,REG_FREE);
    }   
}


// TODO: This should look for a register furthest in the future.
// We may have to have a look back over our local allocator
void acquire_local_reg(LinearAlloc& alloc, Reg& ir_reg, RegisterFile& regs,Block& block, OpcodeNode* node)
{
    // Spill a register for room
    if(!alloc_ir_reg(regs,ir_reg))
    {
        u32 furthest_use = 0;
        RegSlot reg = INVALID_SYM_REG_SLOT;

        for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
        {
            if(is_locked(regs,r))
            {
                continue;
            }

            const auto slot = regs.allocated[r];
            auto& candidate = reg_from_slot(slot,alloc);

            if(candidate.cur_local_uses >= count(candidate.local_uses))
            {
                furthest_use = 0xffff'ffff;
                reg = slot;
            }

            else
            {
                const u32 next_use = candidate.local_uses[candidate.cur_local_uses];
                if(next_use > furthest_use)
                {
                    furthest_use = next_use;
                    reg = slot;
                }
            }
        }

        // We should have been able to find a register
        assert(furthest_use != 0);

        spill(alloc,block,node,reg,insertion_type::before);
        assert(alloc_ir_reg(regs,ir_reg));
    }
}

// TODO: we need to dynamically lock the registers for each function
void init_regs(LinearAlloc& alloc)
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

struct ActiveReg
{
    LinearRange arr[MACHINE_REG_SIZE];
    u32 size = 0;
};

void clean_dead_reg(Interloper& itl, Function& func,LinearAlloc& alloc,ActiveReg &active, const LinearRange& cur)
{
    u32 i;
    for(i = 0; i < active.size; i++)
    {
        auto& cmp = active.arr[i];
    /*
        printf("clean %d %d: %x [%d,%d] : %x [%d,%d]\n",i,active.size,
            cur.slot.handle,cur.start,cur.end,
            cmp.slot.handle,cmp.start,cmp.end);
    */
        // stopping point for purging entires
        if(cmp.end >= cur.start)
        {
            break;
        }

        // free the expired range
        else
        {
            // check which register file to use
            auto& ir_reg = reg_from_slot(itl,func,cmp.slot);
            auto& reg_file = get_register_file(alloc,ir_reg);

            //printf("free %d %d: %x [%d,%d] -> %x\n",i,active.size,cmp.slot.handle,cmp.start,cmp.end,cmp.location);
            free_reg(reg_file,cmp.global_reg);
        }
    }

    // copy over the entries, to remove purged ones from the list
    if(i != 0)
    {
        memmove(&active.arr[0],&active.arr[i],(active.size - i) * sizeof(LinearRange));
        active.size -= i;
    }

    //putchar('\n');
}

void add_active(ActiveReg &active, const LinearRange& cur)
{
    u32 i;

    for(i = 0; i < active.size; i++)
    {
        // look for insertion point
        if(cur.end < active.arr[i].end)
        {
            break;
        }
    }

    //printf("add   %d %d: %x [%d,%d]\n",i,active.size,cur.slot.handle,cur.start,cur.end);

    // copy over by one 
    if(i < active.size)
    {
        memmove(&active.arr[i + 1],&active.arr[i],(active.size - i) * sizeof(LinearRange));
    }

    // write in the entry
    active.arr[i] = cur;
    active.size += 1;
}

void linear_allocate(LinearAlloc& alloc,Interloper& itl, Function& func)
{
    // Don't need to calculate the ranges if doing stack allocation.
    if(itl.stack_alloc)
    {
        return;
    }

    auto range = find_range(itl,func);
    ActiveReg active;

/*
    printf("\n%s:\n",func.name.buf);

    // print the range
    for(u32 r = 0; r < count(range); r++)
    {
        const auto &entry = range[r];

        printf("%x [%d,%d]\n",entry.slot.handle,entry.start,entry.end);
    }
*/
    // init our register set
    init_regs(alloc);

    // perform the allocation
    for(u32 r = 0; r < count(range); r++)
    {
        auto& cur = range[r];

        // actually run the allocation
        auto& ir_reg = reg_from_slot(itl,func,cur.slot);

        // expire any dead sets
        clean_dead_reg(itl,func,alloc,active,cur);

        // check which register file to use
        auto& reg_file = get_register_file(alloc,ir_reg);

        const u32 reg = alloc_reg(reg_file,ir_reg.hint);

        // we have a register
        if(reg != FFS_EMPTY)
        {
            // set location
            cur.global_reg = reg;
            ir_reg.global_reg = cur.global_reg;

            // add to active register set 
            add_active(active,cur);
        }

        // TODO: do a spill of a existing reg, for now we just default
        // it into memory
        else
        {

        }
    }

    destroy_arr(range);
}

void destroy_linear_alloc(LinearAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}



void reload_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg,insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    const auto opcode = make_op(op_type::load,make_raw_operand(reg),make_directive_reg(slot),make_raw_operand(alloc.stack_alloc.stack_offset));
    log_reg(alloc.print,*alloc.table,"reload %r to %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);

    insert_node(block.list,node,opcode,type);

    assert(!is_stack_unallocated(ir_reg));
}


// Force a reload of a allocated register to deal with aliasing.

void reload_slot(LinearAlloc& alloc,Block& block, OpcodeNode* node, RegSlot slot)
{
    auto& ir_reg = reg_from_slot(slot,alloc);

    // If this register is not allocated then let the next instruction that uses it
    // Deal with handling the reload.
    if(is_reg_locally_allocated(ir_reg))
    {
        reload_reg(alloc,block,node,slot,ir_reg.local_reg,insertion_type::before);
    }
}




// Save a register, this can either be a copy to a free reg
// Or by spilling it to memory
void save_reg(LinearAlloc& alloc, Block& block, OpcodeNode* node, RegisterFile& file, u32 reg, insertion_type type)
{
    // TODO: for now just spill back out to memory 
    if(!is_reg_free(file,reg) && !is_locked(file,reg))
    {
        log_reg(alloc.print,*alloc.table,"Saving %r from %s\n",file.allocated[reg],reg_name(alloc.arch,reg));
        spill(alloc,block,node,file.allocated[reg],type);
    }
}

void save_caller_saved_regs(LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    const auto& abi_info = get_abi_info(alloc.arch);

    // then save the caller saved registers
    save_reg(alloc,block,node,alloc.gpr,abi_info.gpr_rv,insertion_type::before);
    save_reg(alloc,block,node,alloc.fpr,abi_info.fpr_rv,insertion_type::before);

    for(u32 a = 0; a < abi_info.gpr_arg_count; a++)
    {
        save_reg(alloc,block,node,alloc.gpr,abi_info.gpr_args[a],insertion_type::before);
    }
}

void alloc_regs_from_live_in(LinearAlloc& alloc, const Set<RegSlot>& live_in)
{
    for(const RegSlot slot : live_in)
    {
        // allocate the register into the appropiate register file
        auto& ir_reg = reg_from_slot(slot,alloc);
        
        if(is_reg_globally_allocated(ir_reg))
        {
            auto& reg_file = get_register_file(alloc,ir_reg);
            log_reg(alloc.print,*alloc.table,"%r is live in on %s\n",ir_reg.slot,reg_name(alloc.arch,ir_reg.global_reg));
            take_local_reg(reg_file,ir_reg,ir_reg.global_reg);
        }
    }
}

void compute_local_uses(LinearAlloc& alloc, Block& block)
{
    u32 pc = 0;
    
    for(const OpcodeNode& node : block.list)
    {
        const auto opcode = node.value;
        const auto info = info_from_op(opcode);

        for(u32 a = 0; a < info.args; a++)
        {
            const auto operand = opcode.v[a];

            if(!is_arg_reg(info.type[a]))
            {
                continue;
            }

            if(!is_special_reg(operand.reg))
            {
                auto& ir_reg = reg_from_slot(operand.reg,alloc);
                push_var(ir_reg.local_uses,pc);
            }
        }

        pc++;
    }    
}

void linear_setup_new_block(LinearAlloc& alloc, Block& block) 
{
    // clean any local registers that aernt live in, so the register is in a correct state
    for(const RegSlot slot : block.def)
    {
        auto& ir_reg = reg_from_slot(slot,alloc);
        if(is_reg_locally_allocated(ir_reg))
        {
            free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
        }
    }

    // All registers free at block start bar live in
    init_regs(alloc);

    // Regs coming rom live in are already allocated
    alloc_regs_from_live_in(alloc,block.live_in);

    compute_local_uses(alloc,block);
}

void allocate_and_rewrite_var_stack(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
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

void allocate_and_rewrite_var(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
{
    // just rewrite the register simply
    if(alloc.stack_only)
    {
        allocate_and_rewrite_var_stack(alloc,block,node,slot,reg);
        return;
    }


    auto& ir_reg = reg_from_slot(slot,alloc);
    auto& reg_file = get_register_file(alloc,ir_reg);

    ir_reg.cur_local_uses++;

    const auto opcode = node->value;
    const auto info = info_from_op(opcode);

    const b32 is_dst = is_arg_dst(info.type[reg]);
    const b32 is_src = is_arg_src(info.type[reg]);

    // var is allocated
    if(is_reg_locally_allocated(ir_reg))
    {
        node->value.v[reg] = make_raw_operand(ir_reg.local_reg);
    }

    // Aquire a new register and reload it
    else
    {
        acquire_local_reg(alloc,ir_reg,reg_file,block,node);
        log_reg(alloc.print,*alloc.table,"Allocated %s to %r\n",reg_name(alloc.arch,ir_reg.local_reg),ir_reg.slot);

        node->value.v[reg] = make_raw_operand(ir_reg.local_reg);

        // src do a reload
        if(is_src) 
        {
            // issue a load
            reload_reg(alloc,block,node,slot, ir_reg.local_reg,insertion_type::before);
        }
    }

    // Local uses have expried and it does not live beyond this block we can free the fregister
    if(ir_reg.cur_local_uses >= count(ir_reg.local_uses))
    {
        ir_reg.cur_local_uses = 0;

        // We still need to clear out the local uses!
        if(contains(block.live_out,ir_reg.slot))
        {
            clear_arr(ir_reg.local_uses);
        }

        else
        {
            log_reg(alloc.print,*alloc.table,"Mark %r in %s for expiry\n",ir_reg.slot,reg_name(alloc.arch,ir_reg.local_reg));
            // Don't terminate the regs during it as we need them allocated
            alloc.dead_slot[alloc.dead_count++] = ir_reg.slot;
        }            
    }

    // if dst and not a local register it needs to be spilled
    if(is_dst && !is_local_reg(ir_reg))
    {
        spill(alloc,block,node,slot,insertion_type::after);
    }
}

void rewrite_special_reg(LinearAlloc& alloc, Block& block, OpcodeNode* node, spec_reg spec, u32 reg)
{
    const u32 location = special_reg_to_reg(alloc.arch,spec);
    auto& reg_file = get_register_file(alloc,find_reg_set(spec));

    const auto opcode = node->value;
    const auto info = info_from_op(opcode);
    const b32 is_dst = is_arg_dst(info.type[reg]);

    // Whatever is in a special reg is about to be clobbered
    if(is_dst)
    {
        const RegSlot saved_slot = reg_file.allocated[location];

        // Check reg is locked to prevent regs we never allocate from attempting to be saved
        if(!is_reg_free(reg_file,location) && !is_locked(reg_file,location) && !marked_for_exipiry(alloc,saved_slot))
        {
            auto& ir_reg = reg_from_slot(saved_slot,alloc);

            log_reg(alloc.print,*alloc.table,"Saving %r clobbered in special reg %s (%s)\n",saved_slot,spec_reg_name(spec),reg_name(alloc.arch,ir_reg.local_reg));

            // Save the register if it has a later use.
            // But feel free to leave the current location for this rewrite
            // As the dst is last thing rewritten 
            save_reg(alloc,block,node,reg_file,ir_reg.local_reg,insertion_type::before);
        }

        lock_reg(reg_file,location);
    }

    // make sure the spec regs are marked as used
    mark_used(reg_file,location);
    node->value.v[reg] = make_raw_operand(location);
}

void allocate_and_rewrite_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg)
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
            rewrite_special_reg(alloc,block,node,slot.spec,reg);
            break;
        }
    }
}

bool marked_for_exipiry(LinearAlloc& alloc, RegSlot slot)
{
    for(u32 i = 0; i < alloc.dead_count; i++)
    {
        if(alloc.dead_slot[i] == slot)
        {
            return true;
        }
    }

    return false;
}

void clean_dead_regs(LinearAlloc& alloc)
{
    while(alloc.dead_count)
    {
        const RegSlot slot = alloc.dead_slot[--alloc.dead_count];
        auto& ir_reg = reg_from_slot(slot,alloc);

        log_reg(alloc.print,*alloc.table,"Expiring %r from %s\n",ir_reg.slot,reg_name(alloc.arch,ir_reg.local_reg));

        clear_arr(ir_reg.local_uses);

        if(is_reg_locally_allocated(ir_reg))
        {
            free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
        }
    }    
}

void allocate_and_rewrite(LinearAlloc& alloc, Block& block, OpcodeNode* node, u32 reg)
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

void rewrite_opcode(LinearAlloc& alloc,Block& block,OpcodeNode* node)
{
    const auto opcode = node->value;
    const auto info = info_from_op(opcode);

    // rewrite source
    for(u32 a = 1; a < info.args; a++)
    {
        allocate_and_rewrite(alloc,block,node,a);
    }

    // free regs early to get reuse out of operands
    // NOTE: this cannot happen on a src
    // because otherwhise a reload will be inserted before
    // the current instruction that clobbers the var we have just rewritten
    if(info.type[0] == arg_type::dst_reg)
    {
        clean_dead_regs(alloc);
    }

    // rewrite potential dst
    if(info.args >= 1)
    {
        allocate_and_rewrite(alloc,block,node,0);
    }
}

void finish_alloc(Reg& reg,LinearAlloc& alloc)
{
    assert(pending_stack_allocation(reg));

    finalise_offset(alloc.stack_alloc,reg);

    const auto print = alloc.stack_alloc.print;

    log_reg(print,*alloc.table,"final offset %r = [%x,%x] -> 0x%x\n",reg.slot,reg.size,reg.count,reg.offset);
}

void finish_stack_alloc(LinearAlloc& alloc)
{
    calc_allocation(alloc.stack_alloc);

    auto& stack_alloc = alloc.stack_alloc;

    for(const RegSlot slot : stack_alloc.pending_allocation)
    {
        auto& ir_reg = reg_from_slot(slot,alloc);
        finish_alloc(ir_reg,alloc);
    }
}

// TODO: Consider using xchg to swap them
void correct_live_out(LinearAlloc& alloc, Block& block)
{
    // No need to correct across blocks
    if(alloc.stack_only)
    {
        return;
    }

    // nothing to correct
    if(!block.list.start || block.flags & HAS_FUNC_EXIT)
    {
        return;
    }

    log(alloc.print,"Correcting live out on: L%d\n",block.label_slot.handle);

    Array<RegSlot> misplaced;

    // Get a list of all register in the wrong posistion
    for(const RegSlot slot : block.live_out)
    {
        auto& ir_reg = reg_from_slot(slot,alloc);

        if(ir_reg.local_reg != ir_reg.global_reg)
        {
            push_var(misplaced,slot);
            log_reg(alloc.print,*alloc.table,"misplaced %r %s %s\n",slot,reg_name(alloc.arch,ir_reg.local_reg),reg_name(alloc.arch,ir_reg.global_reg));
            assert(!is_locked(get_register_file(alloc,ir_reg),ir_reg.global_reg));
        }
    }

    alloc.total_misplaced += count(misplaced);

    const auto& end_info = info_from_op(block.list.finish->value);

    const insertion_type insert_type = is_group_branch(end_info.group)? insertion_type::before : insertion_type::after;

    // Keep running passes until we are done
    while(count(misplaced))
    {
        // while we can keep copying regs
        bool copied_register = true;

        while(copied_register)
        {
            copied_register = false;

            // First pass are there any registers we can simply copy over a dead register
            for(u32 m = 0; m < count(misplaced); m++)
            {
                auto& ir_reg = reg_from_slot(misplaced[m],alloc);
                auto& reg_file = get_register_file(alloc,ir_reg);

                // Not inside a register just go ahead and spill it.
                if(!is_reg_globally_allocated(ir_reg))
                {
                    if(is_reg_locally_allocated(ir_reg))
                    {
                        spill_reg(alloc,block,block.list.finish,ir_reg.slot,ir_reg.local_reg,insert_type);
                    }

                    remove_out_of_place(misplaced,m);
                    copied_register = true;
                }

                // Register we want it go into is free
                else if(is_reg_free(reg_file,ir_reg.global_reg))
                {
                    // If this is not locally allocated we need to issue a reload into it.
                    if(!is_reg_locally_allocated(ir_reg))
                    {
                        reload_reg(alloc,block,block.list.finish,ir_reg.slot,ir_reg.global_reg,insert_type);
                    }

                    // Otherwhise just move it
                    else
                    {
                        const bool is_float = ir_reg.flags & REG_FLOAT;
                        const auto opcode = make_raw_op(is_float? op_type::movf_reg : op_type::mov_reg,ir_reg.global_reg,ir_reg.local_reg);
                        insert_node(block.list,block.list.finish,opcode,insert_type);

                        log_reg(alloc.print,*alloc.table,"Copying %r from %s to %s\n",ir_reg.slot,reg_name(alloc.arch,ir_reg.local_reg),reg_name(alloc.arch,ir_reg.global_reg));

                        free_ir_reg(ir_reg,reg_file);
                    }

                    take_local_reg(reg_file,ir_reg,ir_reg.global_reg);

                    remove_out_of_place(misplaced,m);
                    copied_register = true;
                }
            }
        }

        // If we are still not done evict the first register then start copying again
        // As we have allready done copies any register free that we use will not be needed for live out.
        for(u32 m = 0; m < count(misplaced); m++)
        {
            auto& ir_reg = reg_from_slot(misplaced[m],alloc);
            if(is_reg_locally_allocated(ir_reg))
            {
                auto& reg_file = get_register_file(alloc,ir_reg);
                save_reg(alloc,block,block.list.finish,reg_file,ir_reg.local_reg,insert_type);
                break;
            }
        }
    }

    destroy_arr(misplaced);
}


void lock_out_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node,RegisterFile& reg_file,u32 reg)
{
    log_reg(alloc.print,*alloc.table,"Locking out register %s\n",reg_name(alloc.arch,reg));

    // If its allready free we don't have to do anything just claim it
    if(!is_reg_free(reg_file,reg))
    {
        save_reg(alloc,block,node,reg_file,reg,insertion_type::before);
    }

    claim_register(reg_file,reg);
}

void force_into_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node,RegisterFile& reg_file,u32 reg, RegSlot dst)
{
    auto& ir_reg = reg_from_slot(dst,alloc);

    // issue a load
    if(!is_reg_locally_allocated(ir_reg))
    {
        reload_reg(alloc,block,node,dst,reg,insertion_type::before);
    }

    // Copy the register and then free the other one
    else
    {
        const auto copy = make_raw_op(ir_reg.flags & REG_FLOAT? op_type::movf_reg : op_type::mov_reg,reg, ir_reg.local_reg);
        free_ir_reg(ir_reg,reg_file);
        take_local_reg(reg_file,ir_reg,reg);
        insert_at(block.list,node,copy);
    }
}

void lock_into_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node,RegisterFile& reg_file,u32 reg, RegSlot dst)
{
    log_reg(alloc.print,*alloc.table,"Locking %r into %s\n",dst,reg_name(alloc.arch,reg));

    if(is_special_reg(dst))
    {
        assert(reg == special_reg_to_reg(alloc.arch,dst.spec));
        return;
    }

    // Allready in the correct register just lock it down
    if(reg_file.allocated[reg] == dst)
    {
        claim_register(reg_file,reg);
    }

    // Lock out the register and then move it into place
    else 
    {
        lock_out_reg(alloc,block,node,reg_file,reg);
        force_into_reg(alloc,block,node,reg_file,reg,dst);
    }
}


// If this is not allready in the correct register then we will get bugs
void unlock_into_reg(LinearAlloc& alloc,RegisterFile& reg_file, RegSlot dst,u32 reg)
{
    log_reg(alloc.print,*alloc.table,"Unlocking %r into %s\n",dst,reg_name(alloc.arch,reg));

    switch(dst.kind)
    {
        case reg_kind::spec:
        {
            assert(reg == special_reg_to_reg(alloc.arch,dst.spec));
            release_register(reg_file,reg);
            break;
        }

        case reg_kind::sym:
        case reg_kind::tmp:
        {
            auto& ir_reg = reg_from_slot(dst,alloc);

            release_register(reg_file,reg);
            take_local_reg(reg_file,ir_reg,reg);
            break;
        }
    }
}

void unlock_special_reg(LinearAlloc& alloc, spec_reg reg)
{
    const u32 location = special_reg_to_reg(alloc.arch,reg);
    RegisterFile& reg_file = get_register_file(alloc,find_reg_set(reg));

    log_reg(alloc.print,*alloc.table,"Unlocking special register %s\n",spec_reg_name(reg));

    release_register(reg_file,location);
}

void lock_special_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node, spec_reg reg)
{
    const u32 location = special_reg_to_reg(alloc.arch,reg);
    RegisterFile& reg_file = get_register_file(alloc,find_reg_set(reg));

    lock_out_reg(alloc,block,node,reg_file,location);
}

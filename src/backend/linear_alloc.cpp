#include <reg_alloc.h>

struct LinearRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    LocalSlot slot = {INVALID_HANDLE};
    u32 global_reg = REG_FREE;
    BlockSlot block_slot = {INVALID_HANDLE};
};


struct LinearVariable
{
    Array<LinearRange> range;
};

struct LinearProgram
{
    Array<LinearVariable> var;
};

enum class usage_type
{
    use,
    def
};

LinearProgram make_linear_program(Function& func)
{
    LinearProgram program;

    resize(program.var,count(func.local.registers));

    return program;
}

bool is_reg_locally_allocated(const Reg& reg)
{
    return reg.local_reg != REG_FREE;
}

// TODO: This needs to be sped up.
u32 find_reg_allocation(const Reg& ir_reg, u32 pc)
{
    for(const auto& alloc : ir_reg.global_alloc)
    {
        if(in_range(pc,alloc.start,alloc.end))
        {
            return alloc.reg;
        }
    }

    return REG_FREE;
}


bool is_reg_globally_allocated(const Reg& reg, u32 pc)
{
    return find_reg_allocation(reg,pc) != REG_FREE;
}

Reg& reg_from_slot(LocalSlot slot, LinearAlloc& alloc)
{
    return reg_from_local(alloc.local,slot);
}

Reg& reg_from_slot(RegSlot slot, LinearAlloc& alloc)
{
    return reg_from_slot(*alloc.table,alloc.local,slot);
}

bool marked_for_expiry(LinearAlloc& alloc, RegSlot slot);

bool is_reg_free(RegisterFile& regs,u32 reg)
{
    return is_set(regs.free_set & ~regs.locked_set,reg);
}

RegisterFile& get_register_file(LinearAlloc& alloc, reg_type rtype)
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

RegisterFile& get_register_file(LinearAlloc& alloc, Reg& reg)
{
    return get_register_file(alloc,ir_rtype(reg));
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

        log_reg(alloc,"reg %s -> %r\n",reg_name(alloc.arch,i),slot);
    }

    putchar('\n');
}

void mark_used(RegisterFile& regs, u32 reg)
{
    regs.used_set = set_bit(regs.used_set,reg);
}

LinearAlloc make_linear_alloc(b32 print,b32 stack_only, b32 debug, RegTable local, SymbolTable* table,ArenaAllocator* list_allocator,arch_target arch)
{
    LinearAlloc alloc;

    alloc.print = print;
    alloc.stack_alloc = make_stack_alloc(print);

    alloc.arch = arch;
    alloc.local = local;
    alloc.table = table;
    alloc.list_allocator = list_allocator;
    alloc.stack_only = stack_only;
    alloc.debug = debug;


    return alloc;
}

void collapse_ranges(LinearVariable& variable)
{
    // TODO: There will be more efficient ways to do this
    for(u32 r = 0; r < count(variable.range) - 1; r++)
    {
        auto& cur = variable.range[r];
        auto& next = variable.range[r + 1];

        if(in_range(cur.start,next.start,next.end) || in_range(next.start,cur.start,cur.end))
        {
            cur.end = max(cur.end,next.end);
            cur.start = min(cur.start,next.start);
            remove_out_of_place(variable.range,r+1);
        }
    }
}

void update_range(LinearProgram& program, LocalSlot slot, Block& block,u32 pc)
{
    auto& variable = program.var[slot.handle];

    const b32 def = contains(block.def,slot);

    // If this is the first range or we have just hit a def then we need to create a new range
    if(!variable.range || (def && variable.range[count(variable.range) - 1].block_slot != block.block_slot))
    {
        resize_end(variable.range,1);
    }

    auto& range = variable.range[count(variable.range) - 1];
    range.slot = slot;

    if(pc < range.start)
    {
        range.block_slot = block.block_slot;
        range.start = pc;
    }

    range.end = std::max(range.end,pc);
    collapse_ranges(variable);
}

void update_regs_range(LinearProgram& program, Block& block, u64 pc, const ConstSpan<RegSlot>& regs)
{
    for(const auto& reg : regs)
    {
        if(reg.kind != reg_kind::local)
        {
            continue;
        }

        update_range(program,reg.local,block,pc);
    }
}

void number_cfg(Function& func)
{
    u32 pc = 0;

    // Add in increments of two so we can insert 
    // spills/loads on odd addresses.
    for(auto& block : func.emitter.program)
    {
        block.pc_start = pc;
        pc += 2;

        for(auto& node : block.list)
        {
            node.value.pc = pc;
            pc += 2;
        }

        block.pc_end = pc;
    }
}

Array<LinearRange> find_range(Interloper& itl, Function& func)
{
    LinearProgram program = make_linear_program(func);

    // First we need a flat view of the program
    number_cfg(func);


    // for each block
    for(auto& block : func.emitter.program)
    {
        OpcodeNode* node = block.list.start;

        // If we are live in on a block as a arg (for the first time) we have to force it live from block start
        // This is not a problem for locals because they will actually defined and assigned before use.
        // Function args on the other hand are going to be in memory from being passed, and not a reg they may be put in.
        // We can return to this when we have the ability to setup more granular ranges for blocks via block args.
        /*
        live_in: ptr in rax
            lock rax
            mov rax, ptr <-- This is not loaded until here, but the register has already been locked we have a problem
        */

        for(const LocalSlot slot : block.live_in)
        {
            auto& ir_reg = reg_from_slot(itl,func,slot);

            const auto& var = program.var[slot.handle];

            if(is_stack_arg(ir_reg) && !var.range)
            {
                const auto live_op = make_directive_one(directive_type::live_var,make_reg_operand(slot,ir_reg_type::directive));
                node = insert_at(block.list,node,live_op);
            }

            update_range(program,slot,block,block.pc_start);
        }

        // for each opcode
        while(node)
        {
            const auto& opcode = node->value;

            if(opcode.group == op_group::unary_reg2)
            {
                const auto& unary = opcode.unary_reg2;
                switch(unary.type)
                {
                    case unary_reg2_op::mov_gpr_reg:
                    {
                        const auto src = unary.src.ir;
                        const auto dst = unary.dst.ir;
                        if(!is_special_reg(src) && is_special_reg(dst) && spec_reg_is_gpr(dst.spec))
                        {
                            const u32 location = special_reg_to_reg(itl.arch,dst.spec);
                            auto& ir_reg = reg_from_slot(itl,func,src);
                            ir_reg.hint |= (1 << location);
                        }
                        
                        else if(!is_special_reg(dst) && is_special_reg(src) && spec_reg_is_gpr(src.spec))
                        {
                            const u32 location = special_reg_to_reg(itl.arch,src.spec);
                            auto& ir_reg = reg_from_slot(itl,func,dst);
                            ir_reg.hint |= (1 << location);
                        }

                        break;
                    }

                    default: break;
                }
            }

            const auto regs = opcode_ir_reg_span(opcode,itl.reg_span);

            update_regs_range(program,block,opcode.pc,regs.src);
            update_regs_range(program,block,opcode.pc,regs.dst_src);

            // dst only regs have have a later pc
            update_regs_range(program,block,opcode.pc + 1,regs.dst);


            // next opcode
            node = node->next;
        }
        

        // if its live out then consider it used in every exit of use
        // TODO: We may want to optimize this with a bit set and?
        for(const LocalSlot slot : block.live_out)
        {
            for(const BlockSlot exit: block.exit)
            {
                auto& exit_block = block_from_slot(func,exit);
                if(contains(exit_block.use,slot))
                {
                    update_range(program,slot,block,exit_block.pc_start);
                }
            }
        }
    }

    Array<LinearRange> linear_range;

    // TODO: For debugging maybe we should just combine this at the get go
    // for(auto& var : program.var)
    // {
    //     LinearRange final_range;

    //     for(auto& range: var.range)
    //     {
    //         if(range.start < final_range.start)
    //         {
    //             final_range.start = range.start;
    //             final_range.block_slot = range.block_slot;
    //             final_range.slot = range.slot;
    //             final_range.node = range.node;
    //         }

    //         if(range.end > final_range.end)
    //         {
    //             final_range.end = range.end;
    //         }
    //     }

    //     push_var(linear_range,final_range);
    // }

    for(auto& var : program.var)
    {
        for(auto& range: var.range)
        {
            push_var(linear_range,range);
        }
    }

    // finally sort it
    heap_sort(linear_range,[](const LinearRange& v1, const LinearRange& v2)
    {
        return v1.start > v2.start;
    });


    return linear_range;
}

void free_reg(RegisterFile& regs,u32 reg)
{
    regs.free_set = set_bit(regs.free_set,reg);
    regs.allocated[reg] = spec_reg::null;
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

u32 alloc_fixed_reg(RegisterFile& regs, u32 fixed)
{
    if(!is_reg_free(regs,fixed))
    {
        return FFS_EMPTY;
    }

    remove_reg(regs,fixed);
    mark_used(regs,fixed);

    return fixed;
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

void mark_clean(RegisterFile& regs, u32 reg)
{
    regs.dirty = deset_bit(regs.dirty,reg);
}

void mark_dirty(RegisterFile& regs, u32 reg)
{
    regs.dirty = set_bit(regs.dirty,reg);
}

bool is_dirty(RegisterFile& regs, u32 reg)
{
    return is_set(regs.dirty,reg);
}

void assign_local_reg(RegisterFile& regs,Reg& ir_reg, u32 reg)
{
    ir_reg.local_reg = reg;
    regs.allocated[ir_reg.local_reg] = ir_reg.reg_slot;

    // Newly assigned register mark it is clean
    mark_clean(regs,reg);
}

void take_local_reg(RegisterFile& reg_file,Reg& ir_reg, u32 reg)
{
    assign_local_reg(reg_file,ir_reg,reg);
    remove_reg(reg_file, ir_reg.local_reg);
    mark_used(reg_file,ir_reg.local_reg);

    // If we have forced a register presume it is dirty
    mark_dirty(reg_file,reg);   
}

bool alloc_ir_reg(RegisterFile& regs, Reg& ir_reg, u32 pc)
{
    const u32 global_reg = find_reg_allocation(ir_reg,pc);

    // If global register is free prefer its allocation
    if(global_reg != REG_FREE && is_reg_free(regs,global_reg))
    {
        take_local_reg(regs,ir_reg,global_reg);
        mark_clean(regs,global_reg);
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

void reserve_offset(LinearAlloc& alloc, Reg& ir_reg,u32 reg)
{
    log_reg(alloc,"reserve offset for %r in %s\n",ir_reg.reg_slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}


void free_ir_reg(Reg& ir_reg,RegisterFile& regs)
{
    free_reg(regs, ir_reg.local_reg);
    ir_reg.local_reg = REG_FREE;
}

OpcodeNode* insert_relative_opcode(Block& block,OpcodeNode* node,Opcode& opcode,  insertion_type type)
{
    // If this is at the start of the list
    // we need to just use the block pc
    if(!node)
    {
        opcode.pc = block.pc_start;
    }

    else
    {
        opcode.pc = node->value.pc;

        if((opcode.pc & 1) == 0)
        {
            opcode.pc += type == insertion_type::before? -1 : +1;
        }
    }

    return insert_node(block.list,node,opcode,type);
}

OpcodeNode* force_spill(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg, insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);

    log_reg(alloc,"Force spill %r from %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);

    if(is_mem_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,reg);
    }

    auto opcode = make_spill(reg,slot,alloc.stack_alloc.stack_offset);
    return insert_relative_opcode(block,node,opcode,type);
}

// Spill a register to memory
void spill_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg, insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    const u32 size = ir_reg.size * ir_reg.count;

    if(ir_reg.segment == reg_segment::constant)
    {
        log_reg(alloc,"Constant %r deallocated from %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);
        free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
        return;
    }

    // TODO: handle if structs aernt always in memory
    assert(size <= GPR_SIZE);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(is_mem_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,reg);
    }

    log_reg(alloc,"spill %r from %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);

    auto& regs = get_register_file(alloc,ir_reg);

    if(is_dirty(regs,reg) || alloc.stack_only)
    {
        auto opcode = make_spill(reg,slot,alloc.stack_alloc.stack_offset);
        insert_relative_opcode(block,node,opcode,type);
    }

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

    // is actually in a reg and not immediately spilled
    if(is_reg_locally_allocated(ir_reg))
    {
        spill_reg(alloc,block,node,slot,ir_reg.local_reg,type);
    }

    // reserve a space for this for a later spill
    else if(is_mem_unallocated(ir_reg))
    {
        reserve_offset(alloc,ir_reg,REG_FREE);
    }   
}


// TODO: This should look for a register furthest in the future.
// We may have to have a look back over our local allocator
void acquire_local_reg(LinearAlloc& alloc, Reg& ir_reg, RegisterFile& regs,Block& block, OpcodeNode* node)
{
    if(alloc_ir_reg(regs,ir_reg,node->value.pc))
    {
        return;
    }

    // Spill a register for room
    u32 furthest_use = 0;
    RegSlot reg = spec_reg::null;

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
    assert(alloc_ir_reg(regs,ir_reg,node->value.pc));
}

void reset_all_regs(LinearAlloc& alloc)
{
    // All regs free (though this does not imply they are usable)
    alloc.gpr.free_set = 0xffff'ffff;
    alloc.fpr.free_set = 0xffff'ffff;

    // reg is locked until added
    alloc.gpr.locked_set = 0xffff'ffff;
    alloc.fpr.locked_set = 0xffff'ffff;
}

void reset_stack_only_registers(LinearAlloc& alloc)
{
    reset_all_regs(alloc);

    // just add the scratch registers
    add_reg(alloc.gpr,x86_reg::r11);
    add_reg(alloc.gpr,x86_reg::r12);
    add_reg(alloc.gpr,x86_reg::r13);

    add_reg(alloc.fpr,x86_reg::xmm5);
    add_reg(alloc.fpr,x86_reg::xmm6);
    add_reg(alloc.fpr,x86_reg::xmm7);
}

// TODO: we need to dynamically lock the registers for each function
void init_regs(LinearAlloc& alloc)
{
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.gpr.allocated[i] = spec_reg::null;
        alloc.fpr.allocated[i] = spec_reg::null;
    }

    alloc.fpr.dirty = 0;
    alloc.gpr.dirty = 0;

    if(alloc.stack_only)
    {
        reset_stack_only_registers(alloc);
        return;
    }

    reset_all_regs(alloc);

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

// NOTE: this relies on pow2
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

            free_reg(reg_file,cmp.global_reg);
        }
    }

    // copy over the entries, to remove purged ones from the list
    if(i != 0)
    {
        memmove(&active.arr[0],&active.arr[i],(active.size - i) * sizeof(LinearRange));
        active.size -= i;
    }

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

    // copy over by one 
    if(i < active.size)
    {
        memmove(&active.arr[i + 1],&active.arr[i],(active.size - i) * sizeof(LinearRange));
    }

    // write in the entry
    active.arr[i] = cur;
    active.size += 1;
}

RegAllocation reg_alloc_from_range(const LinearRange& cur)
{
    RegAllocation alloc;
    alloc.start = cur.start;
    alloc.end = cur.end;
    alloc.reg = cur.global_reg;

    return alloc;
}

void alloc_range(LinearAlloc& alloc,Interloper& itl, Function& func, ActiveReg& active, LinearRange& cur)
{
    // actually run the allocation
    auto& ir_reg = reg_from_slot(itl,func,cur.slot);


    // expire any dead sets
    clean_dead_reg(itl,func,alloc,active,cur);

    // check which register file to use
    auto& reg_file = get_register_file(alloc,ir_reg);

    u32 reg = alloc_reg(reg_file,ir_reg.hint);

    // we have a register
    if(reg != FFS_EMPTY)
    {
        // set location
        cur.global_reg = reg;

        push_var(ir_reg.global_alloc,reg_alloc_from_range(cur));

        log_reg(alloc,"%r globally allocated to %s\n",RegSlot(cur.slot),X86_NAMES[reg]);

        // add to active register set 
        add_active(active,cur);
    }

    // TODO: do a spill of a existing reg, for now we just default
    // it into memory
    else
    {

    }
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

    // init our register set
    init_regs(alloc);

    // perform the allocation
    for(auto& cur : range)
    {
        // TODO: We should just binary search this and clip the invalid ranges ahead of time.
        if(!is_valid_slot(cur.slot))
        {
            break;
        }

        alloc_range(alloc,itl,func,active,cur);
    }

    destroy_arr(range);
}

void destroy_linear_alloc(LinearAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}

OpcodeNode* reload_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, u32 reg,insertion_type type)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    auto opcode = make_load(reg,slot,alloc.stack_alloc.stack_offset);
    log_reg(alloc,"reload %r to %s (size %d)\n",ir_reg.reg_slot,reg_name(alloc.arch,reg),ir_reg.size);

    // Just loaded this register is clean
    auto& reg_file = get_register_file(alloc,ir_reg);
    mark_clean(reg_file,reg);

    assert(!is_mem_unallocated(ir_reg));

    return insert_relative_opcode(block,node,opcode,type);
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
        log_reg(alloc,"Saving %r from %s\n",file.allocated[reg],reg_name(alloc.arch,reg));
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

void alloc_regs_from_live_in(LinearAlloc& alloc, Block& block)
{
    for(const LocalSlot slot : block.live_in)
    {
        // allocate the register into the appropiate register file
        auto& ir_reg = reg_from_slot(slot,alloc);
        
        const u32 global_reg = find_reg_allocation(ir_reg,block.pc_start);
        if(global_reg != REG_FREE)
        {
            auto& reg_file = get_register_file(alloc,ir_reg);
            log_reg(alloc,"%r is live in on %s\n",ir_reg.reg_slot,reg_name(alloc.arch,global_reg));
            take_local_reg(reg_file,ir_reg,global_reg);
        }
    }
}

void compute_local_uses_span(LinearAlloc& alloc, const ConstSpan<RegSlot>& regs,u32 pc)
{
    for(const auto& reg: regs)
    {
        if(reg.kind == reg_kind::local)
        {
            auto& ir_reg = reg_from_slot(reg.local,alloc);
            push_var(ir_reg.local_uses,pc);
        }
    }
}

void compute_local_uses(LinearAlloc& alloc, Block& block)
{
    u32 pc = 0;
    
    for(const OpcodeNode& node : block.list)
    {
        const auto opcode = node.value;
        const auto regs = opcode_ir_reg_span(opcode,alloc.ir_reg_span);

        compute_local_uses_span(alloc,regs.src,pc);
        compute_local_uses_span(alloc,regs.dst_src,pc);

        pc++;

        compute_local_uses_span(alloc,regs.dst,pc);

        pc++;
    }    
}

void linear_setup_new_block(LinearAlloc& alloc, Block& block) 
{
    // clean any local registers that aren't live in, so the register is in a correct state
    for(const LocalSlot slot : block.def)
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
    alloc_regs_from_live_in(alloc,block);
    compute_local_uses(alloc,block);
}


bool marked_for_expiry(LinearAlloc& alloc, RegSlot slot)
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
    RegSlot locked_regs[3];
    u32 locked_count = 0;

    while(alloc.dead_count)
    {
        const RegSlot slot = alloc.dead_slot[--alloc.dead_count];
        auto& ir_reg = reg_from_slot(slot,alloc);

        auto& reg_file = get_register_file(alloc,ir_reg);

        if(is_locked(reg_file,ir_reg.local_reg))
        {
            locked_regs[locked_count++] = slot;
            continue;
        }

        log_reg(alloc,"Expiring %r from %s\n",ir_reg.reg_slot,reg_name(alloc.arch,ir_reg.local_reg));

        clear_arr(ir_reg.local_uses);


        if(is_reg_locally_allocated(ir_reg))
        {
            free_ir_reg(ir_reg,reg_file);
        }
    }
    
    memcpy(alloc.dead_slot,locked_regs,locked_count * sizeof(RegSlot));
    alloc.dead_count = locked_count;
}

lowered_reg_t allocate_and_rewrite_var_stack(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, reg_arg_kind arg_kind)
{
    const b32 is_dst = is_arg_dst(arg_kind);
    const b32 is_src = is_arg_src(arg_kind);

    auto& ir_reg = reg_from_slot(slot,alloc);
    auto& reg_file = get_register_file(alloc,ir_reg);

    const u32 scratch_reg = alloc_reg(reg_file,0);
    assert(scratch_reg != FFS_EMPTY);

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

    return scratch_reg;
}

lowered_reg_t allocate_var(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot slot, reg_arg_kind arg_kind)
{
    auto& ir_reg = reg_from_slot(slot,alloc);
    auto& reg_file = get_register_file(alloc,ir_reg);


    const b32 is_dst = is_arg_dst(arg_kind);
    const b32 is_src = is_arg_src(arg_kind);

    // just rewrite the register simply
    if(alloc.stack_only)
    {
        return allocate_and_rewrite_var_stack(alloc,block,node,slot,arg_kind);
    }

    ir_reg.cur_local_uses++;


    // Acquire a new register and reload it
    if(!is_reg_locally_allocated(ir_reg))
    {
        acquire_local_reg(alloc,ir_reg,reg_file,block,node);
        log_reg(alloc,"Allocated %s to %r\n",reg_name(alloc.arch,ir_reg.local_reg),ir_reg.reg_slot);

        // src do a reload
        if(is_src) 
        {
            // issue a load
            reload_reg(alloc,block,node,slot, ir_reg.local_reg,insertion_type::before);
        }
    }

    // Local uses have expired and it does not live beyond this block we can free the fregister
    if(ir_reg.cur_local_uses >= count(ir_reg.local_uses))
    {
        ir_reg.cur_local_uses = 0;

        // We still need to clear out the local uses!
        if(contains(block.live_out,ir_reg.reg_slot))
        {
            clear_arr(ir_reg.local_uses);
        }

        else
        {
            log_reg(alloc,"Mark %r in %s for expiry\n",ir_reg.reg_slot,reg_name(alloc.arch,ir_reg.local_reg));

            // Don't terminate the regs during it as we need them allocated
            alloc.dead_slot[alloc.dead_count++] = ir_reg.reg_slot;
        }            
    }


    const u32 reg = ir_reg.local_reg;

    if(is_dst)
    {
        // If we have just loaded from memory then this is not dirty?
        mark_dirty(reg_file,ir_reg.local_reg);
        
        if(!is_local_reg(ir_reg))
        {
            // if dst and not a local register it needs to be spilled
            spill(alloc,block,node,slot,insertion_type::after);
        }
    }

    return reg;
}

lowered_reg_t allocate_special_reg(LinearAlloc& alloc, Block& block, OpcodeNode* node, spec_reg spec, reg_arg_kind arg_kind)
{
    const lowered_reg_t location = special_reg_to_reg(alloc.arch,spec);

    // This is not a real hardware register don't attempt an allocation
    // Or we are doing stack allocation and these are all reserved
    if(location >= SPECIAL_REG_START || alloc.stack_only)
    {
        return location;
    }

    auto& reg_file = get_register_file(alloc,spec_rtype(spec));

    const b32 is_dst = is_arg_dst(arg_kind);

    // Whatever is in a special reg is about to be clobbered
    if(is_dst)
    {
        const RegSlot saved_slot = reg_file.allocated[location];

        // Check reg is locked to prevent regs we never allocate from attempting to be saved
        if(!is_reg_free(reg_file,location) && !is_locked(reg_file,location) && !marked_for_expiry(alloc,saved_slot))
        {
            auto& ir_reg = reg_from_slot(saved_slot,alloc);

            log_reg(alloc,"Saving %r clobbered in special reg %s (%s)\n",saved_slot,
                spec_reg_name(spec),reg_name(alloc.arch,ir_reg.local_reg));

            // Save the register if it has a later use.
            // But feel free to leave the current location for this rewrite
            // As the dst is last thing rewritten 
            save_reg(alloc,block,node,reg_file,ir_reg.local_reg,insertion_type::before);
        }

        lock_reg(reg_file,location);
        mark_dirty(reg_file,location);
    }

    // make sure the spec regs are marked as used
    mark_used(reg_file,location);
    return location;
}


lowered_reg_t linear_allocate_reg(LinearAlloc& alloc,Block& block,OpcodeNode* node, RegSlot reg, reg_arg_kind arg_kind)
{
    switch(reg.kind)
    {
        case reg_kind::local:
        case reg_kind::global:
        {
            return allocate_var(alloc,block,node,reg,arg_kind);
        }

        case reg_kind::spec:
        {
            return allocate_special_reg(alloc,block,node,reg.spec,arg_kind);
        }
    }

    assert(false);
}

void linear_allocate_reg_span(LinearAlloc& alloc,Block& block,OpcodeNode* node, const ConstSpan<RegSlot>& regs, 
    reg_arg_kind arg_kind, Span<lowered_reg_t>& lowered)
{
    for(auto& reg : regs)
    {
        lowered[lowered.size++] = linear_allocate_reg(alloc,block,node,reg,arg_kind);
    }
}

ConstLoweredRegSpan linear_allocate_registers(LinearAlloc& alloc,Block& block,OpcodeNode* node, const ConstIrRegSpan& ir_reg,LoweredRegSpan& lowered_reg)
{
    blank_lowered_reg_span(lowered_reg);

    linear_allocate_reg_span(alloc,block,node,ir_reg.src,reg_arg_kind::src,lowered_reg.src);
    linear_allocate_reg_span(alloc,block,node,ir_reg.dst_src,reg_arg_kind::dst_src,lowered_reg.dst_src);

    clean_dead_regs(alloc);
    linear_allocate_reg_span(alloc,block,node,ir_reg.dst,reg_arg_kind::dst,lowered_reg.dst);

    // Just continue using debug registers
    if(alloc.stack_only)
    {
        reset_stack_only_registers(alloc);
    }

    return lowered_reg;
}

void finish_alloc(Reg& reg,LinearAlloc& alloc)
{
    log_reg(alloc,"pending stack alloc %r\n",reg.reg_slot);
    assert(pending_stack_allocation(reg));

    finalise_offset(alloc.stack_alloc,reg);

    log_reg(alloc,"final offset %r = [%x,%x] -> 0x%x\n",reg.reg_slot,reg.size,reg.count,reg.offset);
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

struct MisplacedRegister
{
    RegSlot slot;
    u32 local = REG_FREE;
    u32 global = REG_FREE;
};

// TODO: This is not a good format for attempting swaps as we want to be able to reference misplaced by register
// but leave it for now as we are tying to get a simple impl off the ground.
struct MisplacedAllocationFile
{
    RegSlot allocated[MACHINE_REG_SIZE];
    Array<MisplacedRegister> misplaced;
};

struct MisplacedAllocation
{
    MisplacedAllocationFile gpr;
    MisplacedAllocationFile fpr;
};

// TODO: I would suspect we are going to need proper critical edge splitting!
// TODO: Consider using xchg to swap them

// TODO: We are going to write a really bad version of this to begin with because it will make life easier for testing
// we can write one that generates nice code later.

void correct_misplaced_register_file(LinearAlloc& alloc, Block& block, OpcodeNode* node, MisplacedAllocationFile& reg_file, insertion_type type)
{
    while(reg_file.misplaced)
    {
        const MisplacedRegister misplaced = pop(reg_file.misplaced);

        const RegSlot slot = misplaced.slot;
        const u32 global = misplaced.global;
        const u32 local = misplaced.local;

        // Spill anything occupying the reg we want
        if(!is_null_reg(reg_file.allocated[global]))
        {
            node = force_spill(alloc,block,node,reg_file.allocated[global],global,type);
            type = insertion_type::after;

            reg_file.allocated[global] = spec_reg::null;
        }

        // Spill the local register we are holding. 
        if(local != REG_FREE && reg_file.allocated[local] == slot)
        {
            node = force_spill(alloc,block,node,slot,local,type);
            type = insertion_type::after;

            reg_file.allocated[local] = spec_reg::null;
        }

        // reload into the correct reg
        node = reload_reg(alloc,block,node,slot,global,type);
        type = insertion_type::after;

        reg_file.allocated[global] = slot;
    }

}

void correct_misplaced_registers(LinearAlloc& alloc, Block& block, OpcodeNode* node, MisplacedAllocation& reg_alloc, insertion_type type)
{
    correct_misplaced_register_file(alloc,block,node,reg_alloc.gpr,type);
    correct_misplaced_register_file(alloc,block,node,reg_alloc.fpr,type);
}


void disass_opcode(LinearAlloc& alloc, const Opcode& opcode)
{
    Disass disass = Disass(*alloc.table,alloc.local,alloc.arch);
    disass_opcode(opcode,disass);
}

BlockSlot new_basic_block(LinearAlloc& alloc, Function& func)
{
    return new_basic_block(func,*alloc.table,alloc.list_allocator);
}

// TODO: This list has a low number of items but we may want to switch to a bitset?
void replace_block_link(Array<BlockSlot>& list, BlockSlot target, BlockSlot replacement)
{
    for(auto& v : list)
    {
        if(v == target)
        {
            target = replacement;
        }
    }
}

Block& split_exit(LinearAlloc& alloc, Function& func, BlockSlot entry_slot,LabelSlot& entry_label, BlockSlot exit_slot)
{
    // NOTE: We need to take slots as this will invalidate all the old refs
    const BlockSlot split_block_slot = new_basic_block(alloc,func);
    auto& split_block = block_from_slot(func,split_block_slot);
    split_block.flags |= SPLIT_BLOCK;

    auto& entry = block_from_slot(func,entry_slot);
    auto& exit = block_from_slot(func,exit_slot);

    emit_branch(func,split_block.block_slot,exit.block_slot);  
    split_block.pc_start = exit.pc_end - 1;
    split_block.pc_end = split_block.pc_start;
    
    entry_label = split_block.label_slot;
    replace_block_link(entry.exit,exit.block_slot,split_block.block_slot);
    replace_block_link(exit.entry,entry.block_slot,split_block.block_slot);

    return split_block;
}

void correct_critical_edge_registers(Function& func, LinearAlloc& alloc,Block& entry, LabelSlot& entry_label, 
    Block& exit, MisplacedAllocation& reg_alloc)
{
    if(exit.label_slot == entry_label)
    {
        auto& split_block = split_exit(alloc,func,entry.block_slot,entry_label,exit.block_slot);
        correct_misplaced_registers(alloc,split_block,split_block.list.start,reg_alloc,insertion_type::before);
    }

    // This is via the fall, we can probably get away with putting fixup code after the branch.
    // By putting a block with pc + exit
    // TODO: We should insert an extra fall block between these.
    else
    {
        correct_misplaced_registers(alloc,entry,entry.list.finish,reg_alloc,insertion_type::after);
    }
}

void correct_misplaced_block(Function& func, LinearAlloc& alloc,Block& entry, Block& exit, MisplacedAllocation& reg_alloc)
{
    // single exit
    if(count(entry.exit) == 1)
    {
        correct_misplaced_registers(alloc,entry,entry.list.finish,reg_alloc,insertion_type::after);
    }

    // single entry
    else if(count(exit.entry) == 1)
    {
        correct_misplaced_registers(alloc,exit,exit.list.start,reg_alloc,insertion_type::before);
    }

    // critical edge (rewrite the blocks)
    // TODO: For now we will dump this on the end of the blocks
    // we should reordering of the blocks at some stage but it shouldn't matter...

    // TODO: Technically live out can be broken in the same way across several blocks.
    // but we will discount this for now
    else
    {
        auto last = entry.list.finish;
        auto& opcode = last->value;

        switch(opcode.group)
        {
            case op_group::branch_cmp_flag:
            {
                correct_critical_edge_registers(func,alloc,entry,opcode.branch_cmp_flag.label,exit,reg_alloc);
                break;
            }

            default:
            {
                disass_opcode(alloc,opcode);
                unimplemented("Cannot split critical edge");
                break;           
            }
        }
    }
}

void reset_misplaced_allocation_file(const RegisterFile& reg_file,MisplacedAllocationFile& reg_alloc)
{
    memcpy(reg_alloc.allocated,reg_file.allocated,sizeof(reg_file.allocated));
    clear_arr(reg_alloc.misplaced);
}

void reset_misplaced_allocation(const LinearAlloc& alloc,MisplacedAllocation& reg_alloc)
{
    reset_misplaced_allocation_file(alloc.fpr,reg_alloc.fpr);
    reset_misplaced_allocation_file(alloc.gpr,reg_alloc.gpr);
}

void correct_live_out(Function& func, LinearAlloc& alloc, BlockSlot entry_slot)
{
    // No need to correct across blocks
    if(alloc.stack_only)
    {
        return;
    }

    Array<BlockSlot> exits;

    // This will get invalidated dont leak it
    {
        const auto& entry = block_from_slot(func,entry_slot);

        // nothing to correct
        if(!entry.list.start || entry.flags & HAS_FUNC_EXIT)
        {
            return;
        }

        exits = entry.exit;
    }

    MisplacedAllocation reg_alloc;

    for(const auto& exit_slot : exits)
    {
        auto& exit = block_from_slot(func,exit_slot);
        reset_misplaced_allocation(alloc,reg_alloc);

        for(const auto& local : exit.live_in)
        {
            auto& ir_reg = reg_from_local(func,local);
            const u32 global_reg = find_reg_allocation(ir_reg,exit.pc_start);
            if(global_reg != REG_FREE &&  global_reg != ir_reg.local_reg)
            {   
                auto& misplaced_file = ir_rtype(ir_reg) == reg_type::fpr? reg_alloc.fpr : reg_alloc.gpr;

                const MisplacedRegister misplaced = {ir_reg.reg_slot,ir_reg.local_reg,global_reg};
                push_var(misplaced_file.misplaced,misplaced);

                log_reg(alloc,"misplaced %r(%s,%s) -> L%d\n",misplaced.slot,reg_name(alloc.arch,misplaced.global),
                    reg_name(alloc.arch,misplaced.local),exit.label_slot.handle);
            }
        }

        if(reg_alloc.gpr.misplaced || reg_alloc.fpr.misplaced)
        {
            auto& entry = block_from_slot(func,entry_slot);
            correct_misplaced_block(func,alloc,entry,exit,reg_alloc);
        }
    }
}


void lock_out_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node,RegisterFile& reg_file,u32 reg)
{
    log_reg(alloc,"Locking out register %s\n",reg_name(alloc.arch,reg));

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
        const auto copy = make_mov_reg_lowered_instr(reg, ir_reg.local_reg,rtype_from_ir(ir_reg));
        free_ir_reg(ir_reg,reg_file);
        take_local_reg(reg_file,ir_reg,reg);
        insert_at(block.list,node,copy);
    }
}

void lock_into_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node,RegisterFile& reg_file,u32 reg, RegSlot dst)
{
    log_reg(alloc,"Locking %r into %s\n",dst,reg_name(alloc.arch,reg));

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
    if(marked_for_expiry(alloc,dst))
    {
        log_reg(alloc,"Expiring unlocked reg %r(%s)\n",dst,reg_name(alloc.arch,reg));
        release_register(reg_file,reg);

        return;
    }

    log_reg(alloc,"Unlocking %r into %s\n",dst,reg_name(alloc.arch,reg));

    switch(dst.kind)
    {
        case reg_kind::spec:
        {
            assert(reg == special_reg_to_reg(alloc.arch,dst.spec));
            release_register(reg_file,reg);
            break;
        }

        case reg_kind::local:
        case reg_kind::global:
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
    RegisterFile& reg_file = get_register_file(alloc,spec_rtype(reg));

    log_reg(alloc,"Unlocking special register %s\n",spec_reg_name(reg));

    unlock_reg(reg_file,location);
}

void lock_special_reg(LinearAlloc& alloc,Block& block, OpcodeNode* node, spec_reg reg)
{
    const u32 location = special_reg_to_reg(alloc.arch,reg);
    RegisterFile& reg_file = get_register_file(alloc,spec_rtype(reg));

    lock_out_reg(alloc,block,node,reg_file,location);
}

// our bitset can only store 32 regs
static_assert(MACHINE_REG_SIZE <= 32);

struct RegAlloc
{
    // register allocation
    // is this free or does it hold a var?
    SymSlot regs[MACHINE_REG_SIZE];  

    u32 free_regs;

    // free list for register allocator
    u32 free_list[MACHINE_REG_SIZE];

    // keep track of freeable regs
    SymSlot dead_slot[MACHINE_REG_SIZE] = {0};
    u32 dead_count = 0;

    // bitset of which regs this functions needs to use
    // for now we are going to just callee save every register
    u32 used_regs;
    u32 use_count;
   
    b32 print = false;
};

RegAlloc make_reg_alloc(b32 print)
{
    RegAlloc alloc;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;
    alloc.use_count = 0;
    alloc.used_regs = 0;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = {REG_FREE};
        alloc.free_list[i] = i;
    }

    alloc.print = print;

    return alloc;
}

void print_reg_alloc(RegAlloc &alloc,SymbolTable& table)
{
    printf("\n\nallocation:\n\n");

    printf("total registers: %d\n",MACHINE_REG_SIZE);
    printf("free registers: %d\n",alloc.free_regs);
    printf("used regsisters: %d\n",MACHINE_REG_SIZE - alloc.free_regs);
    printf("total used registers: %d\n",alloc.use_count);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        const SymSlot slot = alloc.regs[i];

        if(slot.handle == REG_FREE)
        {
            continue;
        }

        if(is_tmp(slot))
        {
            printf("reg r%d -> temp t%d\n",i,slot.handle);
        }

        else if(is_sym(slot))
        {
            const auto &sym = sym_from_slot(table,slot);
            printf("reg r%d -> sym %s\n",i,sym.name.buf);
        }
    }

    putchar('\n');

}

void print_uses(Reg& ir_reg)
{
    printf("cur use: %d\n",ir_reg.uses);

    for(u32 i = 0; i < count(ir_reg.usage); i++)
    {
        printf("use: %x\n",ir_reg.usage[i]);
    }    
}


b32 is_mem_unallocated(Reg& reg)
{
    return reg.offset == UNALLOCATED_OFFSET;
}

b32 is_mem_allocated(Reg& reg)
{
    return reg.offset != UNALLOCATED_OFFSET;
}

b32 is_stack_unallocated(Reg& reg)
{
    return is_mem_unallocated(reg) && (reg.kind == reg_kind::local || reg.kind == reg_kind::tmp);
}


b32 pending_stack_allocation(Reg& reg)
{
    return reg.flags & PENDING_STACK_ALLOCATION;
}


b32 is_stored_in_mem(Reg& reg)
{
    return reg.flags & STORED_IN_MEM;
}

b32 is_var(SymSlot slot)
{
    return is_tmp(slot) || is_sym(slot);
}

b32 is_callee_saved(u32 reg)
{
    return reg >= R1 && reg < MACHINE_REG_SIZE;
}

void free_reg_internal(RegAlloc& alloc, Reg& ir_reg)
{
    const u32 reg = ir_reg.location;

    ir_reg.location = LOCATION_MEM;

    // add back to the free list
    alloc.regs[reg] = sym_from_idx(REG_FREE);
    alloc.free_list[alloc.free_regs++] = reg;     
}

void mark_used(RegAlloc& alloc, u32 reg)
{
    alloc.use_count += !is_set(alloc.used_regs,reg);
    alloc.used_regs = set_bit(alloc.used_regs,reg);
}

u32 alloc_reg(Reg& ir_reg,RegAlloc& alloc)
{
    const u32 reg = alloc.free_list[--alloc.free_regs];
    
    mark_used(alloc,reg);

    ir_reg.location = reg;
    alloc.regs[reg] = ir_reg.slot;

    return reg;
}

bool request_reg(RegAlloc& alloc, u32 req_reg)
{
    // attempt to swap rv to the end of the free list
    for(u32 r = 0; r < alloc.free_regs; r++)
    {
        if(alloc.free_list[r] == req_reg)
        {
            std::swap(alloc.free_list[r],alloc.free_list[alloc.free_regs - 1]);
            return true;
        }
    }

    return false;
}

bool allocate_into_rv(RegAlloc& alloc,Reg& ir_reg)
{
    if(request_reg(alloc,RV))
    {
        assert(alloc_reg(ir_reg,alloc) == RV);
        return true;
    }

    return false;
}


u32 special_reg_to_reg(SymSlot slot)
{
    switch(slot.handle)
    {
        case SP_IR: return SP;
        case RV_IR: return RV; 
        case R0_IR: return R0;
        case R1_IR: return R1; 
        case R2_IR: return R2; 
        case R3_IR: return R3;

        default: crash_and_burn("unhandled special reg %x\n",slot); 
    }    
}

void check_dead_reg(RegAlloc& alloc, Reg& ir_reg)
{
    // if this is its last use schedule it for cleanup
    if(ir_reg.uses == count(ir_reg.usage))
    {
        alloc.dead_slot[alloc.dead_count++] = ir_reg.slot;
    }   
}

void mark_lifetimes(Function& func,Array<Reg> &tmp_regs, SymbolTable& table)
{
    // clear lifetimes for all global vars
    // because we dont have a multi procedure allocator...
    for(u32 g = 0; g < count(table.global); g++)
    {
        const auto slot = table.global[g];
        auto& sym = sym_from_slot(table,slot);
        clear_arr(sym.reg.usage);
    }

    u32 pc = 0;

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        
        List& list = block.list;

        ListNode *node = list.start;

        while(node)
        {
            const auto opcode = node->opcode;

            const auto info = info_from_op(opcode);


            // make sure our src var's are loaded
            // mark if any are tmp's we can reuse to allocate the dst
            for(u32 a = 0; a < info.args; a++)
            {
                // only interested in registers
                if(info.type[a] != arg_type::src_reg && info.type[a] != arg_type::dst_reg)
                {
                    continue;
                }

                const SymSlot slot = sym_from_idx(opcode.v[a]);

                if(is_special_reg(slot))
                {
                    continue;
                }

                auto& reg = reg_from_slot(table,tmp_regs,slot);
                push_var(reg.usage,pc);
            }

            node = node->next;
            pc++;
        }
    }    
}

std::pair<u32,u32> reg_offset(Interloper& itl,const Reg& ir_reg, u32 stack_offset)
{
    UNUSED(itl);

    switch(ir_reg.kind)
    {
        case reg_kind::local:
        case reg_kind::tmp:
        {
            const u32 offset = ir_reg.offset + stack_offset;
            return std::pair{SP,offset};
        }

        case reg_kind::constant:
        {
            const u32 handle = ir_reg.offset;

            const PoolSlot pool_slot = pool_slot_from_idx(handle);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            return std::pair{CONST_IR,section.offset};
        }

        case reg_kind::global:
        {
            return std::pair{GP_IR,ir_reg.offset};
        }
    }

    assert(false);
}
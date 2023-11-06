// our bitset can only store 32 regs
static_assert(MACHINE_REG_SIZE <= 32);

// is this how we want it?
// or should we just pass stuff through one by one?
struct LocalAlloc
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

    // what instruction are we on?
    u32 pc = 0;


    // debug (TODO: make this a command line flag)
    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false;

    // allcation info of tmp's for current function
    // NOTE: this is owned by the func and we dont have to free it
    Array<Reg> tmp_regs;


    // stack allocation
    Array<ArrayAllocation> array_allocation;


    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset;

    // where does each section for alloc start?
    u32 stack_alloc[4];

    // how much of each type of var is there at the momemnt?
    u32 size_count[4];

    // what is the maximum ammount of vars?
    // this will be used to compute the stack size later
    u32 size_count_max[4];

    // what is the total ammount of space that this functions stack requires!
    u32 stack_size;

    Array<SymSlot> pending_allocation;
};

LocalAlloc make_local_alloc(b32 print_reg_allocation,b32 print_stack_allocation, Array<Reg> tmp)
{
    LocalAlloc alloc;

    alloc.print_reg_allocation = print_reg_allocation;
    alloc.print_stack_allocation = print_stack_allocation;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;
    alloc.use_count = 0;
    alloc.used_regs = 0;
    alloc.pc = 0;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = {REG_FREE};
        alloc.free_list[i] = i;
    }

    alloc.tmp_regs = tmp;

    memset(alloc.stack_alloc,0,sizeof(alloc.stack_alloc));

    memset(alloc.size_count_max,0,sizeof(alloc.size_count_max));
    memset(alloc.size_count,0,sizeof(alloc.size_count));
    alloc.stack_size = 0;
    alloc.stack_offset = 0;    

    return alloc;
}

void destroy_local_alloc(LocalAlloc& alloc)
{
    destroy_arr(alloc.array_allocation);
    destroy_arr(alloc.pending_allocation);
}


void rewrite_reg(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg);

void print_alloc(LocalAlloc &alloc,SymbolTable& table)
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


Reg& reg_from_slot(SymSlot slot, SymbolTable& table, LocalAlloc& alloc)
{
    // bind the allocation info into the slot
    if(is_tmp(slot))
    {
        return alloc.tmp_regs[slot.handle];
    }

    // sym
    else
    {
        auto& sym = sym_from_slot(table,slot);
        return sym.reg;
    }    
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

// NOTE: this just reserves stack space,
// finish_stack_alloc has to be called first before usage
u32 stack_reserve_internal(LocalAlloc& alloc, u32 size, u32 count)
{
    const u32 idx = log2(size);
    const u32 cur = alloc.size_count[idx];


    alloc.size_count[idx] += count;
    alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);

    return cur;    
}

u32 allocate_stack_array(LocalAlloc& alloc,SymbolTable& table ,SymSlot slot, u32 size, u32 alloc_count)
{
    ArrayAllocation allocation;
    allocation.slot = slot;
    allocation.size = size;
    allocation.count = alloc_count;
    allocation.stack_offset = alloc.stack_offset;
    allocation.offset = stack_reserve_internal(alloc,size,alloc_count);

    const u32 idx = count(alloc.array_allocation);

    if(alloc.print_stack_allocation)
    {
        auto& sym = sym_from_slot(table,slot);
        printf("initial array stack offset: %s [%x,%x] -> %x\n",sym.name.buf,size,alloc_count,allocation.offset);
    }

    push_var(alloc.array_allocation,allocation);

    return idx;
}

void stack_reserve_reg(LocalAlloc& alloc, Reg& ir_reg)
{
    // if we attempt to reserve space for a global we have trouble
    assert(ir_reg.kind == reg_kind::local || ir_reg.kind == reg_kind::tmp);

    ir_reg.offset = stack_reserve_internal(alloc,ir_reg.size,ir_reg.count);

    ir_reg.flags |= PENDING_STACK_ALLOCATION;

    log(alloc.print_stack_allocation,"initial stack offset for register %x at %x allocated\n",ir_reg.slot.handle,ir_reg.offset);

    // mark this so we can finalise these later
    push_var(alloc.pending_allocation,ir_reg.slot);
}

void stack_reserve_slot(LocalAlloc& alloc,SymbolTable table, SymSlot slot)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    if(is_sym(slot))
    {
        auto& sym = sym_from_slot(table,slot);

        log(alloc.print_stack_allocation,"initial offset allocated %s: [%x,%x] -> %x\n",sym.name.buf,ir_reg.size,ir_reg.count,ir_reg.offset);    
    }

    else
    {
        // by defintion a tmp has to be local
        log(alloc.print_stack_allocation,"initial offset allocated t%d: [%x,%x] -> %x\n",slot.handle,ir_reg.size,ir_reg.count,ir_reg.offset);
    }

    stack_reserve_reg(alloc,ir_reg);    
}

void spill(SymSlot slot,LocalAlloc& alloc,SymbolTable& table,Block &block,ListNode* node, b32 after = false);

b32 is_var(SymSlot slot)
{
    return is_tmp(slot) || is_sym(slot);
}

void spill_all(LocalAlloc &alloc, SymbolTable& table, Block& block, ListNode* node, bool after)
{
    if(alloc.print_reg_allocation)
    {
        puts("spilling everything"); 
    }
    
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.regs[r];

        if(is_var(slot))
        {
            spill(slot,alloc,table,block,node,after);
        }        
    }
}

// TODO: we probably need to cache this in a bitset so we dont have to loop this every time?
void spill_func_bounds(LocalAlloc& alloc, SymbolTable& table, Block&  block, ListNode* node)
{
    // spill any aliased or global regs when we traverse a function boundary
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.regs[r];

        if(is_sym(slot))
        {
            auto& sym = sym_from_slot(table,slot);

            if(is_aliased(sym.reg) || sym.reg.kind == reg_kind::global)
            {
                spill(slot,alloc,table,block,node,false);
            }
        } 
    }   
}

void free_reg_internal(LocalAlloc& alloc, Reg& ir_reg)
{
    const u32 reg = ir_reg.location;

    ir_reg.location = LOCATION_MEM;

    // add back to the free list
    alloc.regs[reg] = sym_from_idx(REG_FREE);
    alloc.free_list[alloc.free_regs++] = reg;     
}

void free_reg(Reg& ir_reg, SymbolTable& table,LocalAlloc& alloc)
{
    // this register is allready in memory
    if(ir_reg.location == LOCATION_MEM)
    {
        return;
    }

    const u32 reg = ir_reg.location;

    if(is_sym(ir_reg.slot))
    {
        auto& sym = sym_from_slot(table,ir_reg.slot);

        log(alloc.print_reg_allocation,"freed symbol %s from reg r%d\n",sym.name.buf,reg);
    }


    else
    {
    #if 0
        print_uses(ir_reg);
    #endif
        log(alloc.print_reg_allocation,"freed tmp t%d from reg r%d\n",ir_reg.slot,reg);               
    }

    assert(!is_aliased(ir_reg));

    free_reg_internal(alloc,ir_reg);
}

u32 alloc_reg(LocalAlloc& alloc)
{
    return alloc.free_list[--alloc.free_regs];
}

void clean_dead_regs(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node, b32 after = false);

void trash_reg(SymbolTable& table, LocalAlloc& alloc, Block& block, ListNode* node)
{
    u32 max_gap = 0;
    u32 max_reg = REG_FREE;

    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.regs[r];

        if(is_var(slot))
        {
            auto& ir_reg = reg_from_slot(slot,table,alloc);

            // we haven't found something that can be freed yet...
            const u32 cur_gap = ir_reg.usage[ir_reg.uses] - alloc.pc;

            // Find what reg will not be used for the longest
            // NOTE: we can probably improve what factors are at play here
            // e.g the number of accesses to come
            // this is just nice and simple
            if(cur_gap > max_gap)
            {
                max_gap = cur_gap;
                max_reg = r;
            }    
        }
    }

    assert(max_reg != REG_FREE);

    spill(alloc.regs[max_reg],alloc,table,block,node);
}

void alloc_internal(Reg& ir_reg, SymbolTable& table,LocalAlloc &alloc,Block& block, ListNode* node)
{
    u32 reg = REG_FREE;

    // evict a register to make space
    if(!alloc.free_regs)
    {
        trash_reg(table,alloc,block,node);
    }

    // give back a register from the free list
    reg = alloc_reg(alloc);

    // mark as used by the function
    alloc.use_count += !is_set(alloc.used_regs,reg);
    alloc.used_regs = set_bit(alloc.used_regs,reg);




    const SymSlot slot = ir_reg.slot;

    ir_reg.location = reg;
    alloc.regs[reg] = slot;

   
    if(alloc.print_reg_allocation)
    {
        if(is_sym(slot))
        {
            auto& sym = sym_from_slot(table,slot);
            printf("symbol %s allocated into reg r%d\n",sym.name.buf,reg);
        }

        else
        {
            printf("tmp t%d allocated into reg r%d\n",slot.handle,reg);
        }
    }
}

void reload_slot(LocalAlloc& alloc, Block& block, ListNode* node, Reg& ir_reg)
{
    // we need to save the current stack offset here 
    // as by the time we load it it may be different
    const auto opcode = Opcode(op_type::load,ir_reg.location,ir_reg.slot.handle,alloc.stack_offset);
    insert_at(block.list,node,opcode); 

    ir_reg.dirty = false;    
}

void allocate_slot(SymbolTable& table, LocalAlloc& alloc, Block& block, ListNode* node, Reg& ir_reg, b32 is_src)
{
    // is this thing allocated?
    if(ir_reg.location == LOCATION_MEM)
    {
        // reallocate the register
        alloc_internal(ir_reg, table, alloc, block, node);

        // if its a src we need to reload it from spill
        if(is_src)
        {
            reload_slot(alloc,block,node,ir_reg);
        }
    }   
}


void clean_dead_reg(SymbolTable& table, LocalAlloc& alloc, Block& block, ListNode* node, SymSlot slot, b32 after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // scope extends beyond this last use i.e because its in a loop
    const b32 used_beyond = contains(block.live_out,slot); 

    // if a pointer is taken to this, 
    // or if we are in a loop and a reg scope extends past loop
    if(is_aliased(ir_reg) || ir_reg.kind == reg_kind::global || used_beyond)
    {
        spill(slot,alloc,table,block,node,after);
    }

        
    // No way to access it, get rid of the reg
    else
    {
        free_reg(ir_reg,table,alloc);
    }
}

// NOTE: use this to force rewrites of directives
void rewrite_reg_internal(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const SymSlot slot = sym_from_idx(opcode.v[reg]);

    // dont rewrite any special purpose reg
    // NOTE: for now assume this is running under the interpretter
    // so its converted to our interrpetter regs and not a hardware target
    if(is_special_reg(slot))
    {
        switch(slot.handle)
        {
            case SP_IR: opcode.v[reg] = SP; break;
            case RV_IR: opcode.v[reg] = RV; break;
            case R0_IR: opcode.v[reg] = R0; break;
            case R1_IR: opcode.v[reg] = R1; break;
            case R2_IR: opcode.v[reg] = R2; break;

            default: crash_and_burn("unhandled special reg %x\n",slot); break;
        }
    } 

    else
    {
        auto& ir_reg = reg_from_slot(slot,table,alloc);
        assert(ir_reg.location < MACHINE_REG_SIZE);

        opcode.v[reg] = ir_reg.location;
    }         
}

void rewrite_reg(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    if(info.type[reg] == arg_type::src_reg || info.type[reg] == arg_type::dst_reg)
    {
        rewrite_reg_internal(table,alloc,opcode,reg);
    }
}

void rewrite_regs(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode)
{   
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    for(u32 r = 0; r < info.args; r++)
    {
        rewrite_reg(table,alloc,opcode,r);
    }
}


void allocate_and_rewrite(SymbolTable& table,LocalAlloc& alloc,Block& block, ListNode* node,u32 reg)
{
    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    const SymSlot slot = sym_from_idx(node->opcode.v[reg]);

    // special purpose ir reg dont allocate just rewrite it
    if(is_special_reg(slot))
    {
        rewrite_reg(table,alloc,node->opcode,reg);
        return;
    }

    auto& ir_reg = reg_from_slot(slot,table,alloc);

    const b32 is_src = info.type[reg] == arg_type::src_reg;
    const b32 is_dst = info.type[reg] == arg_type::dst_reg;

    // Never a source as this is an initial alloc
    allocate_slot(table,alloc,block,node,ir_reg,is_src);
    rewrite_reg_internal(table,alloc,node->opcode,reg);

    ir_reg.uses++;

    // is this is a dst we need to write this back when spilled
    if(is_dst)
    {
        ir_reg.dirty = true;
    }

    // if this is its last use schedule it for cleanup
    if(ir_reg.uses == count(ir_reg.usage))
    {
        alloc.dead_slot[alloc.dead_count++] = slot;
    }         
}

void clean_dead_regs(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node, b32 after)
{
    while(alloc.dead_count)
    {
        const SymSlot slot = alloc.dead_slot[--alloc.dead_count];

        clean_dead_reg(table,alloc,block,node,slot,after);
    }    
}

void handle_allocation(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node)
{
    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    // make sure our src var's are loaded
    // mark if any are dead we can reuse to allocate the dst
    for(u32 a = 1; a < info.args; a++)
    {
        // only interested in src registers
        if(info.type[a] != arg_type::src_reg)
        {
            continue;
        }

        allocate_and_rewrite(table,alloc,block,node,a);
    }


    
    // alloc the first slot
    // NOTE: this is done seperately in case we can reuse src slots as the dst
    const b32 is_dst = info.type[0] == arg_type::dst_reg;
    const b32 is_src = info.type[0] == arg_type::src_reg;

    // regs can be freed early
    // NOTE: this cannot happen on a src
    // because otherwhise a reload will be inserted before
    // the current instruction that clobbers the var we have just rewritten
    if(is_dst)
    {
        // free any regs that are never used again
        clean_dead_regs(table,alloc,block,node);        
    }

    if(is_src || is_dst)
    {
        allocate_and_rewrite(table,alloc,block,node,0);
    }
}



void rewrite_opcode(Interloper &itl,LocalAlloc& alloc,Block &block, ListNode *node)
{
    // allocate the registers
    handle_allocation(itl.symbol_table,alloc,block,node);
}

void reserve_offset(LocalAlloc& alloc,SymbolTable& table, Reg& ir_reg)
{
    if(is_sym(ir_reg.slot))
    {
        auto& sym = sym_from_slot(table,ir_reg.slot);

        log(alloc.print_reg_allocation,"spill %s from reg r%d\n",sym.name.buf,ir_reg.location);


        // only allocate the local vars by here
        if(!is_arg(sym))
        {
            stack_reserve_reg(alloc,ir_reg);
        }
    }

    else
    {
        log(alloc.print_reg_allocation,"spill t%d from reg r%d\n",ir_reg.slot.handle,ir_reg.location);

        // by defintion a tmp has to be local
        // TODO: fmt this tmp
        stack_reserve_reg(alloc,ir_reg);
    }    
}

void spill(SymSlot slot,LocalAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, b32 after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // no need to spill
    if(ir_reg.location == LOCATION_MEM)
    {
        // make sure it has a stack pos
        if(is_stack_unallocated(ir_reg))
        {
            reserve_offset(alloc,table,ir_reg);
            return;
        }

        if(alloc.print_reg_allocation)
        {
            if(is_sym(slot))
            {
                auto& sym = sym_from_slot(table,slot);

                printf("attempted to spill freed sym %s\n",sym.name.buf);
            }

            else
            {
                printf("attempted to spill freed tmp t%d\n",slot.handle);            
            }
        }
        return;
    }

    const u32 reg = ir_reg.location;
    const u32 size = ir_reg.size * ir_reg.count;

    // TODO: handle if structs aernt always in emory
    assert(size <= GPR_SIZE);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,table,ir_reg);
    }

    // if the value has only been used as a source and not modifed then we can just treat this as a free_reg
    if(ir_reg.dirty)
    {
        const auto opcode = Opcode(op_type::spill,reg,ir_reg.slot.handle,alloc.stack_offset);

        if(!after)
        {
            insert_at(block.list,node,opcode); 
        }

        else 
        {
            insert_after(block.list,node,opcode);
        }

        ir_reg.dirty = false;
    }

    free_reg_internal(alloc,ir_reg);
}


// Alignment functions for for stack, struct, globals etc

void align(u32 *alloc, u32 alignment)
{
    // make sure the last start posistion is even
    const u32 idx = log2(alignment);

    const u32 unaligned = alloc[idx] & (alignment - 1);

    if(unaligned)
    {
        alloc[idx] += (alignment - unaligned); 
    }
}

// NOTE: both arrays are size at number of base type sizes
// i.e 4 (u8,u16,u32,u64)

// byte start defaults to zero, but for structs extra data may be shoved
// at the start of the byte alloc

// start is the offset for each section
// count is how many "slots" each section has
// byte start is the offset for the initial byte section

u32 calc_alloc_sections(u32* start,u32* count, u32 byte_start = 0)
{
    // byte starts at zero
    start[0] = byte_start;

    // u16
    start[1] = start[0] + (count[0] * sizeof(u8));
    align(start,sizeof(u16));

    // u32
    start[2] = start[1] + (count[1] * sizeof(u16));
    align(start,sizeof(u32));

    // u64
    start[3] = start[2] + (count[2] * sizeof(u32));
    align(start,sizeof(u64));

    // get total allocation size
    const u32 size = start[3] + (count[3] * sizeof(u64));
/*
    for(u32 i = 0; i < 4; i++)
    {
        printf("alloc: %d, %d\n",start[i],count[i]);
    }
*/
    return size;
}

u32 calc_final_offset(const u32* start, u32 size, u32 idx)
{
    return start[log2(size)] + (idx * size);
}

u32 finalise_offset(LocalAlloc& alloc,u32 offset, u32 size)
{
    // what pos in the block does this reg have?
    const u32 idx = offset;  
    
    return calc_final_offset(alloc.stack_alloc,size,idx);
}

void finish_alloc(Reg& reg,SymbolTable& table,LocalAlloc& alloc)
{
    if(alloc.print_stack_allocation)
    {
        if(is_sym(reg.slot))
        {
            auto& sym = sym_from_slot(table,reg.slot);
            printf("final offset %s = [%x,%x] -> (%x,%x)\n",sym.name.buf,reg.size,reg.count,reg.offset,reg.offset);
        }

        else
        {
            printf("final offset t%d = [%x,%x] -> (%x,%x)\n",reg.slot.handle,reg.size,reg.count,reg.offset,reg.offset);
        }
    }

    assert(pending_stack_allocation(reg));

    reg.offset = finalise_offset(alloc,reg.offset,reg.size); 
    reg.flags &= ~PENDING_STACK_ALLOCATION;
}


void calc_allocation(LocalAlloc& alloc)
{
    alloc.stack_size = calc_alloc_sections(alloc.stack_alloc,alloc.size_count_max);

    if(alloc.print_stack_allocation)
    {
        printf("byte count: %d\n",alloc.size_count_max[0]);
        printf("half count: %d\n",alloc.size_count_max[1]);
        printf("word count: %d\n",alloc.size_count_max[2]);
        printf("stack size: %d\n",alloc.stack_size);
    }
}


void finish_stack_alloc(SymbolTable& table, LocalAlloc& alloc)
{
    calc_allocation(alloc);

    for(u32 r = 0; r < count(alloc.pending_allocation); r++)
    {
        const SymSlot slot = alloc.pending_allocation[r];
        auto& ir_reg = reg_from_slot(slot,table,alloc);

        finish_alloc(ir_reg,table,alloc);
    }
}

void mark_lifetimes(Function& func,LocalAlloc& alloc, SymbolTable& table)
{
    log(alloc.print_reg_allocation,"marking lifetimes for %s:\n\n",func.name.buf);

    // clear lifetimes for all global vars
    // because we dont have a multi procedure allocator...
    for(u32 g = 0; g < count(table.global); g++)
    {
        const auto slot = table.global[g];
        auto& sym = sym_from_slot(table,slot);
        clear_arr(sym.reg.usage);
    }

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        
        List& list = block.list;

        ListNode *node = list.start;

        u32 pc = 0;

        while(node)
        {
            const auto opcode = node->opcode;

            const auto info = OPCODE_TABLE[u32(opcode.op)];


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

                auto& reg = reg_from_slot(slot,table,alloc);
                push_var(reg.usage,pc);
            }

            node = node->next;
            pc++;
        }
    }    
}


// TODO: need to rethink this when we do register passing
// and when we push off determining stack size to a later pass
void alloc_args(Function &func, LocalAlloc& alloc, SymbolTable& table, u32 saved_regs_offset)
{
    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];

        auto &sym = sym_from_slot(table,slot);

        //printf("%s : %x\n",sym.name.buf,sym.arg_offset);

        // alloc above the stack frame
        sym.reg.offset = sym.arg_offset + alloc.stack_size + saved_regs_offset + GPR_SIZE;
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
            return std::pair{CONST_IR,handle};
        }

        case reg_kind::global:
        {
            return std::pair{GP_IR,ir_reg.offset};
        }
    }

    assert(false);
}


u32 global_alloc_internal(GlobalAlloc& alloc, u32 size, u32 count)
{
    const u32 idx = log2(size);
    const u32 offset = alloc.count[idx];
    alloc.count[idx] += count; 

    return offset;
}

void reserve_global_alloc(Interloper& itl, Symbol& sym)
{
    sym.reg.offset = global_alloc_internal(itl.global_alloc,sym.reg.size,sym.reg.count);
}

u32 allocate_global_array(GlobalAlloc& alloc,SymbolTable& table ,SymSlot slot, u32 size, u32 alloc_count)
{
    ArrayAllocation allocation;
    allocation.slot = slot;
    allocation.size = size;
    allocation.count = alloc_count;
    allocation.offset = global_alloc_internal(alloc,size,alloc_count);

    const u32 idx = count(alloc.array_allocation);

    if(alloc.print_global)
    {
        auto& sym = sym_from_slot(table,slot);
        printf("initial array global offset: %s [%x,%x] -> %x\n",sym.name.buf,size,alloc_count,allocation.offset);
    }

    push_var(alloc.array_allocation,allocation);

    return idx;
}

void finalise_global_offset(Interloper& itl)
{
    // okay now we know how many vars are in each section
    // align them and compute the final intial offsets

    auto& alloc = itl.global_alloc;

    // first calc the sections
    alloc.size = calc_alloc_sections(alloc.start,alloc.count);

    log(alloc.print_global,"Global size %x : start (%x, %x, %x, %x) : count (%x, %x, %x, %x)\n",
        alloc.size,alloc.start[0],alloc.start[1],alloc.start[2],alloc.start[3],alloc.count[0],
        alloc.count[1],alloc.count[2],alloc.count[3]);

    // now we need to give each symbol is final offset from the start of the global table
    
    // by definiton globals (if any) will be stored inside the "top" symbol table
    for(u32 g = 0; g < count(itl.symbol_table.global); g++)
    {
        const auto slot = itl.symbol_table.global[g];
        auto& sym = sym_from_slot(itl.symbol_table,slot);

        const auto [size,count] = std::pair{sym.reg.size,sym.reg.count};

        sym.reg.offset = calc_final_offset(itl.global_alloc.start,size,sym.reg.offset);
        log(alloc.print_global,"Final offset for %s : %x (%x,%x)\n",sym.name.buf,sym.reg.offset,size,count);
    }

    // finalised offsets on any fixed size array allocations handled by global_array_alloc


}
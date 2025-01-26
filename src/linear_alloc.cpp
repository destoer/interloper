struct LinearRange
{
    u32 start = 0xffff'ffff;
    u32 end = 0;
    SymSlot slot = {SYMBOL_NO_SLOT};
    u32 global_reg = UNALLOCATED_REG;
    ListNode* node = nullptr;
    BlockSlot block_slot = {NO_SLOT};
    b32 dst_live = false;
};

bool is_reg_locally_allocated(const Reg& reg)
{
    return reg.local_reg != UNALLOCATED_REG;
}

struct RegisterFile
{
    // what registers have we used total for this function?
    u32 used_set = 0;

    // what registers are we allowed to use?
    u32 free_set = 0;

    // What slot is being used by a register?
    SymSlot allocated[MACHINE_REG_SIZE];
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

    b32 print = false;

    RegisterFile gpr;
    RegisterFile fpr;

    StackAlloc stack_alloc;
};


RegisterFile& get_register_file(LinearAlloc& alloc, Reg& reg)
{
    return (reg.flags & REG_FLOAT)? alloc.fpr : alloc.gpr;
}

void print_reg_alloc(LinearAlloc& alloc, SymbolTable &table)
{
    const u32 free_regs = popcount(alloc.gpr.free_set);
    const u32 used_regs = popcount(alloc.gpr.used_set);

    printf("free gpr registers: %d\n",free_regs);
    printf("total used gpr registers: %d\n",used_regs);

    for(u32 i = 0; i < X86_GPR_SIZE; i++)
    {
        const SymSlot slot = alloc.gpr.allocated[i];

        if(slot.handle == REG_FREE)
        {
            continue;
        }

        log_reg(alloc.print,table,"reg %s -> %r\n",reg_name(alloc.arch,i),slot);
    }

    putchar('\n');
}

void mark_used(RegisterFile& regs, u32 reg)
{
    regs.used_set = set_bit(regs.used_set,reg);
}

LinearAlloc make_linear_alloc(b32 print_reg,b32 print_stack,Array<Reg> registers,arch_target arch)
{
    LinearAlloc alloc;

    alloc.print = print_reg;
    alloc.stack_alloc = make_stack_alloc(print_stack);

    alloc.arch = arch;
    alloc.tmp_regs = registers;

    return alloc;
}

void update_range(Interloper& itl, Function& func,HashTable<SymSlot,LinearRange> table, SymSlot slot,Block& block, ListNode* node,b32 dst_live,u32 pc)
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
        range.dst_live = dst_live;
    }

    range.end = std::max(range.end,pc);
}

Array<LinearRange> find_range(Interloper& itl, Function& func)
{

    auto table = make_table<SymSlot,LinearRange>();

    u32 pc = 0;

    // for each block
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        ListNode *node = block.list.start;

        // for each opcode
        while(node)
        {
            // scan each opcode for any vars
            const auto opcode = node->opcode;
            const auto info = info_from_op(opcode); 

            for(s32 a = info.args - 1; a >= 0; a--)
            {
                const auto slot = sym_from_idx(opcode.v[a]);

                if(is_arg_reg(info.type[a]) && is_var(slot))
                {
                    // dst only has a later lifetime
                    if(info.type[a] == arg_type::dst_reg)
                    {
                        pc += 1;
                        update_range(itl,func,table,slot,block,node,true,pc);
                    }

                    else
                    {
                        update_range(itl,func,table,slot,block,node,false,pc);
                    }
                }
            }

            // next opcode
            pc += 1;
            node = node->next;
        }
        
        // live out has a higher start point
        pc += 1;

        ListNode* last = block.list.end;

        // if its live out then consider it allocated till the end of the block
        for(u32 i = 0; i < count(block.live_out.buf); i++)
        {
            const auto& bucket = block.live_out.buf[i];

            for(u32 j = 0; j < count(bucket); j++)
            {
                const auto slot = bucket[j];
                update_range(itl,func,table,slot,block,last,false,pc);
            }
        }
    }

    // okay we have all the ranges now flatten this into a sorted array
    Array<LinearRange> range;

    // first copy the hash table contents into the array
    for(u32 i = 0; i < count(table.buf); i++)
    {
        const auto& bucket = table.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            push_var(range,bucket[j].v);
        }
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
    regs.allocated[reg] = {REG_FREE};
}

bool is_reg_free(RegisterFile& regs,u32 reg)
{
    return is_set(regs.free_set,reg);
}

void remove_reg(RegisterFile& regs, u32 reg)
{
    regs.free_set = deset_bit(regs.free_set,reg);
}

void add_reg(RegisterFile& regs, x86_reg reg, u32 locked_set)
{
    const u32 locked = (1 << u32(reg)) & locked_set;

    // check register not locked
    if(!locked)
    {
        free_reg(regs,u32(reg));
    }
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
    const u32 reg = find_free_register(regs.free_set,regs.used_set);

    if(reg != FFS_EMPTY)
    {
        remove_reg(regs,reg);
        mark_used(regs,reg);
    }

    return reg;
}

// TODO: This should look for a register furthest in the future.
// We may have to have a look back over our local allocator
void acquire_local_reg(LinearAlloc& alloc, Reg& ir_reg, RegisterFile& regs,Block& block)
{
    UNUSED(alloc);
    UNUSED(block);
    u32 reg = alloc_reg(regs);

    // Spill a register for room
    if(reg == FFS_EMPTY)
    {
        assert(false);
    }

    ir_reg.local_reg = reg;
    regs.allocated[reg] = ir_reg.slot;
}

// TODO: we need to dynamically lock the registers for each function
void init_regs(LinearAlloc& alloc, u32 locked_set)
{
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.gpr.allocated[i] = {REG_FREE};
        alloc.fpr.allocated[i] = {REG_FREE};
    }

    add_reg(alloc.gpr,x86_reg::rax,locked_set);
    add_reg(alloc.gpr,x86_reg::rcx,locked_set);
    add_reg(alloc.gpr,x86_reg::rdx,locked_set);
    add_reg(alloc.gpr,x86_reg::rbx,locked_set);
    add_reg(alloc.gpr,x86_reg::rdp,locked_set);
    add_reg(alloc.gpr,x86_reg::rsi,locked_set);
    add_reg(alloc.gpr,x86_reg::rdi,locked_set);

    add_reg(alloc.gpr,x86_reg::r8,locked_set);
    add_reg(alloc.gpr,x86_reg::r9,locked_set);
    add_reg(alloc.gpr,x86_reg::r10,locked_set);
    add_reg(alloc.gpr,x86_reg::r11,locked_set);
    add_reg(alloc.gpr,x86_reg::r12,locked_set);
    add_reg(alloc.gpr,x86_reg::r13,locked_set);
    add_reg(alloc.gpr,x86_reg::r14,locked_set);
    add_reg(alloc.gpr,x86_reg::r15,locked_set);


    // add sse regs
    add_reg(alloc.fpr,x86_reg::xmm0,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm1,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm2,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm3,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm4,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm5,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm6,locked_set);
    add_reg(alloc.fpr,x86_reg::xmm7,locked_set);
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
    init_regs(alloc,func.locked_set);

    // perform the allocation
    for(u32 r = 0; r < count(range); r++)
    {
        auto& cur = range[r];

        auto& ir_reg = reg_from_slot(itl,func,cur.slot);

        // insert the live ir op, so we know to load it
        // but if live first as dst then we dont care...
        if(is_arg(ir_reg) && !cur.dst_live)
        {
            auto& block = block_from_slot(func,cur.block_slot);

            const auto live_op = make_op(op_type::live_var,cur.slot.handle);
            insert_at(block.list,cur.node,live_op);
        }


        // actually run the allocation

        // expire any dead sets
        clean_dead_reg(itl,func,alloc,active,cur);

        // check which register file to use
        auto& reg_file = get_register_file(alloc,ir_reg);

        const u32 reg = alloc_reg(reg_file);

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

Reg& reg_from_slot(SymSlot slot, SymbolTable& table, LinearAlloc& alloc)
{
    return reg_from_slot(table,alloc.tmp_regs,slot);
}

void reload_reg(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, u32 reg)
{
    const auto opcode = make_op(op_type::load,reg,slot.handle,alloc.stack_alloc.stack_offset);

    auto& ir_reg = reg_from_slot(slot,table,alloc);
    log_reg(alloc.print,table,"reload %r to %s (size %d)\n",ir_reg.slot,reg_name(alloc.arch,reg),ir_reg.size);

    insert_at(block.list,node,opcode);
}


// Force a reload of a allocated register to deal with aliasing.

void reload_slot(LinearAlloc& alloc, SymbolTable& table,Block& block, ListNode* node, SymSlot slot)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // If this register is not allocated then let the next instruction that uses it
    // Deal with handling the reload.
    if(is_reg_locally_allocated(ir_reg))
    {
        reload_reg(alloc,table,block,node,slot,ir_reg.local_reg);
    }
}


void reserve_offset(LinearAlloc& alloc,SymbolTable& table, Reg& ir_reg,u32 reg)
{
    log_reg(alloc.print,table,"reserve offset for %r in %s\n",ir_reg.slot,reg_name(alloc.arch,reg));
    stack_reserve_reg(alloc.stack_alloc,ir_reg);
}

void free_location(Reg& ir_reg,RegisterFile& regs)
{
    free_reg(regs, ir_reg.local_reg);
    ir_reg.local_reg = UNALLOCATED_REG;
}

// Spill a register to memory
void spill_reg(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, u32 reg, bool after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);
    const u32 size = ir_reg.size * ir_reg.count;

    // TODO: handle if structs aernt always in memory
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

    // Mark the register as freed
    assert(is_reg_locally_allocated(ir_reg));

    free_location(ir_reg,get_register_file(alloc,ir_reg));
}

// Spill a slot to memory
void spill(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, SymSlot slot, bool after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // is actually in a reg and not immediatly spilled
    if(is_reg_locally_allocated(ir_reg))
    {
        spill_reg(alloc,table,block,node,slot,ir_reg.local_reg,after);
    }

    // reserve a space for this for a later spill
    else if(is_stack_unallocated(ir_reg))
    {
        reserve_offset(alloc,table,ir_reg,LOCATION_MEM);
    }   
}

// Save a register, this can either be a copy to a free reg
// Or by spilling it to memory
void save_reg(LinearAlloc& alloc,SymbolTable& table, Block& block, ListNode* node, RegisterFile& file, u32 reg)
{
    // TODO: for now just spill back out to memory 
    if(!is_reg_free(file,reg))
    {
        spill(alloc,table,block,node,file.allocated[reg],false);
    }
}

void save_caller_saved_regs(LinearAlloc& alloc, SymbolTable& table, Block& block, ListNode* node)
{
    const auto& abi_info = get_abi_info(alloc.arch);

    // then save the caller saved registers
    save_reg(alloc,table,block,node,alloc.gpr,abi_info.rv);
}

// TODO: We need to redefine the structs we use for data flow analysis
// to make operations like this simpler
void set_local_reg_from_set(LinearAlloc& alloc, SymbolTable& table, const Set<SymSlot>& set)
{
    for(u32 i = 0; i < count(set.buf); i++)
    {
        const auto bucket = set.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            const auto slot = bucket[j];
            auto& ir_reg = reg_from_slot(slot,table,alloc);

            ir_reg.local_reg = ir_reg.global_reg;
        }
    }
}

void alloc_regs_from_live_in(LinearAlloc& alloc, SymbolTable& table, const Set<SymSlot>& live_in)
{
    for(u32 i = 0; i < count(live_in.buf); i++)
    {
        const auto bucket = live_in.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            const auto slot = bucket[j];

            // allocate the register into the appropiate register file
            auto& ir_reg = reg_from_slot(slot,table,alloc);
            
            if(is_reg_locally_allocated(ir_reg))
            {
                auto& reg_file = get_register_file(alloc,ir_reg);
                remove_reg(reg_file, ir_reg.local_reg);
                reg_file.allocated[ir_reg.local_reg] = slot;
            }
        }
    }
}

void linear_setup_new_block(LinearAlloc& alloc, SymbolTable& table, Block& block) 
{
    // All registers free at block start bar live in
    init_regs(alloc,0);

    // Set the local register location for every used register
    // This means any def or live in register
    set_local_reg_from_set(alloc,table,block.def);
    set_local_reg_from_set(alloc,table,block.live_in);

    alloc_regs_from_live_in(alloc,table,block.live_in);
}

void allocate_and_rewrite(LinearAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, u32 reg)
{
    const auto opcode = node->opcode;
    const auto info = info_from_op(opcode);

    const b32 is_dst = is_arg_dst(info.type[reg]);
    const b32 is_src = is_arg_src(info.type[reg]);

    // not a src or dst, not interested
    if(!is_dst && !is_src)
    {
        return;
    }

    // save slot so we can use it if it gets rewritten
    SymSlot slot = sym_from_idx(node->opcode.v[reg]);

    // check we are dealing with a var
    if(is_var(slot))
    {
        auto& ir_reg = reg_from_slot(slot,table,alloc);

        // var is allocated
        if(is_reg_locally_allocated(ir_reg))
        {
            if(is_src || is_dst)
            {
                node->opcode.v[reg] = ir_reg.local_reg;
            }
        }

        // Aquire a new register and reload it
        else
        {
            // check which register file to use
            auto& reg_file = get_register_file(alloc,ir_reg);

            acquire_local_reg(alloc,ir_reg,reg_file,block);

            if(is_src || is_dst)
            {
                node->opcode.v[reg] = ir_reg.local_reg;
            }

            // src do a reload
            if(is_src) 
            {
                // issue a load
                reload_reg(alloc,table,block,node,slot, ir_reg.local_reg);
            }
        }
    }

    // special reg just rewrite it to whatever it wants
    else if(is_special_reg(slot))
    {
        // make sure the spec regs are marked as used
        const u32 location = special_reg_to_reg(alloc.arch,slot);
        mark_used(is_fpr(slot)? alloc.fpr : alloc.gpr,location);
        node->opcode.v[reg] = location;
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
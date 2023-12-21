
// is this how we want it?
// or should we just pass stuff through one by one?
struct LocalAlloc
{
    RegAlloc reg_alloc;

    arch_target arch;

    // what instruction are we on?
    u32 pc = 0;

    // allcation info of tmp's for current function
    // NOTE: this is owned by the func and we dont have to free it
    Array<Reg> tmp_regs;

    StackAlloc stack_alloc;
};

LocalAlloc make_local_alloc(b32 print_reg_allocation,b32 print_stack_allocation, Array<Reg> tmp, arch_target arch)
{
    LocalAlloc alloc;

    alloc.stack_alloc = make_stack_alloc(print_stack_allocation);
    alloc.reg_alloc = make_reg_alloc(print_reg_allocation,arch);

    alloc.pc = 0;
    alloc.arch = arch;

    alloc.tmp_regs = tmp;

    return alloc;
}

void destroy_local_alloc(LocalAlloc& alloc)
{
    destroy_stack_alloc(alloc.stack_alloc);
}

void spill(SymSlot slot,LocalAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, b32 after = false);


Reg& reg_from_slot(SymSlot slot, SymbolTable& table, LocalAlloc& alloc)
{
    return reg_from_slot(table,alloc.tmp_regs,slot);
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
        opcode.v[reg] = special_reg_to_reg(alloc.arch,slot);
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
    const auto info = info_from_op(opcode);

    if(info.type[reg] == arg_type::src_reg || info.type[reg] == arg_type::dst_reg)
    {
        rewrite_reg_internal(table,alloc,opcode,reg);
    }
}

void rewrite_regs(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode)
{   
    const auto info = info_from_op(opcode);

    for(u32 r = 0; r < info.args; r++)
    {
        rewrite_reg(table,alloc,opcode,r);
    }
}

void stack_reserve_slot(LocalAlloc& alloc,SymbolTable table, SymSlot slot)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    const b32 print_stack = alloc.stack_alloc.print;

    if(is_sym(slot))
    {
        auto& sym = sym_from_slot(table,slot);

        log(print_stack,"initial offset allocated %s: [%x,%x] -> %x\n",sym.name.buf,ir_reg.size,ir_reg.count,ir_reg.offset);    
    }

    else
    {
        // by defintion a tmp has to be local
        log(print_stack,"initial offset allocated t%d: [%x,%x] -> %x\n",slot.handle,ir_reg.size,ir_reg.count,ir_reg.offset);
    }

    stack_reserve_reg(alloc.stack_alloc,ir_reg);    
}

void free_reg(Reg& ir_reg, SymbolTable& table,LocalAlloc& alloc)
{
    // this register is allready in memory
    if(ir_reg.location == LOCATION_MEM)
    {
        return;
    }

    const u32 reg = ir_reg.location;

    const b32 print_reg = alloc.reg_alloc.print;

    if(is_sym(ir_reg.slot))
    {
        auto& sym = sym_from_slot(table,ir_reg.slot);

        log(print_reg,"freed symbol %s from reg r%d\n",sym.name.buf,reg);
    }

    else
    {
        log(print_reg,"freed tmp t%d from reg r%d\n",ir_reg.slot,reg);               
    }

    assert(!is_aliased(ir_reg));

    free_reg_internal(alloc.reg_alloc,ir_reg);
}


void trash_reg(SymbolTable& table, LocalAlloc& alloc, Block& block, ListNode* node)
{
    s32 max_gap = 0;
    u32 max_reg = REG_FREE;

    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.reg_alloc.regs[r];

        if(is_var(slot))
        {
            auto& ir_reg = reg_from_slot(slot,table,alloc);

            // we haven't found something that can be freed yet...
            const s32 cur_gap = ir_reg.usage[ir_reg.uses] - alloc.pc;

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

    spill(alloc.reg_alloc.regs[max_reg],alloc,table,block,node);
}


void alloc_internal(Reg& ir_reg, SymbolTable& table,LocalAlloc &alloc,Block& block, ListNode* node)
{
    // evict a register to make space
    if(!alloc.reg_alloc.free_regs)
    {
        trash_reg(table,alloc,block,node);
    }

    const u32 reg = alloc_reg(ir_reg,alloc.reg_alloc);
    const SymSlot slot = ir_reg.slot;
   
    if(alloc.reg_alloc.print)
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
    const auto opcode = Opcode(op_type::load,ir_reg.location,ir_reg.slot.handle,alloc.stack_alloc.stack_offset);
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
    if(!is_local_reg(ir_reg) || used_beyond)
    {
        spill(slot,alloc,table,block,node,after);
    }

        
    // No way to access it, get rid of the reg
    else
    {
        free_reg(ir_reg,table,alloc);
    }
}

// mark usage for internal freeing
void mark_reg_usage(LocalAlloc& alloc, Reg& ir_reg, bool is_dst)
{
    ir_reg.uses++;

    // is this is a dst we need to write this back when spilled
    if(is_dst)
    {
        ir_reg.dirty = true;
    }

    check_dead_reg(alloc.reg_alloc,ir_reg);
}


void allocate_and_rewrite(SymbolTable& table,LocalAlloc& alloc,Block& block, ListNode* node,u32 reg)
{
    const auto opcode = node->opcode;
    const auto info = info_from_op(opcode);

    const SymSlot slot = sym_from_idx(node->opcode.v[reg]);

    const b32 is_src = info.type[reg] == arg_type::src_reg;
    const b32 is_dst = info.type[reg] == arg_type::dst_reg;

    // special purpose ir reg dont allocate just rewrite it
    if(is_special_reg(slot))
    {
        rewrite_reg(table,alloc,node->opcode,reg);
    
        // make sure callee saved regs are marked as used for saving
        const u32 spec_reg = node->opcode.v[reg];

        if(is_callee_saved(alloc.arch,spec_reg) && is_dst)
        {
            mark_used(alloc.reg_alloc,spec_reg);
        }
        return;
    }

    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // Never a source as this is an initial alloc
    allocate_slot(table,alloc,block,node,ir_reg,is_src);
    rewrite_reg_internal(table,alloc,node->opcode,reg);

    mark_reg_usage(alloc,ir_reg,is_dst); 
}


void spill_all(LocalAlloc &alloc, SymbolTable& table, Block& block, ListNode* node, bool after)
{
    if(alloc.reg_alloc.print)
    {
        puts("spilling everything"); 
    }
    
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.reg_alloc.regs[r];

        if(is_var(slot))
        {
            spill(slot,alloc,table,block,node,after);
        }        
    }
}

// TODO: we probably need to cache this in a bitset so we dont have to loop this every time?
void spill_func_bounds(LocalAlloc& alloc, SymbolTable& table, Block&  block, ListNode* node)
{
    // spill any vars for a function called
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        const SymSlot slot = alloc.reg_alloc.regs[r];

        // handle callee saved regs
        if(!is_callee_saved(alloc.arch,r) && is_var(slot))
        {
            spill(slot,alloc,table,block,node,false);
        }

        // handle any aliased or global symbols
        else if(is_sym(slot))
        {
            auto& sym = sym_from_slot(table,slot);

            if(is_aliased(sym.reg) || sym.reg.kind == reg_kind::global)
            {
                spill(slot,alloc,table,block,node,false);
            }
        } 
    }   
}


void clean_dead_regs(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node, b32 after = false)
{
    while(alloc.reg_alloc.dead_count)
    {
        const SymSlot slot = alloc.reg_alloc.dead_slot[--alloc.reg_alloc.dead_count];

        clean_dead_reg(table,alloc,block,node,slot,after);
    }    
}


void handle_allocation(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node)
{
    const auto opcode = node->opcode;
    const auto info = info_from_op(opcode);

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
    const b32 print_reg = alloc.reg_alloc.print;

    if(is_sym(ir_reg.slot))
    {
        auto& sym = sym_from_slot(table,ir_reg.slot);

        log(print_reg,"spill %s from reg r%d\n",sym.name.buf,ir_reg.location);


        // only allocate the local vars by here
        if(!is_arg(sym))
        {
            stack_reserve_reg(alloc.stack_alloc,ir_reg);
        }
    }

    else
    {
        log(print_reg,"spill t%d from reg r%d\n",ir_reg.slot.handle,ir_reg.location);

        // by defintion a tmp has to be local
        // TODO: fmt this tmp
        stack_reserve_reg(alloc.stack_alloc,ir_reg);
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

        if(alloc.reg_alloc.print)
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
        const auto opcode = Opcode(op_type::spill,reg,ir_reg.slot.handle,alloc.stack_alloc.stack_offset);

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

    free_reg_internal(alloc.reg_alloc,ir_reg);
}

void finish_alloc(Reg& reg,SymbolTable& table,LocalAlloc& alloc)
{
    assert(pending_stack_allocation(reg));

    finalise_offset(alloc.stack_alloc,reg);

    if(alloc.stack_alloc.print)
    {
        if(is_sym(reg.slot))
        {
            auto& sym = sym_from_slot(table,reg.slot);
            printf("final offset %s = [%x,%x] -> %x\n",sym.name.buf,reg.size,reg.count,reg.offset);
        }

        else
        {
            printf("final offset t%d = [%x,%x] -> %x\n",reg.slot.handle,reg.size,reg.count,reg.offset);
        }
    }
}

void finish_stack_alloc(SymbolTable& table, LocalAlloc& alloc)
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

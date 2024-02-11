// our bitset can only store 32 regs
static_assert(MACHINE_REG_SIZE <= 32);

struct BlockRegStart
{
    // register allocation
    // is this free or does it hold a var?
    SymSlot regs[MACHINE_REG_SIZE];  

    // set of free registers
    u32 free_set = 0;    
};

struct RegAlloc
{
    // register allocation
    // is this free or does it hold a var?
    SymSlot regs[MACHINE_REG_SIZE];  

    // set of free registers
    u32 free_set = 0;

    // which registers are dirty
    u32 dirty = 0;

    u32 blank_set = 0;

    // keep track of freeable regs
    SymSlot dead_slot[MACHINE_REG_SIZE] = {0};
    u32 dead_count = 0;

    u32 restricted_reg = 0;

    // bitset of which regs this functions needs to use
    // for now we are going to just callee save every register
    u32 used_regs = 0;
   
    b32 print = false;

    arch_target arch;
};

void add_reg(RegAlloc& alloc, u32 reg)
{
    alloc.free_set = set_bit(alloc.free_set,reg);
}

void remove_reg(RegAlloc& alloc, u32 reg)
{
    alloc.free_set = deset_bit(alloc.free_set,reg);
}

void mark_clean(RegAlloc& alloc,u32 reg)
{
    alloc.dirty = deset_bit(alloc.dirty,reg);
}

void mark_dirty(RegAlloc& alloc,u32 reg)
{
    alloc.dirty = set_bit(alloc.dirty,reg);
}

b32 is_dirty(RegAlloc& alloc,u32 reg)
{
    return is_set(alloc.dirty,reg);
}


void add_gpr(RegAlloc& alloc, x86_reg reg)
{
    add_reg(alloc,u32(reg));
}

RegAlloc make_reg_alloc(b32 print, arch_target arch)
{
    RegAlloc alloc;

    const auto info = info_from_arch(arch);

    alloc.used_regs = 0;

    // mark every reg as free
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = {REG_FREE};
    }

    alloc.free_set = 0;

    // add in GPR regs
    switch(arch)
    {
        case arch_target::x86_64_t:
        {
            add_gpr(alloc,x86_reg::rax);
            add_gpr(alloc,x86_reg::rcx);
            add_gpr(alloc,x86_reg::rdx);
            add_gpr(alloc,x86_reg::rbx);
            add_gpr(alloc,x86_reg::rdp);
            add_gpr(alloc,x86_reg::rsi);
            add_gpr(alloc,x86_reg::rdi);

            add_gpr(alloc,x86_reg::r8);
            add_gpr(alloc,x86_reg::r9);
            add_gpr(alloc,x86_reg::r10);
            add_gpr(alloc,x86_reg::r11);
            add_gpr(alloc,x86_reg::r12);
            add_gpr(alloc,x86_reg::r13);
            add_gpr(alloc,x86_reg::r14);
            add_gpr(alloc,x86_reg::r15);
            break;
        }
    }

    // what is the default allocation state?
    alloc.blank_set = alloc.free_set;

    assert(popcount(alloc.free_set) <= info.gpr);

    alloc.print = print;

    alloc.arch = arch;


    return alloc;
}


u32 special_reg_to_reg(arch_target arch,SymSlot slot)
{
    switch(slot.handle)
    {
        case SP_IR:
        { 
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    return x86_reg::rsp;
                }
            }
            assert(false);
        }


        case RV_IR: 
        {
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    return x86_reg::rax;
                }
            }
            assert(false);
        }

        case RAX_IR: return u32(x86_reg::rax);
        case RCX_IR: return u32(x86_reg::rcx);
        case RDX_IR: return u32(x86_reg::rdx);
        case RDI_IR: return u32(x86_reg::rdi); 
        case RSI_IR: return u32(x86_reg::rsi); 
        case R8_IR: return u32(x86_reg::r8);
        case R9_IR: return u32(x86_reg::r9);
        case R10_IR: return u32(x86_reg::r10);

        default: crash_and_burn("unhandled special reg %x\n",slot); 
    }    
}

b32 is_restricted(RegAlloc& alloc, u32 reg)
{
    return is_set(alloc.restricted_reg,reg);
}


void lock_reg(RegAlloc& alloc, SymSlot slot)
{
    const auto reg = special_reg_to_reg(alloc.arch,slot);

    // NOTE: this expects the reg to be unallocated when it locks it
    // use a higher level wrapper
    assert(alloc.regs[reg].handle == REG_FREE);

    if(alloc.print)
    {
        printf("lock reg: %s\n",spec_reg_name(slot));
    }

    assert(!is_restricted(alloc,reg));

    alloc.restricted_reg = set_bit(alloc.restricted_reg,reg);

    remove_reg(alloc,reg);
}

void unlock_reg_internal(RegAlloc& alloc, u32 reg)
{
    if(alloc.print)
    {
        printf("unlock reg: %s\n",reg_name(alloc.arch,reg));
    }

    assert(is_restricted(alloc,reg));

    alloc.restricted_reg = deset_bit(alloc.restricted_reg,reg);

    // put register back inside the free list
    add_reg(alloc,reg);
}

void unlock_reg(RegAlloc& alloc, SymSlot slot)
{
    const auto reg = special_reg_to_reg(alloc.arch,slot);

    unlock_reg_internal(alloc,reg);
}

void unlock_registers(RegAlloc& alloc)
{
    for(u32 reg = 0; reg < MACHINE_REG_SIZE; reg++)
    {
        if(is_restricted(alloc,reg))
        {
            unlock_reg_internal(alloc,reg);
        }
    }
}

void log_reg(b32 print,SymbolTable& table, const String& fmt_string, ...)
{  
    if(!print)
    {
        return;
    }

    va_list args;
    va_start(args,fmt_string);

    for(u32 i = 0; i < fmt_string.size; i++)
    {
        if(fmt_string[i] == '%')
        {
            switch(fmt_string[i + 1])
            {
                // string
                case 's':
                {
                    const auto str = va_arg(args, const char*);
                    printf("%s",str);
                    break;
                }

                // hex
                case 'x':
                {
                    const auto v = va_arg(args, u32);

                    printf("%x",v);
                    break;
                }

                // int
                case 'd':
                {
                    const auto v = va_arg(args, s32);

                    printf("%d",v);
                    break;
                }

                // reg
                case 'r':
                {
                    const auto slot = sym_from_idx(va_arg(args,u32));

                    if(is_special_reg(slot))
                    {
                        printf("%s",spec_reg_name(slot));
                    }

                    else if(is_tmp(slot))
                    {
                        printf("t%d",slot.handle);
                    }

                    else if(is_sym(slot))
                    {
                        const auto &sym = sym_from_slot(table,slot);
                        printf("%s",sym.name.buf);
                    }
                    break;
                }

                default: assert(false);
            }


            // account for format
            i += 1;
        }

        else
        {
            putchar(fmt_string[i]);
        }
    }

    va_end(args);
}

void print_reg_alloc(RegAlloc &alloc,SymbolTable& table)
{
    printf("\n\nallocation:\n\n");

    const u32 free_regs = popcount(alloc.free_set);

    printf("total registers: %d\n",MACHINE_REG_SIZE);
    printf("free registers: %d\n",free_regs);
    printf("used regsisters: %d\n",MACHINE_REG_SIZE - free_regs);
    printf("total used registers: %d\n",popcount(alloc.used_regs));

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        const SymSlot slot = alloc.regs[i];

        if(slot.handle == REG_FREE)
        {
            continue;
        }

        log_reg(alloc.print,table,"reg %s -> %r\n",reg_name(alloc.arch,i),slot);
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

b32 is_free(SymSlot slot)
{
    return slot.handle == REG_FREE;
}

void free_reg_internal(RegAlloc& alloc,u32 reg)
{
    // add back to the free list
    alloc.regs[reg] = sym_from_idx(REG_FREE);
    add_reg(alloc,reg);      
}

// TODO: might want to switch to a better struct than this
// but we dont leak any internals beyond these four functions
// so its fine for now
SymSlot* find_slot(RegAlloc& alloc, SymSlot slot)
{
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        if(alloc.regs[r] == slot)
        {
            return &alloc.regs[r];
        }
    }

    return nullptr;
}

b32 is_allocated(RegAlloc& alloc, Reg& ir_reg)
{
    return find_slot(alloc,ir_reg.slot) != nullptr;
}

u32 slot_ptr_to_reg(RegAlloc& alloc, SymSlot* slot_ptr)
{
    return slot_ptr - alloc.regs;
}

u32 find_reg(RegAlloc& alloc, SymSlot slot)
{
    SymSlot* slot_opt = find_slot(alloc,slot);

    assert(slot_opt);

    return slot_ptr_to_reg(alloc,slot_opt);
}

u32 find_reg_opt(RegAlloc& alloc, SymSlot slot)
{
    SymSlot* slot_opt = find_slot(alloc,slot);

    if(!slot_opt)
    {
        return LOCATION_MEM;
    }

    return slot_ptr_to_reg(alloc,slot_opt);
}

u32 aquire_reg(Reg& ir_reg,RegAlloc& alloc, u32 reg);

void read_reg_block_entry(Interloper& itl,Function& func,RegAlloc& alloc, const Block& block)
{
    // reset the register set
    alloc.free_set = alloc.blank_set;

    // check which registers aern't free and reallocate them
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        // default to free
        alloc.regs[r] = {REG_FREE};

        if(is_var(block.reg_start[r]))
        {
            auto& ir_reg = reg_from_slot(itl.symbol_table,func,block.reg_start[r]);
            aquire_reg(ir_reg,alloc,r);
        }
    }

    // copy across the "old" dirty flags
    alloc.dirty = block.reg_start_dirty;
}

void write_reg_block_entry(const RegAlloc& alloc, Block& block, b32 spill)
{
    memcpy(block.reg_start,alloc.regs,sizeof(alloc.regs));

    // if we require spilling because one of targets
    // has multiple entrys then we dont need to mark the regs as dirty
    block.reg_start_dirty = spill? 0 : alloc.dirty;
}


b32 is_dirty(RegAlloc& alloc, Reg& ir_reg)
{
    const u32 reg = find_reg_opt(alloc,ir_reg.slot);

    if(reg == LOCATION_MEM)
    {
        return false;
    }

    return is_dirty(alloc,reg);
}

void free_ir_reg(RegAlloc& alloc, Reg& ir_reg)
{
    const u32 reg = find_reg(alloc,ir_reg.slot);

    // add back to the free list
    free_reg_internal(alloc,reg);
}

void mark_used(RegAlloc& alloc, u32 reg)
{
    alloc.used_regs = set_bit(alloc.used_regs,reg);
}

u32 aquire_reg(Reg& ir_reg,RegAlloc& alloc, u32 reg)
{
    remove_reg(alloc,reg);
    mark_used(alloc,reg);

    alloc.regs[reg] = ir_reg.slot;

    return reg;
}

u32 find_free_register(u32 set,u32 used,u32 group[], u32 size)
{
    u32 reg = FFS_EMPTY;

    // TODO: which of these propertys is prefable? usage
    // or optimal allocation?
    // if its the latter we need to loop jam the 2nd stmt

    // attempt to alloc registers we have allready used first
    // so we do not have to save extra reegisters by using
    // different ones
    for(u32 g = 0; g < size && reg == FFS_EMPTY; g++)
    {
        reg = ffs(set & group[g] & used);
    }

    // if we have no registers amongst those allready used
    // just grab any that we can
    for(u32 g = 0; g < size && reg == FFS_EMPTY; g++)
    {
        reg = ffs(set & group[g]);
    }

    return reg;
}


const u32 CALLEE_SAVED_X86 = 1 << u32(x86_reg::rax);

const u32 LOWER_REGS_X86 = (1 << u32(x86_reg::rax)) | (1 << u32(x86_reg::rcx)) | (1 << u32(x86_reg::rdx)) | 
    (1 << u32(x86_reg::rbx)) | (1 << u32(x86_reg::rdp)) | (1 << u32(x86_reg::rsi)) | (1 << u32(x86_reg::rdi));

const u32 HIGHER_REGS_X86 = (1 << u32(x86_reg::r8)) | (1 << u32(x86_reg::r9)) | (1 << u32(x86_reg::r10)) |
    (1 << u32(x86_reg::r11)) | (1 << u32(x86_reg::r12)) | (1 << u32(x86_reg::r13)) | 
    (1 << u32(x86_reg::r14)) | (1 << u32(x86_reg::r15));     

u32 alloc_reg(Reg& ir_reg,RegAlloc& alloc)
{
    u32 reg = FFS_EMPTY;

    switch(alloc.arch)
    {
        case arch_target::x86_64_t:
        {
            u32 group[3];
            u32 size = 0;

            // prefer to put tmps in callee saved regs
            // so we don't have to spill them
            if(ir_reg.kind == reg_kind::tmp)
            {
                group[size++] = CALLEE_SAVED_X86;
            }

            group[size++] = LOWER_REGS_X86;
            group[size++] = HIGHER_REGS_X86;
        
            reg = find_free_register(alloc.free_set,alloc.used_regs,group,size);
            break;
        }
    }

    // should not be empty at this point
    assert(reg != FFS_EMPTY);

    return aquire_reg(ir_reg,alloc,reg);
}

// NOTE: this only transposes a reg to another
// it does not actually copy it
u32 realloc_reg(Reg& ir_reg,RegAlloc& alloc)
{
    const u32 old = find_reg(alloc,ir_reg.slot);

    u32 reg = FFS_EMPTY;

    switch(alloc.arch)
    {
        case arch_target::x86_64_t:
        {
            u32 group[3];
            u32 size = 0;

            // prefer higher first when reallocating regs
            // as they do not have a special purpose, and won't have to be shifted again
            group[size++] = HIGHER_REGS_X86;
            group[size++] = LOWER_REGS_X86;
        
            reg = find_free_register(alloc.free_set,alloc.used_regs,group,size);
            break;
        }
    }

    // should not be empty at this point
    assert(reg != FFS_EMPTY);

    // allocate in the reg
    aquire_reg(ir_reg,alloc,reg);

    // copy across the dirty flag
    if(is_dirty(alloc,old))
    {
        mark_dirty(alloc,reg);
    }

    // actually get rid of the old register
    free_reg_internal(alloc,old);

    return reg;
}

b32 request_reg(RegAlloc& alloc, u32 req_reg)
{
    return !is_restricted(alloc,req_reg) && is_set(alloc.free_set,req_reg);
}


void check_dead_reg(RegAlloc& alloc, Reg& ir_reg)
{
    // if this is its last use schedule it for cleanup
    if(ir_reg.uses == count(ir_reg.usage))
    {
        alloc.dead_slot[alloc.dead_count++] = ir_reg.slot;
    }   
}

// mark usage for internal freeing
void mark_reg_usage(RegAlloc& alloc, Reg& ir_reg, bool is_dst)
{
    ir_reg.uses++;

    // is this is a dst we need to write this back when spilled
    if(is_dst)
    {
        mark_dirty(alloc,find_reg(alloc,ir_reg.slot));
    }

    check_dead_reg(alloc,ir_reg);
}


b32 allocate_into_reg(RegAlloc& alloc,Reg& ir_reg,SymSlot spec_reg)
{
    const u32 reg = special_reg_to_reg(alloc.arch,spec_reg);

    if(request_reg(alloc,reg))
    {
        // free any old reg we have
        if(is_allocated(alloc,ir_reg))
        {
            free_ir_reg(alloc,ir_reg);
        }

        // take this for ourself
        aquire_reg(ir_reg,alloc,reg);
        return true;
    }

    return false;
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
                if(!is_arg_reg(info.type[a]))
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
            const u32 SP = arch_sp(itl.arch);

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
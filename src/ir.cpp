#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"
#include "emitter.cpp"
#include "link.cpp"


Block make_block(block_type type,LabelSlot label_slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.type = type;
    block.list = make_list(list_allocator);
    block.label_slot = label_slot;

    return block;
}

void new_block(ArenaAllocator* list_allocator,Function& func,block_type type, LabelSlot label_slot)
{
    push_var(func.emitter.program,make_block(type,label_slot,list_allocator)); 
}


void destroy_emitter(IrEmitter& emitter)
{
    destroy_arr(emitter.program);
}

void ir_memcpy(Interloper&itl, Function& func, SymSlot dst_slot, SymSlot src_slot, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    // emit a call to memcpy with args
    // check function is declared

    Function* func_def = lookup(itl.function_table,String("memcpy"));

    if(!func_def)
    {
        panic(itl,"[COMPILE]: memcpy is required for struct passing\n");
    }
    Function &func_call = *func_def;

    mark_used(itl,func_call);


    const SymSlot imm_slot = emit_res(func,op_type::mov_imm,size);

    emit(func,op_type::push_arg,imm_slot);
    emit(func,op_type::push_arg,src_slot);
    emit(func,op_type::push_arg,dst_slot);

    emit(func,op_type::spill_rv);
    emit(func,op_type::call,func_call.label_slot);

    emit(func,op_type::clean_args,3);
}


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
    Array<Reg> tmp_regs;


    // stack allocation

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset;

    // where does each section for alloc start?
    u32 stack_alloc[3];

    // how much of each type of var is there at the momemnt?
    u32 size_count[3];

    // what is the maximum ammount of vars?
    // this will be used to compute the stack size later
    u32 size_count_max[3];

    // what is the total ammount of space that this functions stack requires!
    u32 stack_size;
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

u32 stack_reserve(LocalAlloc& alloc, u32 size, u32 count, const char *name);

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


// NOTE: this just reserves stack space,
// calc allocation must be called (done before 2nd directive pass)
// then finish_alloc to give the actual offset
u32 stack_reserve(LocalAlloc& alloc, u32 size, u32 count)
{
    const u32 idx = size >> 1;
    const u32 cur = alloc.size_count[idx];


    alloc.size_count[idx] += count;
    alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);

    return cur + PENDING_ALLOCATION;    
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
    
        assert(!sym.referenced);
    }


    else
    {
        log(alloc.print_reg_allocation,"freed tmp t%d from reg r%d\n",ir_reg.slot,reg);               
    }

    ir_reg.location = LOCATION_MEM;

    // add back to the free list
    alloc.regs[reg] = sym_from_idx(REG_FREE);
    alloc.free_list[alloc.free_regs++] = reg; 
}

u32 alloc_reg(LocalAlloc& alloc)
{
    return alloc.free_list[--alloc.free_regs];
}

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
            const u32 cur_gap = ir_reg.usage[ir_reg.uses + 1] - alloc.pc;

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
            // we need to save the current stack offset here 
            // as by the time we load it it may be different
            const auto opcode = Opcode(op_type::load,ir_reg.location,ir_reg.slot.handle,alloc.stack_offset);
            insert_at(block.list,node,opcode); 
        }
    }   
}

void handle_allocation(SymbolTable& table, LocalAlloc& alloc,Block &block, ListNode *node)
{
    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    // keep track of freeable regs
    SymSlot dead_slot[3] = {0};
    u32 dead_count = 0;

    // make sure our src var's are loaded
    // mark if any are tmp's we can reuse to allocate the dst
    for(u32 a = 1; a < info.args; a++)
    {
        // only interested in src registers
        if(info.type[a] != arg_type::src_reg)
        {
            continue;
        }

        const SymSlot slot = sym_from_idx(node->opcode.v[a]);

        // special purpose ir reg dont allocate
        if(is_special_reg(slot))
        {
            rewrite_reg(table,alloc,node->opcode,a);
            continue;
        }


        auto& ir_reg = reg_from_slot(slot,table,alloc);

        allocate_slot(table,alloc,block,node,ir_reg,info.type[a] == arg_type::src_reg);

        // rewrite the register
        rewrite_reg(table,alloc,node->opcode,a); 


        // mark any regs no longer used for cleanup
        if(++ir_reg.uses == count(ir_reg.usage))
        {
            dead_slot[dead_count++] = slot;
        }
    }

    // free any regs that are never used again
    while(dead_count)
    {
        const SymSlot slot = dead_slot[--dead_count];

        // we can just cheat and spill vars for now to make sure they get saved but this kinda defeats the point
        if(is_sym(slot))
        {
            //auto &sym = sym_from_slot(table,slot);

            // if a pointer is taken to this, 
            // or a its last use is in a loop it is defined outside of
            // then we need to spill it as it might be used again
            //if(sym.referenced || (block.loop_nesting != 0 && sym.loop_nesting < block.loop_nesting))
            {
                spill(slot,alloc,table,block,node);
            }

            /*
            // No way to access it, get rid of the reg
            else
            {
                auto& ir_reg = reg_from_slot(slot,table,alloc);
                free_reg(ir_reg,table,alloc);
            }
            */
        }

        // tmp's are fine to delete under any circumstance because they will not live beyond the block
        else
        {
            auto& ir_reg = reg_from_slot(slot,table,alloc);
            free_reg(ir_reg,table,alloc);
        }
    }


    // alloc the first slot
    // NOTE: this is done seperately in case we can reuse src slots as the dst
    const SymSlot dst_slot = sym_from_idx(node->opcode.v[0]);

    if(info.type[0] == arg_type::src_reg || info.type[0] == arg_type::dst_reg)
    {
        // special purpose ir reg dont allocate
        if(is_special_reg(dst_slot))
        {
            rewrite_reg(table,alloc,node->opcode,0);
        }

        else
        {
            auto& ir_reg = reg_from_slot(dst_slot,table,alloc);

            allocate_slot(table,alloc,block,node,ir_reg,info.type[0] == arg_type::src_reg);

            // rewrite the register
            rewrite_reg(table,alloc,node->opcode,0); 

            // dst slot is never used?
            if(++ir_reg.uses == count(ir_reg.usage))
            {
                free_reg(ir_reg,table,alloc);
            }
        }
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


void rewrite_opcode(Interloper &itl,LocalAlloc& alloc,Block &block, ListNode *node)
{
    // allocate the registers
    handle_allocation(itl.symbol_table,alloc,block,node);
}


void spill(SymSlot slot,LocalAlloc& alloc,SymbolTable& table,Block& block,ListNode* node, b32 after)
{
    auto& ir_reg = reg_from_slot(slot,table,alloc);

    // no need to spill
    if(ir_reg.location == LOCATION_MEM)
    {
        return;
    }

    const u32 reg = ir_reg.location;
    const u32 size = ir_reg.size * ir_reg.count;

    // TODO: handle structs
    assert(size <= sizeof(u32));

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(ir_reg.offset == UNALLOCATED_OFFSET)
    {
        if(is_sym(slot))
        {
            auto& sym = sym_from_slot(table,slot);

            log(alloc.print_reg_allocation,"spill %s:%d\n",sym.name.buf,reg);


            // only allocate the local vars by here
            if(!is_arg(sym))
            {
                ir_reg.offset = stack_reserve(alloc,size,1);

                log(alloc.print_stack_allocation,"initial offset allocated %s: [%x] -> %x\n",sym.name.buf,size,ir_reg.offset - PENDING_ALLOCATION);
            }
        }

        else
        {
            log(alloc.print_reg_allocation,"spill t%d:%d\n",slot.handle,reg);

            // by defintion a tmp has to be local
            // TODO: fmt this tmp
            ir_reg.offset = stack_reserve(alloc,size,1);

            log(alloc.print_stack_allocation,"initial offset allocated t%d: [%x] -> %x\n",slot.handle,size,ir_reg.offset - PENDING_ALLOCATION);
        }
    }
    
    // TODO: how do we know if a variable has not been modified
    // i.e it is a ready only copy and has not been modifed so we dont actually need to write it back
    const auto opcode = Opcode(op_type::spill,reg,ir_reg.slot.handle,alloc.stack_offset);

    if(!after)
    {
        insert_at(block.list,node,opcode); 
    }

    else 
    {
        insert_after(block.list,node,opcode);
    }

    // register is back in memory!
    ir_reg.location = LOCATION_MEM;
    alloc.regs[reg] = sym_from_idx(REG_FREE);
    alloc.free_list[alloc.free_regs++] = reg;    
}


ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,Block &block, ListNode *node)
{
    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    UNUSED(func);

    UNUSED(opcode);

    switch(node->opcode.op)
    {
        case op_type::addrof:
        {
            assert(false);
        }

        // have to do opcode rewriting by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            node->opcode =  Opcode(op_type::push,opcode.v[0],0,0);

            // adjust opcode for reg alloc
            rewrite_opcode(itl,alloc,block,node);

            // varaibles now have to be accessed at a different offset
            // until this is corrected by clean call
            alloc.stack_offset += GPR_SIZE;

            node = node->next;
            break;
        }

        case op_type::clean_args:
        {
            // clean up args
            const auto stack_clean = GPR_SIZE * opcode.v[0];

            node->opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,stack_clean);
            alloc.stack_offset -= stack_clean; 

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            log(alloc.print_stack_allocation,"clean args: %x\n",stack_clean);
    
            node = node->next;
            break;
        }

        case op_type::alloc_stack:
        {
            assert(false);
        }

        case op_type::free_stack:
        {
            assert(false);
        }

        case op_type::alloc_slot:
        {
            // TODO: how should we decide if structs go into memory or not?
            return remove(block.list,node);
            break;
        }



        // TODO: we should probably only save regs we use but this is just easy for now
        case op_type::save_regs:
        {
            node->opcode = Opcode(op_type::pushm,0xffffffff,0,0);
            alloc.stack_offset += GPR_SIZE * MACHINE_REG_SIZE;

            node = node->next;
            break;
        }


        // TODO: make this only restore active regs....
        case op_type::restore_regs:
        {
            node->opcode = Opcode(op_type::popm,0xffffffff,0,0);
            alloc.stack_offset -= GPR_SIZE * MACHINE_REG_SIZE;

            node = node->next;
            break;
        }

        // make sure the return value has nothing important when calling functions
        case op_type::spill_rv:
        {
            const SymSlot slot = alloc.regs[RV];

            // if we have a reg here
            if(slot.handle != REG_FREE)
            {
                spill(slot,alloc,table,block,node);
            }

            return remove(block.list,node);
            break;
        }


        case op_type::spill_all:
        {
            spill_all(alloc,itl.symbol_table,block,node,false);
            return remove(block.list,node);
            break;
        }

        // TODO: this needs to have its size emitted directly inside the opcode
        case op_type::free_slot:
        {
            return remove(block.list,node);
            break;
        }


        case op_type::load_arr_data:
        {
            assert(false);
        }

        // TODO: this probably needs to be reworked for multi dimensional arrays
        case op_type::load_arr_len:
        {
            assert(false);
        }

        // pools
        case op_type::pool_addr:
        {
            assert(false);
        }


        default:
        {
            rewrite_opcode(itl,alloc,block,node);
            node = node->next;
            break; 
        }
    }

    return node;
}

void finish_alloc(Reg& reg,SymbolTable& table,LocalAlloc& alloc)
{
    if(alloc.print_stack_allocation)
    {
        if(is_sym(reg.slot))
        {
            auto& sym = sym_from_slot(table,reg.slot);
            printf("final offset %s = %x -> (%x,%x)\n",sym.name.buf,reg.size,reg.offset,reg.offset - PENDING_ALLOCATION);
        }

        else
        {
            printf("final offset t%d = %x -> (%x,%x)\n",reg.slot.handle,reg.size,reg.offset,reg.offset - PENDING_ALLOCATION);
        }
    }

    assert(reg.offset >= PENDING_ALLOCATION && reg.offset != UNALLOCATED_OFFSET);

    // what pos in the block does this reg have?
    const u32 idx = reg.offset - PENDING_ALLOCATION;  
    
    // actually allocate the offset
    reg.offset = alloc.stack_alloc[reg.size >> 1] + (idx * reg.size);     
}

// 2nd pass of rewriting on the IR
ListNode* rewrite_directives(Interloper& itl,LocalAlloc &alloc,Block& block, ListNode *node,const Opcode& callee_restore,
    const Opcode& stack_clean, bool insert_callee_saves)
{
    UNUSED(itl); UNUSED(alloc); UNUSED(block); UNUSED(node); UNUSED(callee_restore);
    UNUSED(stack_clean); UNUSED(insert_callee_saves);

    const auto opcode = node->opcode;

    switch(node->opcode.op)
    {

        case op_type::mov_reg:
        {
            // remove dead stores (still need to perform reg correction)
            // so it has to be done in the 2nd pass
            if(opcode.op == op_type::mov_reg && opcode.v[0] == opcode.v[1])
            {
                return remove(block.list,node);
            }

            node = node->next;
            break;
        }

        case op_type::state_dump:
        {
            crash_and_burn("unused state opcode");
        }


        case op_type::alloc:
        {
            assert(false);
        }

        case op_type::ret:
        {
            ListNode *tmp = node;

            if(alloc.stack_size)
            {
                tmp = insert_at(block.list,tmp,stack_clean);
            }

            if(insert_callee_saves)
            {
                tmp = insert_at(block.list,tmp,callee_restore);
            }

            node = node->next;
            break;
        }

        // reload a reg
        case op_type::load:
        {
            SymSlot slot = sym_from_idx(opcode.v[1]);

            auto& reg = reg_from_slot(slot,itl.symbol_table,alloc);
            const s32 stack_offset = opcode.v[2];


            // reload the spilled var 
            if(reg.flags & SIGNED_FLAG)
            {
                // word is register size (we dont need to extend it)
                static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                // this here does not otherwhise need rewriting so we will emit SP directly
                node->opcode = Opcode(instr[reg.size >> 1],opcode.v[0],SP,reg.offset + stack_offset);        
            }

            // "plain data"
            // just move by size
            else
            {
                static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};

                node->opcode =  Opcode(instr[reg.size >> 1],opcode.v[0],SP,reg.offset + stack_offset);
            }
            
            node = node->next;
            break;
        }

        case op_type::addrof:
        {
            assert(false);
        }

        case op_type::spill:
        {
            SymSlot slot = sym_from_idx(opcode.v[1]);
            auto& reg = reg_from_slot(slot,itl.symbol_table,alloc);

            const s32 stack_offset = opcode.v[2];

            // this is the first stack access so we need to compute the final posistion
            if(reg.offset >= PENDING_ALLOCATION)
            {
                finish_alloc(reg,itl.symbol_table,alloc);
            }

            // write value back out into mem
            // TODO: are these offsets aligned? 

            // TODO: we need to not bother storing these back if the varaible spilled has not been modified
            // and is just used as a const for a calc (how do we impl this?)

            static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
            node->opcode = Opcode(instr[reg.size >> 1],opcode.v[0],SP,reg.offset + stack_offset);   

            node = node->next;
            break;                    
        }

        // arrays
        case op_type::load_arr_data:
        {
            assert(false);
        }


        case op_type::load_arr_len:
        {
            assert(false);
        }

        default: 
        {
            node = node->next; 
            break;
        }
    }

    return node;
}

void align(u32 *alloc, u32 alignment)
{
    // make sure the last start posistion is even
    alignment /= 2;

    if(alloc[alignment] & alignment)
    {
        alloc[alignment] += alignment;
    }
}

void calc_allocation(LocalAlloc& alloc)
{
    // calculate the final stack sizes
    // byte located at start
    alloc.stack_alloc[0] = 0;

    // start u16 at end of byte allocation and align them
    alloc.stack_alloc[1] = alloc.size_count_max[0];
    align(alloc.stack_alloc,sizeof(u16));


    //  u32 at end of half allocation and align them
    alloc.stack_alloc[2] = alloc.stack_alloc[1] + (alloc.size_count_max[1] * sizeof(u16));
    align(alloc.stack_alloc,sizeof(u32));


    // get the total stack size
    alloc.stack_size = alloc.stack_alloc[2] + (alloc.size_count_max[2] * sizeof(u32));

    if(alloc.print_stack_allocation)
    {
        printf("byte count: %d\n",alloc.size_count_max[0]);
        printf("half count: %d\n",alloc.size_count_max[1]);
        printf("word count: %d\n",alloc.size_count_max[2]);
        printf("stack size: %d\n",alloc.stack_size);
    }
}
    
// TODO: need to rethink this when we do register passing
// and when we push off determining stack size to a later pass
void alloc_args(Function &func, LocalAlloc& alloc, SymbolTable& table, u32 saved_regs_offset)
{
    for(u32 a = 0; a < count(func.args); a++)
    {
        const SymSlot slot = func.args[a];

        auto &sym = sym_from_slot(table,slot);

        //printf("%s : %x\n",sym.name.buf,sym.arg_offset);

        // alloc above the stack frame
        sym.reg.offset = sym.arg_offset + alloc.stack_size + saved_regs_offset + sizeof(u32);
    }
             
}


void dump_reg(Reg& reg)
{
    const char* KIND_NAMES[] = {"local","global","tmp"};
    printf("kind: %s\n",KIND_NAMES[u32(reg.kind)]);
    printf("slot: 0x%x\n",reg.slot.handle);

    printf("size: %d\n",reg.size);
    printf("count: %d\n",reg.count);

    printf("offset: 0x%x\n",reg.offset);
    printf("locaiton: 0x%x\n",reg.location);

    printf("uses: %d\n",reg.uses);

    for(u32 i = 0; i < count(reg.usage); i++)
    {
        printf("use[%d] -> %d\n",i,reg.usage[i]);
    }
}

void mark_lifetimes(Function& func,LocalAlloc& alloc, SymbolTable& table)
{
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


void allocate_registers(Interloper& itl,Function &func)
{
    auto alloc = make_local_alloc(itl.print_reg_allocation,itl.print_stack_allocation,func.registers);

    // figure out how long each sym lives
    mark_lifetimes(func,alloc,itl.symbol_table);

    log(alloc.print_reg_allocation,"allocating registers for %s:\n\n",func.name.buf);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        
        ListNode *node = block.list.start;

        
        while(node)
        {
            node = allocate_opcode(itl,func,alloc,block,node);
            alloc.pc++;
        }

        // block has ended spill variables still live
        // TODO: we want to get rid of this when we properly trace vars
        spill_all(alloc,itl.symbol_table,block,block.list.end,true);
    }

    calc_allocation(alloc);

    if(alloc.print_reg_allocation)
    {
        print_alloc(alloc,itl.symbol_table);
    }

    // only allocate a stack if we need it
    if(alloc.stack_size)
    {
        insert_front(func.emitter.program[0].list,Opcode(op_type::sub_imm,SP,SP,alloc.stack_size));
    }


    // iterate over the function by here and add callee cleanup at every ret
    // and insert the stack offsets and load and spill directives

    // TODO: this might be good to loop jam with somethign but just have a seperate loop for simplictiy atm


    // R0 is callee saved
    static constexpr u32 CALLEE_SAVED_MASK = 1;

    // make sure callee saved regs are not saved inside the func
    const u32 saved_regs = alloc.used_regs & ~CALLEE_SAVED_MASK;
    const u32 save_count = popcount(saved_regs);

    const bool insert_callee_saves = func.name != "main" && save_count != 0;


    alloc_args(func,alloc,itl.symbol_table,insert_callee_saves? sizeof(u32) * save_count : 0);

    // entry point does not need to preserve regs
    if(insert_callee_saves)
    {
        insert_front(func.emitter.program[0].list,Opcode(op_type::pushm,saved_regs,0,0));
    }

    // epilogue opcodes
    const auto callee_restore = Opcode(op_type::popm,saved_regs,0,0);
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,alloc.stack_size);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        auto node = block.list.start;

        while(node)
        {
            node = rewrite_directives(itl,alloc,block,node,callee_restore,stack_clean,insert_callee_saves);
        }
    }
}


void fmt_sym_specifier(Array<char> &buffer, const SymbolTable& table, char specifier, u32 handle)
{
    switch(specifier)
    {
        case 'r':
        {
            
            SymSlot slot = sym_from_idx(handle);

            if(is_special_reg(slot))
            {
                const u32 idx = slot.handle - SPECIAL_PURPOSE_REG_START;
                push_mem(buffer,SPECIAL_REG_NAMES[idx]);
            }

            // print a sym
            else if(is_sym(slot))
            {
                const auto& sym = sym_from_slot(table,slot);
                const String& name = sym.name;

                push_mem(buffer,name);
            }


            // print a tmp
            else
            {
                char name[40];
                const u32 len = sprintf(name,"t%d",slot.handle);

                push_mem(buffer,name,len);
            }


            break;
        }


        // hex constant
        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%x",handle);

            push_mem(buffer,name,len);
            break;
        }

        // address
        case 'a':
        {
            const String& name = table.label_lookup[handle].name;
            push_mem(buffer,name);
            break;
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void fmt_raw_specifier(Array<char> &buffer, char specifier, u32 slot)
{
    switch(specifier)
    {
        // raw register
        case 'r':
        {
            if(slot == SP)
            {
                push_mem(buffer,SPECIAL_REG_NAMES[SP_NAME_IDX]);
            }

            else
            {
                char name[40];
                const u32 len = sprintf(name,"r%d",slot);

                push_mem(buffer,name,len);
            }


            break;
        }

        // labeles act as address here
        case 'a':
        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%x",slot);

            push_mem(buffer,name,len);
            break;
        }

        // regm
        case 'm':
        {
            char name[128];

            push_var(buffer,'{');

            u32 count = 0;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(slot,r))
                {
                    const u32 len = sprintf(name,"%sr%d",count != 0? "," : "",r);
                    push_mem(buffer,name,len);
                    count++;
                }
            }

            push_var(buffer,'}');
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void disass_opcode_internal(const Opcode& opcode, const SymbolTable* table)
{
    const auto& info = OPCODE_TABLE[u32(opcode.op)];
    const auto& fmt_string = info.fmt_string;

    Array<char> buffer;

    u32 args = 0;

    for(u32 i = 0; i < fmt_string.size; )
    {
        if(fmt_string[i] == '%')
        {
            if(args == 3)
            {
                crash_and_burn("execeed opcode arg printing");
            }

            const char specifier = fmt_string[i + 1];

            if(table)
            {
                fmt_sym_specifier(buffer,*table,specifier,opcode.v[args++]);
            }

            else
            {
                fmt_raw_specifier(buffer,specifier,opcode.v[args++]);
            }

            i += 2;
        }

        else
        {
            push_var(buffer,fmt_string[i++]);
        }
    }

    // null term the buffer
    push_var(buffer,'\0');

    puts(buffer.data);


    destroy_arr(buffer);
}

// TODO: use table of fmt strings to print this
// just figure out symbol printing first and then generalise it
void disass_opcode_sym(const Opcode &opcode, const SymbolTable& table)
{
    disass_opcode_internal(opcode,&table);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode_internal(opcode,nullptr);
}


void dump_ir(Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);

    u32 l = 0;
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        

        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_sym(node->opcode,table);
            node = node->next;
        }

        l++;
    }

    printf("\n");       
}
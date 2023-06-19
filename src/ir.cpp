#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"
#include "emitter.cpp"
#include "cfg.cpp"
#include "pool.cpp"
#include "link.cpp"
#include "reg_allocator.cpp"
#include "disass.cpp"


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
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>

            const auto slot = sym_from_idx(opcode.v[1]);
            auto& reg = reg_from_slot(slot,table,alloc);


            if(is_stack_unallocated(reg))
            {
                assert(stored_in_mem(reg));
                
                stack_reserve_reg(alloc,reg);  
            }



            // okay apply the stack offset, and let the register allocator deal with it
            // we will get the actual address using it later
            node->opcode = Opcode(op_type::addrof,opcode.v[0],opcode.v[1],alloc.stack_offset);

            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(table,alloc,block,node,0);

            node = node->next;
            break;            
        }

        case op_type::reload_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);
            auto& reg = reg_from_slot(itl.symbol_table,func,slot);

            if(reg.location != LOCATION_MEM)
            {
                reload_slot(alloc,block,node,reg);
            }

            node = remove(block.list,node);
            break;
        }

        case op_type::spill_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);

            spill(slot,alloc,itl.symbol_table,block,node,false);

            node = remove(block.list,node);

            break;          
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
            const u32 size = opcode.v[0];
            node->opcode = Opcode(op_type::sub_imm,SP_IR,SP_IR,size);
            alloc.stack_offset += size;

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.print_stack_allocation)
            {
                printf("allocate stack %x\n",size);
            }


            node = node->next;
            break;
        }

        case op_type::free_stack:
        {
            const u32 size = opcode.v[0];
            node->opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,size);
            alloc.stack_offset -= size;

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.print_stack_allocation)
            {
                printf("free stack %x\n",size);
            }

            node = node->next;
            break;
        }

        case op_type::alloc_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);
            auto& reg = reg_from_slot(slot,table,alloc);
            

            if(alloc.print_reg_allocation)
            {
                if(is_sym(slot))
                {
                    auto& sym = sym_from_slot(table,slot);
                    printf("alloc slot: %s\n",sym.name.buf);
                }

                else
                {
                    printf("alloc slot: t%d\n",reg.slot.handle);
                }
            }

            // explictly force a stack alloc now
            if(opcode.v[1])
            {
                stack_reserve_reg(alloc,reg);    
            }

            node = remove(block.list,node);
            break;
        }

        case op_type::alloc_fixed_array:
        {
            const u32 size = opcode.v[1];
            const u32 count = opcode.v[2];

            const SymSlot slot = sym_from_idx(opcode.v[0]);

            const u32 offset = allocate_stack_array(alloc,table,slot,size,count);
            
            node->opcode = Opcode(op_type::alloc_fixed_array,opcode.v[0],offset,0);

            allocate_and_rewrite(table,alloc,block,node,0);

            node = node->next;
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
        }


        case op_type::spill_all:
        {
            spill_all(alloc,itl.symbol_table,block,node,false);
            return remove(block.list,node);
        }

        // scope has elapsed any resources can be reclaimed
        // TODO: should this support registers?
        case op_type::free_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);
            auto& sym = sym_from_slot(table,slot);


            if(sym.reg.offset != UNALLOCATED_OFFSET)
            {
                if(alloc.print_stack_allocation)
                {
                    printf("reclaiming stack space %s : (%d , %d)\n",sym.name.buf,sym.reg.size,sym.reg.count);
                }

                alloc.size_count[sym.reg.size >> 1] -= sym.reg.count;
            }


            return remove(block.list,node);
        }


        case op_type::free_fixed_array:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);
            auto& sym = sym_from_slot(table,slot);  

            const u32 size = opcode.v[1];
            const u32 count = opcode.v[2];

            if(alloc.print_stack_allocation)
            {
                printf("reclaiming stack space from arr %s : (%d , %d)\n",sym.name.buf,size,count);
            }

            alloc.size_count[size >> 1] -= count;
            
            return remove(block.list,node);          
        }

        // pools (NOTE: we resolve this during linking)
        case op_type::pool_addr:
        {
            // pool_addr <dst>, <offset>, <pool>
            allocate_and_rewrite(itl.symbol_table,alloc,block,node,0);   

            node = node->next;
            break;
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

        case op_type::ret:
        {
            ListNode *tmp = node;

            if(alloc.stack_size)
            {
                tmp = insert_at(block.list,tmp,stack_clean);

                // make sure callee restore comes after stack clean
                if(insert_callee_saves)
                {
                    insert_after(block.list,tmp,callee_restore);
                }
            }

            else if(insert_callee_saves)
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


            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);


            // reload the spilled var 
            if(is_signed(reg))
            {
                // word is register size (we dont need to extend it)
                static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                // this here does not otherwhise need rewriting so we will emit SP directly
                node->opcode = Opcode(instr[reg.size >> 1],opcode.v[0],offset_reg,offset);        
            }

            // "plain data"
            // just move by size
            else
            {
                static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};

                node->opcode =  Opcode(instr[reg.size >> 1],opcode.v[0],offset_reg,offset);
            }
            
            node = node->next;
            break;
        }

        case op_type::addrof:
        {
            const s32 stack_offset = opcode.v[2];
            const SymSlot slot = sym_from_idx(opcode.v[1]);

            auto &reg = reg_from_slot(slot,itl.symbol_table,alloc);

            assert(is_stack_allocated(reg));

            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            node->opcode = Opcode(op_type::lea,opcode.v[0],offset_reg,offset);

            node = node->next;
            break;
        }

        case op_type::alloc_fixed_array:
        {
            const u32 idx = node->opcode.v[1];

            ArrayAllocation &allocation = alloc.array_allocation[idx];

            allocation.offset = finalise_offset(alloc,allocation.offset,allocation.size);

            if(alloc.print_stack_allocation)
            {
                auto& sym = sym_from_slot(itl.symbol_table,allocation.slot);
                printf("final array offset %s = [%x,%x] -> (%x)\n",sym.name.buf,allocation.size,allocation.count,allocation.offset);
            }

            // NOTE: this is allways on the stack, globals handle their own allocation...
            node->opcode = Opcode(op_type::lea,opcode.v[0],SP,allocation.offset + allocation.stack_offset);

            node = node->next;
            break;
        }

        case op_type::spill:
        {
            SymSlot slot = sym_from_idx(opcode.v[1]);
            auto& reg = reg_from_slot(slot,itl.symbol_table,alloc);

            const s32 stack_offset = opcode.v[2];

            // write value back out into mem
            // TODO: are these offsets aligned? 

            // TODO: we need to not bother storing these back if the varaible spilled has not been modified
            // and is just used as a const for a calc (how do we impl this?)

            static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
    
            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            node->opcode = Opcode(instr[reg.size >> 1],opcode.v[0],offset_reg,offset);   

            node = node->next;
            break;                    
        }

        default: 
        {
            node = node->next; 
            break;
        }
    }

    return node;
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

        // ignore empty blocks
        if(!node)
        {
            continue;
        }
        
        while(node)
        {
            // free any regs that are now dead from the last opcode
            clean_dead_regs(itl.symbol_table,alloc,block,node);

            node = allocate_opcode(itl,func,alloc,block,node);
            alloc.pc++;
        }

        auto opcode = block.list.end->opcode;

        const auto& ENTRY = OPCODE_TABLE[u32(opcode.op)];  

        // block has ended spill variables still live 
        // TODO: we want to get rid of this with a proper global allocator...
        if(ENTRY.group == op_group::branch_t)
        {
            // free any regs dead on the last opcode
            clean_dead_regs(itl.symbol_table,alloc,block,block.list.end,false);

            spill_all(alloc,itl.symbol_table,block,block.list.end,false);
        }

        else
        {
            // free any regs dead on the last opcode
            clean_dead_regs(itl.symbol_table,alloc,block,block.list.end,true);


            // fall through spill after data has been written out
            spill_all(alloc,itl.symbol_table,block,block.list.end,true);
        }
    }

    // Figure out how large a stack we need and put everything on it
    finish_stack_alloc(itl.symbol_table,alloc);

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

    destroy_local_alloc(alloc);
}
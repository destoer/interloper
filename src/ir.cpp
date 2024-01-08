#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"
#include "emitter.cpp"
#include "rewrite_arch_ir.cpp"
#include "cfg.cpp"
#include "pool.cpp"
#include "asm.cpp"
#include "x86_emitter.cpp"
#include "stack_allocator.cpp"
#include "reg_allocator.cpp"
#include "local_allocator.cpp"
#include "disass.cpp"


void print_slot(SymbolTable& table, SymSlot slot)
{
    if(is_sym(slot))
    {
        auto &sym = sym_from_slot(table,slot);
        printf("sym: %s\n",sym.name.buf);
    }

    else if(is_special_reg(slot))
    {
        printf("special reg: %s\n",spec_reg_name(slot));
    }

    else
    {
        printf("tmp: t%d\n",slot.handle);
    }
}

ListNode* rewrite_access_struct(Interloper& itl, Function& func,LocalAlloc &alloc,SymbolTable& table,Block &block, ListNode *node)
{
    const auto slot = sym_from_idx(node->opcode.v[1]);
    auto& reg = reg_from_slot(slot,table,alloc);

    if(is_stack_unallocated(reg))
    {
        if(!stored_in_mem(reg))
        {
            printf("error in func: %s\n",func.name.buf);
            print_slot(itl.symbol_table,slot);
            assert(false);
        }
        
        stack_reserve_reg(alloc.stack_alloc,reg);  
    }

    if(is_local(reg))
    {
        // add the stack offset, so this correctly offset for when we fully rewrite this
        node->opcode.v[2] += alloc.stack_alloc.stack_offset;
    }

    allocate_and_rewrite(table,alloc,block,node,0);
    return node->next;
}

ListNode* rewrite_x86_shift(Interloper& itl, LocalAlloc& alloc, Block& block, ListNode* node)
{
    const auto oper = sym_from_idx(RCX_IR);

    // rcx free
    unlock_reg(alloc.reg_alloc,oper);

    rewrite_opcode(itl,alloc,block,node);

    return node->next;
}

ListNode* rewrite_x86_fixed_arith(Interloper& itl, LocalAlloc& alloc, Block& block, ListNode* node, SymSlot out)
{
    // save where our dst is being forced into
    const auto dst = sym_from_idx(node->opcode.v[0]);

    // rewrite src
    allocate_and_rewrite(itl.symbol_table,alloc,block,node,1);

    // rax free
    unlock_reg(alloc.reg_alloc,sym_from_idx(RAX_IR));

    // rdx free
    unlock_reg(alloc.reg_alloc,sym_from_idx(RDX_IR));

    if(is_var(dst))
    {
        auto& ir_reg = reg_from_slot(dst,itl.symbol_table,alloc);

        if(alloc.reg_alloc.print)
        {
            printf("forcing ir %x into %s\n",dst.handle,spec_reg_name(out));
        }

        // force to out reg
        assert(allocate_into_reg(alloc.reg_alloc,ir_reg,out));
        mark_reg_usage(alloc.reg_alloc,ir_reg,true);
    }

    // NOTE: where the dst was written (NOTE: this is actually implict to thei instructiom)
    node->opcode.v[0] = out.handle;

    return node->next;  
}

ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,Block &block, ListNode *node)
{
    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    UNUSED(func);

    UNUSED(opcode);

    switch(node->opcode.op)
    {
        case op_type::mov_reg:
        {
            const auto dst = sym_from_idx(opcode.v[0]);
            const auto src = sym_from_idx(opcode.v[1]);

            // i.e used for return values just
            // rewrite the register into spec RV
            if(is_special_reg(src) && is_var(dst))
            {
                auto& ir_reg = reg_from_slot(dst,table,alloc);

                // attempt to force reg
                if(allocate_into_reg(alloc.reg_alloc,ir_reg,src))
                {
                    if(alloc.reg_alloc.print)
                    {
                        printf("forcing ir %x into %s\n",dst.handle,spec_reg_name(src));
                    }
                    mark_reg_usage(alloc.reg_alloc,ir_reg,true);
                    node = remove(block.list,node);
                    break;
                }
            }

            // if a value is allready held in the machine reg we are about to move to
            // we must make sure we save the value incase we need it later!
            // then the lock the register so it can no longer be freely allocated
            if(is_special_reg(dst))
            {
                const u32 reg = special_reg_to_reg(alloc.arch,dst);

                const auto held_slot = alloc.reg_alloc.regs[reg];

                if(is_var(held_slot))
                {
                    lock_reg(alloc,table,block,node,dst);
                }
            }

            // just do it normally
            rewrite_opcode(itl,alloc,block,node);
            node = node->next;
            
            break;
        }

        // make sure register locks for ret dont leak across blocks
        case op_type::ret:
        {
            unlock_registers(alloc.reg_alloc);
            node = node->next;
            break;
        }

        case op_type::replace_reg:
        {
            const auto spec_reg = sym_from_idx(opcode.v[0]);
            const auto src = sym_from_idx(opcode.v[1]);

            assert(is_var(src));

            // src is allready in the right reg 
            // just save it if need be and then restrict its usage
            if(in_reg(alloc,table,src,spec_reg))
            {
                // mark register as reserved
                lock_reg(alloc,table,block,node,spec_reg);

                node = remove(block.list,node);
            }
            
            else
            {
                // replace reg, slot
                // -> evict reg
                // -> mov reg, slot
            
                // mark register as reserved
                lock_reg(alloc,table,block,node,spec_reg);

                // TODO: if the value has to be reloaded this will result in a uneeded
                // copy 
                node->opcode = make_op(op_type::mov_reg,opcode.v[0],opcode.v[1]);
                rewrite_opcode(itl,alloc,block,node);

                node = node->next;
            }


            break;
        }

        case op_type::div_x86:
        {
            node = rewrite_x86_fixed_arith(itl,alloc,block,node,sym_from_idx(RAX_IR));
            break;
        }

        case op_type::mod_x86:
        {
            node = rewrite_x86_fixed_arith(itl,alloc,block,node,sym_from_idx(RDX_IR));
            break;
        }


        case op_type::mul_x86:
        {
            node = rewrite_x86_fixed_arith(itl,alloc,block,node,sym_from_idx(RAX_IR));
            break;
        }

        case op_type::lsl_x86:
        {
            node = rewrite_x86_shift(itl,alloc,block,node);
            break;
        }

        case op_type::lsr_x86:
        {
            node = rewrite_x86_shift(itl,alloc,block,node);
            break;
        }

        case op_type::asr_x86:
        {
            node = rewrite_x86_shift(itl,alloc,block,node);
            break;
        }

        case op_type::lock_reg:
        {
            const auto spec_reg = sym_from_idx(opcode.v[0]);

            // mark register as reserved
            lock_reg(alloc,table,block,node,spec_reg);

            node = remove(block.list,node);
            break;
        }

        case op_type::unlock_reg:
        {
            const auto spec_reg = sym_from_idx(opcode.v[0]);

            // mark register as reserved
            unlock_reg(alloc.reg_alloc,spec_reg);

            node = remove(block.list,node);
            break;
        }

        case op_type::addrof:
        {
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>

            const auto slot = sym_from_idx(opcode.v[1]);
            auto& reg = reg_from_slot(slot,table,alloc);


            if(is_stack_unallocated(reg))
            {
                if(!stored_in_mem(reg))
                {
                    printf("error in func: %s\n",func.name.buf);
                    print_slot(itl.symbol_table,slot);
                    assert(false);
                }
                
                stack_reserve_reg(alloc.stack_alloc,reg);  
            }

            u32 offset = node->opcode.v[2];

            // local add the stack offset
            if(is_local(reg))
            {
                offset += alloc.stack_alloc.stack_offset;
            }

            // okay apply the stack offset, and let the register allocator deal with it
            // we will get the actual address using it later
            node->opcode = Opcode(op_type::addrof,opcode.v[0],opcode.v[1],offset);

            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(table,alloc,block,node,0);

            node = node->next;
            break;            
        }

        case op_type::load_struct_s8:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_s16:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_s32:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_u8:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_u16:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_u32:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_struct_u64:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::store_struct_u8:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::store_struct_u16:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::store_struct_u32:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::store_struct_u64:
        {
            node = rewrite_access_struct(itl,func,alloc,table,block,node);
            break;
        }

        case op_type::load_func_addr:
        {

            // just rewrite the 1st reg
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
            alloc.stack_alloc.stack_offset += GPR_SIZE;

            node = node->next;
            break;
        }

        case op_type::clean_args:
        {
            // clean up args
            const auto stack_clean = GPR_SIZE * opcode.v[0];

            node->opcode = Opcode(op_type::add_imm2,SP_IR,stack_clean,0);
            alloc.stack_alloc.stack_offset -= stack_clean; 

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            log(alloc.stack_alloc.print,"clean args: %x\n",stack_clean);
    
            node = node->next;
            break;
        }

        case op_type::alloc_stack:
        {
            const u32 size = opcode.v[0];
        
            // nothing to do
            if(size == 0)
            {
                node = remove(block.list,node);
                break;
            }
        
            node->opcode = make_op(op_type::sub_imm2,SP_IR,size);
            alloc.stack_alloc.stack_offset += size;

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.stack_alloc.print)
            {
                printf("allocate stack %x\n",size);
            }


            node = node->next;
            break;
        }

        case op_type::alloc_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);
            auto& reg = reg_from_slot(slot,table,alloc);
            

            if(alloc.reg_alloc.print)
            {
                if(is_sym(slot))
                {
                    auto& sym = sym_from_slot(table,slot);
                    printf("alloc slot: %s : %s\n",sym.name.buf,opcode.v[1]? "forced" : "unforced");
                }

                else
                {
                    printf("alloc slot: t%d : %s\n",reg.slot.handle,opcode.v[1]? "forced" : "unforced");
                }
            }

            // explictly force a stack alloc now
            if(opcode.v[1] && reg.kind != reg_kind::global)
            {
                stack_reserve_reg(alloc.stack_alloc,reg);    
            }

            node = remove(block.list,node);
            break;
        }

        case op_type::alloc_local_array:
        {
            const u32 size = opcode.v[1];
            const u32 count = opcode.v[2];

            const SymSlot slot = sym_from_idx(opcode.v[0]);

            const u32 offset = allocate_stack_array(alloc.stack_alloc,table,slot,size,count);
            
            node->opcode = Opcode(op_type::alloc_local_array,opcode.v[0],offset,0);

            allocate_and_rewrite(table,alloc,block,node,0);

            node = node->next;
            break;
        }

        case op_type::alloc_global_array:
        {
            const u32 idx = node->opcode.v[1];

            auto& global_alloc = itl.global_alloc;

            ArrayAllocation &allocation = global_alloc.array_allocation[idx];

            allocation.offset = calc_final_offset(itl.global_alloc.start,allocation.size,allocation.offset);

            if(alloc.stack_alloc.print)
            {
                auto& sym = sym_from_slot(itl.symbol_table,allocation.slot);
                printf("final array offset %s = [%x,%x] -> (%x)\n",sym.name.buf,allocation.size,allocation.count,allocation.offset);
            }

            allocate_and_rewrite(table,alloc,block,node,0);

            node->opcode = Opcode(op_type::lea,node->opcode.v[0],GP_IR,allocation.offset);

            node = node->next;
            break;
        }

        case op_type::spill_all:
        {
            spill_all(alloc,itl.symbol_table,block,node,false);
            return remove(block.list,node);
        }

        case op_type::spill_func_bounds:
        {
            spill_func_bounds(alloc,itl.symbol_table,block,node);

            return remove(block.list,node);
        }


        // scope has elapsed any resources can be reclaimed
        // TODO: should this support registers?
        case op_type::free_slot:
        {
            // TODO: can we reclaim stack space from this?
            return remove(block.list,node);
        }


        case op_type::free_fixed_array:
        {
            // TODO: can we reclaim stack space from this?            
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


ListNode* rewrite_access_struct_addr(Interloper& itl, LocalAlloc& alloc, ListNode* node, op_type type)
{
    const u32 base_offset = node->opcode.v[2];
    
    const SymSlot slot = sym_from_idx(node->opcode.v[1]);

    auto &reg = reg_from_slot(slot,itl.symbol_table,alloc);

    assert(is_stored_in_mem(reg));

    const auto [offset_reg,offset] = reg_offset(itl,reg,0);

    node->opcode = Opcode(type,node->opcode.v[0],offset_reg,offset + base_offset);

    return node->next;    
}

// 2nd pass of rewriting on the IR
ListNode* rewrite_directives(Interloper& itl,LocalAlloc &alloc,Block& block, ListNode *node,const u32 saved_regs,
    const Opcode& stack_clean, bool insert_callee_saves)
{
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
            if(alloc.stack_alloc.stack_size)
            {
                insert_at(block.list,node,stack_clean);
            }

            // make sure callee restore comes after stack clean
            if(insert_callee_saves)
            {
                emit_popm(itl,block,node,saved_regs);
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
                static const op_type instr[4] = {op_type::lsb, op_type::lsh, op_type::lsw,op_type::ld};

                // this here does not otherwhise need rewriting so we will emit SP directly
                node->opcode = Opcode(instr[log2(reg.size)],opcode.v[0],offset_reg,offset);        
            }

            // "plain data"
            // just move by size
            else
            {
                static const op_type instr[4] = {op_type::lb, op_type::lh, op_type::lw,op_type::ld};

                node->opcode =  Opcode(instr[log2(reg.size)],opcode.v[0],offset_reg,offset);
            }
            
            node = node->next;
            break;
        }

        case op_type::addrof:
        {
            const s32 base_offset = opcode.v[2];
            const SymSlot slot = sym_from_idx(opcode.v[1]);

            auto &reg = reg_from_slot(slot,itl.symbol_table,alloc);


            assert(is_mem_allocated(reg));

            auto [offset_reg,offset] = reg_offset(itl,reg,0);

            node->opcode = Opcode(op_type::lea,opcode.v[0],offset_reg,base_offset + offset);

            node = node->next;
            break;
        }

        case op_type::load_struct_s8:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lsb);
            break;
        }

        case op_type::load_struct_s16:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lsh);
            break;
        }

        case op_type::load_struct_s32:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lsw);
            break;
        }

        case op_type::load_struct_u8:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lb);
            break;
        }

        case op_type::load_struct_u16:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lh);
            break;
        }

        case op_type::load_struct_u32:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lw);
            break;
        }

        case op_type::load_struct_u64:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::ld);
            break;
        }

        case op_type::store_struct_u8:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::sb);
            break;
        }

        case op_type::store_struct_u16:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::sh);
            break;
        }

        case op_type::store_struct_u32:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::sw);
            break;
        }

        case op_type::store_struct_u64:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::sd);
            break;
        }


        case op_type::alloc_local_array:
        {
            const u32 idx = node->opcode.v[1];

            ArrayAllocation &allocation = alloc.stack_alloc.array_allocation[idx];

            allocation.offset = finalise_offset(alloc.stack_alloc,allocation.offset,allocation.size);

            if(alloc.stack_alloc.print)
            {
                auto& sym = sym_from_slot(itl.symbol_table,allocation.slot);
                printf("final array offset %s = [%x,%x] -> (%x)\n",sym.name.buf,allocation.size,allocation.count,allocation.offset);
            }

            // NOTE: this is allways on the stack, globals handle their own allocation...
            node->opcode = Opcode(op_type::lea,opcode.v[0],arch_sp(itl.arch),allocation.offset + allocation.stack_offset);

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

            static const op_type instr[4] = {op_type::sb, op_type::sh, op_type::sw,op_type::sd};
    
            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            node->opcode = Opcode(instr[log2(reg.size)],opcode.v[0],offset_reg,offset);   

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
    auto alloc = make_local_alloc(itl.print_reg_allocation,itl.print_stack_allocation,func.registers,itl.arch);

    log(alloc.reg_alloc.print,"allocating registers for %s:\n\n",func.name.buf);

    // figure out how long each sym lives
    mark_lifetimes(func,func.registers,itl.symbol_table);

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

        const auto& ENTRY = info_from_op(opcode); 

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

    if(alloc.reg_alloc.print)
    {
        print_reg_alloc(alloc.reg_alloc,itl.symbol_table);
    }

    // only allocate a stack if we need it
    if(alloc.stack_alloc.stack_size)
    {
        const u32 SP = arch_sp(itl.arch);
        insert_front(func.emitter.program[0].list,make_op(op_type::sub_imm2,SP,alloc.stack_alloc.stack_size));
    }


    // iterate over the function by here and add callee cleanup at every ret
    // and insert the stack offsets and load and spill directives

    // TODO: this might be good to loop jam with somethign but just have a seperate loop for simplictiy atm


    // R0 is callee saved
    static constexpr u32 CALLEE_SAVED_MASK = 1;

    // make sure callee saved regs are not saved inside the func
    const u32 saved_regs = alloc.reg_alloc.used_regs & ~CALLEE_SAVED_MASK;
    const u32 save_count = popcount(saved_regs);

    const bool insert_callee_saves = func.name != "main" && save_count != 0;


    alloc_args(func,alloc.stack_alloc,itl.symbol_table,insert_callee_saves? GPR_SIZE * save_count : 0);

    // entry point does not need to preserve regs
    if(insert_callee_saves)
    {
        auto& start_block = func.emitter.program[0];

        emit_pushm(itl,start_block,start_block.list.start,saved_regs);
    }

    const u32 SP = arch_sp(itl.arch);
    const auto stack_clean = Opcode(op_type::add_imm2,SP,alloc.stack_alloc.stack_size,0);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        auto node = block.list.start;

        while(node)
        {
            node = rewrite_directives(itl,alloc,block,node,saved_regs,stack_clean,insert_callee_saves);
        }
    }

    destroy_local_alloc(alloc);
}
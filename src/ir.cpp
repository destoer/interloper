#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"
#include "pool.cpp"
#include "emitter.cpp"
#include "rewrite_arch_ir.cpp"
#include "cfg.cpp"
#include "asm.cpp"
#include "x86_emitter.cpp"
#include "stack_allocator.cpp"
#include "linear_alloc.cpp"
#include "disass.cpp"

u32 get_mov_register(LinearAlloc& alloc,SymSlot slot)
{
    if(is_var(slot))
    {
        const auto reg_opt = lookup(alloc.location,slot);

        // var is allocated
        if(reg_opt)
        {
            return *reg_opt;
        }

        // not allocated
        return REG_FREE;
    }
    
    // special reg
    else 
    {
        const u32 location = special_reg_to_reg(alloc.arch,slot);
        mark_used(alloc,location);

        return location;
    }
}

ListNode* move(Interloper& itl,LinearAlloc& alloc, Block& block, ListNode* node)
{
    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    const auto dst = sym_from_idx(opcode.v[0]);
    const auto src = sym_from_idx(opcode.v[1]);

    const u32 src_reg = get_mov_register(alloc,src);
    const u32 dst_reg = get_mov_register(alloc,dst);

    const b32 dst_free = dst_reg == REG_FREE;
    const b32 src_free = src_reg == REG_FREE;

    // no dst and src
    // do memory to memory move
    if(dst_free && src_free)
    {
        reload_reg(alloc,table,block,node,src,alloc.scratch_regs[1]);
        spill_reg(alloc,table,block,node, dst, alloc.scratch_regs[1], false);
        node = remove(block.list,node);
    }

    // spill src into dst
    else if(dst_free)
    {
        spill_reg(alloc,table,block,node, dst, src_reg, false);
        node = remove(block.list,node);
    }

    // reload src into dst
    else if(src_free)
    {
        reload_reg(alloc,table,block,node,src,dst_reg);
        node = remove(block.list,node);
    }

    // reg to reg move just rewrite
    else
    {
        rewrite_opcode(alloc,table,block,node);
        node = node->next;
    }

    return node;
}

ListNode* rewrite_x86_fixed_arith(LinearAlloc& alloc,SymbolTable& table,Block& block, ListNode* node, SymSlot out)
{
    // save where our dst is being forced into
    const auto dst = sym_from_idx(node->opcode.v[0]);

    mark_used(alloc,u32(x86_reg::rax));
    mark_used(alloc,u32(x86_reg::rdx));

    // rewrite src
    allocate_and_rewrite(alloc,table,block,node,1);

    // mark the output reg
    node->opcode.v[0] = out.handle;

    // insert the mov to move it from the out reg into the new one
    const auto mov = make_op(op_type::mov_reg,dst.handle,out.handle);
    node = insert_after(block.list,node,mov);

    // NOTE: RAX, and RDX would be considered unlocked again here

    return node;
}   

ListNode* rewrite_x86_shift(LinearAlloc& alloc,SymbolTable& table,Block& block, ListNode* node)
{
    // NOTE: RCX would be considered unlocked again

    rewrite_opcode(alloc,table,block,node);
    return node->next;
}

ListNode* rewrite_access_struct(LinearAlloc &alloc,SymbolTable& table,Block &block, ListNode *node)
{
    const auto slot = sym_from_idx(node->opcode.v[1]);
    auto& reg = reg_from_slot(slot,table,alloc);

    if(is_stack_unallocated(reg))
    {
        assert(stored_in_mem(reg));

        stack_reserve_reg(alloc.stack_alloc,reg);
    }

    if(is_local(reg))
    {
        // add the stack offset, so this correctly offset for when we fully rewrite this
        node->opcode.v[2] += alloc.stack_alloc.stack_offset;
    }

    allocate_and_rewrite(alloc,table,block,node,0);
    return node->next;
}

ListNode* allocate_opcode(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, ListNode* node)
{
    UNUSED(func);

    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    switch(node->opcode.op)
    {
        case op_type::mov_reg:
        {
            node = move(itl,alloc,block,node);
            break;
        }

        case op_type::udiv_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,table,block,node,sym_from_idx(RAX_IR));
            break;
        }

        case op_type::sdiv_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,table,block,node,sym_from_idx(RAX_IR));
            break;
        }


        case op_type::umod_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,table,block,node,sym_from_idx(RDX_IR));
            break;
        }

        case op_type::smod_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,table,block,node,sym_from_idx(RDX_IR));
            break;
        }

        case op_type::mul_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,table,block,node,sym_from_idx(RAX_IR));
            break;
        }

        case op_type::lsl_x86:
        {
            node = rewrite_x86_shift(alloc,table,block,node);
            break;
        }

        case op_type::lsr_x86:
        {
            node = rewrite_x86_shift(alloc,table,block,node);
            break;
        }

        case op_type::asr_x86:
        {
            node = rewrite_x86_shift(alloc,table,block,node);
            break;
        }

        case op_type::lock_reg:
        {
            // we dont need to acually use this yet
            return remove(block.list,node);
        }

        case op_type::unlock_reg:
        {
            // we dont need to acually use this yet
            return remove(block.list,node);
        }

        case op_type::addrof:
        {
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>
            const auto slot = sym_from_idx(opcode.v[1]);
            const auto dst = sym_from_idx(opcode.v[0]);
            auto& reg = reg_from_slot(slot,table,alloc);

            log_reg(alloc.print,table,"addrof %r <- %r\n",dst,slot);

            if(is_stack_unallocated(reg))
            {
                assert(stored_in_mem(reg));

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
            allocate_and_rewrite(alloc,table,block,node,0);

            node = node->next;
            break;
        }

        case op_type::load_struct_s8:
        case op_type::load_struct_s16:
        case op_type::load_struct_s32:
        case op_type::load_struct_u8:
        case op_type::load_struct_u16:
        case op_type::load_struct_u32:
        case op_type::load_struct_u64:
        case op_type::store_struct_u8:
        case op_type::store_struct_u16:
        case op_type::store_struct_u32:
        case op_type::store_struct_u64:
        {
            node = rewrite_access_struct(alloc,table,block,node);
            break;
        }

        case op_type::load_func_addr:
        {
            // just rewrite the 1st reg
            allocate_and_rewrite(alloc,table,block,node,0);

            node = node->next;
            break;
        }

        case op_type::reload_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);

            reload_slot(alloc,table,block,node,slot);

            node = remove(block.list,node);
            break;  
        }

        case op_type::spill_slot:
        {
            const SymSlot slot = sym_from_idx(opcode.v[0]);

            spill(alloc,table,block,node,slot,false);

            node = remove(block.list,node);
            break;
        }

        // have to do opcode rewriting by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            node->opcode =  Opcode(op_type::push,opcode.v[0],0,0);

            // adjust opcode for reg alloc
            rewrite_opcode(alloc,table,block,node);

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

            // adjust opcode for reg alloc
            rewrite_opcode(alloc,table,block,node);

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
            rewrite_opcode(alloc,table,block,node);

            alloc.stack_alloc.stack_offset += size;

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

            log_reg(alloc.print,table,"alloc slot: %r : %s\n",slot,opcode.v[1]? "forced" : "unforced");

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

            allocate_and_rewrite(alloc,table,block,node,0);

            node = node->next;
            break;
        }

        case op_type::alloc_global_array:
        {
            assert(false);
        }

        case op_type::spill_all:
        {
            assert(false);
        }
    
        case op_type::spill_func_bounds:
        {
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
            allocate_and_rewrite(alloc,table,block,node,0);

            node = node->next;
            break;
        }


        default:
        {
            rewrite_opcode(alloc,table,block,node);
            node = node->next;
            break; 
        }
    }

    return node;
}

ListNode* rewrite_access_struct_addr(Interloper& itl, LinearAlloc& alloc, ListNode* node, op_type type)
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
ListNode* rewrite_directives(Interloper& itl,LinearAlloc &alloc,Block& block, ListNode *node,const u32 saved_regs,
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

            // double check this value has actually been put into memory...
            //assert(reg.offset != UNALLOCATED_OFFSET);

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
    auto alloc = make_linear_alloc(itl.print_reg_allocation,itl.print_stack_allocation,func.registers,itl.arch);

    linear_allocate(alloc,itl,func);

    log(alloc.print,"allocating registers for %s:\n",func.name.buf);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        log(alloc.print,"\nprocessing L%d:\n\n",block.label_slot.handle);
        
        ListNode *node = block.list.start;

        while(node)
        {
            node = allocate_opcode(itl,func,alloc,block,node);
            alloc.pc++;
        }
    }

    // perform 2nd pass!

    // Figure out how large a stack we need and put everything on it
    finish_stack_alloc(itl.symbol_table,alloc);

    if(alloc.print)
    {
        print_reg_alloc(alloc);
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
    const u32 SCRATCH_MASK = (1 << alloc.scratch_regs[0]) | (1 << alloc.scratch_regs[1]) | (1 << alloc.scratch_regs[2]); 

    // make sure callee saved regs are not saved inside the func
    const u32 saved_regs = alloc.used_set & ~(CALLEE_SAVED_MASK | SCRATCH_MASK);
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

    destroy_linear_alloc(alloc);
}
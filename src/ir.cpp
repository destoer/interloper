#include <interloper.h>
#include <op_table.inl>

#include "pool.cpp"
#include "emitter.cpp"
#include "rewrite_arch_ir.cpp"
#include "cfg.cpp"
#include "asm.cpp"
#include "x86_emitter.cpp"
#include "stack_allocator.cpp"
#include "linear_alloc.cpp"
#include "disass.cpp"
#include "ir_x86.cpp"


OpcodeNode* rewrite_access_struct(LinearAlloc &alloc,Block &block, OpcodeNode* node)
{
    const auto slot = node->value.v[1].reg;
    auto& reg = reg_from_slot(slot,alloc);

    if(is_stack_unallocated(reg))
    {
        assert(stored_in_mem(reg));

        stack_reserve_reg(alloc.stack_alloc,reg);
    }

    if(is_local(reg))
    {
        // add the stack offset, so this correctly offset for when we fully rewrite this
        node->value.v[2].imm += alloc.stack_alloc.stack_offset;
    }

    allocate_and_rewrite(alloc,block,node,0);
    return node->next;
}

OpcodeNode* allocate_opcode(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    UNUSED(func);

    const auto &opcode = node->value;

    switch(node->value.op)
    {
        case op_type::load_const_float:
        {
            // grab the offset we want
            const auto pool_slot = pool_slot_from_idx(opcode.v[1].raw);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(alloc,block,node,0);

            node->value = make_raw_op(op_type::lf,node->value.v[0].raw,u32(spec_reg::const_seg),section.offset);

            node = node->next;
            break;
        }

        case op_type::live_var:
        {
            const auto slot = opcode.v[0].reg;
            auto& ir_reg = reg_from_slot(itl,func,slot);

            // issue a reload
            if(is_reg_locally_allocated(ir_reg))
            {
                reload_reg(alloc,block,node,slot,ir_reg.local_reg,insertion_type::before);
            }

            node = remove(block.list,node);
            break;
        }

        case op_type::call_reg:
        {
            const auto slot = opcode.v[0].reg;
            auto& ir_reg = reg_from_slot(itl,func,slot);

            allocate_and_rewrite(alloc,block,node,0);

            // spill_func_bounds is issued before the call (and will save this register if need be)
            // Incase the rv is used as the caller we have to free this up after the func call
            // to keep the state consistent. otherwhise  this will have a clobbered value
            if(ir_reg.local_reg == special_reg_to_reg(alloc.arch,spec_reg::rv_gpr))
            {
                free_ir_reg(ir_reg,get_register_file(alloc,ir_reg));
            }

            node = node->next;
            break;
        }

        case op_type::udiv_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,block,node,op_type::udiv_x86);
            break;
        }

        case op_type::sdiv_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,block,node,op_type::sdiv_x86);
            break;
        }


        case op_type::umod_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,block,node,op_type::umod_x86);
            break;
        }

        case op_type::smod_x86:
        {
            node = rewrite_x86_fixed_arith(alloc,block,node,op_type::smod_x86);
            break;
        }

        case op_type::lsl_x86:
        {
            node = rewrite_x86_shift(alloc,block,node);
            break;
        }

        case op_type::lsr_x86:
        {
            node = rewrite_x86_shift(alloc,block,node);
            break;
        }

        case op_type::asr_x86:
        {
            node = rewrite_x86_shift(alloc,block,node);
            break;
        }

        case op_type::lock_reg:
        {
            const auto reg = opcode.v[0].reg;
            lock_special_reg(alloc,block,node,reg.spec);

            return remove(block.list,node);
        }

        case op_type::mov_unlock:
        {
            const auto reg = opcode.v[1].reg;
            unlock_special_reg(alloc,reg.spec);
            rewrite_opcode(alloc,block,node);
            node->value.op = op_type::mov_reg;
            node = node->next;
            break;
        }

        case op_type::unlock_reg:
        {
            const auto reg = opcode.v[0].reg;
            unlock_special_reg(alloc,reg.spec);

            return remove(block.list,node);
        }

        case op_type::lock_reg_set:
        {
            const auto set = opcode.v[0].imm;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(set,r))
                {
                    lock_reg(alloc.gpr,r);
                }
            }

            return remove(block.list,node);
        }


        case op_type::unlock_reg_set:
        {
            const auto set = opcode.v[0].imm;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(set,r))
                {
                    unlock_reg(alloc.gpr,r);
                }
            }

            return remove(block.list,node);
        }

        case op_type::addrof:
        {
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>
            const auto slot = opcode.v[1].reg;
            const auto dst = opcode.v[0].reg;
            auto& reg = reg_from_slot(slot,alloc);

            log_reg(alloc.print,*alloc.table,"addrof %r <- %r\n",dst,slot);

            if(is_stack_unallocated(reg))
            {
                assert(stored_in_mem(reg));

                stack_reserve_reg(alloc.stack_alloc,reg);
            }

            u32 offset = node->value.v[2].imm;

            // local add the stack offset
            if(is_local(reg))
            {
                offset += alloc.stack_alloc.stack_offset;
            }

            // okay apply the stack offset, and let the register allocator deal with it
            // we will get the actual address using it later
            node->value = make_op(op_type::addrof,opcode.v[0],opcode.v[1],make_imm_operand(offset));

            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(alloc,block,node,0);

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
        case op_type::load_struct_f64:
        case op_type::store_struct_u8:
        case op_type::store_struct_u16:
        case op_type::store_struct_u32:
        case op_type::store_struct_u64:
        case op_type::store_struct_f64:
        {
            node = rewrite_access_struct(alloc,block,node);
            break;
        }

        case op_type::load_func_addr:
        {
            // just rewrite the 1st reg
            allocate_and_rewrite(alloc,block,node,0);

            node = node->next;
            break;
        }

        case op_type::reload_slot:
        {
            const auto slot = opcode.v[0].reg;

            reload_slot(alloc,block,node,slot);

            node = remove(block.list,node);
            break;  
        }

        case op_type::spill_slot:
        {
            const auto slot = opcode.v[0].reg;

            spill(alloc,block,node,slot,insertion_type::before);

            node = remove(block.list,node);
            break;
        }

        // have to do opcode rewriting by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            node->value =  make_op(op_type::push,opcode.v[0]);

            // adjust opcode for reg alloc
            rewrite_opcode(alloc,block,node);

            // varaibles now have to be accessed at a different offset
            // until this is corrected by clean call
            alloc.stack_alloc.stack_offset += GPR_SIZE;

            node = node->next;
            break;
        }

        case op_type::clean_args:
        {
            // clean up args
            const auto stack_clean = GPR_SIZE * opcode.v[0].imm;

            node->value = make_op(op_type::add_imm2,make_spec_operand(spec_reg::sp),make_imm_operand(stack_clean));
            alloc.stack_alloc.stack_offset -= stack_clean;

            // adjust opcode for reg alloc
            rewrite_opcode(alloc,block,node);

            log(alloc.stack_alloc.print,"clean args: %x\n",stack_clean);

            node = node->next;
            break;
        }

        case op_type::alloc_stack:
        {
            const u32 size = opcode.v[0].imm;

            // nothing to do
            if(size == 0)
            {
                node = remove(block.list,node);
                break;
            }

            node->value = make_op(op_type::sub_imm2,make_spec_operand(spec_reg::sp),make_imm_operand(size));
            rewrite_opcode(alloc,block,node);

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
            const auto slot = opcode.v[0].reg;
            auto& reg = reg_from_slot(slot,alloc);

            log_reg(alloc.print,*alloc.table,"alloc slot: %r : %s\n",slot,opcode.v[1].raw? "forced" : "unforced");

            // explictly force a stack alloc now
            if(opcode.v[1].raw && reg.segment != reg_segment::global)
            {
                stack_reserve_reg(alloc.stack_alloc,reg);
            }

            node = remove(block.list,node);  
            break;       
        }

        case op_type::alloc_local_array:
        {
            const u32 size = opcode.v[1].imm;
            const u32 count = opcode.v[2].imm;
            const auto slot = opcode.v[0].reg;

            const u32 offset = allocate_stack_array(alloc.stack_alloc,*alloc.table,slot.sym_slot,size,count);

            node->value = make_op(op_type::alloc_local_array,opcode.v[0],make_imm_operand(offset));

            allocate_and_rewrite(alloc,block,node,0);

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
            // clear our any caller saved regs
            save_caller_saved_regs(alloc,block,node);
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
            allocate_and_rewrite(alloc,block,node,0);

            node = node->next;
            break;
        }


        default:
        {
            rewrite_opcode(alloc,block,node);
            node = node->next;
            break; 
        }
    }

    return node;
}

OpcodeNode* rewrite_access_struct_addr(Interloper& itl, LinearAlloc& alloc, OpcodeNode* node, op_type type)
{
    const u32 base_offset = node->value.v[2].imm;
    const RegSlot slot = node->value.v[1].reg;

    auto &reg = reg_from_slot(slot,alloc);

    assert(is_stored_in_mem(reg));

    const auto [offset_reg,offset] = reg_offset(itl,reg,0);

    node->value = make_op(type,node->value.v[0],make_imm_operand(offset_reg),make_imm_operand(offset + base_offset));

    return node->next;
}

// 2nd pass of rewriting on the IR
OpcodeNode* rewrite_directives(Interloper& itl,LinearAlloc &alloc,Block& block, OpcodeNode* node,
    const u32 saved_gpr, const u32 saved_fpr,const Opcode& stack_clean)
{
    const auto opcode = node->value;

    switch(node->value.op)
    {

        case op_type::movf_reg:
        case op_type::mov_reg:
        {
            // remove dead stores (still need to perform reg correction)
            // so it has to be done in the 2nd pass
            if(opcode.v[0] == opcode.v[1])
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
            // Normal sub emit before pops
            if(alloc.stack_alloc.stack_size)
            {
                insert_at(block.list,node,stack_clean);
            }

            // make sure callee restore comes after stack clean

            // NOTE: as floats were last thing saved, they are tthe first to restore
            OpcodeNode* float_node = emit_popm_float(itl,block,node,saved_fpr);
            emit_popm(itl,block,float_node,saved_gpr);


            if(itl.debug)
            {
                insert_at(block.list,node,make_raw_op(op_type::leave,0,0,0));
            }

            node = node->next;
            break;
        }

        // reload a reg
        case op_type::load:
        {
            RegSlot slot = opcode.v[1].reg;

            auto& reg = reg_from_slot(slot,alloc);

            // double check this value has actually been put into memory...
            //assert(reg.offset != UNALLOCATED_OFFSET);

            const s32 stack_offset = opcode.v[2].imm;


            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            if(!(reg.flags & REG_FLOAT))
            {
                // reload the spilled var
                if(is_signed(reg))
                {
                    // word is register size (we dont need to extend it)
                    static const op_type instr[4] = {op_type::lsb, op_type::lsh, op_type::lsw,op_type::ld};

                    // this here does not otherwhise need rewriting so we will emit SP directly
                    node->value = make_raw_op(instr[log2(reg.size)],opcode.v[0].raw,offset_reg,offset);
                }

                // "plain data"
                // just move by size
                else
                {
                    static const op_type instr[4] = {op_type::lb, op_type::lh, op_type::lw,op_type::ld};

                    node->value = make_raw_op(instr[log2(reg.size)],opcode.v[0].raw,offset_reg,offset);
                }
            }

            else
            {
                node->value = make_raw_op(op_type::lf,opcode.v[0].raw,offset_reg,offset);
            }

            node = node->next;
            break;
        }

        case op_type::addrof:
        {
            const s32 base_offset = opcode.v[2].imm;
            const RegSlot slot = opcode.v[1].reg;

            auto &reg = reg_from_slot(slot,alloc);


            assert(is_mem_allocated(reg));

            auto [offset_reg,offset] = reg_offset(itl,reg,0);

            node->value = make_raw_op(op_type::lea,opcode.v[0].raw,offset_reg,base_offset + offset);

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

        case op_type::load_struct_f64:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::lf);
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

        case op_type::store_struct_f64:
        {
            node = rewrite_access_struct_addr(itl,alloc,node,op_type::sf);
            break;
        }


        case op_type::alloc_local_array:
        {
            const u32 idx = node->value.v[1].imm;

            ArrayAllocation &allocation = alloc.stack_alloc.array_allocation[idx];

            allocation.offset = finalise_offset(alloc.stack_alloc,allocation.offset,allocation.size);

            if(alloc.stack_alloc.print)
            {
                auto& sym = sym_from_slot(itl.symbol_table,allocation.slot);
                printf("final array offset %s = [%x,%x] -> (%x)\n",sym.name.buf,allocation.size,allocation.count,allocation.offset);
            }

            // NOTE: this is allways on the stack, globals handle their own allocation...
            node->value = make_raw_op(op_type::lea,opcode.v[0].raw,arch_sp(itl.arch),allocation.offset + allocation.stack_offset);

            node = node->next;
            break;
        }

        case op_type::spill:
        {
            RegSlot slot = opcode.v[1].reg;
            auto& reg = reg_from_slot(slot,alloc);

            const s32 stack_offset = opcode.v[2].imm;

            // write value back out into mem
            const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            if(!(reg.flags & REG_FLOAT))
            {
                static const op_type instr[4] = {op_type::sb, op_type::sh, op_type::sw,op_type::sd};
                node->value = make_raw_op(instr[log2(reg.size)],opcode.v[0].raw,offset_reg,offset);
            }

            else
            {
                node->value = make_raw_op(op_type::sf,opcode.v[0].raw,offset_reg,offset);
            }

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
    auto alloc = make_linear_alloc(itl.print_reg_allocation,itl.print_stack_allocation,itl.stack_alloc,itl.debug,func.registers,&itl.symbol_table,itl.arch);

    linear_allocate(alloc,itl,func);

    log(alloc.print,"allocating registers for %s:\n",func.name.buf);

    for(auto& block : func.emitter.program)
    {
        log(alloc.print,"\nprocessing L%d:\n\n",block.label_slot.handle);

        linear_setup_new_block(alloc,block);

        OpcodeNode *node = block.list.start;

        while(node)
        {
            clean_dead_regs(alloc);

            node = allocate_opcode(itl,func,alloc,block,node);
            alloc.pc++;
        }

        clean_dead_regs(alloc);
        correct_live_out(alloc,block);
    }

    if(alloc.total_misplaced != 0)
    {
        log(alloc.print,"%s: Total misplaced %d\n",func.name.buf,alloc.total_misplaced);
    }

    // perform 2nd pass!

    // Figure out how large a stack we need and put everything on it
    finish_stack_alloc(alloc);

    log(alloc.stack_alloc.print,"calling stack size: %d\n",func.sig.call_stack_size + GPR_SIZE);

    if(alloc.print)
    {
        print_reg_alloc(alloc);
    }

    auto& entry = func.emitter.program[0];
    const u32 SP = arch_sp(itl.arch);
    const u32 FP = arch_fp(itl.arch);

    // iterate over the function by here and add callee cleanup at every ret
    // and insert the stack offsets and load and spill directives

    // RA is callee saved
    const u32 CALLEE_GPR_SAVED_MASK = (
        1 << arch_rv(alloc.arch) | 
        1 << special_reg_to_reg(alloc.arch,spec_reg::a1) |
        1 << special_reg_to_reg(alloc.arch,spec_reg::a2)
    );

    // RA is callee saved
    const u32 CALLEE_FPR_SAVED_MASK = (1 << arch_frv(alloc.arch));
   
    u32 saved_gpr = 0;
    u32 saved_fpr = 0;
    
    // we dont want to save these on start
    if(func.name != "start")
    {
        // Return address
        u32 frame_offset = 1;

        // make sure callee saved regs are not saved inside the func
        saved_gpr = alloc.gpr.used_set & ~(CALLEE_GPR_SAVED_MASK | (1 << SP));
        if(itl.debug)
        {
            saved_gpr = deset_bit(saved_gpr,FP);
            // Push fp
            frame_offset += 1;
        }

        saved_fpr = alloc.fpr.used_set & ~(CALLEE_FPR_SAVED_MASK);

        // return addr + saved regs + call stack
        const u32 call_align = frame_offset + popcount(saved_gpr) + popcount(saved_fpr) + (func.sig.call_stack_size / GPR_SIZE);

        // add pad to align the stack on the correct boundary
        if(call_align & 1 && !func.leaf_func)
        {
            log(alloc.stack_alloc.print,"adding + GPR_SIZE padding to align stack\n");
            alloc.stack_alloc.stack_size += GPR_SIZE;
        }
    }


    const u32 save_count = popcount(saved_gpr) + popcount(saved_fpr);

    log(alloc.print,"saved registers: %d (0x%x)\n",save_count,saved_gpr);


    alloc_args(func,alloc.stack_alloc,itl.symbol_table,GPR_SIZE * save_count);

    // entry point does not need to preserve regs
    auto& start_block = func.emitter.program[0];

    auto float_node = emit_pushm(itl,start_block,start_block.list.start,saved_gpr);
    auto alloc_node = emit_pushm_float(itl,start_block,float_node,saved_fpr);

    // only allocate a stack if we need it
    if(alloc.stack_alloc.stack_size)
    {
        insert_at(entry.list,alloc_node,make_raw_op(op_type::sub_imm2,SP,alloc.stack_alloc.stack_size));
    }


    // Insert the frame pointer options at the front
    if(itl.debug)
    {
        auto frame_node = insert_front(entry.list,make_raw_op(op_type::push,FP,0,0));
        insert_after(entry.list,frame_node,make_raw_op(op_type::mov_reg,FP,SP,0));
    }


    const auto stack_clean = make_raw_op(op_type::add_imm2,SP,alloc.stack_alloc.stack_size);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        auto node = block.list.start;

        while(node)
        {
            node = rewrite_directives(itl,alloc,block,node,saved_gpr,saved_fpr,stack_clean);
        }
    }

    destroy_linear_alloc(alloc);
}
#include "backend/lower_reg.cpp"

void allocate_and_rewrite_opcode(LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    const auto ir_span = opcode_ir_reg_span(node->value,alloc.ir_reg_span);
    const auto lowered_reg_span = linear_allocate_registers(alloc,block,node,ir_span,alloc.lowered_reg_span);

    auto& opcode = node->value;
    lower_opcode(alloc,opcode,lowered_reg_span);
}

OpcodeNode* allocate_opcode_reg_pass(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    UNUSED(func); 

    const auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::x86_fixed:
        {
            node = lower_x86_fixed(alloc,block,node); 
            break;
        }

        case op_group::directive:
        {
            node = lower_directive_reg_pass(itl,alloc,block,node);
            break;   
        }

        default:
        {
            allocate_and_rewrite_opcode(alloc,block,node);
            node = node->next;
            break;
        }
    }

    return node;
}

OpcodeNode* allocate_opcode_addr_pass(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, OpcodeNode* node, const Opcode& stack_clean)
{
    UNUSED(func); UNUSED(itl); UNUSED(alloc); UNUSED(stack_clean);

    const auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::unary_reg2:
        {
            auto& unary = opcode.unary_reg2;

            switch(unary.type)
            {
                case unary_reg2_op::mov_gpr_reg:
                case unary_reg2_op::mov_fpr_reg:
                {
                    if(unary.dst.reg == unary.src.reg)
                    {
                        return remove(block.list,node);
                    }
                }

                default: break;
            }

            return node->next;
        }

        case op_group::implicit:
        {
            const auto& implicit = opcode.implicit;
            if(implicit.type != implicit_type::ret)
            {
                return node->next;
            }

            // Normal sub emit before pops
            if(alloc.stack_alloc.stack_size)
            {
                insert_at(block.list,node,stack_clean);
            }

            // make sure callee restore comes after stack clean

            // NOTE: as floats were last thing saved, they are the first to restore
            OpcodeNode* float_node = emit_popm_float(itl,block,node,alloc.fpr.saved_set);
            emit_popm(itl,block,float_node,alloc.gpr.saved_set);


            if(itl.debug)
            {
                insert_at(block.list,node,make_lowered_implicit_instr(implicit_type::leave));
            }

            return node->next;
        }

        // reload a reg
        case op_group::load:
        {
            assert(false);
            // RegSlot slot = opcode.v[1].reg;

            // auto& reg = reg_from_slot(slot,alloc);

            // const s32 stack_offset = opcode.v[2].imm;
            // const auto [offset_reg,offset] = reg_offset(itl,reg,stack_offset);

            // if(!(reg.flags & REG_FLOAT))
            // {
            //     // reload the spilled var
            //     if(is_signed(reg))
            //     {
            //         // word is register size (we dont need to extend it)
            //         static const op_type instr[4] = {op_type::lsb, op_type::lsh, op_type::lsw,op_type::ld};

            //         // this here does not otherwise need rewriting so we will emit SP directly
            //         node->value = make_lowered_base_addr_instr(instr[log2(reg.size)],opcode.v[0].lowered,offset_reg,offset);
            //     }

            //     // "plain data"
            //     // just move by size
            //     else
            //     {
            //         static const op_type instr[4] = {op_type::lb, op_type::lh, op_type::lw,op_type::ld};

            //         node->value = make_lowered_base_addr_instr(instr[log2(reg.size)],opcode.v[0].lowered,offset_reg,offset);
            //     }
            // }

            // else
            // {
            //     node->value = make_lowered_base_addr_instr(op_type::lf,opcode.v[0].lowered,offset_reg,offset);
            // }

            node = node->next;
            break;
        }

        case op_group::addrof:
        {
            assert(false);
            // const s32 base_offset = opcode.offset;
            // const RegSlot slot = opcode.v[1].reg;

            // auto &reg = reg_from_slot(slot,alloc);


            // assert(is_mem_allocated(reg));

            // auto [offset_reg,offset] = reg_offset(itl,reg,0);

            // node->value = make_addr_op(op_type::lea,opcode.v[0],make_lowered_operand(offset_reg),opcode.v[2],opcode.scale,base_offset + offset);

            // node = node->next;
            break;
        }


        case op_group::load_struct:
        {
            assert(false);
        }

        case op_group::store_struct:
        {
            assert(false);
        }

        case op_group::directive:
        {
            return lower_directive_addr_pass(itl,alloc,block,node);
        }

        default:
        {
            node = node->next;
            break;
        }
    }

    return node;
}


void allocation_reg_pass(Interloper& itl,Function &func, LinearAlloc& alloc)
{
    log(alloc.print,"allocating registers for %s:\n",func.name.buf);

    for(auto& block : func.emitter.program)
    {
        log(alloc.print,"\nprocessing L%d:\n\n",block.label_slot.handle);

        linear_setup_new_block(alloc,block);

        OpcodeNode *node = block.list.start;

        while(node)
        {
            clean_dead_regs(alloc);

            node = allocate_opcode_reg_pass(itl,func,alloc,block,node);
            alloc.pc++;
        }

        clean_dead_regs(alloc);
        correct_live_out(alloc,block);
    }
}

void allocation_addr_pass(Interloper& itl,Function &func, LinearAlloc& alloc)
{
    if(alloc.total_misplaced != 0)
    {
        log(alloc.print,"%s: Total misplaced %d\n",func.name.buf,alloc.total_misplaced);
    }

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

    // we dont want to save these on start
    if(func.name != "start")
    {
        // Return address
        u32 frame_offset = 1;

        // make sure callee saved regs are not saved inside the func
        alloc.gpr.saved_set = alloc.gpr.used_set & ~(CALLEE_GPR_SAVED_MASK | (1 << SP));
        if(itl.debug)
        {
            alloc.gpr.saved_set = deset_bit(alloc.gpr.saved_set,FP);
            // Push fp
            frame_offset += 1;
        }

        alloc.fpr.saved_set = alloc.fpr.used_set & ~(CALLEE_FPR_SAVED_MASK);

        // return addr + saved regs + call stack
        const u32 call_align = frame_offset + popcount(alloc.gpr.saved_set) + popcount(alloc.fpr.saved_set) + (func.sig.call_stack_size / GPR_SIZE);

        // add pad to align the stack on the correct boundary
        if(call_align & 1 && !func.leaf_func)
        {
            log(alloc.stack_alloc.print,"adding + GPR_SIZE padding to align stack\n");
            alloc.stack_alloc.stack_size += GPR_SIZE;
        }
    }


    const u32 save_count = popcount(alloc.gpr.saved_set) + popcount(alloc.gpr.saved_set);

    log(alloc.print,"saved registers: %d (0x%x) (0x%x)\n",save_count,alloc.gpr.saved_set,alloc.fpr.saved_set);


    alloc_args(func,alloc.stack_alloc,itl.symbol_table,GPR_SIZE * save_count);

    // entry point does not need to preserve regs
    auto& start_block = func.emitter.program[0];

    auto float_node = emit_pushm(itl,start_block,start_block.list.start,alloc.gpr.saved_set);
    auto alloc_node = emit_pushm_float(itl,start_block,float_node,alloc.fpr.saved_set);

    // only allocate a stack if we need it
    if(alloc.stack_alloc.stack_size)
    {
        insert_at(entry.list,alloc_node,make_lowered_arith_imm2_instr(SP,alloc.stack_alloc.stack_size,arith_bin_op::sub_t));
    }


    // Insert the frame pointer options at the front
    if(itl.debug)
    {
        auto frame_node = insert_front(entry.list,make_lowered_reg1_src_instr(FP,reg1_src_type::push));
        insert_after(entry.list,frame_node,make_mov_gpr_lowered_instr(FP,SP));
    }


    const auto stack_clean = make_lowered_arith_imm2_instr(SP,alloc.stack_alloc.stack_size,arith_bin_op::add_t);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        auto node = block.list.start;

        while(node)
        {
            node = allocate_opcode_addr_pass(itl,func,alloc,block,node,stack_clean);
        }
    }

    destroy_linear_alloc(alloc);
}

void allocate_registers(Interloper& itl,Function &func)
{
    auto alloc = make_linear_alloc(itl.print_reg_allocation,itl.print_stack_allocation,itl.stack_alloc,itl.debug,func.registers,&itl.symbol_table,itl.arch);

    linear_allocate(alloc,itl,func);

    allocation_reg_pass(itl,func,alloc);
    allocation_addr_pass(itl,func,alloc);
}
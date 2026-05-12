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
            node = lower_directive_pass1(itl,alloc,block,node);
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
        insert_at(entry.list,alloc_node,make_lowered_imm2_instr(op_type::sub_imm2,SP,alloc.stack_alloc.stack_size));
    }


    // Insert the frame pointer options at the front
    if(itl.debug)
    {
        auto frame_node = insert_front(entry.list,make_lowered_reg1_instr(op_type::push,FP));
        insert_after(entry.list,frame_node,make_lowered_reg2_instr(op_type::mov_reg,FP,SP));
    }


    const auto stack_clean = make_lowered_imm2_instr(op_type::add_imm2,SP,alloc.stack_alloc.stack_size);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        auto node = block.list.start;

        while(node)
        {
            assert(false);
            // node = rewrite_directives_(itl,alloc,block,node,saved_gpr,saved_fpr,stack_clean);
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
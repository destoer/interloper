
OpcodeNode* allocate_opcode(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    UNUSED(itl); UNUSED(func); UNUSED(alloc); UNUSED(block); UNUSED(node);
    assert(false);
}

void allocation_first_pass(Interloper& itl,Function &func, LinearAlloc& alloc)
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

            node = allocate_opcode(itl,func,alloc,block,node);
            alloc.pc++;
        }

        clean_dead_regs(alloc);
        correct_live_out(alloc,block);
    }
}

void allocate_registers(Interloper& itl,Function &func)
{
    auto alloc = make_linear_alloc(itl.print_reg_allocation,itl.print_stack_allocation,itl.stack_alloc,itl.debug,func.registers,&itl.symbol_table,itl.arch);

    linear_allocate(alloc,itl,func);

    allocation_first_pass(itl,func,alloc);
}
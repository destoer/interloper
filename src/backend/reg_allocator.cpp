#include "backend/lower_reg.cpp"

void allocate_and_rewrite_opcode(LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    const auto ir_span = opcode_ir_reg_span(node->value,alloc.ir_reg_span);
    const auto lowered_reg_span = linear_allocate_registers(alloc,block,node,ir_span,alloc.lowered_reg_span);

    auto& opcode = node->value;
    lower_opcode(alloc,opcode,lowered_reg_span);
}

OpcodeNode* allocate_opcode(Interloper& itl,Function &func, LinearAlloc& alloc, Block& block, OpcodeNode* node)
{
    auto disass = Disass(itl.symbol_table,itl.arch); 
    UNUSED(itl); UNUSED(func); 

    const auto& opcode = node->value;

    disass_opcode(node->value,disass);

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
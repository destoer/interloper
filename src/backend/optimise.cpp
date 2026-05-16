#include <interloper.h>


OpcodeNode* optimise_opcode(Interloper& itl, Block& block, OpcodeNode* node)
{
    UNUSED(itl); UNUSED(block); UNUSED(node);

    auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::implicit:
        {
            switch(opcode.implicit.type)
            {
                case implicit_type::ret: node->next = nullptr;
                default: break; 
            }

            break;
        }

        default: break;
    }

    return node->next;
}

void optimise_block(Interloper& itl, Function& func, Block& block)
{
    UNUSED(func);
    auto node = block.list.start;

    while(node)
    {
        node = optimise_opcode(itl,block,node);
    }
}

void optimise_func(Interloper& itl, Function& func)
{
    // for now this is just peephole optimiser
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        optimise_block(itl,func,block);
    }
}


void optimise_ir(Interloper &itl)
{   
    auto start = std::chrono::high_resolution_clock::now();
    
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        optimise_func(itl,func);
    }

    auto end = std::chrono::high_resolution_clock::now();

    itl.optimise_time = std::chrono::duration<double, std::milli>(end-start).count();
}
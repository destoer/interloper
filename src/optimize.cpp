#include <interloper.h>


void optimise_block(Interloper& itl, Function& func, Block& block)
{
    UNUSED(itl); UNUSED(func); UNUSED(block);
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
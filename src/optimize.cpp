#include <interloper.h>

void optimise_ir(Interloper &itl)
{
    for(auto &[key,func] : itl.function_table)
    {
        UNUSED(key);

        for(auto &block : func.emitter.program)
        {     
            UNUSED(block);

            // TODO: implement this!
        }
    }
}
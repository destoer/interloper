#include <interloper.h>

/*
void optimise_block(Interloper& itl, Function& func, Block& block)
{
    ListNode *node = block.list.start;

    while(node)
    {
        // ...

        node = node->next;
    }
}

void optimise_func(Interloper& itl, Function& func)
{
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        optimise_block(itl,func,block);
    }    
}
*/

void optimise_ir(Interloper &itl)
{   
    UNUSED(itl);
/*
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            optimise_func(itl,func);
        }
    }
*/
}
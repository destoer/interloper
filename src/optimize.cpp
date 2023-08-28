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
    // NOTE: this should follow the CFG from start
    // so which definitions of vars flow in and out of each block can be marked
    // which will allow us to fold operations correctly

    // i think we are better off impl our global register allocator first before attempting this though
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
#include <interloper.h>


void dump_ir(Function &func,const SlotLookup &slot_lookup,const LabelLookup &label_lookup)
{
    printf("%s:\n",func.name.c_str());

    u32 l = 0;
    for(u32 b = 0; b < func.emitter.program.size(); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        if(func.emitter.block_slot[b] != 0xffffffff)
        {
            printf("%s:\n",label_lookup[func.emitter.block_slot[b]].name.c_str());
        }

        for(const auto &opcode : block.buf)
        {
            printf("\t");
            disass_opcode_sym(opcode,slot_lookup,label_lookup);
        }

        if(block.last)
        {
            printf("\tspill_last\n");
        }
        
        l++;
    }

    printf("\n");       
}


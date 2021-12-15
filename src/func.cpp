#include <interloper.h>

// TODO: remove this by working out stack allocation requriemnts in a later pass
void add_var(Function &func, u32 size)
{
    // we only want to move quantitys under 4 bytes
    // larger thigns i.e structs
    // will allready have memory semantics
    panic(size > sizeof(u32),"add_var structs implemented");



    // shift by 1 to turn 1, 2, 4 
    // into and idx 
    func.size_count_cur[size >> 1] += 1;
}


void dump_ir(Function &func,const SlotLookup &slot_lookup,const LabelLookup &label_lookup)
{
    printf("%s:\n",func.name.c_str());

    u32 l = 0;
    for(u32 b = 0; b < func.emitter.program.size(); b++)
    {
        const auto &block = func.emitter.program[b];
    
        if(func.emitter.block_slot[b] != 0xffffffff)
        {
            printf("%s:\n",label_lookup[func.emitter.block_slot[b]].name.c_str());
        }

        for(const auto &opcode : block)
        {
            printf("\t");
            disass_opcode_sym(opcode,slot_lookup,label_lookup);
        }
        l++;
    }

    printf("\n");       
}
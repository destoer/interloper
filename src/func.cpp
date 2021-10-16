#include <interloper.h>

// TODO: remove the need to pass a type by emitting dedicated 
// mov signed instrs
u32 add_var(Function &func,const std::string &name,const Type &type, u32 size)
{
    // we only want to move quantitys under 4 bytes
    // larger thigns i.e structs
    // will allready have memory semantics
    assert(size <= 4);

    const auto slot = func.slot_lookup.size();

    // shift by 1 to turn 1, 2, 4 
    // into and idx 
    func.size_count_cur[size >> 1] += 1;

    const bool sign = is_signed_integer(type);

    func.slot_lookup.push_back(VarAlloc(size,name,sign));

    return slot;
}

// allocation on stack is callee handled
// so dont add to size count
u32 add_arg(Function &func,const std::string &name,const Type &type, u32 size)
{

    // we only want to move quantitys under 4 bytes
    // larger thigns i.e structs
    // will allready have memory semantics
    assert(size <= 4);

    const auto slot = func.slot_lookup.size();

    const bool sign = is_signed_integer(type);

    auto alloc = VarAlloc(size,name,sign);

    func.slot_lookup.push_back(alloc); 

    return slot;       
}

void dump_ir(Function &func,const std::vector<Label> &label_lookup)
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
            disass_opcode_sym(opcode,func.slot_lookup,label_lookup);
        }
        l++;
    }

    printf("\n");       
}
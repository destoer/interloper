#include <interloper.h>

Opcode peek_opcode(const std::list<Opcode> &block, opcode_iterator_t it)
{
    if(it != block.end())
    {
        return *it;
    }

    else
    {
        return Opcode(op_type::placeholder,0,0,0);
    }
}


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
#include <interloper.h>

using opcode_iterator_t = std::list<Opcode>::iterator;

Opcode peek_opcode(const std::list<Opcode> &block, opcode_iterator_t it)
{
    // what is the iterator type rofl
    if(it != block.end())
    {
        return *it;
    }

    else
    {
        return Opcode(op_type::placeholder,0,0,0);
    }
}

// for now just do constant folding

void Interloper::optimise_ir()
{
    for(auto &[key,func] : function_table)
    {
        UNUSED(key);

        for(auto &block : func.emitter.program)
        {     
            for(opcode_iterator_t it = block.begin(); it != block.end();)
            {
                auto &opcode = *it;

                
                switch(opcode.op)
                {
                    // constant folding
                    case op_type::mov_imm:
                    {
                        // next opcode is a load of an imm
                        // and operation after is on the two registers i.e
                        
                        auto it_op = it;

                        /*
                        mov r1, 0x7
                        mov r2, 0x2
                        add r1, r1, r2
                        */

                        const auto op2 = peek_opcode(block,++it_op);
                        const auto op3 = peek_opcode(block,++it_op);

                        // can constant fold
                        if(op2.op == op_type::mov_imm && op3.op != op_type::placeholder && op3.v2 == opcode.v1 && op3.v3 == op2.v1)
                        {
                            const u32 dst = op3.v1;

                            interpretter.reset();

                            // execute the ir instrs then read the final result of the reg
                            interpretter.execute_opcode(opcode);
                            interpretter.execute_opcode(op2);
                            interpretter.execute_opcode(op3);

                            *it_op = Opcode(op_type::mov_imm,dst,interpretter.regs[dst],0);

                            // remove the redundant mov_imm
                            it = block.erase(it,it_op);
                            continue;
                        }
                        break;
                    }

                    // implement dead code elimination on returns

                    // and remove branch indirection
                    // i.e branches that immediately branch to another

                    default: break;
                }

                ++it;
            }
        }
    }
}
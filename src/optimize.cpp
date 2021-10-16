#include <interloper.h>

using opcode_iterator_t = std::list<Opcode>::iterator;

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

                            reset(itl.interpretter);

                            // execute the ir instrs then read the final result of the reg
                            execute_opcode(itl.interpretter,opcode);
                            execute_opcode(itl.interpretter,op2);
                            execute_opcode(itl.interpretter,op3);

                            *it_op = Opcode(op_type::mov_imm,dst,itl.interpretter.regs[dst],0);

                            // remove the redundant mov_imm
                            it = block.erase(it,it_op);

                            // folding has enabled more folding to occur
                            it_op--;
                            if(peek_opcode(block,it_op).op == op_type::mov_imm)
                            {
                                it = it_op;
                            }
                            continue;
                        }
                        break;
                    }

                    // implement dead code elimination on returns
                    case op_type::ret:
                    {
                        // remove everything after the return
                        it = block.erase(++it,block.end());
                        continue;
                    }

                    // and remove branch indirection
                    // i.e branches that immediately branch to another
                    case op_type::bnc:
                    case op_type::b:
                    {
                        u32 dst = opcode.v1;


                        // TODO: make sure we cant branch to things outside the function scope with this
                        // have a "far" jump for that purpose
                        for(;;)
                        {
                            // lookup table has the block number rather than the absolute address
                            // before label resolution
                            const u32 block = itl.symbol_table.label_lookup[dst].offset;

                            const auto b = peek_opcode(func.emitter.program[block],func.emitter.program[block].begin());

                            // while an branch that is not to itself
                            if((b.op == op_type::b || b.op == op_type::bnc) && b.v1 != dst)
                            {
                                dst = b.v1;
                                continue;
                            }

                            break;
                        }

                        opcode.v1 = dst;
                        break;
                    }


                    default: break;
                }

                ++it;
            }
        }
    }
}
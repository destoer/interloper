#include <interloper.h>


const OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    {op_group::reg_t,"mov",2},
    {op_group::reg_t,"add",3},
    {op_group::reg_t,"sub",3},
    {op_group::reg_t,"mul",3},
    {op_group::reg_t,"div",3},

    {op_group::imm_t,"mov",2},
    {op_group::imm_t,"add",3},
    {op_group::imm_t,"sub",3},

    {op_group::load_t,"lw",3},
    {op_group::load_t,"sw",3},

    {op_group::implicit_t,"ret",0},
};

void IrEmitter::emit(op_type op, uint32_t v1, uint32_t v2, uint32_t v3)
{
    Opcode opcode(op,v1,v2,v3);

    program.push_back(opcode);
    pc += 1;
}

std::string Interloper::get_ir_operand(uint32_t v)
{
    if(v >= SYMBOL_START)
    {
        return symbol_table[slot_lookup[v-SYMBOL_START]].name;
    }

    if(v == SP)
    {
        return "sp";
    }

    return "r" + std::to_string(v);
}


// we wont worry about keeping things in registers for now
// just constantly shove stuff back out to the stack

// TODO: make sure register number does not exceed our target regs

void Interloper::allocate_registers()
{

/*
    bool used[MACHINE_REG_SIZE];
*/

    printf("symbol count: %d\n",emitter.sym_count);

    // TODO: dont bother with this if we aernt calling another function
    // insert function prologue

    // assume we just have a bunch of s32
    const uint32_t stack_size = emitter.sym_count * 4;

    emitter.program.push_front(Opcode(op_type::sub_imm,SP,SP,stack_size));

    // opcode to re correct the stack
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,stack_size);

    for(auto it = emitter.program.begin(); it != emitter.program.end(); ++it)
    {
        auto &opcode = *it;

        // how do we want to handle allocation?
        // i think at this point a lut is going to be real handy
        // to tell us how many arguments we have in our instruction

        // handle variable accesses
        if(opcode.op == op_type::mov_reg)
        {
            // swap all mov var, reg
            // with sw reg, [sp,var_offset]
            if(!is_reg(opcode.v1))
            {
                // hardcode this to an s32 and dont care about the size for now
                const auto slot = symbol_table[slot_lookup[opcode.v1-SYMBOL_START]].slot;
                const auto offset = slot * 4; // word size

                opcode = Opcode(op_type::sw,opcode.v2,SP,offset);
            }

            // swap all mov reg, var
            // with lw reg, [sp,var_offset]
            else if(!is_reg(opcode.v2))
            {
                // hardcode this to an s32 and dont care about the size for now
                const auto slot = symbol_table[slot_lookup[opcode.v2-SYMBOL_START]].slot;
                const auto offset = slot * 4; // word size

                opcode = Opcode(op_type::lw,opcode.v1,SP,offset);
            }
        }


        // add stack cleanup to all ret functions
        if(opcode.op == op_type::ret)
        {
            emitter.program.insert(it,stack_clean);
        }

    }


}


void Interloper::emit_asm()
{
    for(auto &opcode : emitter.program)
    {
        
        const auto& info = OPCODE_TABLE[static_cast<size_t>(opcode.op)];


        switch(info.group)
        {
            // change from the IR representation of sp to one easier to interpret
            case op_group::load_t:
            {
                if(info.args != 3)
                {
                    printf("emit asm unknown opcode");
                    exit(1);
                }

                if(opcode.v2 == SP_IR)
                {
                    opcode.v2 = SP;
                }

            }

            default: break;
        }

        program.push_back(opcode);
    }
}


void Interloper::print_op3(const char *name, const Opcode &opcode)
{
    printf("%s %s, %s, %s\n",name,
        get_ir_operand(opcode.v1).c_str(), get_ir_operand(opcode.v2).c_str(), 
        get_ir_operand(opcode.v3).c_str());
}

// how do we decouple the get_ir_operand so we can use this dump
// on the copy of the code we want to interpret?
void Interloper::dump_ir()
{

    for(const auto &opcode : emitter.program)
    {
        const auto &info = OPCODE_TABLE[static_cast<size_t>(opcode.op)];

        switch(info.group)
        {
            case op_group::reg_t:
            {
                switch(info.args)
                {
                    case 2:
                    {
                        printf("%s %s, %s\n",info.name ,get_ir_operand(opcode.v1).c_str(),get_ir_operand(opcode.v2).c_str());
                        break;
                    }

                    case 3:
                    {
                        printf("%s %s, %s, %s\n",info.name,
                            get_ir_operand(opcode.v1).c_str(), get_ir_operand(opcode.v2).c_str(), 
                            get_ir_operand(opcode.v3).c_str());
                        break;
                    }

                    default:
                    {
                        printf("unknown opcode");
                        exit(1); 
                        break;                       
                    } 
                }
                break;
            }


            case op_group::imm_t:
            {
                switch(info.args)
                {
                    case 2:
                    {
                        printf("%s %s, %d\n",info.name,get_ir_operand(opcode.v1).c_str(),opcode.v2);
                        break;
                    }

                    case 3:
                    {
                        printf("%s %s, %s, %d\n",info.name,get_ir_operand(opcode.v1).c_str(),get_ir_operand(opcode.v2).c_str(),opcode.v3);
                        break;
                    }

                    default:
                    {
                        printf("unknown opcode");
                        exit(1);                        
                    } 
                }
                break;
            }

            case op_group::load_t:
            {
                if(info.args != 3)
                {
                    printf("unknown opcode");
                    exit(1);
                }

                printf("%s %s, [%s,%d]\n",info.name,get_ir_operand(opcode.v1).c_str(),get_ir_operand(opcode.v2).c_str(),opcode.v3);
                break;
            }

            case op_group::implicit_t:
            {
                printf("%s\n",info.name);
                break;
            }

        }
    }

}
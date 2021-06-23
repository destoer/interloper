#include <interloper.h>

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

    return "r" + std::to_string(v);
}



void Interloper::print_op3(const char *name, const Opcode &opcode)
{
    printf("%s %s, %s, %s\n",name,
        get_ir_operand(opcode.v1).c_str(), get_ir_operand(opcode.v2).c_str(), 
        get_ir_operand(opcode.v3).c_str());
}

// worry about register allocation for now (we are storing back so we cant spill)
// just handle stack stuff
void Interloper::allocate_registers()
{

    // dump in stack operation
    // how do we want to handle inserting this?

    // okay we have a finished IR operating on a set of regs and varaibles
    // now we need to actually allocate them into machine registers

    bool used[MACHINE_REG_SIZE];


    printf("symbol count: %d\n",emitter.sym_count);

    for(const auto &opcode : emitter.program)
    {
        // how do we want to handle allocation?
        // i think at this point a lut is going to be real handy
        // to tell us how many arguments we have in our instruction
        UNUSED(opcode);
        UNUSED(used);
    }


}

void Interloper::dump_ir()
{
    // TODO: would a LUT be a better way to impl this?
    for(const auto &opcode : emitter.program)
    {
        switch(opcode.op)
        {
            case op_type::mov_imm: printf("mov %s, %u\n", get_ir_operand(opcode.v1).c_str(),opcode.v2); break;
            case op_type::mov_reg: printf("mov %s, %s\n", get_ir_operand(opcode.v1).c_str(),get_ir_operand(opcode.v2).c_str()); break; 
            case op_type::add_reg: print_op3("add",opcode); break;
            case op_type::sub_reg: print_op3("sub",opcode); break;
            case op_type::mul_reg: print_op3("mul",opcode); break;
            case op_type::div_reg: print_op3("div",opcode); break;
            case op_type::ret: printf("ret\n"); break;
        }
    }

}
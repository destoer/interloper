#include <interloper.h>


const OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    {op_group::reg_t,"mov",2},
    {op_group::reg_t,"add",3},
    {op_group::reg_t,"sub",3},
    {op_group::reg_t,"mul",3},
    {op_group::reg_t,"div",3},

    {op_group::reg_t,"sxb",2},
    {op_group::reg_t,"sxh",2},

    {op_group::imm_t,"mov",2},
    {op_group::imm_t,"add",3},
    {op_group::imm_t,"sub",3},

    {op_group::imm_t,"and",3},

    {op_group::load_t,"lb",3},
    {op_group::load_t,"lh",3},
    {op_group::load_t,"lw",3},

    {op_group::load_t,"lsb",3},
    {op_group::load_t,"lsh",3},

    {op_group::load_t,"sb",3},
    {op_group::load_t,"sh",3},
    {op_group::load_t,"sw",3},

    {op_group::implicit_t,"ret",0},
};

void IrEmitter::emit(op_type op, uint32_t v1, uint32_t v2, uint32_t v3)
{
    Opcode opcode(op,v1,v2,v3);

    program.push_back(opcode);
    pc += 1;
}





void stack_allocate(u32 *stack_alloc, VarAlloc &var_alloc)
{
    var_alloc.offset = stack_alloc[var_alloc.size >> 1];
    stack_alloc[var_alloc.size >> 1] += var_alloc.size;
}

// we wont worry about keeping things in registers for now
// just constantly shove stuff back out to the stack

// TODO: make sure register number does not exceed our target regs

void Interloper::allocate_registers()
{

/*
    bool used[MACHINE_REG_SIZE];
*/

/*
    printf("symbol count: %d\n",symbol_table.sym_count);
    printf("byte count: %d\n",symbol_table.size_count[0]);
    printf("half count: %d\n",symbol_table.size_count[1]);
    printf("word count: %d\n",symbol_table.size_count[2]);
*/

    /*
        okay we are going to store all byte variables sequentially and algin,
        then store all half variables sequentially and align,
        then store all other variables as ints (this includes structs when added),
    */

    // align for u16
    if(symbol_table.size_count[0] & 1)
    {
        symbol_table.size_count[0] += 1;
    }

    // align for u32 
    if(symbol_table.size_count[1] & 2)
    {
        symbol_table.size_count[1] += 2;
    }



    // TODO: dont bother with this if we aernt calling another function
    // insert function prologue

    
    const u32 stack_size = symbol_table.size_count[0] + (symbol_table.size_count[1] * 2) + (symbol_table.size_count[2] * 4);
    //printf("stack size: %d\n",stack_size);

    emitter.program.push_front(Opcode(op_type::sub_imm,SP,SP,stack_size));

    // opcode to re correct the stack
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,stack_size);

    u32 stack_alloc[3];

    // byte located at start
    stack_alloc[0] = 0;

    // start at end of byte allocation
    stack_alloc[1] = symbol_table.size_count[0];

    // start at end of half allocation
    stack_alloc[2] = stack_alloc[1] + (symbol_table.size_count[1] * 2);

    for(auto it = emitter.program.begin(); it != emitter.program.end(); ++it)
    {
        auto &opcode = *it;

        // how do we want to handle allocation?
        // when we aernt just storing stuff back and forth?

        // handle variable accesses
        if(opcode.op == op_type::mov_reg)
        {
            // swap all mov var, reg
            // with sw reg, [sp,var_offset]
            if(!is_reg(opcode.v1))
            {
                // hardcode this to an s32 and dont care about the size for now
                const auto slot = symbol_to_idx(opcode.v1);

                auto &var_alloc = symbol_table.slot_lookup[slot];

                // we have not allocated where this variable goes yet
                if(var_alloc.offset == UNALLOCATED_OFFSET)
                {
                    stack_allocate(stack_alloc,var_alloc);
                }

                // move by size
                static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};

                opcode = Opcode(instr[var_alloc.size >> 1],opcode.v2,SP,var_alloc.offset);
            }

            // swap all mov reg, var
            // with lw reg, [sp,var_offset]
            else if(!is_reg(opcode.v2))
            {
                const auto slot = symbol_to_idx(opcode.v2);

                auto &var_alloc = symbol_table.slot_lookup[slot];

                // we have not allocated where this variable goes yet
                if(var_alloc.offset == UNALLOCATED_OFFSET)
                {
                    stack_allocate(stack_alloc,var_alloc);
                }


          
                const auto& type = symbol_table[var_alloc.name].type;

                // is a signed integer (we need to sign extend)
                if(is_builtin(type.type_idx) && is_signed_integer(static_cast<builtin_type>(type.type_idx)))
                {
                    // word is register size (we dont need to extend it)
                    static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                    opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,var_alloc.offset);                    
                }

                // "plain data"
                // just move by size
                else
                {
                    static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
                    opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,var_alloc.offset);
                }
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



using IR_OPER_STRING_FUNC = std::string (*)(const SymbolTable* ,uint32_t);

// disassemble with symbols
std::string get_oper_sym(const SymbolTable *table,uint32_t v)
{
    if(v >= SYMBOL_START && symbol_to_idx(v) < table->slot_lookup.size())
    {
        return table->slot_lookup[symbol_to_idx(v)].name;
    }

    if(v == SP_IR)
    {
        return "sp";
    }

    return "r" + std::to_string(v);
}

// disassemble without needing the symbol information
std::string get_oper_raw(const SymbolTable *table,uint32_t v)
{
    UNUSED(table);

    if(v == SP)
    {
        return "sp";
    }

    return "r" + std::to_string(v);
}


// pass in a "optional" table and a operand function so we can either disassemble it with symbol
// information or without
void disass_opcode(const Opcode &opcode, const SymbolTable *table, IR_OPER_STRING_FUNC get_oper)
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
                    printf("%s %s, %s\n",info.name ,get_oper(table,opcode.v1).c_str(),get_oper(table,opcode.v2).c_str());
                    break;
                }

                case 3:
                {
                    printf("%s %s, %s, %s\n",info.name,
                        get_oper(table,opcode.v1).c_str(), get_oper(table,opcode.v2).c_str(), 
                        get_oper(table,opcode.v3).c_str());
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
                    printf("%s %s, %d\n",info.name,get_oper(table,opcode.v1).c_str(),opcode.v2);
                    break;
                }

                case 3:
                {
                    printf("%s %s, %s, %d\n",info.name,get_oper(table,opcode.v1).c_str(),get_oper(table,opcode.v2).c_str(),opcode.v3);
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

            printf("%s %s, [%s,%d]\n",info.name,get_oper(table,opcode.v1).c_str(),get_oper(table,opcode.v2).c_str(),opcode.v3);
            break;
        }

        case op_group::implicit_t:
        {
            printf("%s\n",info.name);
            break;
        }

    }

}

void disass_opcode_sym(const Opcode &opcode, const SymbolTable &table)
{
    disass_opcode(opcode,&table,get_oper_sym);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode(opcode,nullptr,get_oper_raw);
}

// how do we decouple the get_ir_operand so we can use this dump
// on the copy of the code we want to interpret?
void Interloper::dump_ir_sym()
{
    for(const auto &opcode : emitter.program)
    {
        disass_opcode_sym(opcode,symbol_table);
    }
}

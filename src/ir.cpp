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

    {op_group::reg_t,"push",1},

    {op_group::branch_t,"call",1},
    {op_group::implicit_t,"ret",0},

    {op_group::imm_t,"swi",1},
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

// TODO: why does the offset not account for the PC 
// for args
void Interloper::allocate_registers(Function &func)
{

/*
    bool used[MACHINE_REG_SIZE];
*/

/*
    printf("symbol count: %d\n",symbol_table.sym_count);
    printf("byte count: %d\n",func.size_count[0]);
    printf("half count: %d\n",func.size_count[1]);
    printf("word count: %d\n",func.size_count[2]);
*/

    /*
        okay we are going to store all byte variables sequentially and algin,
        then store all half variables sequentially and align,
        then store all other variables as ints (this includes structs when added),
    */

    // align for u16
    if(func.size_count[0] & 1)
    {
        func.size_count[0] += 1;
    }

    // align for u32 
    if(func.size_count[1] & 2)
    {
        func.size_count[1] += 2;
    }




    
    const u32 stack_size = func.size_count[0] + (func.size_count[1] * 2) + (func.size_count[2] * 4);
    //printf("stack size: %d\n",stack_size);

    // only allocate a stack if we need it
    if(stack_size)
    {
        func.emitter.program.push_front(Opcode(op_type::sub_imm,SP,SP,stack_size));
    }

    // opcode to re correct the stack
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,stack_size);

    u32 stack_alloc[3];

    // byte located at start
    stack_alloc[0] = 0;

    // start at end of byte allocation
    stack_alloc[1] = func.size_count[0];

    // start at end of half allocation
    stack_alloc[2] = stack_alloc[1] + (func.size_count[1] * 2);

    for(auto it = func.emitter.program.begin(); it != func.emitter.program.end(); ++it)
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


                auto &var_alloc = func.slot_lookup[slot];

                // we have not allocated where this variable goes yet
                if(var_alloc.offset == UNALLOCATED_OFFSET)
                {
                    stack_allocate(stack_alloc,var_alloc);
                }

                // if is an arg read "above" the stack (stack allocation + return value)
                // TODO: handle arg being a reg
                const u32 offset = is_arg(opcode.v1)? var_alloc.offset + stack_size + sizeof(u32) : var_alloc.offset;

                // move by size
                static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};

                opcode = Opcode(instr[var_alloc.size >> 1],opcode.v2,SP,offset);
            }

            // swap all mov reg, var
            // with lw reg, [sp,var_offset]
            else if(!is_reg(opcode.v2))
            {
                const auto slot = symbol_to_idx(opcode.v2);

                auto &var_alloc = func.slot_lookup[slot];


                // we have not allocated where this variable goes yet
                if(var_alloc.offset == UNALLOCATED_OFFSET)
                {
                    stack_allocate(stack_alloc,var_alloc);
                }


                // if is an arg read "above" the stack (stack allocation + return value)
                // TODO: handle arg being a reg
                const u32 offset = is_arg(opcode.v2)? var_alloc.offset + stack_size + sizeof(u32) : var_alloc.offset;



                // is a signed integer (we need to sign extend)
                if(var_alloc.is_signed)
                {
                    // word is register size (we dont need to extend it)
                    static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                    opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,offset);                    
                }

                // "plain data"
                // just move by size
                else
                {
                    static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
                    opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,offset);
                }
            }
        }


        // add stack cleanup to all ret functions
        if(opcode.op == op_type::ret)
        {
            // if there is no stack allocation there is nothing to clean up
            if(stack_size)
            {
                func.emitter.program.insert(it,stack_clean);
            }
        }

    }


}


void Interloper::emit_asm()
{
    // emit a dummy call to main
    // that will get filled in later once we know where main lives
    // TODO: make sure there is actually a main function
    program.push_back(Opcode(op_type::call,function_table["main"].slot,0,0));

    // program exit
    program.push_back(Opcode(op_type::swi,SWI_EXIT,0,0));

    // dump ever function into one vector and record where it is in the function table
    for(auto &[key, func]: function_table)
    {
        UNUSED(key);

        // when we assembly to an actual arch we will 
        // have to switch over to a byte array
        func.func_offset = program.size() * OP_SIZE;
        for(const auto &op : func.emitter.program)
        {
            program.push_back(op);
        }
    }

    
    // "link" the program and resolve all the labels we now have the absolute
    // posistions for
    for(auto &opcode : program)
    {
        if(opcode.op == op_type::call)
        {
            opcode.v1 = function_table[symbol_table.label_lookup[opcode.v1]].func_offset;
        }
    }

    // program dump
/*
    for(u32 pc = 0; pc < program.size(); pc++)
    {
        printf("0x%08x: ",pc * OP_SIZE);
        disass_opcode_raw(program[pc]);
    }
*/
}



using IR_OPER_STRING_FUNC = std::string (*)(const std::vector<VarAlloc> *table, uint32_t);

// disassemble with symbols
std::string get_oper_sym(const std::vector<VarAlloc> *table,uint32_t v)
{
    auto slot_lookup = *table;

    if(v >= SYMBOL_START && symbol_to_idx(v) < table->size())
    {
        return slot_lookup[symbol_to_idx(v)].name;
    }

    if(v == SP)
    {
        return "sp";
    }

    return "r" + std::to_string(v);
}

// disassemble without needing the symbol information
std::string get_oper_raw(const std::vector<VarAlloc> *table,uint32_t v)
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
void disass_opcode(const Opcode &opcode, const std::vector<VarAlloc> *table, const std::vector<std::string> *label_lookup, IR_OPER_STRING_FUNC get_oper)
{
    const auto &info = OPCODE_TABLE[static_cast<size_t>(opcode.op)];

    switch(info.group)
    {
        case op_group::reg_t:
        {
            switch(info.args)
            {

                case 1:
                {
                    printf("%s %s\n",info.name, get_oper(table,opcode.v1).c_str());
                    break;
                }

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
                case 1:
                {
                    printf("%s 0x%x\n",info.name,opcode.v1);
                    break;
                }

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

        case op_group::branch_t:
        {
            if(info.args != 1)
            {
                printf("unknown opcode");
                exit(1);
            }

            // symbols have been resolved
            if(!label_lookup)
            {
                printf("%s 0x%x\n",info.name,opcode.v1);
            }

            else
            {
                const auto labels = *label_lookup;
                assert(opcode.v1 < labels.size());

                printf("%s %s\n",info.name,labels[opcode.v1].c_str());
            }

        }

    }

}

void disass_opcode_sym(const Opcode &opcode, const std::vector<VarAlloc> &table, const std::vector<std::string> &label_lookup)
{
    disass_opcode(opcode,&table,&label_lookup,get_oper_sym);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode(opcode,nullptr,nullptr,get_oper_raw);
}
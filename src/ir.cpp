#include <interloper.h>


const OpInfo OPCODE_TABLE[OPCODE_SIZE] =
{
    {op_group::reg_t,"mov",2},
    {op_group::reg_t,"add",3},
    {op_group::reg_t,"sub",3},
    {op_group::reg_t,"mul",3},
    {op_group::reg_t,"div",3},
    {op_group::reg_t,"mod",3},


    {op_group::reg_t,"lsl",3},
    {op_group::reg_t,"asr",3},
    {op_group::reg_t,"lsr",3},

    {op_group::reg_t,"xor",3},
    {op_group::reg_t,"or",3},
    {op_group::reg_t,"and",3},
    {op_group::reg_t,"not",1},

    {op_group::reg_t,"sxb",2},
    {op_group::reg_t,"sxh",2},

    {op_group::imm_t,"mov",2},
    {op_group::imm_t,"add",3},
    {op_group::imm_t,"sub",3},

    {op_group::imm_t,"and",3},
    {op_group::imm_t,"xor",3},

    {op_group::load_t,"lb",3},
    {op_group::load_t,"lh",3},
    {op_group::load_t,"lw",3},

    {op_group::load_t,"lsb",3},
    {op_group::load_t,"lsh",3},

    {op_group::load_t,"sb",3},
    {op_group::load_t,"sh",3},
    {op_group::load_t,"sw",3},

    {op_group::reg_t,"push",1},
    {op_group::reg_t,"pop",1},

    {op_group::branch_t,"call",1},
    {op_group::implicit_t,"ret",0},

    {op_group::imm_t,"swi",1},

    // compare unsigned
    {op_group::imm_t,"cmpugt",3},
    {op_group::reg_t,"cmpult",3},
    {op_group::reg_t,"cmpule",3},
    {op_group::reg_t,"cmpugt",3},
    {op_group::reg_t,"cmpuge",3},


    // compare signed
    {op_group::reg_t,"cmpsgt",3},
    {op_group::reg_t,"cmpslt",3},
    {op_group::reg_t,"cmpsle",3},
    {op_group::reg_t,"cmpsgt",3},
    {op_group::reg_t,"cmpsge",3},

    // dont care about sign for equality
    {op_group::reg_t,"cmpeq",3},
    {op_group::reg_t,"cmpne",3},

    {op_group::branch_t,"bnc",2},
    {op_group::branch_t,"bc",2},
    {op_group::branch_t,"b",1},

    // directives
    {op_group::slot_t,"alloc_slot",1},
    {op_group::slot_t,"free_slot",1},
    {op_group::reg_t,"push_arg",1},

    // perform cleanup after a function call
    // free the stack space for args
    // restore callee saved registers
    {op_group::imm_t,"clean_args",1},

    {op_group::reg_t,"save_reg",1},
    {op_group::reg_t,"restore_reg",1},

    {op_group::implicit_t,"exit_block",0},

    {op_group::implicit_t,"placeholder",0},

    // not used
    {op_group::implicit_t,"END",0},
};

void emit(IrEmitter &emitter,op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    emitter.program[emitter.program.size()-1].push_back(opcode);
    emitter.pc += 1;    
}

void new_block(IrEmitter &emitter,u32 slot)
{
    emitter.program.push_back({});
    emitter.block_slot.push_back(slot);    
}


void stack_allocate(u32 *stack_alloc, VarAlloc &var_alloc)
{
    var_alloc.offset = stack_alloc[var_alloc.size >> 1];
    stack_alloc[var_alloc.size >> 1] += var_alloc.size;
}

// we wont worry about keeping things in registers for now
// just constantly shove stuff back out to the stack

// TODO: make sure register number does not exceed our target regs



void allocate_registers(Function &func)
{
#if 0
    u32 free_regs = MACHINE_REG_SIZE;

    static constexpr u32 REG_FREE = 0xffffffff;
    static constexpr u32 REG_TMP = 0xfffffffe;


     // TODO: okay how do we know when something is moved so we know our reg now refers to a tmp
    // because otherwhise we will run into fun problems

    // is this free? or does this hold a var?
    u32 regs[MACHINE_REG_SIZE];

    // when this thing is used in the original IR
    // we need to know what actual reg we have allocated it into
    u32 ir_regs[MACHINE_REG_SIZE];

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        regs[i] = REG_FREE;
        ir_regs[i] = 0;
    }
#endif

    printf("function: %s\n",func.name.c_str());
    printf("byte count: %d\n",func.size_count[0]);
    printf("half count: %d\n",func.size_count[1]);
    printf("word count: %d\n",func.size_count[2]);


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
        func.emitter.program[0].push_front(Opcode(op_type::sub_imm,SP,SP,stack_size));
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

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset = 0;

    for(auto &block : func.emitter.program)
    {
        for(auto it = block.begin(); it != block.end();)
        {
            auto &opcode = *it;

            // how do we want to handle allocation?
            // when we aernt just storing stuff back and forth?

            switch(opcode.op)
            {

                // TODO: this needs to handle reg alloc
                case op_type::save_reg:
                {
                    opcode = Opcode(op_type::push,opcode.v1,0,0);
                    stack_offset += sizeof(u32);
                    break;
                }


                case op_type::restore_reg:
                {
                    opcode = Opcode(op_type::pop,opcode.v1,0,0);
                    stack_offset -= sizeof(u32);
                    break;
                }



                case op_type::push_arg:
                {
                    opcode =  Opcode(op_type::push,opcode.v1,0,0);

                    // varaibles now have to be accessed at a different offset
                    // until this is corrected by clean call
                    stack_offset += sizeof(u32);

                    break;
                }

                case op_type::clean_args:
                {
                    // clean up args
                    const auto stack_clean = sizeof(u32) * opcode.v1;

                    opcode = Opcode(op_type::add_imm,SP,SP,stack_clean);
                    stack_offset -= stack_clean; 
                    break;
                }

                // for now just do nothing with this
                case op_type::alloc_slot:
                {
                    it = block.erase(it);
                    continue;
                }

                case op_type::free_slot:
                {
                    const auto &var_alloc = func.slot_lookup[symbol_to_idx(opcode.v1)];
                    stack_alloc[var_alloc.size >> 1] -= var_alloc.size;

                    // how do we properly erase this?
                    it = block.erase(it);
                    continue;
                }

                // handle variable accesses
                case op_type::mov_reg:
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
                            if(!is_arg(opcode.v1))
                            {
                                stack_allocate(stack_alloc,var_alloc);
                            }

                            else
                            {
                                // arg is above the stack frame
                                var_alloc.offset = (slot * var_alloc.size) + stack_size + sizeof(u32);
                            }
                        }

                        // TODO: start here!, we need to mark this is allocatted by here
                        // but we then need and rewrite the register defintion 

                        // ignore writeback for now we need a seperate directive that makes it clear this is a 'hard' store
                        // i.e due to pointers etc it has to go back into memory
                        // i.e spill x


                        // move by size
                        static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};

                        opcode = Opcode(instr[var_alloc.size >> 1],opcode.v2,SP,var_alloc.offset + stack_offset);
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
                            if(!is_arg(opcode.v2))
                            {
                                stack_allocate(stack_alloc,var_alloc);
                            }

                            else
                            {
                                // arg is above the stack frame
                                var_alloc.offset = (slot * var_alloc.size) + stack_size + sizeof(u32);
                            }
                        }

    

                        // is a signed integer (we need to sign extend)
                        // TODO: directly emit signed mov pseudo instrs so we dont have to do this nonsense
                        if(var_alloc.is_signed)
                        {
                            // word is register size (we dont need to extend it)
                            static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                            opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,var_alloc.offset + stack_offset);                    
                        }

                        // "plain data"
                        // just move by size
                        else
                        {
                            static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
                            opcode = Opcode(instr[var_alloc.size >> 1],opcode.v1,SP,var_alloc.offset + stack_offset);
                        }
                    }
                    break;
                }


                // add stack cleanup to all ret functions
                case op_type::ret:
                {
                    // if there is no stack allocation there is nothing to clean up
                    if(stack_size)
                    {
                        block.insert(it,stack_clean);
                    }
                    break;
                }

                default: break;
            }

            // use continue to skip this statement when we have to delete from the list
            ++it;
        }
    }

}


void emit_asm(Interloper &itl)
{
    // emit a dummy call to main
    // that will get filled in later once we know where main lives
    itl.program.push_back(Opcode(op_type::call,itl.function_table["main"].slot,0,0));

    // program exit
    itl.program.push_back(Opcode(op_type::swi,SWI_EXIT,0,0));

    // dump ever function into one vector and record where it is in the function table
    for(auto &[key, func]: itl.function_table)
    {
        UNUSED(key);

        // when we assembly to an actual arch we will 
        // have to switch over to a byte array
        itl.symbol_table.label_lookup[func.slot].offset = itl.program.size() * OP_SIZE;


        for(u32 b = 0; b < func.emitter.program.size(); b++)
        {
            const auto &block = func.emitter.program[b];

            // resolve label addr.
            // TODO: to locate this properly we need to know
            // how many labels were allocated at the start of this func
            // for now we will cheat as we only have one func with labels
            // so just move the labels past the function ones
            itl.symbol_table.label_lookup[func.emitter.block_slot[b]].offset = itl.program.size() * OP_SIZE;

            for(const auto &op : block)
            {
                itl.program.push_back(op);
            }
        }
    }

    // label dump
/*
    puts("\n\nlabels");
    for(const auto &label : itl.symbol_table.label_lookup)
    {
        printf("label %s = %x\n",label.name.c_str(),label.offset);
    }
    putchar('\n');
*/
    
    // "link" the program and resolve all the labels we now have the absolute
    // posistions for
    // TODO: how do we want to labels for a mov i.e
    // x = @some_function;
    for(auto &opcode : itl.program)
    {
        // handle all the branch labels
        // TODO: this probably needs to be changed for when we have call <reg>
        if(OPCODE_TABLE[static_cast<u32>(opcode.op)].group == op_group::branch_t)
        {
            opcode.v1 = itl.symbol_table.label_lookup[opcode.v1].offset;
        }
    }

    // program dump

    puts("raw program dump\n\n\n");
    for(u32 pc = 0; pc < itl.program.size(); pc++)
    {
        printf("0x%08x: ",pc * OP_SIZE);
        disass_opcode_raw(itl.program[pc]);
    }

}



using IR_OPER_STRING_FUNC = std::string (*)(const std::vector<VarAlloc> *table, u32);

// disassemble with symbols
std::string get_oper_sym(const std::vector<VarAlloc> *table,u32 v)
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
std::string get_oper_raw(const std::vector<VarAlloc> *table,u32 v)
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
void disass_opcode(const Opcode &opcode, const std::vector<VarAlloc> *table, const std::vector<Label> *label_lookup, IR_OPER_STRING_FUNC get_oper)
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
                    printf("%s %s, 0x%x\n",info.name,get_oper(table,opcode.v1).c_str(),opcode.v2);
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

        case op_group::slot_t:
        {
            if(info.args != 1)
            {
                puts("unknown opcode");
                exit(1);
            }

            printf("%s %s\n",info.name,get_oper(table,opcode.v1).c_str());
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
            switch(info.args)
            {
                // unconditonal branch
                case 1:
                {
                    // symbols have been resolved
                    if(!label_lookup)
                    {
                        printf("%s 0x%x\n",info.name,opcode.v1);
                    }

                    else
                    {
                        const auto labels = *label_lookup;
                        assert(opcode.v1 < labels.size());

                        printf("%s %s\n",info.name,labels[opcode.v1].name.c_str());
                    }
                    break;
                }

                // conditional branch
                // add name resolution for this
                case 2:
                {
                    // assume this is only going for a branch now
                    // we will want a table lookup later
                    if(label_lookup)
                    {
                        const auto labels = *label_lookup;
                        assert(opcode.v1 < labels.size());

                        printf("%s %s,%s\n",info.name,labels[opcode.v1].name.c_str(),get_oper(table,opcode.v2).c_str());
                    }

                    else
                    {
                        printf("%s %x,%s\n",info.name,opcode.v1,get_oper(table,opcode.v2).c_str());
                    }
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

    }

}

void disass_opcode_sym(const Opcode &opcode, const std::vector<VarAlloc> &table, const std::vector<Label> &label_lookup)
{
    disass_opcode(opcode,&table,&label_lookup,get_oper_sym);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode(opcode,nullptr,nullptr,get_oper_raw);
}
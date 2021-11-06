#include <interloper.h>
#include <op_table.inl>

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


static constexpr u32 REG_FREE = 0xffffffff;
static constexpr u32 REG_TMP = 0xfffffffe;


// is this how we want it?
// or should we just pass stuff through one by one?
struct LocalAlloc
{
    // register allocation

    // is this free or does it hold a var?
    u32 regs[MACHINE_REG_SIZE];  

    // when this thing is used in the original IR
    // we need to know what actual reg we have allocated it into
    u32 ir_regs[MACHINE_REG_SIZE];

    u32 free_regs;

    // free list for register allocator
    u32 free_list[MACHINE_REG_SIZE];


    // stack allocation

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset;

    // how much space has been used so far for each var
    u32 stack_alloc[3];
};

void spill_reg(Block &block,opcode_iterator_t block_ptr, u32 stack_offset, VarAlloc& var_alloc, u32 reg)
{
    printf("spill %d\n",reg);

    // write value back out into mem
    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};

    const auto opcode = Opcode(instr[var_alloc.size >> 1],reg,SP,var_alloc.offset + stack_offset); 

    block.insert(block_ptr,opcode); 

    // register is back in memory!
    var_alloc.location = LOCATION_MEM;
}

u32 alloc_internal(LocalAlloc &alloc,Block &block, opcode_iterator_t block_ptr,std::vector<VarAlloc> &slot_lookup)
{
    u32 reg = REG_FREE;

    // if there is a free register remove from free list
    // and give to alloc
    if(alloc.free_regs)
    {
        // allocate a register
        reg = alloc.free_list[--alloc.free_regs];
    }

    // TODO: fixme how do we handle regs we are going to free being in the calc or can this not happen
    // under this design?

    // if there is not spill another register (not a tmp or this will break)
    // back into memory
    else 
    {
        for(reg = 0; reg < MACHINE_REG_SIZE; reg++)
        {
            // we have a register to spill
            if(alloc.regs[reg] < REG_TMP)
            {
                auto &var_alloc = slot_lookup[alloc.regs[reg]];
                spill_reg(block,block_ptr,alloc.stack_offset,var_alloc,reg);
                break;
            }
        }

        // failed to find a reg to spill should not happen
        // TODO: at the momement this is triggering because tmps aernt getting cleaned up properly
        // the logical test somehow requires 8 registers to compute the expression fully which is way beyond
        // what should be required

        assert(reg != MACHINE_REG_SIZE);
    }

    return reg;    
}


void alloc_tmp(LocalAlloc &alloc, u32 ir_reg, Block &block, opcode_iterator_t block_ptr, std::vector<VarAlloc> &slot_lookup)
{
    const u32 reg = alloc_internal(alloc, block, block_ptr, slot_lookup);
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = REG_TMP;
}

// mark a tmp as allocated to a var
void alloc_into_tmp(VarAlloc &var_alloc,LocalAlloc &alloc, u32 ir_reg, u32 slot)
{   
    printf("%s allocated into tmp %d\n",var_alloc.name.c_str(),alloc.ir_regs[ir_reg]);


    const u32 reg = alloc.ir_regs[ir_reg];
    var_alloc.location = reg;
    alloc.regs[reg] = slot;
}

// directly allocate a var ito a reg
u32 alloc_reg(VarAlloc &var_alloc, LocalAlloc &alloc, Block &block,opcode_iterator_t block_ptr,std::vector<VarAlloc> &slot_lookup, u32 slot)
{
    const u32 reg = alloc_internal(alloc, block, block_ptr, slot_lookup);
    alloc.regs[reg] = slot;
    var_alloc.location = reg;

    return reg;
}


void free_reg(LocalAlloc &alloc, u32 reg)
{
    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;
}

void free_reg(LocalAlloc &alloc, VarAlloc &var_alloc)
{
    printf("freed %s from %d\n",var_alloc.name.c_str(),var_alloc.location);

    free_reg(alloc,var_alloc.location);
    var_alloc.location = LOCATION_MEM;
}

void reclaim_tmp(LocalAlloc &alloc)
{
    // TODO: this may need to be faster
    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(alloc.regs[i] == REG_TMP)
        {
            free_reg(alloc,i);
        }
    }
}

u32 rewrite_reg(const LocalAlloc &alloc, u32 ir_reg)
{
    // do not rewrite special purpose regs
    if(ir_reg >= MACHINE_REG_SIZE)
    {
        return ir_reg;
    }

    else
    {
        return alloc.ir_regs[ir_reg];
    }
}


void handle_allocation(Function &func, LocalAlloc &alloc, Block &block, opcode_iterator_t op_ptr, Opcode &opcode, u32 args)
{
    UNUSED(op_ptr); UNUSED(block);

    // ensure src var is allocated 
    // (if we are using a tmp as an arg then it must allready have been allocated)
    for(u32 i = 1; i < args; i++)
    {
        if(!is_reg(opcode.v[i]))
        {
            const auto slot = symbol_to_idx(opcode.v[i]);
            auto &var_alloc = func.slot_lookup[slot];

            // unallocated
            if(var_alloc.location == LOCATION_MEM)
            {
                assert(false);
            }
        }
    }

    // handle dst allocation
    const u32 idx = opcode.v[0];

    // TODO: we need to look if a src arg is a tmp here and allocate into that instead of something new

    // is a var
    if(!is_reg(idx))
    {
        const auto slot = symbol_to_idx(idx);
        auto &var_alloc = func.slot_lookup[slot];        

        if(var_alloc.location == LOCATION_MEM)
        {
            alloc_reg(var_alloc,alloc,block,op_ptr,func.slot_lookup,slot);
        }


        // we have stored back into a var
        // we no longer need any temps
        reclaim_tmp(alloc);
    }

    // tmp
    else
    {
        if(alloc.regs[alloc.ir_regs[idx]] == REG_FREE)
        {
            alloc_tmp(alloc,opcode.v[0],block,op_ptr,func.slot_lookup);
        }
    }

    


    // rewrite the register names
    for(u32 i = 0; i < args; i++)
    {
        const bool is_var = !is_reg(opcode.v[i]);

        if(is_var)
        {
            const auto slot = symbol_to_idx(opcode.v[i]);
            auto &var_alloc = func.slot_lookup[slot]; 

            opcode.v[i] = var_alloc.location;
        }

        else
        {
            // is a tmp 
            opcode.v[i] = rewrite_reg(alloc,opcode.v[i]);
        }

    }
}

// rewrite the opcode regs based on allocation and opcode type!
void correct_reg(Function &func, LocalAlloc &alloc, Block &block, opcode_iterator_t op_ptr,Opcode &opcode)
{

    UNUSED(alloc);

    const auto &info = OPCODE_TABLE[static_cast<size_t>(opcode.op)];
    switch(info.group)
    {


        // we need to do var alloc in each of these
        // we need to understand what part of the instructions are dsts and which part are src

        // should be enough to have a lut for op_group + the number of regs to give us back what is what
        


        // to know if we just need to alloc or we actually need to load the damn thing
        // at that point we can just go -> is this thing a tmp or a var
        // because now we have a very clear distinction

        // if we have a var dst and one tmp src then we can just put the result into the tmp
        // and move it after the calc 
        
        // and to handle tmp freeing -> might emit a directive for it
        // if we can do this here, then our register allocation will be fine


        /*
            for(u32 i = 0; i < info.args; i++)
            {
                opcode.v[i] = rewrite_reg(alloc,opcode.v[i]);
            }
        */

        case op_group::reg_t:
        {
            handle_allocation(func,alloc,block,op_ptr,opcode,info.args);
            break;
        }

        case op_group::imm_t:
        {
            if(info.args == 1)
            {
                // just an immediate on its own does not require correction
                break;
            }

            handle_allocation(func,alloc,block,op_ptr,opcode,info.args-1);
            break;
        }

        case op_group::load_t:
        {
            assert(false);
            break;
        }

        // no regs dont rewrite
        case op_group::implicit_t:
        {
            break;
        }

        case op_group::branch_t:
        {
            break;
        }

        case op_group::slot_t:
        {
            assert(false);
            break;
        }

    }
}

// TODO: we need to do a set of register rewriting rules that will properly handle the following

// how do know when something is a tmp
// could go for mov imm, or when something is is just registers
// but that scheme seemsl like it will fail fast


// TODO: this still hasnt been solved
// and it causes problems with unary minus

// how do we know when a tmp is freed?
// we can remove them when they are stored back as regs
// but what about when we have extras?


// do we just drop the other tmp taht aint stored?
// do we just have certain instrs where we BURN all the tmps left allocated?
// do we emit directives to tell us when expressions are "dead"
// i.e branches
// i.e stores into vars

// TODO: cantt we get conflicts with tmps?
// ans: no we will rewrite hte ir_reg pos when it gets loaded out again



// TODO: we need a way to rewrite the allocation posistions of temp's

void allocate_registers(Function &func)
{
    LocalAlloc alloc;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = REG_FREE;
        alloc.ir_regs[i] = i;
        alloc.free_list[i] = i;
    }


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


    // byte located at start
    alloc.stack_alloc[0] = 0;

    // start at end of byte allocation
    alloc.stack_alloc[1] = func.size_count[0];

    // start at end of half allocation
    alloc.stack_alloc[2] = alloc.stack_alloc[1] + (func.size_count[1] * 2);


    alloc.stack_offset = 0;

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
                    opcode = Opcode(op_type::push,opcode.v[0],0,0);
                    alloc.stack_offset += sizeof(u32);
                    break;
                }


                case op_type::restore_reg:
                {
                    opcode = Opcode(op_type::pop,opcode.v[0],0,0);
                    alloc.stack_offset -= sizeof(u32);
                    break;
                }

                // allocate a tmp
                case op_type::mov_imm:
                {
                    alloc_tmp(alloc,opcode.v[0],block,it,func.slot_lookup);
                    break;
                }



                case op_type::push_arg:
                {
                    opcode =  Opcode(op_type::push,opcode.v[0],0,0);

                    // varaibles now have to be accessed at a different offset
                    // until this is corrected by clean call
                    alloc.stack_offset += sizeof(u32);

                    break;
                }


                case op_type::clean_args:
                {
                    // clean up args
                    const auto stack_clean = sizeof(u32) * opcode.v[0];

                    opcode = Opcode(op_type::add_imm,SP,SP,stack_clean);
                    alloc.stack_offset -= stack_clean; 
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
                    auto &var_alloc = func.slot_lookup[symbol_to_idx(opcode.v[0])];
                    alloc.stack_alloc[var_alloc.size >> 1] -= var_alloc.size;

                    if(var_alloc.location != LOCATION_MEM)
                    {
                        // this var is gone so we can free it
                        free_reg(alloc,var_alloc);
                    }


                    it = block.erase(it);
                    continue;
                }


                case op_type::ret_var:
                {
                    // convert to a standard ret
                    // rewrite the registers here
                    correct_reg(func, alloc, block, it, opcode);

                    // convert into a standard ret
                    opcode = Opcode(op_type::mov_reg,RV,opcode.v[0],0);
                    it = block.insert(++it,Opcode(op_type::ret,0,0,0));
                    continue;
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



            // adjust opcode for reg alloc
            correct_reg(func, alloc, block, it, opcode);

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
            opcode.v[0] = itl.symbol_table.label_lookup[opcode.v[0]].offset;
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
                    printf("%s %s\n",info.name, get_oper(table,opcode.v[0]).c_str());
                    break;
                }

                case 2:
                {
                    printf("%s %s, %s\n",info.name ,get_oper(table,opcode.v[0]).c_str(),get_oper(table,opcode.v[1]).c_str());
                    break;
                }

                case 3:
                {
                    printf("%s %s, %s, %s\n",info.name,
                        get_oper(table,opcode.v[0]).c_str(), get_oper(table,opcode.v[1]).c_str(), 
                        get_oper(table,opcode.v[2]).c_str());
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
                    printf("%s 0x%x\n",info.name,opcode.v[0]);
                    break;
                }

                case 2:
                {
                    printf("%s %s, 0x%x\n",info.name,get_oper(table,opcode.v[0]).c_str(),opcode.v[1]);
                    break;
                }

                case 3:
                {
                    printf("%s %s, %s, %d\n",info.name,get_oper(table,opcode.v[0]).c_str(),get_oper(table,opcode.v[1]).c_str(),opcode.v[2]);
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

            printf("%s %s\n",info.name,get_oper(table,opcode.v[0]).c_str());
            break;
        }

        case op_group::load_t:
        {
            if(info.args != 3)
            {
                printf("unknown opcode");
                exit(1);
            }

            printf("%s %s, [%s,%d]\n",info.name,get_oper(table,opcode.v[0]).c_str(),get_oper(table,opcode.v[1]).c_str(),opcode.v[2]);
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
                        printf("%s 0x%x\n",info.name,opcode.v[0]);
                    }

                    else
                    {
                        const auto labels = *label_lookup;
                        assert(opcode.v[0] < labels.size());

                        printf("%s %s\n",info.name,labels[opcode.v[0]].name.c_str());
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
                        assert(opcode.v[0] < labels.size());

                        printf("%s %s,%s\n",info.name,labels[opcode.v[0]].name.c_str(),get_oper(table,opcode.v[1]).c_str());
                    }

                    else
                    {
                        printf("%s %x,%s\n",info.name,opcode.v[0],get_oper(table,opcode.v[1]).c_str());
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
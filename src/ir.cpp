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


void stack_allocate(u32 *stack_alloc, Symbol &sym)
{
    sym.offset = stack_alloc[sym.size >> 1];
    stack_alloc[sym.size >> 1] += sym.size;
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
    std::map<u32,u32> ir_regs;

    u32 free_regs;

    // free list for register allocator
    u32 free_list[MACHINE_REG_SIZE];


    // stack allocation

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset;

    // how much space has been used so far for each var
    u32 stack_alloc[3];

    // what is the total ammount of space that this functions stack requires!
    u32 stack_size;
};

void spill_reg(LocalAlloc &alloc,Block &block,opcode_iterator_t block_ptr, Symbol& sym, u32 reg)
{
    printf("spill %s:%d\n",sym.name.c_str(),reg);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    // TODO:
    // we will probably want to do some extra marking here for removing stack prologues later
    // and re parse the rets to insert the prologues
    // rather than attempting to do it ahead of time this will also allow us to spill args
    // once they are kept inside registers

    if(sym.offset == UNALLOCATED_OFFSET)
    {
        if(is_arg)
        {
            stack_allocate(alloc.stack_alloc,sym);
        }

        else
        {
            // arg is above the stack frame

            // TODO: how we will handle this when we dont acutally know how large we need to have the stack yet?
            sym.offset = (sym.slot * sym.size) + alloc.stack_size + sizeof(u32);
        }        
    }


    // write value back out into mem
    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
    const auto opcode = Opcode(instr[sym.size >> 1],reg,SP,sym.offset + alloc.stack_offset); 

    block.insert(block_ptr,opcode); 

    // register is back in memory!
    sym.location = LOCATION_MEM;
}

u32 alloc_internal(SlotLookup &slot_lookup,LocalAlloc &alloc,Block &block, opcode_iterator_t block_ptr)
{
    u32 reg = REG_FREE;

    // if there is a free register remove from free list
    // and give to alloc
    if(alloc.free_regs)
    {
        // allocate a register
        reg = alloc.free_list[--alloc.free_regs];
    }

    // TODO: fixme how do we properly handle regs that need to be used in the current opcode
    // being freed?

    // if there is not spill another register (not a tmp or this will break)
    // back into memory
    else 
    {
        for(reg = 0; reg < MACHINE_REG_SIZE; reg++)
        {
            const auto slot = alloc.regs[reg]; 

            bool in_expr = false;

            const auto &opcode = *block_ptr;

            // ^ for now we are just going to check we are not currently using the varaible inside this opcode
            // with a linear scan -> fix this later
            for(int i = 1; i < 3; i++)
            {
                if(slot == opcode.v[i])
                {
                    in_expr = true;
                    break;
                }
            }

            // we have a register to spill
            if(slot < REG_TMP && !in_expr)
            {
                const auto slot = alloc.regs[reg];
                auto &sym = slot_lookup[slot];
                spill_reg(alloc,block,block_ptr,sym,reg);
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


void alloc_tmp(LocalAlloc &alloc, u32 ir_reg, Block &block, opcode_iterator_t block_ptr, SlotLookup &slot_lookup)
{
    const u32 reg = alloc_internal(slot_lookup, alloc, block, block_ptr);
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = REG_TMP;

    printf("%d allocated tmp into %d\n",ir_reg,reg);
}

// mark a tmp as allocated to a var
void alloc_into_tmp(Symbol &sym,LocalAlloc &alloc, u32 ir_reg)
{   
    printf("%s allocated into tmp %d\n",sym.name.c_str(),alloc.ir_regs[ir_reg]);

    const u32 reg = alloc.ir_regs[ir_reg];
    alloc.regs[reg] = sym.slot;
    sym.location = reg;
}

void alloc_into_tmp(LocalAlloc &alloc, u32 ir_dst, u32 ir_src)
{
    alloc.ir_regs[ir_dst] = alloc.ir_regs[ir_src];
}

// directly allocate a var ito a reg
u32 alloc_reg(Symbol &sym, LocalAlloc &alloc, Block &block,opcode_iterator_t block_ptr,SlotLookup &slot_lookup)
{
    const u32 reg = alloc_internal(slot_lookup,alloc, block, block_ptr);
    alloc.regs[reg] = sym.slot;
    sym.location = reg;

    printf("%s allocated into reg %d\n",sym.name.c_str(),reg);

    return reg;
}


void free_reg(LocalAlloc &alloc, u32 reg)
{
    printf("freed tmp %d\n",reg);

    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;
}

void free_reg(LocalAlloc &alloc, Symbol &sym)
{
    printf("freed %s from %d\n",sym.name.c_str(),sym.location);

    free_reg(alloc,sym.location);
    sym.location = LOCATION_MEM;
}

u32 rewrite_reg(LocalAlloc &alloc, u32 ir_reg)
{
    // dont rewrite any special purpose reg

    // TODO: for now assume this is being rewritten the interprettter
    if(ir_reg >= SPECIAL_PURPOSE_REG_START)
    {
        switch(ir_reg)
        {
            case SP_IR: return SP;

            default: assert(false);
        }
    }

    else
    {
        return alloc.ir_regs[ir_reg];
    }
}


void handle_allocation(SlotLookup &slot_lookup, LocalAlloc &alloc, Block &block, opcode_iterator_t op_ptr, Opcode &opcode, u32 args)
{
    u32 tmp_reg = REG_FREE;

    // ensure src var is allocated 
    // (if we are using a tmp as an arg then it must allready have been allocated)
    for(u32 i = 1; i < args; i++)
    {
        if(is_symbol(opcode.v[i]))
        {
            const auto slot = symbol_to_idx(opcode.v[i]);
            auto &sym = slot_lookup[slot];

            // unallocated we need to reload it
            if(sym.location == LOCATION_MEM)
            {
                // TODO: does this need to return the opcode ptr incase it inserts?
                alloc_reg(sym,alloc,block,op_ptr,slot_lookup);

                // reload the spilled var 
                if(is_signed_integer(sym.type))
                {
                    // word is register size (we dont need to extend it)
                    static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                    // this here does not otherwhise need rewriting so we will emit SP directly
                    const auto reload_op = Opcode(instr[sym.size >> 1],sym.location,SP,sym.offset + alloc.stack_offset);
                    block.insert(op_ptr,reload_op);               
                }

                // "plain data"
                // just move by size
                else
                {
                    static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};

                    const auto reload_op =  Opcode(instr[sym.size >> 1],sym.location,SP,sym.offset + alloc.stack_offset);
                    block.insert(op_ptr,reload_op); 
                }
            }
        }

        else
        {
            tmp_reg = opcode.v[i];
        }
    }

    // handle dst allocation
    const u32 idx = opcode.v[0];

    // TODO: we need to look if a src arg is a tmp here and allocate into that instead of something new
    



    // is a var
    if(is_symbol(idx))
    {
        const auto slot = symbol_to_idx(idx);
        auto &sym = slot_lookup[slot];        

        // not currently loaded
        if(sym.location == LOCATION_MEM)
        {
            // allocate into a tmp
            if(tmp_reg != REG_FREE)
            {
                alloc_into_tmp(sym,alloc,tmp_reg);
            }

            else
            {
                alloc_reg(sym,alloc,block,op_ptr,slot_lookup);
            }
        }
    }

    // tmp
    else if(!is_special_reg(idx))
    {  
        // this is not allready a tmp we need to allocate this
        if(alloc.regs[alloc.ir_regs[idx]] != REG_TMP)
        {
            // put this tmp directly into another tmp we will be done with
            // after the end of the expr
            if(tmp_reg != REG_FREE)
            {
                alloc_into_tmp(alloc,idx,tmp_reg);
            }

            // spill something we need to allocate
            else
            {
                alloc_tmp(alloc,idx,block,op_ptr,slot_lookup); 
            }       
        }
    }

    


    // rewrite the register names
    for(u32 i = 0; i < args; i++)
    {
        if( is_symbol(opcode.v[i]))
        {
            const auto slot = symbol_to_idx(opcode.v[i]);
            auto &sym = slot_lookup[slot]; 

            opcode.v[i] = sym.location;
        }

        else
        {
            opcode.v[i] = rewrite_reg(alloc,opcode.v[i]);
        }

    }
}

// rewrite the opcode regs based on allocation and opcode type!
void correct_reg(SlotLookup &slot_lookup, LocalAlloc &alloc, Block &block, opcode_iterator_t op_ptr,Opcode &opcode)
{

    const auto &info = OPCODE_TABLE[static_cast<size_t>(opcode.op)];

    u32 reg_args = info.args;

    switch(info.group)
    {

        case op_group::reg_t:
        {
            handle_allocation(slot_lookup,alloc,block,op_ptr,opcode,info.args);
            break;
        }

        case op_group::imm_t:
        {
            if(info.args == 1)
            {
                // just an immediate on its own does not require correction
                break;
            }

            // which args are actually for registers?
            reg_args = info.args - 1;

            handle_allocation(slot_lookup,alloc,block,op_ptr,opcode,info.args-1);
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
            reg_args = 0;
            break;
        }

        case op_group::branch_t:
        {
            reg_args = 0;
            break;
        }

        case op_group::slot_t:
        {
            assert(false);
            break;
        }
    }

    // TODO: is this sufficent to clean up tmps?
    // or do we need to foribly clean all of them on some specific
    // instructions? 

    const u32 dst_ir = opcode.v[0];

    // free all the tmps that aint the dest as we are done with them
    for(u32 a = 1; a < reg_args; a++)
    {
        // ignore special puprose registers
        if(opcode.v[a] >= MACHINE_REG_SIZE)
        {
            continue;
        }

        // these registers have been converted we have to look inside the 
        // allocation struct to find out what they are for now
        if(alloc.regs[opcode.v[a]] == REG_TMP && opcode.v[a] != dst_ir)
        {
            free_reg(alloc,opcode.v[a]);
        }
    }
}


void allocate_registers(Function &func, SlotLookup &slot_lookup)
{
    LocalAlloc alloc;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = REG_FREE;
        //alloc.ir_regs[i] = i;
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




    
    alloc.stack_size = func.size_count[0] + (func.size_count[1] * 2) + (func.size_count[2] * 4);
    //printf("stack size: %d\n",stack_size);

    // only allocate a stack if we need it
    if(alloc.stack_size)
    {
        func.emitter.program[0].push_front(Opcode(op_type::sub_imm,SP_IR,SP_IR,alloc.stack_size));
    }

    // opcode to re correct the stack
    const auto stack_clean = Opcode(op_type::add_imm,SP_IR,SP_IR,alloc.stack_size);


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
                    alloc_tmp(alloc,opcode.v[0],block,it,slot_lookup);
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

                    opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,stack_clean);
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
                    auto &sym = slot_lookup[symbol_to_idx(opcode.v[0])];
                    alloc.stack_alloc[sym.size >> 1] -= sym.size;

                    if(sym.location != LOCATION_MEM)
                    {
                        // this var is gone so we can free it
                        free_reg(alloc,sym);
                    }


                    it = block.erase(it);
                    continue;
                }


                case op_type::ret_var:
                {
                    // convert to a standard ret
                    // rewrite the registers here
                    correct_reg(slot_lookup, alloc, block, it, opcode);

                    // convert into a standard ret
                    opcode = Opcode(op_type::mov_reg,RV,opcode.v[0],0);
                    it = block.insert(++it,Opcode(op_type::ret,0,0,0));
                    continue;
                }

                // add stack cleanup to all ret functions
                case op_type::ret:
                {
                    // if there is no stack allocation there is nothing to clean up
                    if(alloc.stack_size)
                    {
                        auto it_ret = block.insert(it,stack_clean);
                        
                        correct_reg(slot_lookup,alloc,block,it_ret,*it_ret);
                    }
                    break;
                }

                default: break;
            }



            // adjust opcode for reg alloc
            correct_reg(slot_lookup, alloc, block, it, opcode);

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



using IR_OPER_STRING_FUNC = std::string (*)(const SlotLookup *table, u32);

// disassemble with symbols
std::string get_oper_sym(const SlotLookup *table,u32 v)
{
    auto slot_lookup = *table;

    if(v >= SYMBOL_START && symbol_to_idx(v) < table->size())
    {
        return slot_lookup[symbol_to_idx(v)].name;
    }

    return "v" + std::to_string(v);
}

// disassemble without needing the symbol information
std::string get_oper_raw(const SlotLookup *table,u32 v)
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
void disass_opcode(const Opcode &opcode, const SlotLookup *table, const std::vector<Label> *label_lookup, IR_OPER_STRING_FUNC get_oper)
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

void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table, const LabelLookup &label_lookup)
{
    disass_opcode(opcode,&table,&label_lookup,get_oper_sym);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode(opcode,nullptr,nullptr,get_oper_raw);
}
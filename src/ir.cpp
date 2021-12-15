#include <interloper.h>
#include <op_table.inl>

std::string get_oper_sym(const SlotLookup *table,u32 v);
std::string get_oper_raw(const SlotLookup *table,u32 v);

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
static constexpr u32 REG_TMP_START = 0xf0000000;


bool reg_is_var(u32 loc)
{
    return loc < REG_TMP_START;
}

bool reg_is_tmp(u32 loc)
{
    return loc >= REG_TMP_START && loc != REG_FREE;
}

u32 tmp_to_ir(u32 loc)
{
    return loc - REG_TMP_START;
}

u32 tmp(u32 ir_reg)
{
    return ir_reg + REG_TMP_START;
}

// our bitset can only store 32 regs
static_assert(MACHINE_REG_SIZE <= 32);

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

    // bitset of which regs this functions needs to use
    // for now we are going to just callee save every register
    u32 used_regs;
    u32 use_count;


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

    if(sym.offset == UNALLOCATED_OFFSET)
    {
        // only allocate the stack args by here
        if(!is_arg(sym))
        {
            stack_allocate(alloc.stack_alloc,sym);
        }

        // TODO: fixme
        // an arg should allways have its offset allocated for now as we dont have register passing
        // or stack alloc during reg alloc properly
        else
        {
            panic("attempted to spill unallocated var");
        }   
    }

    const auto opcode = Opcode(op_type::spill,reg,slot_idx(sym),0);

    block.insert(block_ptr,opcode); 

    // register is back in memory!
    sym.location = LOCATION_MEM;
    alloc.regs[reg] = REG_FREE;
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

        // mark as used by the function
        alloc.use_count += !is_set(alloc.used_regs,reg);

        alloc.used_regs = set_bit(alloc.used_regs,reg);
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

            b32 in_expr = false;

            const auto &opcode = *block_ptr;

            // ^ for now we are just going to check we are not currently using the varaible inside this opcode
            // with a linear scan -> fix this later
            for(int i = 1; i < 3; i++)
            {
                if(is_symbol(opcode.v[i]) && slot == symbol_to_idx(opcode.v[i]))
                {
                    in_expr = true;
                    break;
                }
            }

            // we have a var to spill
            if(reg_is_var(slot) && !in_expr)
            {
                const auto slot = alloc.regs[reg];
                auto &sym = slot_lookup[slot];
                spill_reg(alloc,block,block_ptr,sym,reg);
                break;
            }
        }

        // failed to find a reg to spill should not happen
        panic(reg == MACHINE_REG_SIZE,"failed to allocate register!");
    }

    return reg;    
}


void alloc_tmp(LocalAlloc &alloc, u32 ir_reg, Block &block, opcode_iterator_t block_ptr, SlotLookup &slot_lookup)
{
    const u32 reg = alloc_internal(slot_lookup, alloc, block, block_ptr);
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = tmp(ir_reg);

    printf("tmp t%d allocated into r%d\n",ir_reg,reg);
}

// mark a tmp as allocated to a var
void alloc_into_tmp(Symbol &sym,LocalAlloc &alloc, u32 ir_reg)
{   
    const u32 reg = alloc.ir_regs[ir_reg];

    printf("symbol %s allocated into tmp t%d -> r%d\n",sym.name.c_str(),tmp_to_ir(alloc.regs[reg]),reg);

    alloc.regs[reg] = sym.slot;
    sym.location = reg;
}

void destroy_ir_reg(LocalAlloc &alloc, u32 ir_reg)
{
    alloc.ir_regs[ir_reg] = MACHINE_REG_SIZE;
}


void alloc_into_tmp(LocalAlloc &alloc, u32 ir_dst, u32 ir_src)
{
    alloc.ir_regs[ir_dst] = alloc.ir_regs[ir_src];
    alloc.regs[alloc.ir_regs[ir_dst]] = tmp(ir_dst);
    

    // as ir_src is unqiue once it has been used in an expr it wont be used again
    // so we dont need to worry about unlinking the ref

    printf("tmp t%d allocated into existing tmp t%d -> r%d\n",ir_dst,ir_src,alloc.ir_regs[ir_dst]);
}

// directly allocate a var ito a reg
u32 alloc_reg(Symbol &sym, LocalAlloc &alloc, Block &block,opcode_iterator_t block_ptr,SlotLookup &slot_lookup)
{
    const u32 reg = alloc_internal(slot_lookup,alloc, block, block_ptr);
    alloc.regs[reg] = sym.slot;
    sym.location = reg;

    printf("symbol %s into reg r%d\n",sym.name.c_str(),reg);

    return reg;
}


void free_reg(LocalAlloc &alloc, u32 reg)
{
    printf("freed tmp t%d from r%d\n",tmp_to_ir(alloc.regs[reg]),reg);

    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;
}

void free_reg(LocalAlloc &alloc, Symbol &sym)
{
    printf("freed sym %s from r%d\n",sym.name.c_str(),sym.location);

    alloc.regs[sym.location] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = sym.location;
    sym.location = LOCATION_MEM;
}


void save_rv(LocalAlloc &alloc,Block &block,opcode_iterator_t block_ptr,SlotLookup &slot_lookup,u32 tmp)
{
    //puts("need to realloc tmp"); exit(1);
    

    // get a new register
    const u32 reg = alloc_internal(slot_lookup, alloc, block, block_ptr);

    const u32 ir_reg = tmp_to_ir(tmp);

    const auto op = Opcode(op_type::mov_reg,ir_reg,RV,0);

    // emit a mov from the the current tmp to the new one
    block.insert(block_ptr,op);

    printf("moved tmp t%d from rv to r%d\n",ir_reg,reg);

    // rewrite the ir allocation
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = ir_reg;

    // free RV
    free_reg(alloc,RV);
}


u32 rewrite_reg(LocalAlloc &alloc, u32 ir_reg)
{
    // dont rewrite any special purpose reg

    // TODO: for now assume this is running under the interpretter
    // so its converted to our interrpetter regs and not a hardware target
    if(is_special_reg(ir_reg))
    {
        switch(ir_reg)
        {
            case SP_IR: return SP;
            case RV_IR: return RV;

            default: panic("unhandled special reg %x\n",ir_reg);
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

                const auto reload_op = Opcode(op_type::load,sym.location,opcode.v[i],0);
                block.insert(op_ptr,reload_op); 
            }
        }

        else if(is_tmp(opcode.v[i]))
        {
            tmp_reg = opcode.v[i];
        }
    }

    // handle dst allocation
    const u32 idx = opcode.v[0];


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

    // each tmp store is unique we only need to allocate at first use
    else if(is_tmp(idx))
    {  
        // this is not allready a tmp we need to allocate this
        if(!alloc.ir_regs.count(idx))
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
        if(is_symbol(opcode.v[i]))
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
        case op_group::regm_t:
        {
            panic("regm in correct_reg");
            break;
        }


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
            panic("load in correct_reg");
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
            panic("slot in correct_reg");
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
        // these registers have been converted we have to look inside the 
        // allocation struct to find out what they are for now
        if(opcode.v[a] != dst_ir && reg_is_tmp(alloc.regs[opcode.v[a]]))
        {
            free_reg(alloc,opcode.v[a]);
        }
    }
}

// TODO: need to rethink this when we do register passing
// and when we push off determining stack size to a later pass
void alloc_args(Function &func, LocalAlloc& alloc, SlotLookup &slot_lookup, u32 saved_regs_offset)
{
    for(auto slot : func.args)
    {
        auto &sym = slot_lookup[slot];

        //printf("%s : %d\n",sym.name.c_str(),sym.arg_num);

        // alloc above the stack frame
        sym.offset = (sym.arg_num  * sizeof(u32)) + alloc.stack_size + saved_regs_offset + sizeof(u32);
    }
             
}

void allocate_registers(Function &func, SlotLookup &slot_lookup)
{
    LocalAlloc alloc;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;
    alloc.use_count = 0;
    alloc.used_regs = 0;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = REG_FREE;
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


        // TODO: rework this when we add proper stack allocation under the register allocator
        // as this is wasting a ton of stack space right now
    */



    // byte located at start
    alloc.stack_alloc[0] = 0;

    // start at end of byte allocation
    alloc.stack_alloc[1] = func.size_count[0];

    // align for u16
    if(alloc.stack_alloc[1] & 1)
    {
        alloc.stack_alloc[1] += 1;
    }


    // start at end of half allocation
    alloc.stack_alloc[2] = alloc.stack_alloc[1] + (func.size_count[1] * 2);

    // align for u32
    if(alloc.stack_alloc[2] & 2)
    {
        alloc.stack_alloc[2] += 2;
    }

    // get the total stack size
    alloc.stack_size =  alloc.stack_alloc[2] + (func.size_count[2] * 4);
    printf("stack size: %d\n",alloc.stack_size);

    // only allocate a stack if we need it
    if(alloc.stack_size)
    {
        func.emitter.program[0].push_front(Opcode(op_type::sub_imm,SP_IR,SP_IR,alloc.stack_size));
    }

    // opcode to re correct the stack
    const auto stack_clean = Opcode(op_type::add_imm,SP_IR,SP_IR,alloc.stack_size);


    alloc.stack_offset = 0;


    for(auto &block : func.emitter.program)
    {
        for(auto it = block.begin(); it != block.end();)
        {
            auto &opcode = *it;



            switch(opcode.op)
            {

                // TODO: reviist when we add caller saved regs
                //case op_type::save_regs:
                //case op_type::restore_regs:
                


                // allocate a tmp
                case op_type::mov_imm:
                {
                    if(is_tmp(opcode.v[0]))
                    {
                        alloc_tmp(alloc,opcode.v[0],block,it,slot_lookup);
                    }

                    else if(is_symbol(opcode.v[0]))
                    {
                        // alloc_reg(sym,alloc,block,op_ptr,slot_lookup);
                        auto &sym = slot_lookup[symbol_to_idx(opcode.v[0])];
                        alloc_reg(sym,alloc,block,it,slot_lookup);
                    }
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

                // make sure the return value has nothing important when calling functions
                case op_type::spill_rv:
                {
                    if(reg_is_var(alloc.regs[RV]))
                    {
                        spill_reg(alloc,block,it,slot_lookup[alloc.regs[RV]],RV);
                    }


                    else if(reg_is_tmp(alloc.regs[RV]))
                    {
                        // should we have this get pushed instead?
                        // and then popped into another reg?
                        save_rv(alloc,block,it,slot_lookup,alloc.regs[RV]);
                    }

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


                default: break;
            }



            // adjust opcode for reg alloc
            correct_reg(slot_lookup, alloc, block, it, opcode);

            // use continue to skip this statement when we have to delete from the list
            ++it;
        }
    }



    // iterate over the function by here and add callee cleanup at every ret
    // and insert the stack offsets and load and spill directives

    // TODO: this might be good to loop jam with somethign but just have a seperate loop for simplictiy atm


    // TODO: make register allactor not emit mov r0,r0 after function calls with return values


    const bool insert_callee_saves = func.name != "main" && alloc.use_count != 0;


    alloc_args(func,alloc,slot_lookup,insert_callee_saves? sizeof(u32) * alloc.use_count : 0);

    // entry point does not need to preserve regs
    if(insert_callee_saves)
    {
        func.emitter.program[0].push_front(Opcode(op_type::pushm,alloc.used_regs,0,0));
    }

    const auto callee_restore = Opcode(op_type::popm,alloc.used_regs,0,0);

    for(auto &block : func.emitter.program)
    {
        for(auto it = block.begin(); it != block.end();)
        {
            auto &opcode = *it;

            switch(opcode.op)
            {


                case op_type::ret:
                {
                    const auto old = it;

                    // if there is no stack allocation there is nothing to clean up
                    if(alloc.stack_size)
                    {
                        it = block.insert(it,stack_clean);
                        correct_reg(slot_lookup,alloc,block,it,*it);
                    }

                    if(insert_callee_saves)
                    {
                        block.insert(it,callee_restore);
                    }

                    it = old;
                    break;
                }

                // reload a reg
                case op_type::load:
                {
                    const auto slot = symbol_to_idx(opcode.v[1]);
                    auto &sym = slot_lookup[slot];


                    // reload the spilled var 
                    if(is_signed_integer(sym.type))
                    {
                        // word is register size (we dont need to extend it)
                        static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                        // this here does not otherwhise need rewriting so we will emit SP directly
                        opcode = Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + alloc.stack_offset);        
                    }

                    // "plain data"
                    // just move by size
                    else
                    {
                        static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};

                        opcode =  Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + alloc.stack_offset);
                    }
                    break;
                }

                case op_type::spill:
                {
                    const auto slot = symbol_to_idx(opcode.v[1]);
                    auto &sym = slot_lookup[slot];

                    // write value back out into mem
                    // TOOD: we need to make sure when we finish up our stack alloc rewrite
                    // that the offsets on this are aligned properly atm on x86 this doesnt matter but it will later

                    // TODO: we need to not bother storing these back if the varaible spilled has not been modified
                    // and is just used as a const for a calc (how do we impl this?)

                    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
                    opcode = Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + alloc.stack_offset);   
                    break;                  
                }


                default: break;

            }

            it++;
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

    if(v == RV_IR)
    {
        return "rv";
    }

    else if(v >= SYMBOL_START && symbol_to_idx(v) < table->size())
    {
        return slot_lookup[symbol_to_idx(v)].name;
    }

    return "t" + std::to_string(v);
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
        case op_group::regm_t:
        {
            switch(info.args)
            {
                
                case 1:
                {
                    u32 count = 0;
                    printf("%s {",info.name);
                    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
                    {
                        if(is_set(opcode.v[0],r))
                        {
                            printf("%sr%d",count != 0? "," : "",r);
                            count++;
                        }
                    }

                    printf("}\n");

                    break;
                }

                default: printf("invalid regm opcode\n"); exit(1); 
            }
            break;
        }

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
                    printf("unknown reg opcode\n");
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
                    printf("unknown imm opcode\n");
                    exit(1);                        
                } 
            }
            break;
        }

        case op_group::slot_t:
        {
            if(info.args != 1)
            {
                puts("unknown slot opcode\n");
                exit(1);
            }

            printf("%s %s\n",info.name,get_oper(table,opcode.v[0]).c_str());
            break;
        }

        case op_group::load_t:
        {
            if(info.args != 3)
            {
                printf("unknown opcode\n");
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
                        panic(opcode.v[0] >= labels.size(),"out of range label in branch");

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
                        panic(opcode.v[0] >= labels.size(),"out of range label in cond branch");

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
                    printf("unknown branch opcode\n");
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
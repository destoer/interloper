#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"

std::string get_oper_sym(const SlotLookup *table,u32 v);
std::string get_oper_raw(const SlotLookup *table,u32 v);



void emit(IrEmitter &emitter,op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto &list = emitter.program[emitter.program.size()-1].list;
    append(list,opcode);
}

void new_block(IrEmitter &emitter,block_type type, u32 slot)
{
    emitter.program.push_back(Block(type));
    emitter.block_slot.push_back(slot);    
}


static constexpr u32 REG_FREE = 0xffffffff;
static constexpr u32 REG_TMP_START = 0xf0000000;

u32 reg(u32 r)
{
    return r;
}


u32 symbol_to_idx(u32 s)
{
    return s - SYMBOL_START;
}

bool is_symbol(u32 s)
{
    return s >= SYMBOL_START;
}


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

bool is_reg(u32 r)
{
    return r < SPECIAL_PURPOSE_REG_START;
}

bool is_special_reg(u32 r)
{
    return r >= SPECIAL_PURPOSE_REG_START && r < SYMBOL_START;
}

bool is_tmp(u32 r)
{
    return r < SPECIAL_PURPOSE_REG_START;
}

Symbol& sym_from_slot(SlotLookup &slot_lookup, u32 slot)
{
    return slot_lookup[symbol_to_idx(slot)]; 
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

    // how much of each type of var is there at the momemnt?
    u32 size_count[3];

    // what is the maximum ammount of vars?
    // this will be used to compute the stack size later
    u32 size_count_max[3];

    // what is the total ammount of space that this functions stack requires!
    u32 stack_size;
};


void print_alloc(LocalAlloc &alloc,SlotLookup &slot_lookup)
{
    printf("\n\nallocation:\n\n");

    printf("total registers: %d\n",MACHINE_REG_SIZE);
    printf("free registers: %d\n",alloc.free_regs);
    printf("used regsisters: %d\n",MACHINE_REG_SIZE - alloc.free_regs);
    printf("total used registers: %d\n",alloc.use_count);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        const u32 slot = alloc.regs[i];

        if(reg_is_tmp(slot))
        {
            printf("reg r%d -> temp t%d\n",i,tmp_to_ir(slot));
        }

        else if(reg_is_var(slot))
        {
            const auto &sym = slot_lookup[slot];

            printf("reg r%d -> var %s\n",i,sym.name.c_str());
        }
    }

    putchar('\n');

}


u32 alloc_internal(Interloper &itl,LocalAlloc &alloc,List &list, ListNode* node)
{
    UNUSED(itl); UNUSED(list); UNUSED(node);
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
        unimplemented("spill_allocation");
    }

    return reg;    
}



void alloc_tmp(Interloper& itl,LocalAlloc &alloc, List &list, ListNode* node, u32 ir_reg)
{
    const u32 reg = alloc_internal(itl, alloc, list, node);
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = tmp(ir_reg);

    printf("tmp t%d allocated into r%d\n",ir_reg,reg);
}

void alloc_sym(Interloper& itl,LocalAlloc &alloc, List &list, ListNode* node, Symbol &sym)
{
    const u32 reg = alloc_internal(itl, alloc, list, node);
    alloc.regs[reg] = sym.slot; 
    sym.location = reg;

    printf("symbol %s into reg r%d\n",sym.name.c_str(),reg);
}

// mark a tmp as allocated to a var
void alloc_sym_into_tmp(Symbol &sym,LocalAlloc &alloc, u32 ir_reg)
{   
    const u32 reg = alloc.ir_regs[ir_reg];

    printf("symbol %s allocated into tmp t%d -> r%d\n",sym.name.c_str(),tmp_to_ir(alloc.regs[reg]),reg);

    alloc.regs[reg] = sym.slot;
    sym.location = reg;
}



/* TODO: 
    make register rewriting have an easier interface (make it so you can just rewrite individual opcode fields if need be)
    make accessing symbols easier
    make inserting opcodes easier -> (roll your own doubly linked list)
    refactor the loop to make deletion less tricky
    implement proper IR descriptions, i.e what the purpose of a field is 
    and if it is a source or a dst inside the lookup table so its easier to disassembly opcodes
    and to rewrite registers because the table allready has all the information
*/
/*
void rewrite_field()
{

}
*/


void free_tmp(LocalAlloc &alloc, u32 reg)
{
    printf("freed tmp t%d from r%d\n",tmp_to_ir(alloc.regs[reg]),reg);

    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;
}

void free_sym(LocalAlloc &alloc, Symbol &sym)
{
    printf("freed sym %s from r%d\n",sym.name.c_str(),sym.location);

    alloc.regs[sym.location] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = sym.location;
    sym.location = LOCATION_MEM;
}


void handle_allocation(Interloper &itl, LocalAlloc& alloc,List &list, ListNode *node)
{
    UNUSED(list); UNUSED(alloc);

    auto& slot_lookup = itl.symbol_table.slot_lookup;

    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    u32 tmp_reg = REG_FREE; UNUSED(tmp_reg);

    // make sure our src var's are loaded
    // mark if any are tmp's we can reuse to allocate the dst
    for(u32 a = 0; a < info.args; a++)
    {
        // only interested in registers
        if(info.type[a] != arg_type::src_reg)
        {
            continue;
        }


        if(is_symbol(opcode.v[a]))
        {
            const auto sym = sym_from_slot(slot_lookup,opcode.v[a]);

            // in memory reload into register
            if(sym.location == LOCATION_MEM)
            {
                unimplemented("reload spilled symbol");
            }

            // pointer taken to var reload anyways
            if(sym.referenced)
            {
                unimplemented("pointer symbol reload");
            }

        }
        
        
        else if(is_tmp(opcode.v[a]))
        {
            tmp_reg = opcode.v[a];
        }
    }


    const u32 slot = opcode.v[0];

    // allocate the dst
    // NOTE: this can only appear in the 0 posistion
    // and there can only be one
    if(info.type[0] == arg_type::dst_reg)
    {
        if(is_symbol(slot))
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[0]);

            if(sym.location == LOCATION_MEM)
            {
                if(tmp_reg != REG_FREE)
                {
                    alloc_sym_into_tmp(sym,alloc,tmp_reg);
                }

                else
                {
                    alloc_sym(itl,alloc,list,node,sym);
                }
            }

            // spill the register as soon as it is written
            else if(sym.referenced)
            {
                unimplemented("sym dst pointer spill");
            }
        }

        else if(is_tmp(slot))
        {
            if(tmp_reg != REG_FREE)
            {
                unimplemented("allocate tmp into tmp");
            }

            else
            {
                alloc_tmp(itl,alloc,list,node,slot);
            }
        }
    }

    else if(info.type[0] == arg_type::src_reg)
    {
        unimplemented("src_reg first arg");
    }
}

void rewrite_reg(SlotLookup& slot_lookup,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    

    if(info.type[reg] == arg_type::src_reg || info.type[reg] == arg_type::dst_reg)
    {
        const u32 slot = opcode.v[reg];

        if(is_symbol(slot))
        {
            const auto sym = sym_from_slot(slot_lookup,slot);

            opcode.v[reg] = sym.location;
        }


        // dont rewrite any special purpose reg
        // NOTE: for now assume this is running under the interpretter
        // so its converted to our interrpetter regs and not a hardware target
        else if(is_special_reg(slot))
        {
            switch(slot)
            {
                case SP_IR: opcode.v[reg] = SP; break;
                case RV_IR: opcode.v[reg] = RV; break;

                default: panic("unhandled special reg %x\n",slot); break;
            }
        }

        else
        {
            opcode.v[reg] = alloc.ir_regs[slot];
        }           
    }
}

void rewrite_regs(SlotLookup& slot_lookup,LocalAlloc& alloc,Opcode &opcode)
{   
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    for(u32 r = 0; r < info.args; r++)
    {
        rewrite_reg(slot_lookup,alloc,opcode,r);
    }
}

void rewrite_opcode(Interloper &itl,LocalAlloc& alloc,List &list, ListNode *node)
{
    // allocate the registers
    handle_allocation(itl,alloc,list,node);

    auto &opcode = node->opcode;

    // rewrite each slot to its allocated register
    rewrite_regs(itl.symbol_table.slot_lookup,alloc,opcode);

    const auto info = OPCODE_TABLE[u32(opcode.op)];

    // make sure not to free a tmp that has been repurposed
    const u32 dst = info.type[0] == arg_type::dst_reg? opcode.v[0] : REG_FREE;

    // free any temp's that are "source" registers as we are done with them
    for(u32 r = 0; r < info.args; r++)
    {
        const u32 reg = opcode.v[r];

        if(info.type[r] == arg_type::src_reg && reg_is_tmp(alloc.regs[reg]) && reg != dst)
        {
            free_tmp(alloc,reg);
        }
    }
}



ListNode *allocate_opcode(Interloper& itl,LocalAlloc &alloc,List &list, ListNode *node)
{
    UNUSED(itl); UNUSED(alloc); UNUSED(list); UNUSED(node);

    auto &slot_lookup = itl.symbol_table.slot_lookup;
    const auto &opcode = node->opcode;

    switch(node->opcode.op)
    {
        // TODO: revisit when we add caller saved regs
        //case op_type::save_regs:
        //case op_type::restore_regs:
/*
        // allocate a tmp
        case op_type::mov_imm:
        {
            unimplemented("mov_imm");
            break;
        }
*/

        case op_type::load_arr_data:
        {
            unimplemented("load_arr_data");
            break;
        }

        case op_type::arr_index:
        {
            unimplemented("arr_index");
            break;
        }

        case op_type::load_arr_len:
        {
            unimplemented("load_arr_len");
            break;
        }

        case op_type::init_arr_idx:
        {
            unimplemented("init_arr_idx");
            break;
        }                

        case op_type::addrof:
        {
            unimplemented("addrof");
            break;
        }

        // have to do correct reg by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            unimplemented("push_arg");
            break;
        }

        case op_type::clean_args:
        {
            unimplemented("clean_args");
            break;
        }

        case op_type::alloc_slot:
        {
            const auto &sym = sym_from_slot(slot_lookup,opcode.v[0]);

            // if we have an array and we know the size
            // then we need to allocate memory for it
            if(is_array(sym.type))
            {
                unimplemented("alloc slot array");
            }

            else
            {
                return remove(list,node);
            }
            break;
        }

        // make sure the return value has nothing important when calling functions
        case op_type::spill_rv:
        {
            unimplemented("spill_rv");
            break;
        }


        
        case op_type::free_slot:
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[0]);

            // this var is done so we can reclaim the stack space
            if(sym.offset >= PENDING_ALLOCATION)
            {
                if(!is_array(sym.type))
                {
                    alloc.size_count[sym.size >> 1] -= 1;
                }

                else
                {
                    const auto [size,count] = get_arr_size(itl,sym.type); 
                    alloc.size_count[size >> 1] -= count;
                }
            }


            if(sym.location != LOCATION_MEM)
            {
                // this var is gone so we can free it
                free_sym(alloc,sym);
            }

            return remove(list,node);
        }

        default:
        {
            rewrite_opcode(itl,alloc,list,node);
            node = node->next;
            break; 
        }
    }

    return node;
}

void allocate_registers(Interloper& itl,Function &func)
{
    printf("allocating registers for %s:\n\n",func.name.c_str());

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

    memset(alloc.stack_alloc,0,sizeof(alloc.stack_alloc));

    memset(alloc.size_count_max,0,sizeof(alloc.size_count_max));
    memset(alloc.size_count,0,sizeof(alloc.size_count));
    alloc.stack_size = 0;
    alloc.stack_offset = 0;

   

    for(auto &block : func.emitter.program)
    {
        List& list = block.list;

        ListNode *node = list.start;
        while(node)
        {
            node = allocate_opcode(itl,alloc,list,node);
        }
    }
}


void dump_program(std::vector<Opcode> &program, std::map<u32,u32> &inv_label_lookup, LabelLookup &label_lookup)
{
    for(u32 pc = 0; pc < program.size(); pc++)
    {
        if(inv_label_lookup.count(pc * OP_SIZE))
        {
            printf("0x%08x %s:\n",pc * OP_SIZE,label_lookup[inv_label_lookup[pc * OP_SIZE]].name.c_str());
        }

        printf("  0x%08x:\t ",pc * OP_SIZE);    
        disass_opcode_raw(program[pc]);
    }
}

void emit_asm(Interloper &itl)
{
    std::map<u32,u32> inv_label_lookup;


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


        inv_label_lookup[itl.program.size() * OP_SIZE] = func.slot;

        for(u32 b = 0; b < func.emitter.program.size(); b++)
        {
            const auto &block = func.emitter.program[b];

            // resolve label addr.
            itl.symbol_table.label_lookup[func.emitter.block_slot[b]].offset = itl.program.size() * OP_SIZE;

            // prefer function name
            if(b != 0)
            {
                inv_label_lookup[itl.program.size() * OP_SIZE] = func.emitter.block_slot[b];
            }

            auto node = block.list.start;
            while(node)
            {
                itl.program.push_back(node->opcode);
                node = node->next;
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

    dump_program(itl.program,inv_label_lookup,itl.symbol_table.label_lookup);
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

                default: panic("invalid regm opcode\n"); 
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
                    panic("unknown reg opcode\n");
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
                    panic("unknown imm opcode\n");                       
                } 
            }
            break;
        }

        case op_group::slot_t:
        {
            switch(info.args)
            {
                case 1:
                { 
                    printf("%s %s\n",info.name,get_oper(table,opcode.v[0]).c_str()); 
                    break;
                }

                case 3:
                { 
                    printf("%s %s, %d, %d\n",info.name,get_oper(table,opcode.v[0]).c_str(),opcode.v[1],opcode.v[2]); 
                    break;
                }

                default: panic("unknown slot opcode\n"); break;
            }
            break;
        }

        case op_group::store_t:
        case op_group::load_t:
        {
            if(info.args != 3)
            {
                panic("unknown opcode\n");
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
                    panic("unknown branch opcode\n");
                }
            }   
            break;
        }

    }

}

void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table)
{
    disass_opcode(opcode,&table,nullptr,get_oper_sym);
}

void disass_opcode_sym(const Opcode &opcode, const SlotLookup &table, const LabelLookup &label_lookup)
{
    disass_opcode(opcode,&table,&label_lookup,get_oper_sym);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode(opcode,nullptr,nullptr,get_oper_raw);
}


void dump_ir(Function &func,const SlotLookup &slot_lookup,const LabelLookup &label_lookup)
{
    printf("%s:\n",func.name.c_str());

    u32 l = 0;
    for(u32 b = 0; b < func.emitter.program.size(); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        if(func.emitter.block_slot[b] != 0xffffffff)
        {
            printf("%s:\n",label_lookup[func.emitter.block_slot[b]].name.c_str());
        }


        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_sym(node->opcode,slot_lookup,label_lookup);
            node = node->next;
        }

        if(block.last)
        {
            printf("\tspill_last\n");
        }
        
        l++;
    }

    printf("\n");       
}
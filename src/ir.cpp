#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"

std::string get_oper_sym(const SlotLookup *table,u32 v);
std::string get_oper_raw(const SlotLookup *table,u32 v);



void emit(IrEmitter &emitter,op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto list = emitter.program[emitter.program.size()-1].list;
    insert_end(list,opcode);
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


/* TODO: 
    make register rewriting have an easier interface (make it so you can just rewrite individual opcode fields if need be)
    make accessing symbols easier
    make inserting opcodes easier -> (roll your own doubly linked list)
    refactor the loop to make deletion less tricky
    implement proper IR descriptions, i.e what the purpose of a field is 
    and if it is a source or a dst inside the lookup table so its easier to disassembly opcodes
    and to rewrite registers because the table allready has all the information
*/

void allocate_registers(Interloper& itl,Function &func, SlotLookup &slot_lookup)
{
    UNUSED(itl); UNUSED(func); UNUSED(slot_lookup);
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
            while(node->next != nullptr)
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
        while(node->next != nullptr)
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
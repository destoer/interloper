#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"

std::string get_oper_sym(const SlotLookup *table,u32 v);
std::string get_oper_raw(const SlotLookup *table,u32 v);


void emit(IrEmitter &emitter,const Opcode& opcode)
{
    auto &list = emitter.program[emitter.program.size()-1].list;
    append(list,opcode);
}

void emit(IrEmitter &emitter,op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto &list = emitter.program[emitter.program.size()-1].list;
    append(list,opcode);
}

Opcode write_ptr(u32 dst_slot, u32 addr_slot, u32 size, u32 offset)
{
    if(size <= sizeof(u32))
    {
        static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
        return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);
    }   

    else
    {
        unimplemented("struct write");
    }    
}

Block make_block(block_type type,ArenaAllocator* list_allocator)
{
    Block block;

    block.type = type;
    block.last = false;
    block.list = make_list(list_allocator);

    return block;
}

void new_block(ArenaAllocator* list_allocator,IrEmitter &emitter,block_type type, u32 slot)
{
    emitter.program.push_back(make_block(type,list_allocator));
    emitter.block_slot.push_back(slot);    
}


static constexpr u32 REG_FREE = 0xffffffff;
static constexpr u32 REG_TMP_START = 0xf0000000;

u32 reg(u32 r)
{
    return r;
}


u32 sym_to_idx(u32 s)
{
    return s - SYMBOL_START;
}

bool is_sym(u32 s)
{
    return s >= SYMBOL_START;
}


bool reg_is_sym(u32 loc)
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

// dont correct special regs
bool is_reg(u32 r)
{
    return r < MACHINE_REG_SIZE;
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
    return slot_lookup[sym_to_idx(slot)]; 
}


u32 new_slot(Function &func)
{       
    return reg(func.emitter.reg_count++);
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

LocalAlloc make_local_alloc()
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

    memset(alloc.stack_alloc,0,sizeof(alloc.stack_alloc));

    memset(alloc.size_count_max,0,sizeof(alloc.size_count_max));
    memset(alloc.size_count,0,sizeof(alloc.size_count));
    alloc.stack_size = 0;
    alloc.stack_offset = 0;    

    return alloc;
}

void rewrite_reg(SlotLookup& slot_lookup,LocalAlloc& alloc,Opcode &opcode, u32 reg);

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

        else if(reg_is_sym(slot))
        {
            const auto &sym = slot_lookup[slot];

            printf("reg r%d -> var %s\n",i,sym.name.c_str());
        }
    }

    putchar('\n');

}


void spill_sym(LocalAlloc& alloc,List &list,ListNode *node,Symbol &sym, bool after=false)
{
    // no need to spill
    if(sym.location == LOCATION_MEM)
    {
        return;
    }

    const u32 reg = sym.location;

    printf("spill %s:%d\n",sym.name.c_str(),reg);

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(sym.offset == UNALLOCATED_OFFSET)
    {
        // only allocate the local vars by here
        if(!is_arg(sym))
        {
            const u32 size = sym.size;
            const u32 idx = size >> 1;

            // TODO: handle structs
            assert(size <= sizeof(u32));

            // extract how far into the size block is it currently?
            const u32 cur = alloc.size_count[idx];
            sym.offset = PENDING_ALLOCATION + cur;

            // another var of this size allocated
            alloc.size_count[idx]++;
            alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);
        }

        // args need to be allocated later
        // because we need to know the stack size to know there posistions
    }


    // TODO: how do we know if a variable has not been modified
    // i.e it is a ready only copy and has not been modifed so we dont actually need to write it back
    const auto opcode = Opcode(op_type::spill,reg,slot_idx(sym),alloc.stack_offset);

    if(!after)
    {
        insert_at(list,node,opcode); 
    }

    else 
    {
        insert_after(list,node,opcode);
    }

    // register is back in memory!
    sym.location = LOCATION_MEM;
    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;    
}

void spill_all(LocalAlloc &alloc, SlotLookup &slot_lookup, List& list, ListNode* node, bool after)
{
    puts("spilling everything"); 
         
    // TODO: revisit this once we come up with a scheme to make sure that vars get put into the proper regs
    
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        if(reg_is_sym(alloc.regs[r]))
        {
            auto &sym = slot_lookup[alloc.regs[r]];
            spill_sym(alloc,list,node,sym,after);
        }
    }    
}

u32 alloc_internal(SlotLookup &slot_lookup,LocalAlloc &alloc,List &list, ListNode* node)
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

    // TODO: devise a proper method for spilling regs that isn't just the first one we can find

    // if there is not spill another register (not a tmp or this will break)
    // back into memory
    else 
    {   
        const auto &opcode = node->opcode;
        //const auto info = OPCODE_TABLE[u32(opcode.op)];

        for(reg = 0; reg < MACHINE_REG_SIZE; reg++)
        {
            const auto idx = alloc.regs[reg]; 

            b32 in_expr = false;

            // ^ for now we are just going to check we are not currently using the varaible inside this opcode
            // with a linear scan -> fix this later
            for(u32 i = 1; i < 3; i++)
            {
                if(is_sym(opcode.v[i]) && idx == sym_to_idx(opcode.v[i]))
                {
                    in_expr = true;
                    break;
                }
            }

            // we have a var to spill
            if(reg_is_sym(idx) && !in_expr)
            {
                auto &sym = slot_lookup[idx];
                spill_sym(alloc,list,node,sym);

                // claim the register
                reg = alloc.free_list[--alloc.free_regs];
                break;
            }
        }

        // failed to find a reg to spill should not happen
        if(reg == MACHINE_REG_SIZE)
        {
            disass_opcode_sym(opcode,slot_lookup);
            print_alloc(alloc,slot_lookup);
            panic("failed to allocate register!");
        }
    }

    return reg;    
}



void alloc_tmp(SlotLookup &slot_lookup,LocalAlloc &alloc, List &list, ListNode* node, u32 ir_reg)
{
    const u32 reg = alloc_internal(slot_lookup, alloc, list, node);
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = tmp(ir_reg);

    printf("tmp t%d allocated into r%d\n",ir_reg,reg);
}

void alloc_sym(SlotLookup &slot_lookup,LocalAlloc &alloc, List &list, ListNode* node, Symbol &sym)
{
    const u32 reg = alloc_internal(slot_lookup, alloc, list, node);
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


// alloc based on it being a var or a tmp
void alloc_slot(u32 slot,LocalAlloc &alloc, List &list,ListNode* node,SlotLookup &slot_lookup)
{
    if(is_tmp(slot))
    {
        alloc_tmp(slot_lookup,alloc,list,node,slot);
    }

    else if(is_sym(slot))
    {
        auto &sym = sym_from_slot(slot_lookup,slot);
        alloc_sym(slot_lookup,alloc,list,node,sym);
    }
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


void reload_sym(Symbol &sym,u32 slot,LocalAlloc &alloc,List &list, ListNode *node,SlotLookup &slot_lookup)
{
    alloc_sym(slot_lookup,alloc,list,node,sym);

    printf("reloading sym %s into r%d\n",sym.name.c_str(),sym.location);

    // we need to save the current stack offset here as by the time we load it 
    // it may be different
    const auto opcode = Opcode(op_type::load,sym.location,slot,alloc.stack_offset);
    insert_at(list,node,opcode); 
}


void alloc_tmp_into_tmp(LocalAlloc &alloc, u32 ir_dst, u32 ir_src)
{
    alloc.ir_regs[ir_dst] = alloc.ir_regs[ir_src];
    alloc.regs[alloc.ir_regs[ir_dst]] = tmp(ir_dst);
    

    // as ir_src is unqiue once it has been used in an expr it wont be used again
    // so we dont need to worry about unlinking the ref

    printf("tmp t%d allocated into existing tmp t%d -> r%d\n",ir_dst,ir_src,alloc.ir_regs[ir_dst]);
}


void handle_allocation(SlotLookup &slot_lookup, LocalAlloc& alloc,List &list, ListNode *node)
{
    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    u32 tmp_reg = REG_FREE;

    // make sure our src var's are loaded
    // mark if any are tmp's we can reuse to allocate the dst
    for(u32 a = 0; a < info.args; a++)
    {
        // only interested in registers
        if(info.type[a] != arg_type::src_reg)
        {
            continue;
        }


        if(is_sym(opcode.v[a]))
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[a]);

            // in memory reload into register
            if(sym.location == LOCATION_MEM)
            {
                reload_sym(sym,opcode.v[a],alloc,list,node,slot_lookup);
            }

            // pointer taken to var reload anyways
            else if(sym.referenced)
            {
                reload_sym(sym,opcode.v[a],alloc,list,node,slot_lookup);
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
        if(is_sym(slot))
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
                    alloc_sym(slot_lookup,alloc,list,node,sym);
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
                alloc_tmp_into_tmp(alloc,slot,tmp_reg);
            }

            else
            {
                alloc_tmp(slot_lookup,alloc,list,node,slot);
            }
        }
    }
}


void save_rv(LocalAlloc &alloc,List &list,ListNode* node,SlotLookup &slot_lookup,u32 tmp)
{
    //panic("need to realloc tmp");
    

    // get a new register
    const u32 reg = alloc_internal(slot_lookup, alloc, list, node);
    const auto op = Opcode(op_type::mov_reg,reg,RV,0);

    // emit a mov from the the current tmp to the new one
    insert_at(list,node,op);
    
    const u32 ir_reg = tmp_to_ir(tmp);

    printf("moved tmp t%d from rv to r%d\n",ir_reg,reg);

    // rewrite the ir allocation
    alloc.ir_regs[ir_reg] = reg;
    alloc.regs[reg] = tmp;

    // free RV
    free_tmp(alloc,RV);
}

// NOTE: use this to force rewrites of directives
void rewrite_reg_internal(SlotLookup& slot_lookup,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const u32 slot = opcode.v[reg];

    if(is_sym(slot))
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

void rewrite_reg(SlotLookup& slot_lookup,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    

    if(info.type[reg] == arg_type::src_reg || info.type[reg] == arg_type::dst_reg)
    {
        rewrite_reg_internal(slot_lookup,alloc,opcode,reg);
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
    handle_allocation(itl.symbol_table.slot_lookup,alloc,list,node);

    auto &opcode = node->opcode;

    // rewrite each slot to its allocated register
    rewrite_regs(itl.symbol_table.slot_lookup,alloc,opcode);

    const auto info = OPCODE_TABLE[u32(opcode.op)];

    // branch is happening spill everything
    // TODO: revisit this with a proper reg alloc method
    // we dont want to spill on a branch at all but rather the block types
    // this is just easy...
    if(info.group == op_group::branch_t)
    {
        spill_all(alloc,itl.symbol_table.slot_lookup,list,node,false);        
    }

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


void allocate_and_rewrite(LocalAlloc& alloc,List& list, ListNode* node,u32 reg, SlotLookup& slot_lookup)
{
    alloc_slot(node->opcode.v[reg],alloc,list,node,slot_lookup);
    rewrite_reg_internal(slot_lookup,alloc,node->opcode,reg);         
}

ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,List &list, ListNode *node)
{
    auto &slot_lookup = itl.symbol_table.slot_lookup;
    const auto &opcode = node->opcode;

    switch(node->opcode.op)
    {
        // TODO: revisit when we add caller saved regs
        //case op_type::save_regs:
        //case op_type::restore_regs:

        case op_type::load_arr_data:
        {

            // we dont have the information for this yet so we have to fill it in later
            /*
                load_arr_data <dst>, <sym>
                lea <dst>, [<sym_offset>]
            */

            node->opcode = Opcode(op_type::load_arr_data,opcode.v[0],opcode.v[1],alloc.stack_offset);

            // only want to allocate the dst,
            // we need to use the sym as a "slot" later
            allocate_and_rewrite(alloc,list,node,0,slot_lookup);                
   
            node = node->next;
            break;
        }

        case op_type::arr_index:
        {
            // arr_index <dst>, <array_data_ptr>, <subscript_offset>
            // add <dst>, <array_data_ptr>,<subcript_offset>
            node->opcode = Opcode(op_type::add_reg,opcode.v[0],opcode.v[1],opcode.v[2]);
            rewrite_opcode(itl,alloc,list,node);

            node = node->next;
            break;
        }

        case op_type::load_arr_len:
        {
            // load_arr_len <dst> <symbol> <dimension>
            // TODO: this assumes an array with a known size
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);
            node->opcode = Opcode(op_type::mov_imm,opcode.v[0],sym.type.dimensions[0],0);
            rewrite_opcode(itl,alloc,list,node);

            node = node->next;
            break;
        }

        case op_type::init_arr_idx:
        {
            // init_arr_idx <arr> <slot> <index>
            const auto &arr = sym_from_slot(slot_lookup,opcode.v[0]);
            const u32 index = opcode.v[2];
            const u32 var_slot = opcode.v[1];

            // load_arr_data
            // store <slot> <hard coded offset>

            const u32 arr_slot = new_slot(func);

            node->opcode = Opcode(op_type::load_arr_data,arr_slot,opcode.v[0],alloc.stack_offset);
            allocate_and_rewrite(alloc,list,node,0,slot_lookup);
            

            
            // calc the offset we will use for the store
            auto accessed_type = index_array(arr.type);
            const auto size = type_size(itl,accessed_type);            

            // as the index is an immdediate we can jsut calc this ahead of time
            const u32 offset = index * size;

            const auto opcode = write_ptr(var_slot,arr_slot,size,offset);

            return insert_after(list,node,opcode);
        }                

        case op_type::addrof:
        {
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>

            // mark the var as having a pointer taken to it
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);
            sym.referenced = true;

            // okay apply the stack offset, and let the register allocator deal with it
            // we will get the actual address using it later
            node->opcode = Opcode(op_type::addrof,opcode.v[0],opcode.v[1],alloc.stack_offset);
            
            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(alloc,list,node,0,slot_lookup);


            // spill the var that has had a pointer taken to it
            spill_sym(alloc,list,node,sym);
            
            node = node->next;
            break;
        }

        // have to do opcode rewriting by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            node->opcode =  Opcode(op_type::push,opcode.v[0],0,0);


            // adjust opcode for reg alloc
            rewrite_opcode(itl,alloc,list,node);

            // varaibles now have to be accessed at a different offset
            // until this is corrected by clean call
            alloc.stack_offset += sizeof(u32);

            node = node->next;
            break;
        }

        case op_type::clean_args:
        {
            // clean up args
            const auto stack_clean = sizeof(u32) * opcode.v[0];

            node->opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,stack_clean);
            alloc.stack_offset -= stack_clean; 

            rewrite_regs(itl.symbol_table.slot_lookup,alloc,node->opcode);

            node = node->next;
            break;
        }

        case op_type::alloc_slot:
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[0]);

            // if we have an array and we know the size
            // then we need to allocate memory for it
            if(is_array(sym.type))
            {
                // get size, len
                // emit a alloc ir op
                auto [size,count] = get_arr_size(itl,sym.type);                    

                if(is_runtime_size(count))
                {
                    // TODO: we want this allocation to give us the locaiton of the array struct
                    // not directly the location of the pointer (unless the array is fixed sized)
                    // so in the load_arr_data directive we need to check what type of array we are screwing with
                    // along with load_arr_len so that it can get loaded in the correct way...

                    // we can just dump the allocation in the location slot 
                    // and then when we get the stack alloc dump it into the initial stack location?
                    // but we probably want a way of marking that we are expecting this to happen

                    unimplemented("allocate runtime array struct");

                    if(!runtime_size_unk(count))
                    {
                        // get the initial runtime size
                        count = initial_runtime_size(count);
                    }

                    // we have no intial data we have nothing to do
                    else
                    {   
                        node = node->next;
                        break;
                    }
                }

                const u32 idx = size >> 1;

                node->opcode = Opcode(op_type::alloc,opcode.v[0],size,count);

                const u32 cur = alloc.size_count[idx];
                sym.offset = PENDING_ALLOCATION + cur;

                
                alloc.size_count[idx] += count;
                alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);
                
                node = node->next;      
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
            if(reg_is_sym(alloc.regs[RV]))
            {
                spill_sym(alloc,list,node,slot_lookup[alloc.regs[RV]]);
            }


            else if(reg_is_tmp(alloc.regs[RV]))
            {
                // should we have this get pushed instead?
                // and then popped into another reg?
                save_rv(alloc,list,node,slot_lookup,alloc.regs[RV]);
            }

            return remove(list,node);
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
                    auto [size,count] = get_arr_size(itl,sym.type); 
                    if(is_runtime_size(count))
                    {
                        // had an initial size on the stack this needs to be gone
                        if(!runtime_size_unk(count))
                        {
                            count = initial_runtime_size(count);
                            alloc.size_count[size >> 1] -= count;
                        }
                    }

                    else
                    {
                        alloc.size_count[size >> 1] -= count;
                    }
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

// 2nd pass of rewriting on the IR
ListNode* rewrite_directives(Interloper& itl,LocalAlloc &alloc,List &list, ListNode *node,const Opcode& callee_restore,
    const Opcode& stack_clean, bool insert_callee_saves)
{
    auto& slot_lookup = itl.symbol_table.slot_lookup;
    const auto opcode = node->opcode;

    switch(node->opcode.op)
    {

        case op_type::mov_reg:
        {
            // remove dead stores (still need to perform reg correction)
            // so it has to be done in the 2nd pass
            if(opcode.op == op_type::mov_reg && opcode.v[0] == opcode.v[1])
            {
                return remove(list,node);
            }

            node = node->next;
            break;
        }


        case op_type::load_arr_data:
        {
            /*
                load_arr_data <dst>, <sym>, stack_offset
                lea <dst>, [<sym_offset>]
            */

            // TODO: assumes static array
            const s32 stack_offset = opcode.v[2];
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);

            node->opcode = Opcode(op_type::lea,opcode.v[0],SP,sym.offset + stack_offset);

            node = node->next;
            break;
        }

        case op_type::alloc:
        {
            // alloc <slot>, <size>, <count>
            auto &sym = sym_from_slot(slot_lookup,opcode.v[0]);

            const u32 idx = sym.offset - PENDING_ALLOCATION;  
            sym.offset = alloc.stack_alloc[opcode.v[1] >> 1] + (idx * opcode.v[1]);                    

            return remove(list,node);
        }

        case op_type::ret:
        {
            ListNode *tmp = node;

            if(alloc.stack_size)
            {
                tmp = insert_at(list,tmp,stack_clean);
            }

            if(insert_callee_saves)
            {
                tmp = insert_at(list,tmp,callee_restore);
            }

            node = node->next;
            break;
        }

        // reload a reg
        case op_type::load:
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);


            const s32 stack_offset = opcode.v[2];

            // reload the spilled var 
            if(is_signed_integer(sym.type))
            {
                // word is register size (we dont need to extend it)
                static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};

                // this here does not otherwhise need rewriting so we will emit SP directly
                node->opcode = Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + stack_offset);        
            }

            // "plain data"
            // just move by size
            else
            {
                static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};

                node->opcode =  Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + stack_offset);
            }
            
            node = node->next;
            break;
        }

        case op_type::addrof:
        {
            const s32 stack_offset = opcode.v[2];
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);

                // this is the first stack access so we need to compute the final posistion
            if(sym.offset >= PENDING_ALLOCATION)
            {
                const u32 idx = sym.offset - PENDING_ALLOCATION;  
                sym.offset = alloc.stack_alloc[sym.size >> 1] + (idx * sym.size);
            }

            node->opcode = Opcode(op_type::lea,opcode.v[0],SP,sym.offset + stack_offset);

            node = node->next;
            break;
        }

        case op_type::spill:
        {
            auto &sym = sym_from_slot(slot_lookup,opcode.v[1]);

            const s32 stack_offset = opcode.v[2];

            // this is the first stack access so we need to compute the final posistion
            if(sym.offset >= PENDING_ALLOCATION)
            {
                const u32 idx = sym.offset - PENDING_ALLOCATION;  
                sym.offset = alloc.stack_alloc[sym.size >> 1] + (idx * sym.size);
            }

            // write value back out into mem
            // TODO: are these offsets aligned? 

            // TODO: we need to not bother storing these back if the varaible spilled has not been modified
            // and is just used as a const for a calc (how do we impl this?)

            static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
            node->opcode = Opcode(instr[sym.size >> 1],opcode.v[0],SP,sym.offset + stack_offset);   

            node = node->next;
            break;                  
        }


        default: node = node->next; break;
    }

    return node;
}

void calc_allocation(LocalAlloc& alloc)
{
    // calculate the final stack sizes
    // byte located at start
    alloc.stack_alloc[0] = 0;

    // start at end of byte allocation
    alloc.stack_alloc[1] = alloc.size_count_max[0];

    // align for u16
    if(alloc.stack_alloc[1] & 1)
    {
        alloc.stack_alloc[1] += 1;
    }


    // start at end of half allocation
    alloc.stack_alloc[2] = alloc.stack_alloc[1] + (alloc.size_count_max[1] * sizeof(u16));

    // align for u32
    if(alloc.stack_alloc[2] & 2)
    {
        alloc.stack_alloc[2] += 2;
    }

    // get the total stack size
    alloc.stack_size =  alloc.stack_alloc[2] + (alloc.size_count_max[2] * sizeof(u32));


    printf("byte count: %d\n",alloc.size_count_max[0]);
    printf("half count: %d\n",alloc.size_count_max[1]);
    printf("word count: %d\n",alloc.size_count_max[2]);
    printf("stack size: %d\n",alloc.stack_size);
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


void allocate_registers(Interloper& itl,Function &func)
{
    printf("allocating registers for %s:\n\n",func.name.c_str());

    auto alloc = make_local_alloc();

    for(auto &block : func.emitter.program)
    {
        List& list = block.list;

        ListNode *node = list.start;
        while(node)
        {
            node = allocate_opcode(itl,func,alloc,list,node);
        }

        // block is directly falls to the next one we need to spill regs
        // TODO: fix register allocation so we dont have to spill everyhting across basic blocks
        if(block.last)
        {
            puts("spilling last");
            spill_all(alloc,itl.symbol_table.slot_lookup,list,list.end,true);
        }
    }

    calc_allocation(alloc);

    print_alloc(alloc,itl.symbol_table.slot_lookup);

    // only allocate a stack if we need it
    if(alloc.stack_size)
    {
        insert_front(func.emitter.program[0].list,Opcode(op_type::sub_imm,SP,SP,alloc.stack_size));
    }


    // iterate over the function by here and add callee cleanup at every ret
    // and insert the stack offsets and load and spill directives

    // TODO: this might be good to loop jam with somethign but just have a seperate loop for simplictiy atm


    // R0 is callee saved
    static constexpr u32 CALLEE_SAVED_MASK = 1;

    // make sure callee saved regs are not saved inside the func
    const u32 saved_regs = alloc.used_regs & ~CALLEE_SAVED_MASK;
    const u32 save_count = popcount(saved_regs);

    const bool insert_callee_saves = func.name != "main" && save_count != 0;


    alloc_args(func,alloc,itl.symbol_table.slot_lookup,insert_callee_saves? sizeof(u32) * save_count : 0);

    // entry point does not need to preserve regs
    if(insert_callee_saves)
    {
        insert_front(func.emitter.program[0].list,Opcode(op_type::pushm,saved_regs,0,0));
    }

    // epilogue opcodes
    const auto callee_restore = Opcode(op_type::popm,saved_regs,0,0);
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,alloc.stack_size);

    for(auto &block : func.emitter.program)
    {
        List& list = block.list;

        ListNode *node = list.start;
        while(node)
        {
            node = rewrite_directives(itl,alloc,list,node,callee_restore,stack_clean,insert_callee_saves);
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

    else if(v >= SYMBOL_START && sym_to_idx(v) < table->size())
    {
        return slot_lookup[sym_to_idx(v)].name;
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
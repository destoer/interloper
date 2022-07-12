#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"


ListNode* get_cur_end(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list.end;    
}

List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

// TODO: should this return the dst slot as a matter of convience?
void emit(IrEmitter &emitter,const Opcode& opcode)
{
    auto &list = get_cur_list(emitter);
    append(list,opcode);
}

void emit(IrEmitter &emitter,op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto &list = emitter.program[count(emitter.program)-1].list;
    append(list,opcode);
}

u32 gpr_count(u32 size)
{
    return size / GPR_SIZE;
}

Opcode store_ptr(u32 dst_slot, u32 addr_slot, u32 size, u32 offset)
{
    static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
    return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);
}

// this function only supports up to 32 bit reads atm
static_assert(GPR_SIZE == sizeof(u32));

Opcode load_ptr(u32 dst_slot, u32 addr_slot,u32 offset, u32 size, bool is_signed)
{
    if(is_signed)
    {
        // word is register size (we dont need to extend it)
        static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);       
    }

    // "plain data"
    // just move by size
    else
    {
        static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
        return Opcode(instr[size >> 1],dst_slot,addr_slot,offset);
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
    push_var(emitter.program,make_block(type,list_allocator));
    push_var(emitter.block_slot,slot);    
}


void destroy_emitter(IrEmitter& emitter)
{
    destroy_arr(emitter.program);
    destroy_arr(emitter.block_slot);
}

static constexpr u32 REG_FREE = SPECIAL_PURPOSE_REG_START - 1;
static constexpr u32 REG_TMP_START = 0x00000000;

bool is_sym(u32 s)
{
    return s >= SYMBOL_START;
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
    return r >= SPECIAL_PURPOSE_REG_START && r <= SPECIAL_PURPOSE_REG_START + SPECIAL_REG_SIZE;
}

bool is_tmp(u32 r)
{
    return r < REG_FREE;
}

u32 new_tmp(Function &func)
{       
    return func.emitter.reg_count++;
}

// get back a longer lived tmp
// stored internally as a symbol
u32 new_tmp(Interloper& itl, u32 size)
{
    char name[40];
    sprintf(name,"v%d",itl.symbol_table.var_count);

    Symbol sym = make_sym(itl.symbol_table,name,Type(builtin_type::void_t),size);

    sym.slot = symbol(count(itl.symbol_table.slot_lookup));
    push_var(itl.symbol_table.slot_lookup,sym);      

    itl.symbol_table.var_count++;  

    return sym.slot;
}



void ir_memcpy(Interloper&itl, Function& func, u32 dst_slot, u32 src_slot, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    // emit a call to memcpy with args
    // check function is declared

    Function* func_def = lookup(itl.function_table,"memcpy");

    if(!func_def)
    {
        panic(itl,"[COMPILE]: memcpy is required for struct passing\n");
    }
    Function &func_call = *func_def;

    mark_used(itl,func_call);


    const u32 imm_slot = new_tmp(func);
    emit(func.emitter,op_type::mov_imm,imm_slot,size);

    emit(func.emitter,op_type::push_arg,imm_slot);
    emit(func.emitter,op_type::push_arg,src_slot);
    emit(func.emitter,op_type::push_arg,dst_slot);

    emit(func.emitter,op_type::spill_rv);
    emit(func.emitter,op_type::call,func_call.slot);

    emit(func.emitter,op_type::clean_args,3);
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
    Array<u32> ir_regs;

    u32 free_regs;

    // free list for register allocator
    u32 free_list[MACHINE_REG_SIZE];

    // bitset of which regs this functions needs to use
    // for now we are going to just callee save every register
    u32 used_regs;
    u32 use_count;


    // debug (TODO: make this a command line flag)
    b32 print_reg_allocation = false;
    b32 print_stack_allocation = false;

    // stack allocation

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to varaibles
    u32 stack_offset;

    // where does each section for alloc start?
    u32 stack_alloc[3];

    // how much of each type of var is there at the momemnt?
    u32 size_count[3];

    // what is the maximum ammount of vars?
    // this will be used to compute the stack size later
    u32 size_count_max[3];

    // what is the total ammount of space that this functions stack requires!
    u32 stack_size;
};

LocalAlloc make_local_alloc(b32 print_reg_allocation,b32 print_stack_allocation,u32 tmp_count)
{
    LocalAlloc alloc;

    alloc.print_reg_allocation = print_reg_allocation;
    alloc.print_stack_allocation = print_stack_allocation;

    // every register is free!
    alloc.free_regs = MACHINE_REG_SIZE;
    alloc.use_count = 0;
    alloc.used_regs = 0;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        alloc.regs[i] = REG_FREE;
        alloc.free_list[i] = i;
    }


    resize(alloc.ir_regs,tmp_count);

    memset(alloc.stack_alloc,0,sizeof(alloc.stack_alloc));

    memset(alloc.size_count_max,0,sizeof(alloc.size_count_max));
    memset(alloc.size_count,0,sizeof(alloc.size_count));
    alloc.stack_size = 0;
    alloc.stack_offset = 0;    

    return alloc;
}

u32 stack_reserve(LocalAlloc& alloc, u32 size, u32 count, const char *name);

void rewrite_reg(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg);

void print_alloc(LocalAlloc &alloc,SymbolTable& table)
{
    printf("\n\nallocation:\n\n");

    printf("total registers: %d\n",MACHINE_REG_SIZE);
    printf("free registers: %d\n",alloc.free_regs);
    printf("used regsisters: %d\n",MACHINE_REG_SIZE - alloc.free_regs);
    printf("total used registers: %d\n",alloc.use_count);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        const u32 slot = alloc.regs[i];

        if(is_tmp(slot))
        {
            printf("reg r%d -> temp t%d\n",i,slot);
        }

        else if(is_sym(slot))
        {
            printf("slot %x\n",slot);
            const auto &sym = sym_from_slot(table,slot);
            printf("reg r%d -> sym %s\n",i,sym.name.buf);
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

    if(alloc.print_reg_allocation)
    {
        printf("spill %s:%d\n",sym.name.buf,reg);
    }

    // we have not spilled this value on the stack yet we need to actually allocate its posistion

    if(sym.offset == UNALLOCATED_OFFSET)
    {
        // only allocate the local vars by here
        if(!is_arg(sym))
        {
            const u32 size = sym.size;

            // TODO: handle structs
            assert(size <= sizeof(u32));

            sym.offset = stack_reserve(alloc,size,1,sym.name.buf);
        }

        // args need to be allocated later
        // because we need to know the stack size to know there posistions
    }


    // TODO: how do we know if a variable has not been modified
    // i.e it is a ready only copy and has not been modifed so we dont actually need to write it back
    const auto opcode = Opcode(op_type::spill,reg,sym.slot,alloc.stack_offset);

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

void spill_all(LocalAlloc &alloc, SymbolTable& table, List& list, ListNode* node, bool after)
{
    if(alloc.print_reg_allocation)
    {
        puts("spilling everything"); 
    }
    
    // TODO: revisit this once we come up with a scheme to make sure that vars get put into the proper regs
    
    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        if(is_sym(alloc.regs[r]))
        {
            auto &sym = sym_from_slot(table,alloc.regs[r]);
            spill_sym(alloc,list,node,sym,after);
        }
    }    
}

u32 alloc_internal(SymbolTable& table,LocalAlloc &alloc,List &list, ListNode* node)
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
            const auto slot = alloc.regs[reg]; 

            b32 in_expr = false;

            // ^ for now we are just going to check we are not currently using the varaible inside this opcode
            // with a linear scan -> fix this later
            for(u32 i = 1; i < 3; i++)
            {
                if(is_sym(opcode.v[i]) && slot == opcode.v[i])
                {
                    in_expr = true;
                    break;
                }
            }

            // we have a var to spill
            if(is_sym(slot) && !in_expr)
            {
                auto &sym = sym_from_slot(table,slot);
                spill_sym(alloc,list,node,sym);

                // claim the register
                reg = alloc.free_list[--alloc.free_regs];
                break;
            }
        }

        // failed to find a reg to spill should not happen
        if(reg == MACHINE_REG_SIZE)
        {
            disass_opcode_sym(opcode,table);
            print_alloc(alloc,table);
            panic("failed to allocate register!");
        }
    }

    return reg;    
}



void alloc_tmp(SymbolTable& table,LocalAlloc &alloc, List &list, ListNode* node, u32 tmp)
{
    const u32 reg = alloc_internal(table, alloc, list, node);
    alloc.ir_regs[tmp] = reg;
    alloc.regs[reg] = tmp;

    if(alloc.print_reg_allocation)
    {
        printf("tmp t%d allocated into r%d\n",tmp,reg);
    }
}

void alloc_sym(SymbolTable& table,LocalAlloc &alloc, List &list, ListNode* node, Symbol &sym)
{
    const u32 reg = alloc_internal(table, alloc, list, node);
    alloc.regs[reg] = sym.slot; 
    sym.location = reg;

    if(alloc.print_reg_allocation)
    {
        printf("symbol %s into reg r%d\n",sym.name.buf,reg);
    }
}

// mark a tmp as allocated to a var
void alloc_sym_into_tmp(Symbol &sym,LocalAlloc &alloc, u32 tmp)
{   
    const u32 reg = alloc.ir_regs[tmp];

    if(alloc.print_reg_allocation)
    {
        printf("symbol %s allocated into tmp t%d -> r%d\n",sym.name.buf,tmp,reg);
    }

    alloc.regs[reg] = sym.slot;
    sym.location = reg;
}


// alloc based on it being a var or a tmp
void alloc_slot(u32 slot,LocalAlloc &alloc, List &list,ListNode* node,SymbolTable& table)
{
    if(is_tmp(slot))
    {
        alloc_tmp(table,alloc,list,node,slot);
    }

    else if(is_sym(slot))
    {
        auto &sym = sym_from_slot(table,slot);
        alloc_sym(table,alloc,list,node,sym);
    }
}


void free_tmp(LocalAlloc &alloc, u32 reg)
{
    if(alloc.print_reg_allocation)
    {
        printf("freed tmp t%d from r%d\n",alloc.regs[reg],reg);
    }

    alloc.regs[reg] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = reg;
}

void free_sym(LocalAlloc &alloc, Symbol &sym)
{
    if(alloc.print_reg_allocation)
    {
        printf("freed sym %s from r%d\n",sym.name.buf,sym.location);
    }

    alloc.regs[sym.location] = REG_FREE;
    alloc.free_list[alloc.free_regs++] = sym.location;
    sym.location = LOCATION_MEM;
}

void free_slot(u32 slot,LocalAlloc &alloc,SymbolTable& table)
{
    if(is_tmp(slot))
    {
        free_tmp(alloc,alloc.ir_regs[slot]);
    }

    else if(is_sym(slot))
    {
        auto &sym = sym_from_slot(table,slot);
        free_sym(alloc,sym);
    }
}


void reload_sym(Symbol &sym,u32 slot,LocalAlloc &alloc,List &list, ListNode *node,SymbolTable& table)
{
    alloc_sym(table,alloc,list,node,sym);

    if(alloc.print_reg_allocation)
    {
        printf("reloading sym %s into r%d\n",sym.name.buf,sym.location);
    }

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
    if(alloc.print_reg_allocation)
    {
        printf("tmp t%d allocated into existing tmp t%d -> r%d\n",ir_dst,ir_src,alloc.ir_regs[ir_dst]);
    }
}


void handle_allocation(SymbolTable& table, LocalAlloc& alloc,List &list, ListNode *node)
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
            auto &sym = sym_from_slot(table,opcode.v[a]);

            // in memory reload into register
            if(sym.location == LOCATION_MEM)
            {
                reload_sym(sym,opcode.v[a],alloc,list,node,table);
            }

            // pointer taken to var reload anyways
            else if(sym.referenced)
            {
                reload_sym(sym,opcode.v[a],alloc,list,node,table);
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
            auto &sym = sym_from_slot(table,opcode.v[0]);

            if(sym.location == LOCATION_MEM)
            {
                if(tmp_reg != REG_FREE)
                {
                    alloc_sym_into_tmp(sym,alloc,tmp_reg);
                }

                else
                {
                    alloc_sym(table,alloc,list,node,sym);
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
                alloc_tmp(table,alloc,list,node,slot);
            }
        }
    }
}


void save_rv(LocalAlloc &alloc,List &list,ListNode* node,SymbolTable& table,u32 tmp)
{
    //panic("need to realloc tmp");
    

    // get a new register
    const u32 reg = alloc_internal(table, alloc, list, node);
    const auto op = Opcode(op_type::mov_reg,reg,RV,0);

    // emit a mov from the the current tmp to the new one
    insert_at(list,node,op);
    
    if(alloc.print_reg_allocation)
    {
        printf("moved tmp t%d from rv to r%d\n",tmp,reg);
    }

    // rewrite the ir allocation
    alloc.ir_regs[tmp] = reg;
    alloc.regs[reg] = tmp;

    // free RV
    free_tmp(alloc,RV);
}

// NOTE: use this to force rewrites of directives
void rewrite_reg_internal(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const u32 slot = opcode.v[reg];

    if(is_sym(slot))
    {
        const auto& sym = sym_from_slot(table,slot);

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
            case R0_IR: opcode.v[reg] = R0; break;
            case R1_IR: opcode.v[reg] = R1; break;

            default: panic("unhandled special reg %x\n",slot); break;
        }
    }

    else
    {
        opcode.v[reg] = alloc.ir_regs[slot];
    }          
}

void rewrite_reg(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    

    if(info.type[reg] == arg_type::src_reg || info.type[reg] == arg_type::dst_reg)
    {
        rewrite_reg_internal(table,alloc,opcode,reg);
    }
}

void rewrite_regs(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode)
{   
    const auto info = OPCODE_TABLE[u32(opcode.op)];

    for(u32 r = 0; r < info.args; r++)
    {
        rewrite_reg(table,alloc,opcode,r);
    }
}

void rewrite_opcode(Interloper &itl,LocalAlloc& alloc,List &list, ListNode *node)
{
    // allocate the registers
    handle_allocation(itl.symbol_table,alloc,list,node);

    auto &opcode = node->opcode;

    // rewrite each slot to its allocated register
    rewrite_regs(itl.symbol_table,alloc,opcode);

    const auto info = OPCODE_TABLE[u32(opcode.op)];

    // branch is happening spill everything
    // TODO: revisit this with a proper reg alloc method
    // we dont want to spill on a branch at all but rather the block types
    // this is just easy...
    if(info.group == op_group::branch_t)
    {
        spill_all(alloc,itl.symbol_table,list,node,false);        
    }

    // make sure not to free a tmp that has been repurposed
    const u32 dst = info.type[0] == arg_type::dst_reg? opcode.v[0] : REG_FREE;

    // free any temp's that are "source" registers as we are done with them
    for(u32 a = 0; a < info.args; a++)
    {
        const u32 reg = opcode.v[a];

        // NOTE: the registers are allocated now so we wont be getting back the slots
        if(is_reg(reg) && info.type[a] == arg_type::src_reg && is_tmp(alloc.regs[reg]) && reg != dst)
        {
            free_tmp(alloc,reg);
        }
    }
}


void allocate_and_rewrite(LocalAlloc& alloc,List& list, ListNode* node,u32 reg, SymbolTable& table)
{
    alloc_slot(node->opcode.v[reg],alloc,list,node,table);
    rewrite_reg_internal(table,alloc,node->opcode,reg);         
}

// NOTE: this just reserves stack space,
// calc allocation must be called (done before 2nd directive pass)
// then finish_alloc to give the actual offset
u32 stack_reserve(LocalAlloc& alloc, u32 size, u32 count, const char* name)
{
    const u32 idx = size >> 1;

    const u32 cur = alloc.size_count[idx];

    if(alloc.print_stack_allocation)
    {
        printf("intial offset allocated %s: (%x,%x) -> %x\n",name,size,count,cur);
    }

    alloc.size_count[idx] += count;
    alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);

    return cur + PENDING_ALLOCATION;    
}


u32 alloc_const_pool(Interloper& itl, const void* data,u32 count,u32 size)
{
    const u32 pos = itl.const_pool.size;

    const u32 bytes = count * size;
    push_mem(itl.const_pool,data,bytes);
    return pos;
}


ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,List &list, ListNode *node)
{
    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    UNUSED(func);

    switch(node->opcode.op)
    {
        case op_type::addrof:
        {

            if(is_tmp(opcode.v[1]))
            {
                unimplemented("addr taken on tmp");
            }

            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>

            // mark the var as having a pointer taken to it
            auto &sym = sym_from_slot(table,opcode.v[1]);
            sym.referenced = true;

            // okay apply the stack offset, and let the register allocator deal with it
            // we will get the actual address using it later
            node->opcode = Opcode(op_type::addrof,opcode.v[0],opcode.v[1],alloc.stack_offset);
            
            // just rewrite the 1st reg we dont want the address of the 2nd
            allocate_and_rewrite(alloc,list,node,0,table);


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
            alloc.stack_offset += GPR_SIZE;

            node = node->next;
            break;
        }

        case op_type::clean_args:
        {
            // clean up args
            const auto stack_clean = GPR_SIZE * opcode.v[0];

            node->opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,stack_clean);
            alloc.stack_offset -= stack_clean; 

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.print_stack_allocation)
            {
                printf("clean args: %x\n",stack_clean);
            }

            node = node->next;
            break;
        }

        case op_type::alloc_stack:
        {
            const u32 size = opcode.v[0];
            node->opcode = Opcode(op_type::sub_imm,SP_IR,SP_IR,size);
            alloc.stack_offset += size;

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.print_stack_allocation)
            {
                printf("allocate stack %x\n",size);
            }


            node = node->next;
            break;
        }

        case op_type::free_stack:
        {
            const u32 size = opcode.v[0];
            node->opcode = Opcode(op_type::add_imm,SP_IR,SP_IR,size);
            alloc.stack_offset -= size;

            rewrite_regs(itl.symbol_table,alloc,node->opcode);

            if(alloc.print_stack_allocation)
            {
                printf("free stack %x\n",size);
            }

            node = node->next;
            break;
        }

        case op_type::alloc_slot:
        {
            auto &sym = sym_from_slot(table,opcode.v[0]);

            if(alloc.print_reg_allocation)
            {
                printf("alloc slot: %s\n",sym.name.buf);
            }

            // if we have anything that wont just fit inside a reg
            const u32 size = opcode.v[1];
            const u32 count = opcode.v[2];

            if(size)
            {
                node->opcode = Opcode(op_type::alloc,opcode.v[0],size,count);
                sym.offset = stack_reserve(alloc,size,count,sym.name.buf); 

                node = node->next;     
            }

            else
            {
                return remove(list,node);
            }
            break;
        }

        // TODO: we should probably only save regs we use but this is just easy for now
        case op_type::save_regs:
        {
            node->opcode = Opcode(op_type::pushm,0xffffffff,0,0);
            alloc.stack_offset += GPR_SIZE * MACHINE_REG_SIZE;

            node = node->next;
            break;
        }

        case op_type::restore_regs:
        {
            node->opcode = Opcode(op_type::popm,0xffffffff,0,0);
            alloc.stack_offset -= GPR_SIZE * MACHINE_REG_SIZE;

            node = node->next;
            break;
        }

        // make sure the return value has nothing important when calling functions
        case op_type::spill_rv:
        {
            if(is_sym(alloc.regs[RV]))
            {
                auto &sym = sym_from_slot(table,alloc.regs[RV]);
                spill_sym(alloc,list,node,sym);
            }


            else if(is_tmp(alloc.regs[RV]))
            {
                // should we have this get pushed instead?
                // and then popped into another reg?
                save_rv(alloc,list,node,table,alloc.regs[RV]);
            }

            return remove(list,node);
        }


        // TODO: this needs to have its size emitted directly inside the opcode
        case op_type::free_slot:
        {
            auto &sym = sym_from_slot(table,opcode.v[0]);

            // specified size
            if(opcode.v[2])
            {
                if(alloc.print_stack_allocation)
                {
                    printf("reclaiming stack space %s : (%d , %d)\n",sym.name.buf,opcode.v[1],opcode.v[2]);
                }

                alloc.size_count[opcode.v[1] >> 1] -= opcode.v[2];
            }

            // this var is done so we can reclaim the stack space
            // TODO: this wont work if these go out of order due to tmp's
            // for now we simply dont free them
            else if(sym.offset >= PENDING_ALLOCATION)
            {
                if(alloc.print_stack_allocation)
                {
                    printf("reclaiming stack space %s : %d\n",sym.name.buf,sym.size);
                }                

                alloc.size_count[sym.size >> 1] -= 1;
            }

            if(sym.location != LOCATION_MEM)
            {
                // this var is gone so we can free it
                free_sym(alloc,sym);
            }

            return remove(list,node);
        }


        case op_type::load_arr_data:
        {
            // we dont have the information for this yet so we have to fill it in later
            // load_arr_data <dst>, <sym>, <stack_offset>

            node->opcode = Opcode(op_type::load_arr_data,opcode.v[0],opcode.v[1],alloc.stack_offset);

            // only want to allocate the dst,
            // we need to use the sym as a "slot" later
            allocate_and_rewrite(alloc,list,node,0,table);                
   
            node = node->next;
            break;
        }

        // TODO: this probably needs to be reworked for multi dimensional arrays
        case op_type::load_arr_len:
        {
            // load_arr_len <dst> <symbol>, <stack_offset>
            node->opcode = Opcode(op_type::load_arr_len,opcode.v[0],opcode.v[1],alloc.stack_offset);
            allocate_and_rewrite(alloc,list,node,0,table);                
   
            node = node->next;
            break;
        }

        // pools
        case op_type::pool_addr:
        {
            // pool_addr <dst>, <offset>, <pool>
            allocate_and_rewrite(alloc,list,node,0,table);   

            node = node->next;
            break;
        }

        case op_type::free_reg:
        {   
            free_slot(opcode.v[0],alloc,table);
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

u32 finish_alloc(LocalAlloc& alloc,u32 offset,u32 size, const char* name)
{
    if(alloc.print_stack_allocation)
    {
        printf("final offset %s = %x -> (%x,%x)\n",name,size,offset,offset - PENDING_ALLOCATION);
    }

    assert(offset >= PENDING_ALLOCATION && offset != UNALLOCATED_OFFSET);

    const u32 idx = offset - PENDING_ALLOCATION;  
    return alloc.stack_alloc[size >> 1] + (idx * size);     
}

// 2nd pass of rewriting on the IR
ListNode* rewrite_directives(Interloper& itl,LocalAlloc &alloc,List &list, ListNode *node,const Opcode& callee_restore,
    const Opcode& stack_clean, bool insert_callee_saves)
{
    auto& table = itl.symbol_table;
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

        case op_type::state_dump:
        {
            panic("unused state opcode");
        }


        case op_type::alloc:
        {
            // alloc <slot>, <size>, <count>
            auto &sym = sym_from_slot(table,opcode.v[0]);

            if(sym.offset >= PENDING_ALLOCATION)
            {
                sym.offset = finish_alloc(alloc,sym.offset,opcode.v[1],sym.name.buf);
            }

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
            auto &sym = sym_from_slot(table,opcode.v[1]);


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
            auto &sym = sym_from_slot(table,opcode.v[1]);

                // this is the first stack access so we need to compute the final posistion
            if(sym.offset >= PENDING_ALLOCATION)
            {
                sym.offset = finish_alloc(alloc,sym.offset,sym.size,sym.name.buf);
            }

            node->opcode = Opcode(op_type::lea,opcode.v[0],SP,sym.offset + stack_offset);

            node = node->next;
            break;
        }

        case op_type::spill:
        {
            auto &sym = sym_from_slot(table,opcode.v[1]);

            const s32 stack_offset = opcode.v[2];

            // this is the first stack access so we need to compute the final posistion
            if(sym.offset >= PENDING_ALLOCATION)
            {
                sym.offset = finish_alloc(alloc,sym.offset,sym.size,sym.name.buf);
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

        // arrays
        case op_type::load_arr_data:
        {

            const s32 stack_offset = opcode.v[2];
            auto &sym = sym_from_slot(table,opcode.v[1]);

            if(is_runtime_size(sym.type,0))
            {
                node->opcode = load_ptr(opcode.v[0],SP,sym.offset + stack_offset + 0,GPR_SIZE,false);
            }

            // static array
            else
            {
                node->opcode = Opcode(op_type::lea,opcode.v[0],SP,sym.offset + stack_offset);
            }

            node = node->next;
            break;
        }


        case op_type::load_arr_len:
        {
            auto &sym = sym_from_slot(table,opcode.v[1]);
            const s32 stack_offset = opcode.v[2];            

            if(is_runtime_size(sym.type,0))
            {
                // TODO: this assumes GPR_SIZE is 4
                node->opcode = load_ptr(opcode.v[0],SP,sym.offset + stack_offset + GPR_SIZE,GPR_SIZE,false);
            }

            else
            {
                node->opcode = Opcode(op_type::mov_imm,opcode.v[0],sym.type.dimensions[0],0);
            }


            node = node->next;
            break;
        }

        default: node = node->next; break;
    }

    return node;
}

void align(u32 *alloc, u32 alignment)
{
    // make sure the last start posistion is even
    alignment /= 2;

    if(alloc[alignment] & alignment)
    {
        alloc[alignment] += alignment;
    }
}

void calc_allocation(LocalAlloc& alloc)
{
    // calculate the final stack sizes
    // byte located at start
    alloc.stack_alloc[0] = 0;

    // start u16 at end of byte allocation and align them
    alloc.stack_alloc[1] = alloc.size_count_max[0];
    align(alloc.stack_alloc,sizeof(u16));


    //  u32 at end of half allocation and align them
    alloc.stack_alloc[2] = alloc.stack_alloc[1] + (alloc.size_count_max[1] * sizeof(u16));
    align(alloc.stack_alloc,sizeof(u32));


    // get the total stack size
    alloc.stack_size = alloc.stack_alloc[2] + (alloc.size_count_max[2] * sizeof(u32));

    if(alloc.print_stack_allocation)
    {
        printf("byte count: %d\n",alloc.size_count_max[0]);
        printf("half count: %d\n",alloc.size_count_max[1]);
        printf("word count: %d\n",alloc.size_count_max[2]);
        printf("stack size: %d\n",alloc.stack_size);
    }
}
    
// TODO: need to rethink this when we do register passing
// and when we push off determining stack size to a later pass
void alloc_args(Function &func, LocalAlloc& alloc, SymbolTable& table, u32 saved_regs_offset)
{
    for(u32 a = 0; a < count(func.args); a++)
    {
        const u32 slot = func.args[a];

        auto &sym = sym_from_slot(table,slot);

        //printf("%s : %x\n",sym.name.buf,sym.arg_offset);

        // alloc above the stack frame
        sym.offset = sym.arg_offset + alloc.stack_size + saved_regs_offset + sizeof(u32);
    }
             
}


void allocate_registers(Interloper& itl,Function &func)
{
    auto alloc = make_local_alloc(itl.print_reg_allocation,itl.print_stack_allocation,func.emitter.reg_count);

    // figure out how long each sym lives
    // mark_lifetimes(itl,func);

    if(alloc.print_reg_allocation)
    {
        printf("allocating registers for %s:\n\n",func.name.buf);
    }

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        
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
            if(alloc.print_reg_allocation)
            {
                puts("spilling last");
            }
            spill_all(alloc,itl.symbol_table,list,list.end,true);
        }
    }

    calc_allocation(alloc);

    if(alloc.print_reg_allocation)
    {
        print_alloc(alloc,itl.symbol_table);
    }

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


    alloc_args(func,alloc,itl.symbol_table,insert_callee_saves? sizeof(u32) * save_count : 0);

    // entry point does not need to preserve regs
    if(insert_callee_saves)
    {
        insert_front(func.emitter.program[0].list,Opcode(op_type::pushm,saved_regs,0,0));
    }

    // epilogue opcodes
    const auto callee_restore = Opcode(op_type::popm,saved_regs,0,0);
    const auto stack_clean = Opcode(op_type::add_imm,SP,SP,alloc.stack_size);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        List& list = block.list;

        ListNode *node = list.start;
        while(node)
        {
            node = rewrite_directives(itl,alloc,list,node,callee_restore,stack_clean,insert_callee_saves);
        }
    }


    destroy_arr(alloc.ir_regs);
}

// NOTE: pass in a size, so we only print the code section
void dump_program(const Array<u8> &program,u32 size, std::map<u32,u32> &inv_label_lookup, LabelLookup &label_lookup)
{
    for(u32 pc = 0; pc < size; pc += sizeof(Opcode))
    {
        if(inv_label_lookup.count(pc))
        {
            printf("0x%08x %s:\n",pc,label_lookup[inv_label_lookup[pc]].name.buf);
        }

        printf("  0x%08x:\t ",pc);   

        const auto opcode = read_var<Opcode>(program,pc);
        disass_opcode_raw(opcode);
    }
}

void insert_program(Interloper& itl, const Opcode& opcode)
{
    push_var(itl.program,opcode);
}

void emit_asm(Interloper &itl)
{
    std::map<u32,u32> inv_label_lookup;


    // TODO: we want to make this be a start function defined inside the stl
    // so we can swap it easily with something that does more finicky things later

    // emit a dummy call to main
    // that will get filled in later once we know where main lives
    insert_program(itl,Opcode(op_type::call,lookup(itl.function_table,"main")->slot,0,0));

    // program exit
    insert_program(itl,Opcode(op_type::swi,SYSCALL_EXIT,0,0));

    // dump every function into one vector and record where it is in the function table
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            Function& func = bucket[i].v;

            // when we assembly to an actual arch we will 
            // have to switch over to a byte array
            itl.symbol_table.label_lookup[func.slot].offset = itl.program.size;


            inv_label_lookup[itl.program.size] = func.slot;

            for(u32 b = 0; b < count(func.emitter.program); b++)
            {
                const auto &block = func.emitter.program[b];

                // resolve label addr.
                itl.symbol_table.label_lookup[func.emitter.block_slot[b]].offset = itl.program.size;

                // prefer function name
                if(b != 0)
                {
                    inv_label_lookup[itl.program.size] = func.emitter.block_slot[b];
                }

                auto node = block.list.start;
                while(node)
                {
                    insert_program(itl,node->opcode);
                    node = node->next;
                }
            }   
        }
    }


    const u32 const_pool_loc = itl.program.size;
    push_mem(itl.program,itl.const_pool.data,itl.const_pool.size);
    destroy_arr(itl.const_pool);


    // label dump
/*
    puts("\n\nlabels");
    for(const auto &label : itl.symbol_table.label_lookup)
    {
        printf("label %s = %x\n",label.name.buf,label.offset);
    }
    putchar('\n');
*/
    
    // "link" the program and resolve all the labels we now have the absolute
    // posistions for
    // TODO: how do we want to labels for a mov i.e
    // x = @some_function;

    for(u32 i = 0; i < const_pool_loc; i += sizeof(Opcode))
    {
        auto opcode = read_var<Opcode>(itl.program,i);

        // handle all the branch labels
        // TODO: this probably needs to be changed for when we have call <reg>
        if(OPCODE_TABLE[u32(opcode.op)].group == op_group::branch_t)
        {
            opcode.v[0] = itl.symbol_table.label_lookup[opcode.v[0]].offset;
            write_var(itl.program,i,opcode);
        }

        // resolve pools
        else if(opcode.op == op_type::pool_addr)
        {
            const u32 pool = opcode.v[2];
            const u32 offset = opcode.v[1];

            switch(pool)
            {
                case CONST_POOL:
                {
                    // TODO: this needs to deal with the pro
                    opcode = Opcode(op_type::mov_imm,opcode.v[0],PROGRAM_ORG + const_pool_loc + offset,0);
                    break;
                }

                default: panic("unknown pool %d\n",pool);
            }

            write_var(itl.program,i,opcode);
        }
    }

    if(itl.print_ir)
    {
        dump_program(itl.program,const_pool_loc,inv_label_lookup,itl.symbol_table.label_lookup);
    }
}


void fmt_sym_specifier(Array<char> &buffer, const SymbolTable& table, char specifier, u32 slot)
{
    switch(specifier)
    {
        case 'r':
        {
            
            if(is_special_reg(slot))
            {
                const u32 idx = slot - SPECIAL_PURPOSE_REG_START;
                push_mem(buffer,SPECIAL_REG_NAMES[idx].buf,SPECIAL_REG_NAMES[idx].size);
            }

            // print a sym
            else if(is_sym(slot))
            {
                const auto& sym = sym_from_slot(table,slot);
                const String& name = sym.name;

                push_mem(buffer,name.buf,name.size);
            }


            // print a tmp
            else
            {
                char name[40];
                const u32 len = sprintf(name,"t%d",slot);

                push_mem(buffer,name,len);
            }


            break;
        }


        // hex constant
        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%x",slot);

            push_mem(buffer,name,len);
            break;
        }

        // address
        case 'a':
        {
            const String& name = table.label_lookup[slot].name;
            push_mem(buffer,name.buf,name.size);
            break;
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void fmt_raw_specifier(Array<char> &buffer, char specifier, u32 slot)
{
    switch(specifier)
    {
        // raw register
        case 'r':
        {
            if(slot == SP)
            {
                push_mem(buffer,SPECIAL_REG_NAMES[SP_NAME_IDX].buf,SPECIAL_REG_NAMES[SP_NAME_IDX].size);
            }

            else
            {
                char name[40];
                const u32 len = sprintf(name,"r%d",slot);

                push_mem(buffer,name,len);
            }


            break;
        }

        // labeles act as address here
        case 'a':
        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%x",slot);

            push_mem(buffer,name,len);
            break;
        }

        // regm
        case 'm':
        {
            char name[128];

            push_var(buffer,'{');

            u32 count = 0;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(slot,r))
                {
                    const u32 len = sprintf(name,"%sr%d",count != 0? "," : "",r);
                    push_mem(buffer,name,len);
                    count++;
                }
            }

            push_var(buffer,'}');
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void disass_opcode_internal(const Opcode& opcode, const SymbolTable* table)
{
    const auto& info = OPCODE_TABLE[u32(opcode.op)];
    const auto& fmt_string = info.fmt_string;

    Array<char> buffer;

    u32 args = 0;

    for(u32 i = 0; i < fmt_string.size; )
    {
        if(fmt_string[i] == '%')
        {
            if(args == 3)
            {
                panic("execeed opcode arg printing");
            }

            const char specifier = fmt_string[i + 1];

            if(table)
            {
                fmt_sym_specifier(buffer,*table,specifier,opcode.v[args++]);
            }

            else
            {
                fmt_raw_specifier(buffer,specifier,opcode.v[args++]);
            }

            i += 2;
        }

        else
        {
            push_var(buffer,fmt_string[i++]);
        }
    }

    // null term the buffer
    push_var(buffer,'\0');

    puts(buffer.data);


    destroy_arr(buffer);
}

// TODO: use table of fmt strings to print this
// just figure out symbol printing first and then generalise it
void disass_opcode_sym(const Opcode &opcode, const SymbolTable& table)
{
    disass_opcode_internal(opcode,&table);
}

void disass_opcode_raw(const Opcode &opcode)
{
    disass_opcode_internal(opcode,nullptr);
}


void dump_ir(Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);

    u32 l = 0;
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        if(func.emitter.block_slot[b] != 0xffffffff)
        {
            printf("%s:\n",table.label_lookup[func.emitter.block_slot[b]].name.buf);
        }


        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_sym(node->opcode,table);
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
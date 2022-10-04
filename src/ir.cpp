#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"
#include "emitter.cpp"
#include "link.cpp"


Block make_block(block_type type,u32 slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.type = type;
    block.list = make_list(list_allocator);
    block.slot = slot;

    return block;
}

void new_block(ArenaAllocator* list_allocator,Function& func,block_type type, u32 slot)
{
    push_var(func.emitter.program,make_block(type,slot,list_allocator)); 
}


void destroy_emitter(IrEmitter& emitter)
{
    destroy_arr(emitter.program);
}

void ir_memcpy(Interloper&itl, Function& func, u32 dst_slot, u32 src_slot, u32 size)
{
    // TODO: if we reuse internal calling multiple times in the IR we need to make something that will do this for us
    // because this alot of boilerplate

    // emit a call to memcpy with args
    // check function is declared

    Function* func_def = lookup(itl.function_table,String("memcpy"));

    if(!func_def)
    {
        panic(itl,"[COMPILE]: memcpy is required for struct passing\n");
    }
    Function &func_call = *func_def;

    mark_used(itl,func_call);


    const u32 imm_slot = emit_res(func,op_type::mov_imm,size);

    emit(func,op_type::push_arg,imm_slot);
    emit(func,op_type::push_arg,src_slot);
    emit(func,op_type::push_arg,dst_slot);

    emit(func,op_type::spill_rv);
    emit(func,op_type::call,func_call.slot);

    emit(func,op_type::clean_args,3);
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

LocalAlloc make_local_alloc(b32 print_reg_allocation,b32 print_stack_allocation)
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

// NOTE: this just reserves stack space,
// calc allocation must be called (done before 2nd directive pass)
// then finish_alloc to give the actual offset
u32 stack_reserve(LocalAlloc& alloc, u32 size, u32 count, const char* name)
{
    const u32 idx = size >> 1;

    const u32 cur = alloc.size_count[idx];

    if(alloc.print_stack_allocation)
    {
        printf("initial offset allocated %s: (%x,%x) -> %x\n",name,size,count,cur);
    }

    alloc.size_count[idx] += count;
    alloc.size_count_max[idx] = std::max(alloc.size_count_max[idx],alloc.size_count[idx]);

    return cur + PENDING_ALLOCATION;    
}


void handle_allocation(SymbolTable& table, LocalAlloc& alloc,List &list, ListNode *node)
{
    UNUSED(table); UNUSED(list); UNUSED(alloc);

    const auto opcode = node->opcode;
    const auto info = OPCODE_TABLE[u32(opcode.op)];


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
            assert(false);
        }
        
        
        else if(is_tmp(opcode.v[a]))
        {
            assert(false);
        }
    }


    const u32 slot = opcode.v[0];

    // handle allocation of dst
    if(info.type[0] == arg_type::dst_reg)
    {
        if(is_sym(slot))
        {
            assert(false);
        }

        else if(is_tmp(slot))
        {
            assert(false);
        }
    }
}



// NOTE: use this to force rewrites of directives
void rewrite_reg_internal(SymbolTable& table,LocalAlloc& alloc,Opcode &opcode, u32 reg)
{
    UNUSED(alloc); UNUSED(table);

    const u32 slot = opcode.v[reg];

    if(is_sym(slot))
    {
        assert(false);
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

            default: crash_and_burn("unhandled special reg %x\n",slot); break;
        }
    }

    else
    {
        assert(false);
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

    // rewrite each slot to its allocated register
    rewrite_regs(itl.symbol_table,alloc,node->opcode);
}



ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,List &list, ListNode *node)
{
    UNUSED(func); UNUSED(itl); UNUSED(alloc); UNUSED(list); UNUSED(node);

    auto &table = itl.symbol_table;
    const auto &opcode = node->opcode;

    UNUSED(table); UNUSED(opcode);

    switch(node->opcode.op)
    {
        case op_type::addrof:
        {
            assert(false);
        }

        // have to do opcode rewriting by here to make sure hte offset is applied after any reloads occur
        case op_type::push_arg:
        {
            assert(false);
        }

        case op_type::clean_args:
        {
            assert(false);
        }

        case op_type::alloc_stack:
        {
            assert(false);
        }

        case op_type::free_stack:
        {
            assert(false);
        }

        case op_type::alloc_slot:
        {
            assert(false);
        }

        case op_type::restore_regs:
        {
            assert(false);
        }

        // make sure the return value has nothing important when calling functions
        case op_type::spill_rv:
        {
            assert(false);
        }


        case op_type::spill_all:
        {
            assert(false);
        }

        // TODO: this needs to have its size emitted directly inside the opcode
        case op_type::free_slot:
        {
            assert(false);
        }


        case op_type::load_arr_data:
        {
            assert(false);
        }

        // TODO: this probably needs to be reworked for multi dimensional arrays
        case op_type::load_arr_len:
        {
            assert(false);
        }

        // pools
        case op_type::pool_addr:
        {
            assert(false);
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
    UNUSED(itl); UNUSED(alloc); UNUSED(list); UNUSED(node); UNUSED(callee_restore);
    UNUSED(stack_clean); UNUSED(insert_callee_saves);
    assert(false);
 
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
        sym.reg.offset = sym.arg_offset + alloc.stack_size + saved_regs_offset + sizeof(u32);
    }
             
}


void allocate_registers(Interloper& itl,Function &func)
{
    auto alloc = make_local_alloc(itl.print_reg_allocation,itl.print_stack_allocation);

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
                push_mem(buffer,SPECIAL_REG_NAMES[idx]);
            }

            // print a sym
            else if(is_sym(slot))
            {
                const auto& sym = sym_from_slot(table,slot);
                const String& name = sym.name;

                push_mem(buffer,name);
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
            push_mem(buffer,name);
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
                push_mem(buffer,SPECIAL_REG_NAMES[SP_NAME_IDX]);
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
                crash_and_burn("execeed opcode arg printing");
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
    
        if(block.slot != 0xffffffff)
        {
            printf("%s:\n",table.label_lookup[block.slot].name.buf);
        }


        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_sym(node->opcode,table);
            node = node->next;
        }

        l++;
    }

    printf("\n");       
}
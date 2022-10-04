#include <interloper.h>
#include <op_table.inl>

#include "list.cpp"



List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).end;    
}


u32 new_tmp(Function &func)
{       
    return func.emitter.reg_count++;
}

void emit(Function& func,const Opcode& opcode)
{
    auto &list = get_cur_list(func.emitter);
    append(list,opcode);
}

void emit_block(Function& func, u32 block, op_type op, u32 v1, u32 v2, u32 v3)
{
    Opcode opcode(op,v1,v2,v3);

    auto &list = func.emitter.program[block].list;
    append(list,opcode);    
}


u32 cur_block(Function& func)
{
    return count(func.emitter.program) - 1;
}


void emit(Function& func,op_type op, u32 v1, u32 v2, u32 v3)
{
    emit_block(func,cur_block(func),op,v1,v2,v3);
}

// emit an opcode, and give back a new dst as a tmp
u32 emit_res(Function& func, op_type op, u32 v2, u32 v3)
{
    const u32 tmp = new_tmp(func);
    emit(func,op,tmp,v2,v3);

    return tmp;
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

// get back a longer lived tmp
// stored internally as a symbol
u32 new_tmp(Interloper& itl, u32 size)
{
    UNUSED(itl); UNUSED(size);
    assert(false);
}

u32 slot_to_idx(u32 slot)
{
    return is_sym(slot)? sym_to_idx(slot) : slot;
}

Reg make_reg(reg_kind kind,u32 size, u32 slot)
{
    Reg reg;
    reg.kind = kind;
    reg.size = size;
    reg.slot = slot;

    return reg;
}

void print(const Reg& reg)
{
    printf("offset: %x\n",reg.offset);
    printf("location: %x\n\n",reg.location);    
    printf("slot: %x\n",reg.slot);
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


u32 push_const_pool(Interloper& itl, pool_type type, const void* data,u32 size)
{
    PoolSection section;

    section.type = type;
    section.offset = itl.const_pool.size;
    section.size = size;

    // add section information + data to the pool
    push_var(itl.pool_sections,section);
    push_mem(itl.const_pool,data,size);

    return section.offset;
}

u32 reserve_const_pool(Interloper& itl, pool_type type, u32 size)
{
    PoolSection section;

    section.type = type;
    section.offset = itl.const_pool.size;
    section.size = size;

    // add section information + reserve inside the pool
    push_var(itl.pool_sections,section);
    resize(itl.const_pool,count(itl.const_pool) + size);

    return section.offset;    
}



ListNode *allocate_opcode(Interloper& itl,Function &func,LocalAlloc &alloc,List &list, ListNode *node)
{
    UNUSED(func); UNUSED(itl); UNUSED(alloc); UNUSED(list); UNUSED(node);

    assert(false);
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
void dump_program(const Array<u8> &program,u32 size, HashTable<u32,u32> &inv_label_lookup, LabelLookup &label_lookup)
{
    for(u32 pc = 0; pc < size; pc += sizeof(Opcode))
    {
        if(contains(inv_label_lookup,pc))
        {
            printf("0x%08x %s:\n",pc,label_lookup[*lookup(inv_label_lookup,pc)].name.buf);
        }

        printf("  0x%08x:\t ",pc);   

        const Opcode opcode = read_mem<Opcode>(program,pc);
        disass_opcode_raw(opcode);
    }
}

void insert_program(Interloper& itl, const Opcode& opcode)
{
    push_var(itl.program,opcode);
}

void emit_asm(Interloper &itl)
{
    HashTable<u32,u32> inv_label_lookup = make_table<u32,u32>();




    // emit a dummy call to start
    // that will get filled in later once we know where it is
    insert_program(itl,Opcode(op_type::call,lookup(itl.function_table,String("start"))->slot,0,0));


    // resolve all our labels, dump all our machine code into a buffer
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            Function& func = bucket[i].v;


            itl.symbol_table.label_lookup[func.slot].offset = itl.program.size;

            add(inv_label_lookup,itl.program.size,func.slot);

            for(u32 b = 0; b < count(func.emitter.program); b++)
            {
                const auto &block = func.emitter.program[b];

                // resolve label addr.
                itl.symbol_table.label_lookup[block.slot].offset = itl.program.size;

                // if this is the first block prefer function name
                if(b != 0)
                {
                    add(inv_label_lookup,itl.program.size,block.slot);
                }

                // dump every opcode into the final program
                auto node = block.list.start;
                while(node)
                {
                    insert_program(itl,node->opcode);
                    node = node->next;
                }
            }   
        }
    }

    // add any data required into the const pool before adding it to the program
    for(u32 i = 0; i < count(itl.pool_sections); i++)
    {
        const PoolSection& pool = itl.pool_sections[i];

        switch(pool.type)
        {
            case pool_type::label:
            {
                static_assert(GPR_SIZE == sizeof(u32));

                const u32 offset = pool.offset;
                const u32 size = pool.size;

                // rewrite final label posistions
                for(u32 addr = offset; addr < size + offset; addr += GPR_SIZE)
                {
                    const u32 label = read_mem<u32>(itl.const_pool,addr);

                    //printf("table: %d -> %x\n",label,itl.symbol_table.label_lookup[label].offset);

                    write_mem<u32>(itl.const_pool, addr, itl.symbol_table.label_lookup[label].offset);
                }                
            }

            case pool_type::string_literal: break;
        }
    }


    // add the constant pool, into the final program
    const u32 const_pool_loc = itl.program.size;
    push_mem(itl.program,itl.const_pool);

    // clean up the mem from the constt pool
    destroy_arr(itl.const_pool);
    destroy_arr(itl.pool_sections);


    // TODO: how do we want to labels for a mov i.e
    // x = @some_function;
    
    // "link" the program and resolve the labels
    for(u32 i = 0; i < const_pool_loc; i += sizeof(Opcode))
    {
        auto opcode = read_mem<Opcode>(itl.program,i);

        // handle all the branch labels
        // TODO: this probably needs to be changed for when we have call <reg>
        if(OPCODE_TABLE[u32(opcode.op)].group == op_group::branch_t)
        {
            opcode.v[0] = itl.symbol_table.label_lookup[opcode.v[0]].offset;
            write_mem(itl.program,i,opcode);
        }

        // resolve pools
        else if(opcode.op == op_type::pool_addr)
        {
            const pool_type pool = pool_type(opcode.v[2]);
            const u32 offset = opcode.v[1];

            switch(pool)
            {
                case pool_type::string_literal:
                {
                    opcode = Opcode(op_type::mov_imm,opcode.v[0],PROGRAM_ORG + const_pool_loc + offset,0);
                    break;
                }

                // Resolve all the addresses within 
                case pool_type::label:
                {
                    unimplemented("label pool");
                    break;
                }

                default: crash_and_burn("unknown pool %d\n",pool);
            }

            write_mem(itl.program,i,opcode);
        }
    }

    if(itl.print_ir)
    {
        dump_program(itl.program,const_pool_loc,inv_label_lookup,itl.symbol_table.label_lookup);
    }

    destroy_table(inv_label_lookup);
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
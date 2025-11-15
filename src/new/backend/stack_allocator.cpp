// TODO: we need to handle 16 byte alignment
// if we need to call external functions or use sse

struct StackAlloc
{
    Array<ArrayAllocation> array_allocation;

    // how much has our stack been screwed up by function calls etc
    // so how much do we need to offset accesses to variables
    u32 stack_offset = 0;

    // where does each section for alloc start?
    u32 stack_alloc[4] = {0};

    // how much of each type of var is there at the moment?
    u32 size_count[4] = {0};

    // what is the total amount of space that this functions stack requires!
    u32 stack_size = 0;

    Array<RegSlot> pending_allocation;

    b32 print = false;
    b32 debug = false;
};

StackAlloc make_stack_alloc(b32 print, b32 debug)
{
    StackAlloc alloc = {};

    alloc.print = print;
    alloc.debug = debug;

    return alloc;
}

void destroy_stack_alloc(StackAlloc& alloc)
{
    destroy_arr(alloc.pending_allocation);
    destroy_arr(alloc.array_allocation);
}

// NOTE: this just reserves stack space,
// finish_stack_alloc has to be called first before usage
u32 stack_reserve_internal(StackAlloc& alloc, u32 size, u32 count)
{
    const u32 idx = log2(size);
    const u32 cur = alloc.size_count[idx];

    alloc.size_count[idx] += count;

    return cur;    
}

u32 allocate_stack_array(StackAlloc& alloc,SymbolTable& table, SymSlot slot, u32 size, u32 alloc_count)
{
    ArrayAllocation allocation;
    allocation.slot = slot;
    allocation.size = size;
    allocation.count = alloc_count;
    allocation.stack_offset = alloc.stack_offset;
    allocation.offset = stack_reserve_internal(alloc,size,alloc_count);

    const u32 idx = count(alloc.array_allocation);

    if(alloc.print)
    {
        auto& sym = sym_from_slot(table,slot);
        printf("initial array stack offset: %s [%x,%x] -> %x\n",sym.name.buf,size,alloc_count,allocation.offset);
    }

    push_var(alloc.array_allocation,allocation);

    return idx;
}

void stack_reserve_reg(StackAlloc& alloc, Reg& ir_reg)
{
    // if we attempt to reserve space for a global we have trouble
    assert(is_local(ir_reg));

    ir_reg.offset = stack_reserve_internal(alloc,ir_reg.size,ir_reg.count);

    ir_reg.flags |= STACK_ALLOCATED;
    ir_reg.flags |= PENDING_STACK_ALLOCATION;

    log(alloc.print,"initial stack offset for register at %x allocated\n",ir_reg.slot,ir_reg.offset);

    // mark this so we can finalise these later
    push_var(alloc.pending_allocation,ir_reg.slot);
}


// Alignment functions for for stack, struct, globals etc

u32 align_val(u32 v,u32 alignment)
{
    const u32 unaligned = v & (alignment - 1);

    if(unaligned)
    {
        v += (alignment - unaligned); 
    }

    return v;
}

void align(u32 *alloc, u32 alignment)
{
    // make sure the last start position is even
    const u32 idx = log2(alignment);

    alloc[idx] = align_val(alloc[idx],alignment);
}

// NOTE: both arrays are size at number of base type sizes
// i.e 4 (u8,u16,u32,u64)

// byte start defaults to zero, but for structs extra data may be shoved
// at the start of the byte alloc

// start is the offset for each section
// count is how many "slots" each section has
// byte start is the offset for the initial byte section

u32 calc_alloc_sections(u32* start,u32* count, u32 byte_start = 0)
{
    // byte starts at zero
    start[0] = byte_start;

    // u16
    start[1] = start[0] + (count[0] * sizeof(u8));
    align(start,sizeof(u16));
    
    // u32
    start[2] = start[1] + (count[1] * sizeof(u16));
    align(start,sizeof(u32));

    // u64
    start[3] = start[2] + (count[2] * sizeof(u32));
    align(start,sizeof(u64));

    // get total allocation size
    const u32 size = start[3] + (count[3] * sizeof(u64));
/*
    for(u32 i = 0; i < 4; i++)
    {
        printf("alloc: %d, %d\n",start[i],count[i]);
    }
*/
    return size;
}

u32 calc_final_offset(const u32* start, u32 size, u32 idx)
{
    return start[log2(size)] + (idx * size);
}

u32 finalise_offset(StackAlloc& alloc,u32 offset, u32 size)
{
    // what pos in the block does this reg have?
    const u32 idx = offset;  
    
    return calc_final_offset(alloc.stack_alloc,size,idx);
}

void finalise_offset(StackAlloc& alloc, Reg& ir_reg)
{
    ir_reg.offset = finalise_offset(alloc,ir_reg.offset,ir_reg.size); 
    ir_reg.flags &= ~PENDING_STACK_ALLOCATION;
}

void calc_allocation(StackAlloc& alloc)
{
    alloc.stack_size = calc_alloc_sections(alloc.stack_alloc,alloc.size_count);

    align_val(alloc.stack_size,GPR_SIZE * 2);

    if(alloc.print)
    {
        printf("byte count: %d\n",alloc.size_count[0]);
        printf("half count: %d\n",alloc.size_count[1]);
        printf("word count: %d\n",alloc.size_count[2]);
        printf("dword count: %d\n",alloc.size_count[3]);
        printf("stack size: %d\n",alloc.stack_size);
    }
}

// TODO: need to rethink this when we do register passing
// and when we push off determining stack size to a later pass
void alloc_args(Function &func, StackAlloc& alloc, SymbolTable& table, u32 saved_regs_offset)
{
    const u32 FRAME_OFFSET = alloc.debug? GPR_SIZE * 2 : GPR_SIZE;

    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        if(func.sig.pass_as_reg[a] != NON_ARG)
        {
            continue;
        }

        const SymSlot slot = func.sig.args[a];

        auto &sym = sym_from_slot(table,slot);

        // alloc above the stack frame
        sym.reg.offset = sym.arg_offset + alloc.stack_size + saved_regs_offset + FRAME_OFFSET;

        log_reg(alloc.print,table,"Arg offset %r(0x%x) -> 0x%x\n",slot,sym.arg_offset,sym.reg.offset);
    }           
}



// global alloc


u32 global_alloc_internal(GlobalAlloc& alloc, u32 size, u32 count)
{
    const u32 idx = log2(size);
    const u32 offset = alloc.count[idx];
    alloc.count[idx] += count; 

    return offset;
}

void reserve_global_alloc(Interloper& itl, Symbol& sym)
{
    sym.reg.offset = global_alloc_internal(itl.global_alloc,sym.reg.size,sym.reg.count);
}

u32 allocate_global_array(GlobalAlloc& alloc,SymbolTable& table ,SymSlot slot, u32 size, u32 alloc_count)
{
    ArrayAllocation allocation;
    allocation.slot = slot;
    allocation.size = size;
    allocation.count = alloc_count;
    allocation.offset = global_alloc_internal(alloc,size,alloc_count);

    const u32 idx = count(alloc.array_allocation);

    if(alloc.print_global)
    {
        auto& sym = sym_from_slot(table,slot);
        printf("initial array global offset: %s [%x,%x] -> %x\n",sym.name.buf,size,alloc_count,allocation.offset);
    }

    push_var(alloc.array_allocation,allocation);

    return idx;
}

void finalise_global_offset(Interloper& itl)
{
    // okay now we know how many vars are in each section
    // align them and compute the final initial offsets

    auto& alloc = itl.global_alloc;

    // first calc the sections
    alloc.size = calc_alloc_sections(alloc.start,alloc.count);

    log(alloc.print_global,"Global size %x : start (%x, %x, %x, %x) : count (%x, %x, %x, %x)\n",
        alloc.size,alloc.start[0],alloc.start[1],alloc.start[2],alloc.start[3],alloc.count[0],
        alloc.count[1],alloc.count[2],alloc.count[3]);

    // now we need to give each symbol is final offset from the start of the global table
    
    // by definition globals (if any) will be stored inside the "top" symbol table
    for(u32 g = 0; g < count(itl.symbol_table.global); g++)
    {
        const auto slot = itl.symbol_table.global[g];
        auto& sym = sym_from_slot(itl.symbol_table,slot);

        const auto [size,count] = std::pair{sym.reg.size,sym.reg.count};

        sym.reg.offset = calc_final_offset(itl.global_alloc.start,size,sym.reg.offset);
        log(alloc.print_global,"Final offset for %s : %x (%x,%x)\n",sym.name.buf,sym.reg.offset,size,count);
    }

    // finalised offsets on any fixed size array allocations handled by global_array_alloc
}
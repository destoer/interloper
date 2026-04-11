
Block make_block(LabelSlot label_slot,BlockSlot block_slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.list = make_list<Opcode>(list_allocator);
    block.label_slot = label_slot;
    block.block_slot = block_slot;

    block.use = make_set<RegSlot>();
    block.def = make_set<RegSlot>();
    block.live_in = make_set<RegSlot>();
    block.live_out = make_set<RegSlot>();

    return block;
}

BlockSlot new_block(ArenaAllocator* list_allocator,Function& func,LabelSlot label_slot)
{
    const u32 handle = count(func.emitter.program);
    const BlockSlot block_slot = block_from_idx(handle);

    push_var(func.emitter.program,make_block(label_slot,block_slot,list_allocator));


    return block_slot; 
}

String label_name(SymbolTable& table,u32 slot)
{
    char name[40];
    const u32 len = sprintf(name,"L%d",slot);

    return make_string(*table.string_allocator,name,len);
}

BlockSlot new_basic_block(Interloper &itl,Function &func)
{
    const u32 label_count = count(itl.symbol_table.label_lookup);


    const LabelSlot label_slot = add_label(itl.symbol_table,label_name(itl.symbol_table,label_count));
    const BlockSlot block_slot = new_block(&itl.list_allocator,func,label_slot);

    // offset is the block slot until full resolution
    itl.symbol_table.label_lookup[label_slot.handle].offset = block_slot.handle;

    return block_slot;   
}



BlockSlot block_from_idx(u32 v)
{
    BlockSlot slot;

    slot.handle = v;

    return slot;
}

BlockSlot cur_block(Function& func)
{
    return block_from_idx(count(func.emitter.program) - 1);
}

Block& block_from_slot(Function& func, BlockSlot slot)
{
    return func.emitter.program[slot.handle];
}

BlockSlot block_from_label(Interloper& itl, LabelSlot slot)
{
    const auto label = label_from_slot(itl.symbol_table.label_lookup,slot);

    return block_from_idx(label.offset);
}


b32 is_func_exit(BlockSlot slot)
{
    return slot.handle == BLOCK_FUNC_EXIT_HANDLE;
}

void add_block_exit(Function& func,BlockSlot slot, BlockSlot exit)
{
    auto& block = block_from_slot(func,slot);

    // once we have exited the func everything else is unreachable
    if(block.flags & HAS_FUNC_EXIT)
    {
        return;
    }

    push_var(block.exit,exit);

    // add entry to our target block
    auto& exit_block = block_from_slot(func,exit);
    push_var(exit_block.entry,slot);
}

void remove_block_exit(Function& func, BlockSlot slot, BlockSlot exit)
{
    auto& block = block_from_slot(func,slot);

    // remove exit
    remove_unordered_key(block.exit,exit);


    // remove entry as well
    auto& exit_block = block_from_slot(func,exit);
    remove_unordered_key(exit_block.entry,slot);
}

void add_func_exit(Function& func, BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);

    // if this block exits then we cant actually reach any other blocks
    for(auto& exit : block.exit)
    {
        remove_block_exit(func,slot,exit);
    }

    block.flags = block.flags | HAS_FUNC_EXIT | REACH_FUNC_EXIT;
}

b32 has_func_exit(Function& func, BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);
    return block.flags & HAS_FUNC_EXIT;
}

b32 can_reach_exit(Function& func, BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);
    return block.flags & REACH_FUNC_EXIT;
}

b32 can_reach_exit(Block& block)
{
    return block.flags & REACH_FUNC_EXIT;
}


b32 in_loop(Block& block)
{
    return block.flags & IN_LOOP;
}

void add_cond_exit(Function& func,BlockSlot slot, BlockSlot target, BlockSlot fall)
{
    add_block_exit(func,slot,target);
    add_block_exit(func,slot,fall);
}


BlockSlot add_fall(Interloper& itl,Function& func)
{
    const auto cur = cur_block(func);

    const BlockSlot exit = new_basic_block(itl,func);
    add_block_exit(func,cur,exit);

    return exit;
}

void check_block_branch(Interloper& itl,Function& func, BlockSlot& block_slot)
{
    UNUSED(itl);
    auto& block = block_from_slot(func,block_slot);
    block.branch_count += 1;

    if(block.branch_count > 1)
    {
        crash_and_burn("Basic block has too many branches: at L%d\n",block.label_slot.handle);
    }
}

void emit_cond_branch(Interloper& itl,Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, RegSlot reg_slot, branch_cond_type type)
{
    const auto& target_block = block_from_slot(func,target);

    const Opcode opcode = make_branch_cond(reg_slot,target_block.label_slot,type);
    emit_block_internal(itl,func,block,opcode);

    // build links into the cfg 
    add_cond_exit(func,block,target,fall);

    check_block_branch(itl,func,block);
}

void emit_branch(Interloper& itl, Function& func, BlockSlot block,BlockSlot target)
{
    const auto& target_block = block_from_slot(func,target);

    const Opcode opcode = make_branch_label(branch_type::branch,target_block.label_slot);
    emit_block_internal(itl,func,block,opcode);
    add_block_exit(func,block,target);

    check_block_branch(itl,func,block);
}
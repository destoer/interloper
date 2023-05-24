Block make_block(LabelSlot label_slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.list = make_list(list_allocator);
    block.label_slot = label_slot;

    return block;
}

BlockSlot new_block(ArenaAllocator* list_allocator,Function& func,LabelSlot label_slot)
{
    push_var(func.emitter.program,make_block(label_slot,list_allocator));

    auto block_slot = cur_block(func);

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


void add_block_exit(Function& func,BlockSlot slot, BlockSlot exit)
{
    auto& block = block_from_slot(func,slot);

    push_var(block.exit,exit);
}

void add_cond_exit(Function& func,BlockSlot slot, BlockSlot target, BlockSlot fall)
{
    auto& block = block_from_slot(func,slot);

    push_var(block.exit,target);
    push_var(block.exit,fall);
}


BlockSlot add_fall(Interloper& itl,Function& func, BlockSlot prev)
{
    const BlockSlot exit = new_basic_block(itl,func);
    add_block_exit(func,prev,exit);

    return exit;
}

void emit_call(Function& func, LabelSlot label, b32 save_regs)
{
    if(save_regs)
    {
        // Handle saving any caller saved regs
        emit(func,op_type::spill_rv);
    }
    
    emit_internal(func,op_type::call, label.handle,0,0);
}

void emit_cond_branch(Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, SymSlot sym, b32 cond)
{
    const op_type branch_type = cond? op_type::bc : op_type::bnc;

    const auto& target_block = block_from_slot(func,target);

    emit_block_internal(func,block,branch_type,target_block.label_slot.handle,sym.handle,0);

    // build links into the cfg 
    add_cond_exit(func,block,target,fall);
}

void emit_branch(Function& func, BlockSlot block,BlockSlot target)
{
    const auto& target_block = block_from_slot(func,target);

    emit_block_internal(func,block,op_type::b,target_block.label_slot.handle,0,0);
    add_block_exit(func,block,target);
}
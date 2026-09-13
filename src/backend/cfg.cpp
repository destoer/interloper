
Block make_block(LabelSlot label_slot,BlockSlot block_slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.list = make_list<Opcode>(list_allocator);
    block.label_slot = label_slot;
    block.block_slot = block_slot;

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

void add_branch_exit(Function& func, BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);
    block.flags |= BRANCH_EXIT;
}

void add_cond_exit(Function& func,BlockSlot slot, BlockSlot target, BlockSlot fall)
{
    add_branch_exit(func,slot);
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

void branch_nez(Interloper& itl,Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, RegSlot reg_slot)
{
    emit_cond_branch(itl,func,block,target,fall,reg_slot,branch_cond_type::nez);
}

void branch_eqz(Interloper& itl,Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, RegSlot reg_slot)
{
    emit_cond_branch(itl,func,block,target,fall,reg_slot,branch_cond_type::eqz);
}

void emit_branch(Interloper& itl, Function& func, BlockSlot block,BlockSlot target)
{
    const auto& target_block = block_from_slot(func,target);

    const Opcode opcode = make_branch_label(branch_type::branch,target_block.label_slot);
    emit_block_internal(itl,func,block,opcode);
    add_block_exit(func,block,target);
    add_branch_exit(func,block);

    check_block_branch(itl,func,block);
}


struct BlockWorkList
{
    Array<BlockSlot> to_visit;
    BitSet seen;
};

void destroy_block_worklist(BlockWorkList& work_list)
{
    destroy_arr(work_list.to_visit);
    destroy_bit_set(work_list.seen);
}


bool push_worklist(BlockWorkList& work_list, BlockSlot slot)
{
    if(!set_bit_set(work_list.seen,slot.handle))
    {
        return false;
    }

    push_var(work_list.to_visit,slot);
    return true;
}


void reset_block_worklist(BlockWorkList& work_list, BlockSlot slot)
{
    clear_arr(work_list.to_visit);
    clear_bit_set(work_list.seen);

    push_worklist(work_list,slot);
}


BlockWorkList make_block_worklist(const Function& func)
{
    BlockWorkList work_list;

    work_list.seen = make_bit_set(count(func.emitter.program));
    return work_list;
}

BlockWorkList make_block_worklist(const Function& func, BlockSlot entry)
{
    BlockWorkList work_list = make_block_worklist(func);
    push_worklist(work_list,entry);

    return work_list;
}


void append_worklist(BlockWorkList& work_list, const Array<BlockSlot>& list)
{
    for(const BlockSlot slot : list)
    {
        push_worklist(work_list,slot);
    }
}

void connect_node(Function& func,BlockWorkList& work_list, BlockSlot slot)
{
    // Reset the worklist with our current block entry
    // We do this to avoid making extra allocations.
    reset_block_worklist(work_list,slot);
    auto& block = block_from_slot(func,slot);
    
    // while we still have unseen nodes
    while(work_list.to_visit)
    {
        // get next scan
        const BlockSlot cur = pop(work_list.to_visit);
        const auto& scan_block = block_from_slot(func,cur);

        // iter over edges add any unseen
        for(const auto& edge_slot : scan_block.exit)
        {
            // can reach self this means we have a loop!
            if(slot == edge_slot)
            {
                block.flags |= IN_LOOP;
            }

            if(has_func_exit(func,edge_slot))
            {
                block.flags |= REACH_FUNC_EXIT;
            }

            if(push_worklist(work_list,edge_slot))
            {
                // add as new link
                push_var(block.links,edge_slot);
            }
        }
    }
}


void print_ir_set(Interloper& itl, Function& func, const LocalRegSet& reg_set, const char* tag)
{
    printf("%s: {",tag);

    for(const auto slot : reg_set)
    {
        const auto &reg = reg_from_slot(itl.symbol_table,func.local,slot);
        print_reg_name_internal(reg,itl.symbol_table);
        printf(",");
    }

    printf("}\n");
}

void print_block_connection(Function& func, const Array<BlockSlot> con, const char* tag)
{
    printf("%s: {",tag);

    for(const BlockSlot block_slot : con)
    {
        auto& block = block_from_slot(func, block_slot);
        printf("L%d,",block.label_slot.handle);
    }

    printf("}\n");
}

void dump_cfg(Interloper& itl, Function& func)
{
    // empty function we are done!!
    if(!count(func.emitter.program))
    {
        return;
    }

    printf("\ncfg for function %s:\n",func.name.buf);

    BlockSlot start = block_from_idx(0);
    BlockWorkList work_list = make_block_worklist(func,start);
    
    while(work_list.to_visit)
    {
        const BlockSlot cur = pop(work_list.to_visit);
        const auto& block = block_from_slot(func,cur); 

        // print cur
        printf("\nL%d:\n",block.label_slot.handle);
        printf("flags: %x\n",block.flags);

        print_block_connection(func,block.entry,"entry: ");

        print_ir_set(itl,func,block.use,"use: ");
        print_ir_set(itl,func,block.def,"def: ");
        print_ir_set(itl,func,block.live_in,"live in: ");
        print_ir_set(itl,func,block.live_out,"live out: ");

        print_block_connection(func,block.exit,"exit: ");

        // add any we haven't seen for a print
        append_worklist(work_list,block.exit);     
    }

    destroy_block_worklist(work_list);
}


// after we have finished emitting the IR we need to mark which nodes can be reached
// from any one node
void connect_flow_graph(Function& func)
{
    BlockWorkList work_list = make_block_worklist(func);

    // TODO: we can do better than redoing the entire graph for each node
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        const BlockSlot slot = block_from_idx(b);
        connect_node(func,work_list,slot);
    }

    destroy_block_worklist(work_list);
}

void handle_src_regs(Function& func, Block& block, const ConstSpan<RegSlot>& src_span)
{
    for(const auto& src : src_span)
    {
        // Only interested in local registers
        if(src.kind != reg_kind::local)
        {
            continue;
        }

        const auto local = src.local;
        auto& ir_reg = reg_from_local(func,local);

        // ir reg, that is not stored in memory
        if(!stored_in_mem(ir_reg) && !contains(block.def,local))
        {
            // used as src, without a def -> use
            add(block.use,local); 
        }
    }
}


void handle_dst_regs(Function& func, Block& block, const ConstSpan<RegSlot>& dst_span)
{
    for(const auto& dst : dst_span)
    {
        // Only interested in local registers
        if(dst.kind != reg_kind::local)
        {
            continue;
        }

        const auto local = dst.local;
        assert(local.handle < count(func.local.registers));
        auto& ir_reg = reg_from_local(func,local);

        // used as dst before use, def 
        if(!stored_in_mem(ir_reg) && !contains(block.use,local))
        {
            add(block.def,local);
        }
    }
}


// TODO: would it be cheaper to do this inside the emitter?
void compute_use_def(Interloper& itl,Function& func)
{
    // each block
    for(auto& block : func.emitter.program)
    {
        block.live_in = make_local_reg_set(func.local);
        block.live_out = make_local_reg_set(func.local);
        block.def = make_local_reg_set(func.local);
        block.use = make_local_reg_set(func.local);

        // ignore empty blocks
        if(!block.list.start)
        {
            continue;
        }
        
        // run a pass on the block
        for(const OpcodeNode& node : block.list)
        {
            const auto regs = opcode_ir_reg_span(node.value,itl.reg_span);

            handle_src_regs(func,block,regs.src);
            handle_src_regs(func,block,regs.dst_src);

            handle_dst_regs(func,block,regs.dst);
        }

        // if block has a use of a var it must be an input
        // computed here for speed rather than in liveness func
        bit_set_union(block.live_in.bit_set,block.use.bit_set);        
    }
}

BlockSlot find_last_reachable_block(Function& func)
{
    // Find last node that is reachable from the first node
    const BlockSlot entry_slot = block_from_idx(0);
    auto& entry_block = block_from_slot(func,entry_slot);

    BlockSlot last_reachable_block = entry_slot;

    for(const BlockSlot link : entry_block.links)
    {
        if(link.handle > last_reachable_block.handle)
        {
            last_reachable_block = block_from_idx(link.handle);
        }
    }

    return last_reachable_block;
}

void compute_var_live(Interloper& itl, Function& func)
{
    // empty function we are done!!
    if(!count(func.emitter.program))
    {
        return;
    }

    // first compute a use def chain for each block
    compute_use_def(itl,func);

    const auto last_reachable_block = find_last_reachable_block(func);

    // run a liveness pass
    BlockWorkList work_list = make_block_worklist(func);

    // backprop until we get no changes to account for loops!
    b32 modified = true;

    while(modified)
    {
        modified = false;
        reset_block_worklist(work_list,last_reachable_block);

        // run pass on cur block
        while(work_list.to_visit)
        {
            const BlockSlot cur = pop(work_list.to_visit);
            auto& block = block_from_slot(func,cur);

            // used as a use -> input
            // NOTE: computed above in use def for speed 
            // as it will not change from propagation
            // set_union(block.input,block.use);

            // input of exit -> output
            for(const auto& block_slot : block.exit)
            {
                const auto& exit = block_from_slot(func,block_slot);
                modified |= bit_set_union(block.live_out.bit_set,exit.live_in.bit_set);
            }

            // finally if there is no def for an output 
            // then it must be an input (the value must arise somewhere)  
            modified |= bit_set_difference(block.live_in.bit_set,block.live_out.bit_set,block.def.bit_set);

            append_worklist(work_list,block.entry);
            append_worklist(work_list,block.exit);
        }
    }

    destroy_block_worklist(work_list);


    if(itl.print_ir)
    {
        dump_cfg(itl,func);
    }
}

void destroy_block_use_def(Block& block)
{
    destroy_local_reg_set(block.def);
    destroy_local_reg_set(block.use);
}

void destroy_block_liveness(Block& block)
{
    destroy_local_reg_set(block.live_in);
    destroy_local_reg_set(block.live_out);
}

void destroy_liveness_info(Function& func)
{
    for(auto& block : func.emitter.program)
    {
        destroy_block_use_def(block);
        destroy_block_liveness(block);
    }
}
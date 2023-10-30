

Block make_block(LabelSlot label_slot,BlockSlot block_slot,ArenaAllocator* list_allocator)
{
    Block block;

    block.list = make_list(list_allocator);
    block.label_slot = label_slot;
    block.block_slot = block_slot;

    block.live_in = make_set<SymSlot>();
    block.live_out = make_set<SymSlot>();

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

void add_func_exit(Function& func, BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);
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
    auto& block = block_from_slot(func,slot);

    push_var(block.exit,target);
    push_var(block.exit,fall);
}


BlockSlot add_fall(Interloper& itl,Function& func)
{
    const auto cur = cur_block(func);

    const BlockSlot exit = new_basic_block(itl,func);
    add_block_exit(func,cur,exit);

    return exit;
}

void emit_cond_branch(Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, SymSlot sym, b32 cond)
{
    const op_type branch_type = cond? op_type::bc : op_type::bnc;

    const auto& target_block = block_from_slot(func,target);

    // TODO: handle src storage

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

void print_edges(Interloper& itl, Function& func, Array<BlockSlot> links, const char* prefix)
{
    printf("%s:",prefix);

    // print each link for blocks
    for(u32 l = 0; l < count(links); l++)
    {
        const BlockSlot link_slot = links[l];
        const auto& link_block = block_from_slot(func,link_slot);

        const auto& link_label = label_from_slot(itl.symbol_table.label_lookup,link_block.label_slot);
        printf("(%d : %s), ",link_slot.handle,link_label.name.buf);
    }

    putchar('\n');
}

void dump_cfg(Interloper& itl,Function& func)
{
    printf("cfg for: %s\n",func.name.buf);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        const auto& label = label_from_slot(itl.symbol_table.label_lookup,block.label_slot);


        printf("Block %d(%s):\n",b,label.name.buf);

        printf("flags: %x\n",block.flags);
        print_edges(itl,func,block.exit,"edges");
        print_edges(itl,func,block.links,"links");

        putchar('\n');
    }

    putchar('\n');    
}

void connect_node(Function& func,BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);

    // Which nodes have we allready looked at?
    HashTable<u32, u32> seen = make_table<u32,u32>();

    // setup intial scan
    Array<BlockSlot> scan;
    // note this is not added as seen
    // so we can add it later if its a loop
    push_var(scan,slot);

    // while we still have unseen nodes
    while(count(scan))
    {
        // get next scan
        const BlockSlot cur = pop(scan);
        const auto& scan_block = block_from_slot(func,cur);

        // iter over edges add any unseen
        for(u32 e = 0; e < count(scan_block.exit); e++)
        {
            const BlockSlot edge_slot = scan_block.exit[e];

            // can reach self this means we have a loop!
            if(slot.handle == edge_slot.handle)
            {
                block.flags |= IN_LOOP;
            }

            if(has_func_exit(func,edge_slot))
            {
                block.flags |= REACH_FUNC_EXIT;
            }

            if(!contains(seen,edge_slot.handle))
            {
                // add as a scan target
                add(seen,edge_slot.handle,u32(0));
                push_var(scan,edge_slot);
                

                // add as new link
                push_var(block.links,edge_slot);
            }
        }
    }

    
    destroy_table(seen);
    destroy_arr(scan);
}

// after we have finished emitting the IR we need to mark which nodes can be reached
// from any one node
void connect_flow_graph(Interloper& itl,Function& func)
{
    UNUSED(itl);

    // TODO: we can do better than redoing the entire graph for each node
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        const BlockSlot slot = block_from_idx(b);
        connect_node(func,slot);
    }
}
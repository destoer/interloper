

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
    for(u32 e = 0; e < count(block.exit); e++)
    {
        remove_block_exit(func,slot,block.exit[e]);
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

void emit_cond_branch(Interloper& itl,Function& func, BlockSlot block,BlockSlot target,BlockSlot fall, RegSlot reg_slot, b32 cond)
{
    const op_type branch_type = cond? op_type::bc : op_type::bnc;

    const auto& target_block = block_from_slot(func,target);

    // TODO: handle src storage

    const Opcode opcode = make_cond_branch_instr(branch_type,target_block.label_slot,reg_slot);
    emit_block_internal(func,block,opcode);

    // build links into the cfg 
    add_cond_exit(func,block,target,fall);

    check_block_branch(itl,func,block);
}

void emit_branch(Interloper& itl, Function& func, BlockSlot block,BlockSlot target)
{
    const auto& target_block = block_from_slot(func,target);

    const Opcode opcode = make_branch_instr(op_type::b,target_block.label_slot);
    emit_block_internal(func,block,opcode);
    add_block_exit(func,block,target);

    check_block_branch(itl,func,block);
}




void connect_node(Function& func,BlockSlot slot)
{
    auto& block = block_from_slot(func,slot);

    // Which nodes have we allready looked at?
    Set<BlockSlot> seen = make_set<BlockSlot>();

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

            if(!contains(seen,edge_slot))
            {
                // add as a scan target
                add(seen,edge_slot);
                push_var(scan,edge_slot);
                

                // add as new link
                push_var(block.links,edge_slot);
            }
        }
    }

    
    destroy_set(seen);
    destroy_arr(scan);
}


void print_ir_set(Interloper& itl, const Set<RegSlot>& set, const char* tag)
{
    printf("%s: {",tag);

    for(const auto slot : set)
    {
        switch(slot.kind)
        {
            case reg_kind::sym:
            {
                auto& sym = sym_from_slot(itl.symbol_table,slot.sym_slot);
                printf("%s,",sym.name.buf);
                break;
            }

            case reg_kind::tmp:
            {
                printf("t%d,",slot.tmp_slot.handle);
                break;
            }

            // This should not flow through blocks
            case reg_kind::spec:
            {
                assert(false);
                break;
            }
        }
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

    Set<BlockSlot> seen = make_set<BlockSlot>();
    Array<BlockSlot> to_visit;

    // print from start
    BlockSlot start = block_from_idx(0);
    add(seen,start);
    push_var(to_visit,start);

    
    while(count(to_visit))
    {
        const BlockSlot cur = pop(to_visit);
        const auto& block = block_from_slot(func,cur); 

        // print cur
        printf("\nL%d:\n",block.label_slot.handle);
        printf("flags: %x\n",block.flags);

        print_block_connection(func,block.entry,"entry: ");

        print_ir_set(itl,block.use,"use: ");
        print_ir_set(itl,block.def,"def: ");
        print_ir_set(itl,block.live_in,"live in: ");
        print_ir_set(itl,block.live_out,"live out: ");

        print_block_connection(func,block.exit,"exit: ");

        // add any we havent seen for a print
        for(const BlockSlot exit : block.exit)
        {
            if(!contains(seen,exit))
            {
                add(seen,exit);
                push_var(to_visit,exit);            
            }
        }      
    }
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

// TODO: would it be cheaper to do this inside the emitter?
void compute_use_def(Interloper& itl,Function& func)
{
    // each block
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        const BlockSlot slot = block_from_idx(b);
        auto& block = block_from_slot(func,slot);

        // ignore empty blocks
        if(!block.list.start)
        {
            continue;
        }
        
        // run a pass on the block
        for(const OpcodeNode& node : block.list)
        {
            // mark three address code
            const auto opcode = node.value;

            const auto info = info_from_op(opcode);

            // look at src regs first, then dst!
            for(s32 r = info.args - 1; r >= 0; r--)
            {
                const auto operand = opcode.v[r];

                if(operand.type != operand_type::reg)
                {
                    continue;
                }

                const auto slot = operand.reg;

                // Not interested in special regs
                if(is_special_reg(slot))
                {
                    continue;
                }

                auto& ir_reg = reg_from_slot(itl.symbol_table,func,slot);

                // ir reg, that is not stored in memory
                if(!stored_in_mem(ir_reg))
                {
                    // used as src, without a def -> use
                    if(is_arg_src(info.type[r]) && !contains(block.def,slot))
                    {
                        add(block.use,slot); 
                    }

                    // used as dst, def 
                    else if(is_arg_dst(info.type[r]))
                    {
                        add(block.def,slot);
                    }
                }
            }
        }

        // if block has a use of a var it must be an input
        // computed here for speed rather than in liveness func
        set_union(block.live_in,block.use);        
    }
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

    // backprop until we get no changes to account for loops!
    b32 modified = true;

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

    while(modified)
    {
        modified = false;

        // run a liveness pass
        auto seen = make_set<BlockSlot>();
        Array<BlockSlot> to_visit;

        // run complete analysis from last node
        add(seen,last_reachable_block);
        push_var(to_visit,last_reachable_block);

        // run pass on cur block
        while(count(to_visit))
        {
            const BlockSlot cur = pop(to_visit);
            auto& block = block_from_slot(func,cur);

            // used as a use -> input
            // NOTE: computed above in use def for speed 
            // as it will not change from propagation
            // set_union(block.input,block.use);

            // input of exit -> output
            for(const auto& block_slot : block.exit)
            {
                const auto& exit = block_from_slot(func,block_slot);
                modified |= set_union(block.live_out,exit.live_in);
            }

            // finally if there is no def for an output 
            // then it must be an input (the value must arise somewhere)  
            for(const RegSlot slot : block.live_out)
            {
                if(!contains(block.def,slot))
                {
                    modified |= add(block.live_in,slot);
                }
            }

            // add entrys we havent seen for parsing
            for(const BlockSlot entry : block.entry)
            {
                if(!contains(seen,entry))
                {
                    add(seen,entry);
                    push_var(to_visit,entry);            
                }
            }

            // add any exits while we are at it 
            for(const BlockSlot exit : block.exit)
            {
                if(!contains(seen,exit))
                {
                    add(seen,exit);
                    push_var(to_visit,exit);            
                }
            }
        }

        // cleanup mem for current pass
        destroy_set(seen);
        destroy_arr(to_visit);
    }

    if(itl.print_ir)
    {
        dump_cfg(itl,func);
    }
}

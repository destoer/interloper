
void insert_program(Interloper& itl, const Opcode& opcode)
{
    push_var(itl.program,opcode);
}



// NOTE: pass in a size, so we only print the code section
void dump_program(const Array<u8> &program,u32 size, HashTable<u32,LabelSlot> &inv_label_lookup, LabelLookup &label_lookup)
{
    for(u32 pc = 0; pc < size; pc += sizeof(Opcode))
    {
        if(contains(inv_label_lookup,pc))
        {
            const auto label = label_from_slot(label_lookup,*lookup(inv_label_lookup,pc));
            printf("0x%08x %s:\n",pc,label.name.buf);
        }

        printf("  0x%08x:\t ",pc);   

        const Opcode opcode = read_mem<Opcode>(program,pc);
        disass_opcode_raw(opcode);
    }
}


void emit_asm(Interloper &itl)
{
    HashTable<u32,LabelSlot> inv_label_lookup = make_table<u32,LabelSlot>();




    LabelSlot start_label = lookup(itl.function_table,String("start"))->label_slot;

    // emit a dummy call to start
    // that will get filled in later once we know where it is
    insert_program(itl,Opcode(op_type::call,start_label.handle,0,0));


    // resolve all our labels, dump all our machine code into a buffer
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            Function& func = bucket[i].v;


            itl.symbol_table.label_lookup[func.label_slot.handle].offset = itl.program.size;

            add(inv_label_lookup,itl.program.size,func.label_slot);

            for(u32 b = 0; b < count(func.emitter.program); b++)
            {
                const auto &block = func.emitter.program[b];

                // resolve label addr.
                itl.symbol_table.label_lookup[block.label_slot.handle].offset = itl.program.size;

                // if this is the first block prefer function name
                if(b != 0)
                {
                    add(inv_label_lookup,itl.program.size,block.label_slot);
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

    // get raw pool data to rewrite its contents
    auto& pool_data = itl.const_pool.buf;

    const u32 const_pool_loc = itl.program.size;

    for(u32 i = 0; i < count(itl.const_pool.sections); i++)
    {
        const PoolSection& section = itl.const_pool.sections[i];
        const u32 section_offset = section.offset;

        // rewrite all labels
        for(u32 l = 0; l < count(section.label); l++)
        {
            // assumes 32bit handles
            static_assert(sizeof(LabelSlot) == sizeof(u32));

            const u32 label_offset = section.label[l];
            const u32 addr = label_offset + section_offset;

            // read out the label handle so we can write back the offset
            const u32 label_handle = read_mem<u32>(pool_data,addr);

            write_mem<u32>(pool_data, addr, itl.symbol_table.label_lookup[label_handle].offset);
        }

        // rewrite all pointers
        for(u32 p = 0; p < count(section.pool_pointer); p++)
        {
            const u32 pointer_offset = section.pool_pointer[p];
            const u32 addr = pointer_offset + section_offset;

            const u32 pool_handle = read_mem<u32>(pool_data,addr);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot_from_idx(pool_handle));

            write_mem<u32>(pool_data,addr,section.offset + const_pool_loc);
        }
    }

    // add the constant pool, into the final program
    push_mem(itl.program,pool_data);


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
            const PoolSlot pool_slot = pool_slot_from_idx(opcode.v[1]);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            opcode = Opcode(op_type::mov_imm,opcode.v[0],PROGRAM_ORG + const_pool_loc + section.offset,0);

            write_mem(itl.program,i,opcode);
        }
    }


    // clean up the mem from the const pool
    destroy_const_pool(itl.const_pool);


    if(itl.print_ir)
    {
        dump_program(itl.program,const_pool_loc,inv_label_lookup,itl.symbol_table.label_lookup);
    }

    destroy_table(inv_label_lookup);
}
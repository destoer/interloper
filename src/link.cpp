
void insert_program(Interloper& itl, const Opcode& opcode)
{
    push_var(itl.program,opcode);
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
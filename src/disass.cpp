void fmt_sym_specifier(Array<char> &buffer, const SymbolTable& table, char specifier, u64 handle)
{
    switch(specifier)
    {
        case 'r':
        {
            
            SymSlot slot = sym_from_idx(handle);

            if(is_special_reg(slot))
            {
                const u32 idx = slot.handle - SPECIAL_PURPOSE_REG_START;
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
                const u32 len = sprintf(name,"t%d",slot.handle);

                push_mem(buffer,name,len);
            }


            break;
        }


        // hex constant
        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%lx",handle);

            push_mem(buffer,name,len);
            break;
        }

        // address
        case 'a':
        {
            const String& name = table.label_lookup[handle].name;
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

void fmt_raw_specifier(Array<char> &buffer,const SymbolTable* table, char specifier, u64 slot, arch_target arch)
{
    switch(specifier)
    {
        // raw register
        case 'r':
        {
            switch(arch)
            {
                case arch_target::x86_64_t:
                {
                    push_mem(buffer,X86_NAMES[slot],strlen(X86_NAMES[slot]));
                    break;
                }
            }

            break;
        }

        // labeles act as address here
        case 'a':
        {
            if(table)
            {
                const auto sym_table = *table;

                const auto label_slot = label_from_idx(slot);
                const auto& label = label_from_slot(sym_table.label_lookup,label_slot);

                push_mem(buffer,label.name);
            }

            else
            {
                char name[40];
                const u32 len = sprintf(name,"0x%lx",slot);

                push_mem(buffer,name,len);
            }
            break;
        }

        case 'x':
        {
            char name[40];
            const u32 len = sprintf(name,"0x%lx",slot);

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

void disass_opcode_internal(const Opcode& opcode, const SymbolTable* table,b32 format_reg,arch_target arch)
{
    const auto& info = info_from_op(opcode);
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

            if(format_reg)
            {
                fmt_raw_specifier(buffer,table,specifier,opcode.v[args++],arch);
            }

            else
            {
                fmt_sym_specifier(buffer,*table,specifier,opcode.v[args++]);
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
void disass_opcode_sym(const Opcode &opcode, const SymbolTable& table,arch_target arch)
{
    disass_opcode_internal(opcode,&table,false,arch);
}

void disass_opcode_raw(const Opcode &opcode, arch_target arch)
{
    disass_opcode_internal(opcode,nullptr,true,arch);
}


void disass_opcode_reg(const Opcode &opcode, const SymbolTable& table, arch_target arch)
{
    disass_opcode_internal(opcode,&table,true,arch);
}


void dump_ir_sym(Interloper& itl,Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);

    u32 l = 0;
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        

        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_sym(node->opcode,table,itl.arch);
            node = node->next;
        }

        l++;
    }

    printf("\n");       
}


void dump_ir_reg(Interloper& itl,Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);

    u32 l = 0;
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {   
        const auto &block = func.emitter.program[b];
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        

        auto node = block.list.start;
        while(node)
        {
            printf("\t");
            disass_opcode_reg(node->opcode,table,itl.arch);
            node = node->next;
        }

        l++;
    }

    printf("\n");       
}
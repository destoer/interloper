void format_regm(StringBuffer& buffer, u64 slot)
{
    char name[128];

    push_var(buffer,'{');

    u32 count = 0;

    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        if(is_set(slot,r))
        {
            const u32 len = sprintf(name,"%s%s",count != 0? "," : "",X86_NAMES[r]);
            push_mem(buffer,name,len);
            count++;
        }
    }

    push_var(buffer,'}');
}

void fmt_sym_register(StringBuffer& buffer, const SymbolTable& table, RegSlot reg)
{
    switch(reg.kind)
    {
        case reg_kind::spec:
        {
            push_string(buffer,spec_reg_name(reg.spec));
            break;
        }

        // print a sym
        case reg_kind::sym:
        {
            const auto& sym = sym_from_slot(table,reg.sym_slot);
            const String& name = sym.name;

            push_string(buffer,name);
            break;
        }

        case reg_kind::tmp:
        {
            char name[40] = {0};
            const u32 len = sprintf(name,"t%d",reg.tmp_slot.handle);

            push_mem(buffer,name,len);
            break;
        }
    }
}

void fmt_sym_specifier(StringBuffer &buffer, const SymbolTable& table, char specifier, Operand operand)
{
    switch(specifier)
    {
        case 'r':
        {
            const auto reg = operand.reg;
            fmt_sym_register(buffer,table,reg);
            break;
        }


        // hex constant
        case 'x':
        {
            char name[40] = {0};
            const u32 len = sprintf(name,"0x%lx",operand.imm);

            push_mem(buffer,name,len);
            break;
        }

        case 'f':
        {
            char name[40] = {0};
            const u32 len = sprintf(name,"%lf",operand.decimal);

            push_mem(buffer,name,len);
            break;
        }

        case 'm':
        {
            format_regm(buffer,operand.imm);
            break;
        }

        // address
        case 'a':
        {
            const String& name = table.label_lookup[operand.label.handle].name;
            push_string(buffer,name);
            break;
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void fmt_raw_register(StringBuffer& buffer, u64 slot, arch_target arch)
{
    if(is_raw_special_reg(slot))
    {
        const u32 idx = slot - SPECIAL_REG_START;
        push_mem(buffer,SPECIAL_REG_NAMES[idx]);
    }

    else 
    {
        switch(arch)
        {
            case arch_target::x86_64_t:
            {
                push_mem(buffer,X86_NAMES[slot],strlen(X86_NAMES[slot]));
                break;
            }
        }
    }
}

void fmt_raw_specifier(StringBuffer &buffer,const SymbolTable* table, char specifier, u64 slot, arch_target arch)
{
    switch(specifier)
    {
        // raw register
        case 'r':
        {
            fmt_raw_register(buffer,slot,arch);
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
            format_regm(buffer,slot);
            break;
        }

        // ignore printing the fmt
        default:
        {
            break;
        }
    }    
}

void format_address(StringBuffer& buffer, const Opcode& opcode, const SymbolTable* table,b32 format_reg,arch_target arch)
{
    if(format_reg)
    {
        // Format base
        fmt_raw_register(buffer,opcode.v[1].lowered,arch);


        if(u32(opcode.v[2].lowered) != u32(spec_reg::null))
        {
            push_string(buffer,opcode.scale == 1? " + " : " + (");
            fmt_raw_register(buffer,opcode.v[2].lowered,arch);

            char scale[40];
            const u32 scale_len = sprintf(scale,opcode.scale == 1? "" : " * 0x%x)",opcode.scale);
            push_mem(buffer,scale,scale_len); 
        }
    }

    else
    {
        // Format base
        fmt_sym_register(buffer,*table,opcode.v[1].reg);

        // Format index
        if(!is_null_reg(opcode.v[2].reg))
        {
            push_string(buffer,opcode.scale == 1? " + " : " + (");
            fmt_sym_register(buffer,*table,opcode.v[2].reg);

            char scale[40];
            const u32 scale_len = sprintf(scale,opcode.scale == 1? "" : " * 0x%x)",opcode.scale);
            push_mem(buffer,scale,scale_len);
        }
    }

    // Format offset
    if(opcode.offset)
    {
        char offset[40];
        const u32 imm_len = sprintf(offset," + 0x%x",opcode.offset);

        push_mem(buffer,offset,imm_len);
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
                crash_and_burn("exceed opcode arg printing");
            }

            const char specifier = fmt_string[i + 1];

            if(specifier == 'i')
            {
                args += 2;
                if(args > 3)
                {
                    crash_and_burn("exceed opcode arg printing");
                }

                format_address(buffer,opcode,table,format_reg,arch);
            }

            if(format_reg)
            {
                fmt_raw_specifier(buffer,table,specifier,opcode.v[args++].lowered,arch);
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
    for(auto& block : func.emitter.program)
    {   
        //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        

        for(const OpcodeNode& node : block.list)
        {
            printf("\t");
            disass_opcode_sym(node.value,table,itl.arch);
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
        

        for(const OpcodeNode& node : block.list)
        {
            printf("\t");
            disass_opcode_reg(node.value,table,itl.arch);
        }

        l++;
    }

    printf("\n");       
}
struct Disass
{
    Disass(SymbolTable& src_table, arch_target src_arch) : table(src_table), arch(src_arch) {}

    SymbolTable& table;
    arch_target arch;
};

void print_regm(u64 slot)
{
    printf("{");

    u32 count = 0;

    for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
    {
        if(is_set(slot,r))
        {
            printf("%s%s",count != 0? "," : "",X86_NAMES[r]);
            count++;
        }
    }

    printf("}");
}

void print_ir_reg(const SymbolTable& table, RegSlot reg)
{
    switch(reg.kind)
    {
        case reg_kind::spec:
        {
            printf("%s",spec_reg_name(reg.spec));
            break;
        }

        // print a sym
        case reg_kind::sym:
        {
            const auto& sym = sym_from_slot(table,reg.sym_slot);
            const String& name = sym.name;

            printf("%s",name.buf);
            break;
        }

        case reg_kind::tmp:
        {
            printf("t%d",reg.tmp_slot.handle);
            break;
        }
    }
}



void vprint_disass(const Opcode& opcode, const Disass& disass, const String& fmt, va_list args)
{
    assert(!opcode.lowered);

    // %x  hex
    // %f  float
    // %r  register
    // %p  pool
    // %a  address
    // %m  register set
    // %s  string

    for(size_t i = 0; i < fmt.size; i++)
    {
        const char token = fmt[i];

        if(token != '%')
        {
            putchar(token);
            continue;
        }

        assert(i != fmt.size);

        const char specifier = fmt[++i];
        switch(specifier)
        {
            case 'r':
            {
                const RegSlot reg = va_arg(args,RegSlot);
                print_ir_reg(disass.table,reg);
                break;
            }


            // hex constant
            case 'x':
            {
                const u64 imm = va_arg(args,u64);
                printf("0x%lx",imm);
                break;
            }

            case 'f':
            {
                const f64 decimal = va_arg(args,f64);
                printf("%lf",decimal);
                break;
            }

            case 'm':
            {
                const u64 imm = va_arg(args,u64);
                print_regm(imm);
                break;
            }

            // pool
            case 'p':
            {
                const PoolSlot pool = va_arg(args,PoolSlot);
                printf("%d",pool.handle);
                break;
            }

            // address
            case 'a':
            {
                const LabelSlot label = va_arg(args,LabelSlot);
                const String& name = disass.table.label_lookup[label.handle].name;
                printf("%s",name.buf);
                break;
            }

            case 's':
            {
                const char* str = va_arg(args,const char*);
                printf("%s",str);
                break;
            }

            default: assert(false);
        }
    }
}

void print_disass(const Opcode& opcode, const Disass& disass, const String& fmt, ...)
{
    va_list args;
    va_start(args,fmt);

    vprint_disass(opcode,disass,fmt,args);

    putchar('\n');

    va_end(args);
}


void disass_directive(const Opcode& opcode, const Disass& disass)
{
    assert(!opcode.lowered);

    const auto& directive = opcode.directive;
    printf("%s ",DIRECTIVE_NAMES[u32(directive.type)]);

    for(u32 i = 0; i < directive.size; i++)
    {
        auto& operand = directive.operand[i];


        switch(operand.type)
        {
            case directive_operand_type::src:
            case directive_operand_type::dst:
            case directive_operand_type::dst_src:
            case directive_operand_type::directive_reg:
            {
                print_ir_reg(disass.table,operand.reg);
                break;
            }

            case directive_operand_type::decimal: printf("%f",operand.decimal); break;
            case directive_operand_type::imm: printf("%lx",operand.imm); break;
            case directive_operand_type::pool: printf("%x",operand.pool.handle); break;
            case directive_operand_type::label: printf("L%d",operand.label.handle); break;
        }


        if(i != directive.size - 1)
        {
            printf(", ");
        }
    }

    printf("\n");
}

void disass_mov_gpr_imm(const Opcode& opcode, const Disass& disass)
{
    auto& mov = opcode.mov_gpr_imm;
    print_disass(opcode,disass,"mov %r, %x",mov.dst,mov.imm);
}

void disass_branch_label(const Opcode& opcode, const Disass& disass)
{
    auto& branch = opcode.branch_label;
    print_disass(opcode,disass,"%s %r, %a",BRANCH_NAMES[u32(branch.type)],branch.label);
}

void disass_opcode(const Opcode& opcode, const Disass& disass)
{
    switch(opcode.group)
    {
        case op_group::directive: disass_directive(opcode,disass); break;
        case op_group::mov_gpr_imm: disass_mov_gpr_imm(opcode,disass); break;
        case op_group::branch_label: disass_branch_label(opcode,disass); break;
        default: unimplemented("Cannot disassemble group: %d",opcode.group); break;
    }
}

void dump_ir(Interloper& itl,Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);
    Disass disass =  Disass(table,itl.arch);

    for(auto& block : func.emitter.program)
    {       
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        
        for(const OpcodeNode& node : block.list)
        {
            printf("\t");
            disass_opcode(node.value,disass);
        }
    }

    printf("\n");  
}
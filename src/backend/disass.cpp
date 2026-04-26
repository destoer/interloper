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
                const IrRegister reg = va_arg(args,IrRegister);
                print_ir_reg(disass.table,reg.ir);
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
            case directive_operand_type::imm: printf("0x%lx",operand.imm); break;
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
    print_disass(opcode,disass,"mov %r, %x\n",mov.dst,mov.imm);
}

void disass_branch_label(const Opcode& opcode, const Disass& disass)
{
    auto& branch = opcode.branch_label;
    print_disass(opcode,disass,"%s %a\n",BRANCH_NAMES[u32(branch.type)],branch.label);
}

template<typename type>
void disass_unary_reg2(const Opcode& opcode, const Disass& disass, const UnaryReg2<type>& unary, const char* names[])
{
    print_disass(opcode,disass,"%s %r, %r\n",names[u32(unary.type)],unary.dst,unary.src);
}

template<typename type, const bool IS_LOAD, const bool IS_STRUCT>
void disass_addr(const Opcode& opcode, const Disass& disass, const AddrOpcode<type,IS_LOAD,IS_STRUCT>& addr_op, const char* names[])
{
    assert(!opcode.lowered);
    const auto& addr = addr_op.addr_ir;

    print_disass(opcode,disass,"%s %r, [%r",names[u32(addr_op.type)],addr_op.v1,addr.base);

    if(!is_null_reg(addr.index))
    {
        if(addr.scale == 1)
        {
            print_disass(opcode,disass," + %r",addr.index);
        }

        else
        {
            print_disass(opcode,disass," + (%r * %x)",addr.index,addr.scale);
        }
    }

    if(addr.offset)
    {
        printf(" + 0x%x",addr.offset);
    }

    printf("]\n");
}

void disass_branch_cond(const Opcode& opcode, const Disass& disass)
{
    auto& branch = opcode.branch_cond;
    print_disass(opcode,disass,"%s %r, %a\n",BRANCH_COND_NAMES[u32(branch.type)],branch.src,branch.label);
}

template<typename type>
void disass_reg3(const Opcode& opcode, const Disass& disass,const RegThree<type>& reg, const char* names[])
{
    print_disass(opcode,disass,"%s %r, %r, %r\n",names[u32(reg.type)],reg.dst,reg.v1,reg.v2);
}

void disass_opcode(const Opcode& opcode, const Disass& disass)
{
    switch(opcode.group)
    {
        case op_group::implicit: printf("%s\n",IMPLICIT_NAMES[u32(opcode.implicit.type)]); break;
        case op_group::branch_cond: disass_branch_cond(opcode,disass); break;
        case op_group::directive: disass_directive(opcode,disass); break;
        case op_group::mov_gpr_imm: disass_mov_gpr_imm(opcode,disass); break;
        case op_group::branch_label: disass_branch_label(opcode,disass); break;
        case op_group::unary_reg2: disass_unary_reg2(opcode,disass,opcode.unary_reg2,UNARY_REG_NAMES); break;
        case op_group::lea: disass_addr(opcode,disass,opcode.lea,TAKE_ADDR_NAMES); break;
        case op_group::addrof: disass_addr(opcode,disass,opcode.addrof,TAKE_ADDR_NAMES); break;
        case op_group::load_struct: disass_addr(opcode,disass,opcode.load_struct,LOAD_STRUCT_NAMES); break;
        case op_group::store_struct: disass_addr(opcode,disass,opcode.store_struct,STORE_STRUCT_NAMES); break;
        case op_group::arith_gpr3: disass_reg3(opcode,disass,opcode.arith_gpr3,ARITH_NAMES); break;
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
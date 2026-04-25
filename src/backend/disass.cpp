void print_ir_reg(SymbolTable& table, RegSlot slot)
{
    switch(slot.kind)
    {
        case reg_kind::spec:
        {
            printf("%s",spec_reg_name(slot.spec));
            break;
        }

        case reg_kind::sym:
        {
            const auto &sym = sym_from_slot(table,slot.sym_slot);
            printf("%s",sym.name.buf);
            break;
        }

        case reg_kind::tmp:
        {
            printf("t%d",slot.tmp_slot.handle);
            break;
        }
    }
}

void print_register(const Opcode& opcode, SymbolTable& table, const IrRegister& reg)
{
    assert(!opcode.lowered);

    print_ir_reg(table,reg.ir);
}

void disass_directive(const Opcode& opcode, SymbolTable& table)
{
    UNUSED(table);
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
                print_ir_reg(table,operand.reg);
                break;
            }

            case directive_operand_type::decimal: printf("%f",operand.decimal); break;
            case directive_operand_type::imm: printf("%lx",operand.imm); break;
            case directive_operand_type::pool: printf("%x",operand.pool.handle); break;
            case directive_operand_type::label: printf("L%d",operand.label.handle); break;
        }

        if(i == 0)
        {
            printf(" ");
        }

        else if(i != directive.size - 1)
        {
            printf(", ");
        }
    }

    printf("\n");
}


void disass_opcode(const Opcode& opcode, SymbolTable& table, arch_target arch)
{
    UNUSED(table); UNUSED(arch);

    switch(opcode.group)
    {
        case op_group::directive: disass_directive(opcode,table); break;
        default: unimplemented("Cannot disassemble group: %d",opcode.group); break;
    }
}

void dump_ir(Interloper& itl,Function &func,SymbolTable& table)
{
    printf("%s:\n",func.name.buf);

    for(auto& block : func.emitter.program)
    {       
        const auto label = label_from_slot(table.label_lookup,block.label_slot);
        printf("%s:\n",label.name.buf);
        
        for(const OpcodeNode& node : block.list)
        {
            printf("\t");
            disass_opcode(node.value,table,itl.arch);
        }
    }

    printf("\n");  
}
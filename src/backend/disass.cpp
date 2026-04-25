
void disass_opcode(const Opcode& opcode, SymbolTable& table, arch_target arch)
{
    UNUSED(opcode); UNUSED(table); UNUSED(arch);
    puts("nop");
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
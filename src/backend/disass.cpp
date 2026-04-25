

void dump_ir_sym(Interloper& itl,Function &func,SymbolTable& table)
{
    UNUSED(itl); UNUSED(func); UNUSED(table);
    // printf("%s:\n",func.name.buf);

    // u32 l = 0;
    // for(auto& block : func.emitter.program)
    // {   
    //     //printf("block type: %s\n",block_names[static_cast<int>(block.type)]);
    
    //     const auto label = label_from_slot(table.label_lookup,block.label_slot);
    //     printf("%s:\n",label.name.buf);
        

    //     for(const OpcodeNode& node : block.list)
    //     {
    //         printf("\t");
    //         disass_opcode_sym(node.value,table,itl.arch);
    //     }

    //     l++;
    // }

    // printf("\n");       
}
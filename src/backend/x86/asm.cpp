void emit_x86_opcode(AsmEmitter& emitter, const Opcode& opcode)
{
    UNUSED(emitter); UNUSED(opcode);
    assert(false);
}

void emit_x86_func(Interloper& itl, Function& func)
{
    const u32 func_idx = add_func(itl.asm_emitter,func);

    for(auto& block : func.emitter.program)
    {
        // store cur relative offset to finalise later
        write_cur_rel_offset(itl,block.label_slot);

        for(const OpcodeNode &node : block.list)
        {
            emit_x86_opcode(itl.asm_emitter,node.value);
        }
    }

    end_func(itl.asm_emitter,func_idx);
}

void emit_x86_asm(Interloper& itl)
{
    for(auto& func : itl.func_table.used)
    {
        emit_x86_func(itl,*func);
    }
}


void destroy_asm_emitter(AsmEmitter& emitter)
{
    destroy_arr(emitter.buffer);
    destroy_arr(emitter.link);
    destroy_arr(emitter.func);
}

template<typename T>
void push(AsmEmitter& emitter, T v)
{
    push_var(emitter.buffer,v);
}

void push_u8(AsmEmitter& emitter, u8 v)
{
    push(emitter,v);
}

void push_u16(AsmEmitter& emitter, u16 v)
{
    push(emitter,v);
}

void push_u32(AsmEmitter& emitter, u32 v)
{
    push(emitter,v);
}

void push_u64(AsmEmitter& emitter, u64 v)
{
    push(emitter,v);
}

u32 add_func(AsmEmitter& emitter, Function& func)
{
    AsmFunc asm_func = {func.name,func.label_slot,emitter.buffer.size,0};

    const u32 idx = count(emitter.func);

    push_var(emitter.func,asm_func);

    return idx;
}


void end_func(AsmEmitter& emitter, u32 idx)
{
    auto& asm_func = emitter.func[idx];
    asm_func.size = emitter.buffer.size - asm_func.offset; 
}

void finalise_labels(Interloper& itl, u64 base)
{
    itl.asm_emitter.base_vaddr = base;

    // record all label locations
    for(u32 f = 0; f < count(itl.asm_emitter.func); f++)
    {
        auto& func = itl.asm_emitter.func[f];
        
        const u64 vaddr = base + func.offset;
        itl.symbol_table.label_lookup[func.label.handle].offset = vaddr;
    }
}

void add_link(AsmEmitter& emitter, const Opcode& opcode, u32 offset)
{
    LinkOpcode link = {opcode,offset};

    push_var(emitter.link,link);
}

void emit_asm(Interloper &itl)
{
    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            x86::emit_asm(itl);
            break;
        }
    }
}
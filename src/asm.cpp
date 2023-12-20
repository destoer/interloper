

AsmEmitter make_asm_emitter()
{
    AsmEmitter emitter;
    emitter.func = make_table<String,AsmFunc>();

    return emitter;
}

void destroy_asm_emitter(AsmEmitter& emitter)
{
    destroy_arr(emitter.buffer);
    destroy_table(emitter.func);
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

void add_func(AsmEmitter& emitter, Function& func)
{
    AsmFunc asm_func = {emitter.buffer.size,0};

    add(emitter.func,func.name,asm_func);
}


void emit_asm(Interloper &itl)
{
    itl.asm_emitter = make_asm_emitter();

    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            x86::emit_asm(itl);
            break;
        }
    }
}
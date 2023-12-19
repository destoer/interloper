
void emit_x86_opcode(AsmEmitter& emitter, const Opcode& opcode)
{
    UNUSED(emitter);

    switch(opcode.op)
    {
        default:
        {
            auto& info = info_from_op(opcode);
            printf("[EMIT X86]: unknown opcode: %s\n",info.fmt_string.buf);
            assert(false);
            break;
        } 
    }
}

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


void add_func(AsmEmitter& emitter, Function& func)
{
    AsmFunc asm_func = {emitter.buffer.size,0};

    add(emitter.func,func.name,asm_func);
}

void end_func(AsmEmitter& emitter, Function& func)
{
    auto& asm_func = *lookup(emitter.func,func.name);
    asm_func.size = emitter.buffer.size - asm_func.offset; 
}

void emit_x86_func(Interloper& itl, Function& func)
{
    add_func(itl.asm_emitter,func);

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        ListNode* node = block.list.start;

        while(node)
        {
            emit_x86_opcode(itl.asm_emitter,node->opcode);
            node = node->next;
        }
    }

    end_func(itl.asm_emitter,func);
}

void emit_x86_asm(Interloper& itl)
{
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            emit_x86_func(itl,func);
        }
    }
}


void emit_asm(Interloper &itl)
{
    itl.asm_emitter = make_asm_emitter();

    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            emit_x86_asm(itl);
            break;
        }
    }
}
void rewrite_opcode(Opcode& opcode, const ConstIrRegSpan& regs)
{
    UNUSED(regs);

    switch(opcode.group)
    {
        default:
        {
            unimplemented("[Rewrite opcode]: Invalid group: %s",OP_GROUP_NAMES[u32(opcode.group)]);
        }
    }
}

void rewrite_spill_span_arg(Interloper& itl, Function& func, Block& block, OpcodeNode* node, 
    const ConstSpan<RegSlot>& span,  Span<RegSlot> &rewritten, reg_arg_kind kind, u32 arg)
{
    const auto& slot = span[arg];
    auto& reg = reg_from_slot(itl.symbol_table,func, slot);

    const auto tmp = copy_reg_unaliased(func,reg);
    rewritten[arg] = tmp;

    if(is_arg_src(kind))
    {
        const auto opcode = make_reload_slot_into(tmp,slot);
        insert_at(block.list,node,opcode);
    }

    if(is_arg_dst(kind))
    {
        const auto opcode = make_spill_slot(tmp);
        insert_after(block.list,node,opcode);         
    }
}

bool rewrite_aliased_span(Interloper& itl, Function& func, Block& block, OpcodeNode* node, 
    const ConstSpan<RegSlot>& span,  Span<RegSlot> &rewritten, reg_arg_kind kind)
{
    bool modified = false;

    for(u32 arg = 0; arg < span.size; arg++)
    {
        const auto& slot = span[arg];

        if(is_special_reg(slot))
        {
            continue;
        }

        auto& reg = reg_from_slot(itl.symbol_table,func, slot);

        if(!is_aliased(reg))
        {
            continue;
        }

        modified = true;

        rewrite_spill_span_arg(itl,func,block,node,span,rewritten,kind,arg);
    }

    return modified;
}


// rewrite aliased variables to be locals we can actually allocate.
void correct_aliased_block(Interloper& itl, Function& func, Block& block, OpcodeNode* node)
{
    auto& opcode = node->value;

    IrRegSpanStorage reg_span_storage;
    const auto reg = opcode_ir_reg_span(opcode,itl.reg_span);
    IrRegSpan reg_span_rewritten = copy_ir_reg_span(reg_span_storage,reg);

    bool modified = false;

    modified |= rewrite_aliased_span(itl,func,block,node,reg.src,reg_span_rewritten.src,reg_arg_kind::src);
    modified |= rewrite_aliased_span(itl,func,block,node,reg.dst,reg_span_rewritten.dst,reg_arg_kind::dst);
    modified |= rewrite_aliased_span(itl,func,block,node,reg.dst_src,reg_span_rewritten.dst_src,reg_arg_kind::dst_src);

    if(modified)
    {
        rewrite_opcode(opcode,reg_span_rewritten);
    }
}

void correct_aliased_regs(Interloper& itl, Function& func)
{
    for(auto& block: func.emitter.program)
    {
        OpcodeNode* node = block.list.start;

        while(node)
        {
            correct_aliased_block(itl,func,block,node);
            node = node->next;
        }
    }
}
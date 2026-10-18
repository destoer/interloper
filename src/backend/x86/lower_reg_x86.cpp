lowered_reg_t graph_allocate_reg(GraphAlloc& alloc,Block& block,OpcodeNode* node, RegSlot reg, reg_arg_kind arg_kind);
OpcodeNode* rewrite_x86_fixed_arith_stack(GraphAlloc& alloc,Block& block, OpcodeNode* node);
OpcodeNode* rewrite_x86_fixed_shift_stack(GraphAlloc& alloc,Block& block, OpcodeNode* node);

OpcodeNode* rewrite_x86_fixed_arith(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    if(alloc.stack_only)
    {
        return rewrite_x86_fixed_arith_stack(alloc,block,node);
    }

    assert(false);
    return node->next;
}   

OpcodeNode* rewrite_x86_fixed_shift(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    if(alloc.stack_only)
    {
        return rewrite_x86_fixed_shift_stack(alloc,block,node);
    }

    assert(false);
    return node->next;
}

OpcodeNode* lower_x86_fixed(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;

    switch(fixed.type)
    {
        case x86_fixed_type::lsl:
        case x86_fixed_type::asr:
        case x86_fixed_type::lsr:
        {
            return rewrite_x86_fixed_shift(alloc,block,node);
        }

        case x86_fixed_type::udiv:
        case x86_fixed_type::sdiv:
        case x86_fixed_type::umod:
        case x86_fixed_type::smod:
        {
            return rewrite_x86_fixed_arith(alloc,block,node);
        }
    }

    assert(false);
    return node->next;
}
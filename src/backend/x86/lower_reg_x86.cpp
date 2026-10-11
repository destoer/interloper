

OpcodeNode* rewrite_x86_fixed_arith(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    UNUSED(alloc); UNUSED(block); UNUSED(node);
    assert(false);
}   

OpcodeNode* rewrite_x86_fixed_shift(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    UNUSED(alloc); UNUSED(block); UNUSED(node);
    assert(false);
}

OpcodeNode* lower_x86_fixed(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;

    switch(fixed.type)
    {
        case x86_fixed_type::lsl:
        {
            return rewrite_x86_fixed_shift(alloc,block,node);
        }

        case x86_fixed_type::asr:
        {
            return rewrite_x86_fixed_shift(alloc,block,node);
        }

        case x86_fixed_type::lsr:
        {
            return rewrite_x86_fixed_shift(alloc,block,node);
        }

        case x86_fixed_type::udiv:
        {
            return rewrite_x86_fixed_arith(alloc,block,node);
        }

        case x86_fixed_type::sdiv:
        {
            return rewrite_x86_fixed_arith(alloc,block,node);
        }

        case x86_fixed_type::umod:
        {
            return rewrite_x86_fixed_arith(alloc,block,node);
        }

        case x86_fixed_type::smod:
        {
            return rewrite_x86_fixed_arith(alloc,block,node);
        }
    }

    assert(false);
    return node->next;
}
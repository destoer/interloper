
void reload_fixed_reg_stack(GraphAlloc& alloc, Block& block, OpcodeNode* node, RegSlot dst, x86_reg reg, u32 arg)
{   
    // spec just rewrite it
    if(dst.kind == reg_kind::spec)
    {
        allocate_and_rewrite(alloc,block,node,arg);
    }

    else
    {
        reload_reg(alloc,block,node,dst,reg,insertion_type::before);
        node->value.v[arg] = make_raw_operand(reg);
    }
}

OpcodeNode* rewrite_x86_fixed_arith(GraphAlloc& alloc,Block& block, OpcodeNode* node, op_type type)
{
    // save where our dst is being forced into
    const auto dst = node->value.v[0].reg;

    // RAX, is allways the "dst" with RDX an implicit operand
    // What register we actually want the result out of varies
    if(alloc.stack_only)
    {
        reload_fixed_reg_stack(alloc,block,node,dst,x86_reg::rax,0);
    }

    else
    {
        assert(false);
    }

    // rewrite src for flexible operand
    allocate_and_rewrite(alloc,block,node,1);

    // NOTE: these are both fully rewritten so we can just dump them in into the instruction stream
    static const Opcode UNSIGNED_SETUP = make_raw_op(op_type::mov_imm,u32(x86_reg::rdx),0);
    static const Opcode SIGNED_SETUP = make_raw_op(op_type::cqo);

    const bool is_unsigned = type == op_type::udiv_x86 || type == op_type::umod_x86;
    insert_at(block.list,node,is_unsigned? UNSIGNED_SETUP : SIGNED_SETUP);

    const bool rdx_out = type == op_type::umod_x86 || type == op_type::smod_x86;
    const x86_reg out_reg = rdx_out? x86_reg::rdx : x86_reg::rax;

    if(alloc.stack_only)
    {
        if(dst.kind != reg_kind::spec)
        {
            spill_reg(alloc,block,node, dst, out_reg, insertion_type::after);

        }

        node->value.v[0] = make_raw_operand(out_reg);
    }

    else
    {
        assert(false);
    }

    return node->next;
}   

OpcodeNode* rewrite_x86_shift(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    const auto src = node->value.v[1].reg;

    if(alloc.stack_only)
    {
        // Issue the load into rcx
        reload_fixed_reg_stack(alloc,block,node,src,x86_reg::rcx,1);
        allocate_and_rewrite(alloc,block,node,0);
    }

    else 
    {
        assert(false);
    }

    return node->next;
}
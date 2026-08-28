

// We want to be more clever about reshuffling regs, for now lets just do it simply.
// ideally if our dst is in rdx we can just spill rax and copy it over
void lock_out_fixed_arith(LinearAlloc& alloc, Block& block, OpcodeNode* node, RegSlot dst)
{
    lock_into_reg(alloc,block,node,alloc.gpr,x86_reg::rax,dst);
    lock_out_reg(alloc,block,node,alloc.gpr,x86_reg::rdx);
}

void unlock_fixed_arith(LinearAlloc& alloc, RegSlot dst, x86_reg x86_dst, x86_reg x86_oper)
{
    unlock_into_reg(alloc,alloc.gpr,dst,x86_dst);
    release_register(alloc.gpr,x86_oper);
}

lowered_reg_t reload_fixed_reg_stack(LinearAlloc& alloc,Block& block, OpcodeNode* node, RegSlot dst, u32 reg)
{
    // issue a load
    reload_reg(alloc,block,node,dst,reg,insertion_type::before);
    return reg;
}

OpcodeNode* rewrite_x86_fixed_arith(LinearAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;
    const auto type = fixed.type;

    // save where our dst is being forced into
    const auto dst = fixed.dst.ir;

    // RAX, is always the "dst" with RDX an implicit operand
    // What register we actually want the result out of varies
    if(alloc.stack_only)
    {
        fixed.dst.reg = reload_fixed_reg_stack(alloc,block,node,dst,x86_reg::rax);
    }

    else
    {
        lock_out_fixed_arith(alloc,block,node,dst);
    }

    // Rewrite flexible operand
    fixed.src.reg = linear_allocate_reg(alloc,block,node,fixed.src.ir,reg_arg_kind::src); 

    // NOTE: these are both fully rewritten so we can just dump them in into the instruction stream
    static const Opcode UNSIGNED_SETUP = mov_imm_lowered(x86_reg::rdx,0);
    static const Opcode SIGNED_SETUP = make_lowered_implicit_instr(implicit_type::cqo);

    const bool is_unsigned = type == x86_fixed_type::udiv || type == x86_fixed_type::umod;
    insert_at(block.list,node,is_unsigned? UNSIGNED_SETUP : SIGNED_SETUP);

    const bool rdx_out = type == x86_fixed_type::umod || type == x86_fixed_type::smod;
    const x86_reg out_reg = rdx_out? x86_reg::rdx : x86_reg::rax;
    const x86_reg src_reg = rdx_out? x86_reg::rax : x86_reg::rdx;

    if(alloc.stack_only && dst.kind != reg_kind::spec)
    { 
        spill_reg(alloc,block,node, dst, out_reg, insertion_type::after);
    }

    else
    {
        unlock_fixed_arith(alloc,dst,out_reg,src_reg);
        fixed.dst.reg = linear_allocate_reg(alloc,block,node,fixed.dst.ir,reg_arg_kind::dst); 
    }

    node->value.state = opcode_state::lowered;
    return node->next;
}   

OpcodeNode* rewrite_x86_fixed_shift(LinearAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;
    const auto src = fixed.src.ir;
    const auto dst = fixed.dst.ir;

    if(alloc.stack_only)
    {
        // Issue the load into rcx
        fixed.src.reg = reload_fixed_reg_stack(alloc,block,node,src,x86_reg::rcx);
        
        // Finally allocate in dst
        fixed.dst.reg = linear_allocate_reg(alloc,block,node,dst,reg_arg_kind::dst_src);
    }

    else 
    {
        lock_into_reg(alloc,block,node,alloc.gpr,x86_reg::rcx,src);

        allocate_and_rewrite_opcode(alloc,block,node);

        unlock_into_reg(alloc,alloc.gpr,src,x86_reg::rcx);
    }

    return node->next;
}

OpcodeNode* lower_x86_fixed(LinearAlloc& alloc,Block& block, OpcodeNode* node)
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
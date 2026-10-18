// This should use a very simple state.
// There should be no pinning to registers
// It should just be reading out of the set and then binning the entire set at the instruction end
// There should be little to know room for bugs.

// Locks should still be followed?


lowered_reg_t stack_allocate_and_rewrite_var(GraphAlloc& alloc, Block& block, OpcodeNode* node, const RegSlot& slot, reg_arg_kind kind)
{
    auto& ir_reg = reg_from_slot(alloc,slot);
    auto& file = get_register_file(alloc,ir_reg);
    const auto scratch_reg = alloc_reg(file,ir_reg.hint);

    // This allocation should never fail for the stack.
    assert(scratch_reg != FFS_EMPTY);

    // src do a reload
    if(is_arg_src(kind)) 
    {
        reload_reg(alloc,block,node,slot,scratch_reg,insertion_type::before);
    }

    // If this is also a dst just spill
    if(is_arg_dst(kind))
    {
        spill_reg(alloc,block,node,slot,scratch_reg,insertion_type::after);      
    }

    return scratch_reg;
}

OpcodeNode* rewrite_x86_fixed_shift_stack(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;
    const auto src = fixed.src.ir;
    const auto dst = fixed.dst.ir;

    // Issue the load into rcx
    fixed.src.reg = reload_reg(alloc,block,node,src,x86_reg::rcx,insertion_type::before);
    
    // Finally allocate in dst
    fixed.dst.reg = graph_allocate_reg(alloc,block,node,dst,reg_arg_kind::dst_src);

    node->value.state = opcode_state::lowered;
    
    return node->next;
}

OpcodeNode* rewrite_x86_fixed_arith_stack(GraphAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& fixed = node->value.x86_fixed;

    // RAX, is always the "dst" with RDX an implicit operand
    // What register we actually want the result out of varies
    const auto dst = fixed.dst.ir;
    fixed.dst.reg = reload_reg(alloc,block,node,dst,x86_reg::rax,insertion_type::before);

    // Rewrite flexible operand
    fixed.src.reg = graph_allocate_reg(alloc,block,node,fixed.src.ir,reg_arg_kind::src);
    
    // NOTE: these are both fully rewritten so we can just dump them in into the instruction stream
    static const Opcode UNSIGNED_SETUP = mov_imm_lowered(x86_reg::rdx,0);
    static const Opcode SIGNED_SETUP = make_lowered_implicit_instr(implicit_type::cqo);

    const auto type = fixed.type;

    const bool is_unsigned = type == x86_fixed_type::udiv || type == x86_fixed_type::umod;
    insert_at(block.list,node,is_unsigned? UNSIGNED_SETUP : SIGNED_SETUP);

    if(dst.kind != reg_kind::spec)
    { 
        const bool rdx_out = type == x86_fixed_type::umod || type == x86_fixed_type::smod;
        const x86_reg out_reg = rdx_out? x86_reg::rdx : x86_reg::rax;

        spill_reg(alloc,block,node, dst, out_reg, insertion_type::after);
    }

    node->value.state = opcode_state::lowered;
    return node->next;
}
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

    const b32 is_dst = is_arg_dst(kind);
    const b32 is_src = is_arg_src(kind);

    // src do a reload
    if(is_src) 
    {
        // issue a load
        reload_reg(alloc,block,node,slot,scratch_reg,insertion_type::before);

        // do the writeback as well
        if(is_dst)
        {
            spill_reg(alloc,block,node,slot,scratch_reg,insertion_type::after);
        }
    }

    // just a dst
    else if(is_dst)
    {
        spill_reg(alloc,block,node,slot,scratch_reg,insertion_type::after);      
    }

    return scratch_reg;
}
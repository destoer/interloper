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


void reload_fixed_reg_stack(LinearAlloc alloc, Block& block, OpcodeNode* node, RegSlot dst, x86_reg reg, u32 arg)
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

OpcodeNode* rewrite_x86_fixed_arith(LinearAlloc& alloc,Block& block, OpcodeNode* node, op_type type)
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
        lock_out_fixed_arith(alloc,block,node,dst);
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
    const x86_reg src_reg = rdx_out? x86_reg::rax : x86_reg::rdx;

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
        unlock_fixed_arith(alloc,dst,out_reg,src_reg);

        // rewrite the dst
        allocate_and_rewrite(alloc,block,node,0);
    }

    return node->next;
}   

OpcodeNode* rewrite_x86_shift(LinearAlloc& alloc,Block& block, OpcodeNode* node)
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
        lock_into_reg(alloc,block,node,alloc.gpr,x86_reg::rcx,src);
        rewrite_opcode(alloc,block,node);

        unlock_into_reg(alloc,alloc.gpr,src,x86_reg::rcx);
    }

    return node->next;
}
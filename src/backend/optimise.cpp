#include <interloper.h>

// TODO: The comparision elision requires extra analysis. IF the cmp dst is actually require beyond the branch these optimisations breaks

bool next_instr_is(OpcodeNode* next, op_group group)
{
    return next && next->value.group == group;
}

OpcodeNode* collapse_cmp_branch(Block& block, OpcodeNode* node)
{
    const auto& branch = node->next->value.branch_cond;
    const auto& cmp = node->value.cmp_gpr3;

    if(branch.src.ir != cmp.dst.ir || contains(block.live_out,cmp.dst.ir))
    {
        return node->next;
    }

	// cmpult t0, v1, v2
	// beqz L393, t0

    // buge L393, v1, v2

    const auto cmp_type = branch.type == branch_cond_type::nez? cmp.type : CMP_SIGN_INVERSE[u32(cmp.type)];
    node->value = make_branch_cmp(cmp.v1.ir,cmp.v2.ir,branch.label,cmp_type);

    return node->next = remove(block.list,node->next);
}

OpcodeNode* collapse_inverted_branch(Block& block, OpcodeNode* node)
{
    const auto& branch = node->next->value.branch_cond;
    const auto& invert = node->value.arith_imm3;

    const bool inverted_branch = invert.imm == 1 && invert.dst.ir == branch.src.ir;
    if(!inverted_branch || contains(block.live_out,invert.dst.ir)) {
        return node->next;
    }

	// xor t0, v1, 1
	// beqz L393, t0

    // bnez L393, v1

    const auto inverse = branch.type == branch_cond_type::nez? branch_cond_type::eqz : branch_cond_type::nez;
    node->next->value = make_branch_cond(invert.src.ir,branch.label,inverse);

    return remove(block.list,node);
}

OpcodeNode* optimise_opcode(Interloper& itl, Block& block, OpcodeNode* node)
{
    UNUSED(itl);
    auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::implicit:
        {
            switch(opcode.implicit.type)
            {
                case implicit_type::ret: node->next = nullptr;
                default: break; 
            }

            break;
        }

        // TODO: Should work for imm cmp too
        case op_group::cmp_gpr3:
        {
            if(next_instr_is(node->next,op_group::branch_cond))
            {
                return collapse_cmp_branch(block,node);
            }

            break;
        }

        case op_group::arith_imm3:
        {
            if(opcode.arith_imm3.type == arith_bin_op::xor_t && next_instr_is(node->next,op_group::branch_cond))
            {
                return collapse_inverted_branch(block,node);
            }

            break;
        }

        default: break;
    }

    return node->next;
}

void optimise_block(Interloper& itl, Function& func, Block& block)
{
    UNUSED(func);
    auto node = block.list.start;

    while(node)
    {
        node = optimise_opcode(itl,block,node);
    }
}

void optimise_func(Interloper& itl, Function& func)
{
    // for now this is just peephole optimiser
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        optimise_block(itl,func,block);
    }
}


Option<itl_error> func_graph_pass(Interloper& itl, Function& func);

Option<itl_error> optimise_ir(Interloper &itl)
{   
    auto start = std::chrono::high_resolution_clock::now();
    
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        const auto err = func_graph_pass(itl,func);
        if(err)
        {
            return err;
        }

        optimise_func(itl,func);
        destroy_liveness_info(func);
    }

    auto end = std::chrono::high_resolution_clock::now();

    itl.optimise_time = std::chrono::duration<double, std::milli>(end-start).count();

    return option::none;
}
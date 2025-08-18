
// Do a top level rewrite of the IR opcode so
// The machine code translator is basically a 1 to 1 mapping
// outside of optimal instruction selection

OpcodeNode* rewrite_reg3_two_commutative(Block& block, OpcodeNode* node,op_type opcode_type, reg_type register_type)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto v2 = node->value.v[2];

    // add dst, dst, v2
    // -> add dst, v2
    if(dst == v1)
    {
        node->value = make_op(opcode_type,dst,v2);
    }

    // add dst, v1, dst
    // -> add dst, v1
    else if(dst == v2)
    {
        node->value = make_op(opcode_type,dst,v1);
    }

    // add dst, v1, v2
    // -> mov dst, v1
    // -> add dst, v2
    else
    {
        node->value = make_op(register_type == reg_type::float_t? op_type::movf_reg : op_type::mov_reg,dst,v1);
        node = insert_after(block.list,node,make_op(opcode_type,dst,v2));
    }

    return node->next;
}

OpcodeNode* rewrite_imm3_two(Block& block, OpcodeNode* node,op_type type)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto imm = node->value.v[2];

    // add dst, dst, imm
    // -> add dst, imm
    if(dst == v1)
    {
        node->value = make_op(type,dst,imm);
    }

    // add dst, v1, imm
    // -> mov dst, v1
    // -> add dst, imm
    else
    {
        node->value = make_op(op_type::mov_reg,dst,v1);
        node = insert_after(block.list,node,make_op(type,dst,imm));
    }

    return node->next;
}

OpcodeNode* rewrite_reg3_two(Function& func, Block& block, OpcodeNode* node,op_type opcode_type, reg_type register_type)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto v2 = node->value.v[2];

    const op_type mov_op = register_type == reg_type::float_t? op_type::movf_reg : op_type::mov_reg;

    // sub dst, dst, v2
    // -> sub dst, v2
    if(dst == v1)
    {
        node->value = make_op(opcode_type,dst,v2);
    }

    // sub dst, v1, dst
    // -> mov t0, v1
    // -> sub t0, dst
    // -> mov dst, t0
    else if(dst == v2)
    {
        const auto tmp_slot = register_type == reg_type::float_t? new_float(func) : new_tmp(func,GPR_SIZE);
        const auto tmp = make_reg_operand(tmp_slot);
        node->value = make_op(mov_op,tmp,v1);
        node = insert_after(block.list,node,make_op(opcode_type,tmp,v2));
        node = insert_after(block.list,node,make_op(mov_op,dst,tmp));
    }

    // sub dst, v1, v2
    // -> mov dst, v1
    // -> sub dst, v2
    else
    {
        node->value = make_op(mov_op,dst,v1);
        node = insert_after(block.list,node,make_op(opcode_type,dst,v2));
    }

    return node->next;
}

OpcodeNode* rewrite_reg2_one(Block& block, OpcodeNode* node,op_type type)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];

    node->value = make_op(op_type::mov_reg,dst,v1);
    node = insert_after(block.list,node,make_op(type,dst));

    return node->next;
}

void assert_bound(u64 v,u64 min, u64 max)
{
    assert(in_range(v,min,max));
}

// TODO: this assumes we have no access to the instruction
OpcodeNode* emit_popm(Interloper& itl, Block& block, OpcodeNode* node, u32 bitset)
{
    UNUSED(itl);

    for(s32 i = MACHINE_REG_SIZE - 1; i >= 0; i--)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,make_raw_op(op_type::pop,i,0,0));
            node = node->next;
        }
    }

    return node;
}

OpcodeNode* emit_pushm(Interloper& itl, Block& block, OpcodeNode* node,u32 bitset)
{
    UNUSED(itl);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,make_raw_op(op_type::push,i,0,0));
            node = node->next;
        }
    }

    return node;
}

OpcodeNode* emit_popm_float(Interloper& itl, Block& block, OpcodeNode* node, u32 bitset)
{
    if(!bitset)
    {
        return node;
    }

    const u32 size = popcount(bitset) * FLOAT_SIZE;

    u32 offset = size - FLOAT_SIZE;

    const u32 sp = arch_sp(itl.arch);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,make_raw_op(op_type::lf,i,sp,offset));
            node = node->next;
            offset -= FLOAT_SIZE;
        }
    }

    node = insert_at(block.list,node,make_raw_op(op_type::add_imm2,sp,size,0));
    node = node->next;

    return node;
}

OpcodeNode* emit_pushm_float(Interloper& itl, Block& block, OpcodeNode* node,u32 bitset)
{
    if(!bitset)
    {
        return node;
    }

    const u32 size = popcount(bitset) * FLOAT_SIZE;
    u32 offset = size - FLOAT_SIZE;

    const u32 sp = arch_sp(itl.arch);

    node = insert_at(block.list,node,make_raw_op(op_type::sub_imm2,sp,size,0));
    node = node->next;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,make_raw_op(op_type::sf,i,sp,offset));
            node = node->next;
            offset -= FLOAT_SIZE;
        }
    }

    return node;
}

OpcodeNode* rewrite_cmp_flag_reg(Block& block, OpcodeNode* node, op_type set)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto v2 = node->value.v[2];

    // cmpsgt dst,v1,v2
    // -> cmp_flags v1, v2
    // -> setsgt dst
    node->value = make_op(op_type::cmp_flags,v1,v2);
    node = insert_after(block.list,node,make_op(set,dst));

    return node->next;
}


OpcodeNode* rewrite_cmp_flag_float(Block& block, OpcodeNode* node, op_type set)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto v2 = node->value.v[2];

    // cmpfgt dst,v1,v2
    // -> cmp_flags_float v1, v2
    // -> setugt dst
    node->value = make_op(op_type::cmp_flags_float,v1,v2);
    node = insert_after(block.list,node,make_op(set,dst));

    return node->next;
}

OpcodeNode* rewrite_cmp_flag_imm(Block& block, OpcodeNode* node, op_type set)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto imm = node->value.v[2];

    // cmpsgt dst,v1,imm
    // -> cmp_flags_imm v1, imm
    // -> setsgt dst
    node->value = make_op(op_type::cmp_flags_imm,v1,imm);
    node = insert_after(block.list,node,make_op(set,dst));

    return node->next;
}

OpcodeNode* rewrite_no_imm(Function& func, Block& block,OpcodeNode* node, op_type type)
{
    const auto dst = node->value.v[0];
    const auto v1 = node->value.v[1];
    const auto imm = node->value.v[2];

    // mul dst, v1, imm
    // -> mov t0, imm
    // -> mul dst, v1, t0
    const auto tmp_slot = new_tmp(func,GPR_SIZE);
    const auto tmp = make_reg_operand(tmp_slot);

    node->value = make_op(op_type::mov_imm,tmp,imm);
    node = insert_after(block.list,node,make_op(type,dst,v1,tmp));

    // NOTE: another writing pass has to happen on this opcode
    return node;   
}

#include "rewrite_arch_x86.cpp"
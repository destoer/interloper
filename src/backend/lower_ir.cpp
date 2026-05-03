void insert_mov_reg2(Block& block, OpcodeNode* node, RegSlot dst, RegSlot src, reg_type type)
{
    Opcode opcode;
    opcode.unary_reg2 = make_unary_reg2(dst,src,type == reg_type::gpr_t? unary_reg2_op::mov_gpr_reg : unary_reg2_op::mov_fpr_reg);
    opcode.group = op_group::unary_reg2;

    insert_at(block.list,node,opcode);
}

template<typename op_type>
OpcodeNode* lower_reg3(Block& block, OpcodeNode* node, const RegThree<op_type>& reg3, RegTwoDst<op_type>* reg2, 
    op_group new_group, reg_type rtype, const u64 commutative_set) 
{
    const auto type = reg3.type;
    const auto dst = reg3.dst.ir;
    const auto v1 = reg3.v1.ir;
    const auto v2 = reg3.v2.ir;

    // add dst, dst, v2
    // -> add dst, v2
    if(dst == v1)
    {
        reg2->dst.ir = dst;
        reg2->src.ir = v2;
    }

    // add dst, v1, dst
    // -> add dst, v1
    else if(dst == v2 && is_set(commutative_set,u32(reg3.type)))
    {
        reg2->dst.ir = dst;
        reg2->src.ir = v1;
    }

    // sub dst, v1, v2
    // -> mov dst, v1
    // -> sub dst, v2
    else
    {
        insert_mov_reg2(block,node,dst,v1,rtype);

        reg2->dst.ir = dst;
        reg2->src.ir = v2;       
    }

    reg2->type = type;
    node->value.group = new_group;

    return node->next;
}

template<typename op_type>
OpcodeNode* lower_reg3_opt(Block& block, OpcodeNode* node, const RegThree<op_type>& reg3, RegTwoDst<op_type>* reg2, 
    op_group new_group, reg_type rtype, const u64 commutative_set) 
{
    // lower if it will crush the encoding
    if(reg3.dst.ir == reg3.v1.ir || (is_set(u32(reg3.type),commutative_set) && reg3.dst.ir == reg3.v2.ir))
    {
        return lower_reg3(block,node,reg3,reg2,new_group,rtype,commutative_set);
    }

    return node->next;
}

// TODO: Handle large imm's
template<typename type>
OpcodeNode* lower_imm3(Block& block, OpcodeNode* node, const ImmThree<type>& imm3, ImmTwoDst<type>* imm2, op_group new_group) 
{
    const auto dst = imm3.dst.ir;
    const auto src = imm3.src.ir;
    const auto imm = imm3.imm;

    // add dst, src, imm
    // -> mov dst, src
    // -> add dst, imm
    if(src != dst)
    {
        insert_mov_reg2(block,node,dst,src,reg_type::gpr_t);
    }

    // same dst can just rewrite here
    imm2->dst.ir = dst;
    imm2->imm = imm;

    imm2->type = imm3.type;
    node->value.group = new_group;

    return node->next;
}

template<typename type>
OpcodeNode* lower_imm3_opt(Block& block, OpcodeNode* node, const ImmThree<type>& imm3, ImmTwoDst<type>* imm2, op_group new_group) 
{
    // only lower if it would result in a shorter encoding
    if(imm3.dst.ir == imm3.src.ir)
    {
        return lower_imm3(block,node,imm3,imm2,new_group);
    }

    return node->next;
}

template<typename op_type, const reg_type RTYPE>
OpcodeNode* lower_reg3_cmp_flag(Block& block, OpcodeNode* node, const RegThree<op_type>& cmp3, UnaryReg1<op_type>* set,op_group group)
{

    const auto dst = cmp3.dst.ir;
    const auto v1 = cmp3.v1.ir;
    const auto v2 = cmp3.v2.ir;

    const auto type = cmp3.type;

    // cmpsgt dst,v1,v2
    // -> cmp_flags v1, v2
    // -> setsgt dst

    const auto rtype = RTYPE ==  reg_type::float_t? reg_two_src::cmp_flags_fpr : reg_two_src::cmp_flags_gpr;
    Opcode cmp_flags;
    cmp_flags.group = op_group::reg2_src;
    cmp_flags.reg2_src = make_reg2_src(v1,v2,rtype);
    insert_at(block.list,node,cmp_flags);

    auto& opcode = node->value;
    opcode.group = group;
    *set = make_unary_reg1(dst,type);

    return node->next;

}

OpcodeNode* lower_imm3_cmp_flag(Block& block, OpcodeNode* node)
{
    auto& opcode = node->value;

    const auto& cmp = opcode.cmp_imm3;
    const auto dst = cmp.dst.ir;
    const auto src = cmp.src.ir;
    const auto imm = cmp.imm;

    const auto type = cmp.type;

    // cmpsgt dst, src, imm
    // -> cmp_flags_imm src, imm
    // -> setsgt dst
    opcode.imm2_src = make_imm2_src(src,imm,imm_two_src::cmp_flags_imm);
    opcode.group = op_group::imm2_src;  

    Opcode set;
    set.group = op_group::set_from_flag_gpr;
    set.set_from_flag_gpr = make_unary_reg1(dst,type);

    node = insert_after(block.list,node,set);

    return node->next;
}


OpcodeNode* lower_no_imm(Function& func, Block& block,OpcodeNode* node)
{
    const auto &imm = node->value.arith_imm3;
    const auto dst = imm.dst.ir;
    const auto src = imm.src.ir;
    const auto value = imm.imm;
    const auto type = imm.type;

    // mul dst, src, imm
    // -> mov t0, imm
    // -> mul dst, src, t0
    const auto tmp_slot = new_tmp(func,GPR_SIZE);

    node->value = make_mov_imm(tmp_slot,value);

    Opcode opcode;
    opcode.arith_gpr3 = make_reg3(dst,src,tmp_slot,type);
    opcode.group = op_group::arith_gpr3;

    node = insert_after(block.list,node,opcode);

    // NOTE: another writing pass has to happen on this opcode
    return node;   
}

OpcodeNode* lower_unary_reg2(Block& block, OpcodeNode* node,unary_reg1_op type)
{
    const auto& unary = node->value.unary_reg2;
    const auto dst = unary.dst.ir; 
    const auto src = unary.dst.ir;

    if(src != dst)
    {
        insert_mov_reg2(block,node,dst,src,reg_type::gpr_t);
    }

    node->value.group = op_group::unary_reg1;
    node->value.unary_reg1 = make_unary_reg1(dst,type);

    return node->next;
}
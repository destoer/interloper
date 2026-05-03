OpcodeNode* insert_mov_reg2(Block& block, OpcodeNode* node, RegSlot dst, RegSlot src, reg_type type, insertion_type insert_type)
{
    Opcode opcode;
    const auto rtype = type == reg_type::gpr_t? unary_reg2_op::mov_gpr_reg : unary_reg2_op::mov_fpr_reg;
    opcode.unary_reg2 = make_unary_reg2<unary_reg2_op,op_group::unary_reg2>(dst,src,rtype);
    opcode.group = op_group::unary_reg2;

    return insert_node(block.list,node,opcode,insert_type);
}

OpcodeNode* insert_mov_reg2_at(Block& block, OpcodeNode* node, RegSlot dst, RegSlot src, reg_type type)
{
    return insert_mov_reg2(block,node,dst,src,type,insertion_type::before);
}

OpcodeNode* insert_mov_reg2_after(Block& block, OpcodeNode* node, RegSlot dst, RegSlot src, reg_type type)
{
    return insert_mov_reg2(block,node,dst,src,type,insertion_type::after);    
}



template<typename op_type,op_group group3,op_group group2>
OpcodeNode* lower_reg3(Function& func, Block& block, OpcodeNode* node, const RegThree<op_type,group3>& reg3, RegTwoDst<op_type,group2>* reg2, 
    reg_type rtype, const u64 commutative_set) 
{
    const auto type = reg3.type;
    const auto dst = reg3.dst.ir;
    const auto v1 = reg3.v1.ir;
    const auto v2 = reg3.v2.ir;

    reg2->type = type;

    // add dst, dst, v2
    // -> add dst, v2
    if(dst == v1)
    {
        reg2->dst.ir = dst;
        reg2->src.ir = v2;
        node->value.group = group2;
    }


    else if(dst == v2)
    {
        // add dst, v1, dst
        // -> add dst, v1
        if(is_set(commutative_set,u32(reg3.type)))
        {
            reg2->dst.ir = dst;
            reg2->src.ir = v1;
            node->value.group = group2;
        }

        // sub dst, v1, dst
        // -> mov t0, v1
        // -> sub t0, dst
        // -> mov dst, t0
        else
        {
            const auto tmp = rtype == reg_type::float_t? new_float(func) : new_tmp(func,GPR_SIZE);
            insert_mov_reg2_at(block,node,tmp,v1,rtype);

            reg2->dst.ir = tmp;
            reg2->src.ir = dst;
            node->value.group = group2;

            node = insert_mov_reg2_after(block,node,dst,tmp,rtype);
        }
    }

    // sub dst, v1, v2
    // -> mov dst, v1
    // -> sub dst, v2
    else
    {
        insert_mov_reg2_at(block,node,dst,v1,rtype);

        reg2->dst.ir = dst;
        reg2->src.ir = v2;     
        node->value.group = group2;
    }

    return node->next;
}

template<typename op_type,op_group group3,op_group group2>
OpcodeNode* lower_reg3_opt(Function& func,Block& block, OpcodeNode* node, const RegThree<op_type,group3>& reg3, RegTwoDst<op_type,group2>* reg2, 
    reg_type rtype, const u64 commutative_set) 
{
    // lower if it will crush the encoding
    if(reg3.dst.ir == reg3.v1.ir || (is_set(u32(reg3.type),commutative_set) && reg3.dst.ir == reg3.v2.ir))
    {
        return lower_reg3(func,block,node,reg3,reg2,rtype,commutative_set);
    }

    return node->next;
}

template<typename type,op_group group_imm3,op_group group2,op_group group_reg3>
OpcodeNode* lower_imm3(Function& func, Block& block, OpcodeNode* node, 
    const ImmThree<type,group_imm3>& imm3, ImmTwoDst<type,group2>* imm2, RegThree<type,group_reg3>* reg3) 
{
    const auto dst = imm3.dst.ir;
    const auto src = imm3.src.ir;
    const auto imm = imm3.imm;

    if(!fit_into_u32(imm))
    {
        const auto tmp = new_tmp(func,GPR_SIZE);
        insert_at(block.list,node,make_mov_imm(tmp,imm));
        make_reg3_opcode(node->value,reg3,dst,src,tmp,imm3.type);

        // Need another rewrite pass on the reg3 we have just written
        return node;
    }

    // add dst, src, imm
    // -> mov dst, src
    // -> add dst, imm
    if(src != dst)
    {
        insert_mov_reg2_at(block,node,dst,src,reg_type::gpr_t);
    }

    // same dst can just rewrite here
    imm2->dst.ir = dst;
    imm2->imm = imm;

    imm2->type = imm3.type;
    node->value.group = group2;

    return node->next;
}

template<typename type,op_group group_imm3,op_group group2,op_group group_reg3>
OpcodeNode* lower_imm3_opt(Function& func,
    Block& block, OpcodeNode* node, const ImmThree<type,group_imm3>& imm3, ImmTwoDst<type,group2>* imm2,RegThree<type,group_reg3>* reg3) 
{
    // only lower if it would result in a shorter encoding
    if(imm3.dst.ir == imm3.src.ir || !fit_into_u32(imm3.imm))
    {
        return lower_imm3(func,block,node,imm3,imm2,reg3);
    }

    return node->next;
}

template<typename op_type, const reg_type RTYPE,op_group group2,op_group group1>
OpcodeNode* lower_reg3_cmp_flag(Block& block, OpcodeNode* node, const RegThree<op_type,group2>& cmp3, UnaryReg1<op_type,group1>* set)
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
    opcode.group = group1;
    *set = make_unary_reg1<op_type,group1>(dst,type);

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
    make_unary_reg1_opcode(set,&set.set_from_flag_gpr,dst,type);

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
    make_reg3_opcode(opcode,&opcode.arith_gpr3,dst,src,tmp_slot,type);

    // NOTE: another writing pass has to happen on this opcode
    return insert_after(block.list,node,opcode);
}

OpcodeNode* lower_unary_reg2(Block& block, OpcodeNode* node,unary_reg1_op type)
{
    const auto& unary = node->value.unary_reg2;
    const auto dst = unary.dst.ir; 
    const auto src = unary.src.ir;

    if(src != dst)
    {
        insert_mov_reg2_at(block,node,dst,src,reg_type::gpr_t);
    }

    node->value.group = op_group::unary_reg1;
    node->value.unary_reg1 = make_unary_reg1<unary_reg1_op,op_group::unary_reg1>(dst,type);

    return node->next;
}

OpcodeNode* lower_fpr_const(Interloper& itl, Block& block, OpcodeNode* node)
{
    UNUSED(block);

    const auto& mov = node->value.mov_fpr_imm;
    const auto dst = mov.dst.ir;
    const auto decimal = mov.imm;

    // dump float in the const pool table so we can do a relative load
    const auto pool_slot = push_const_pool(itl.const_pool,pool_type::var,&decimal,sizeof(f64));

    auto& opcode = node->value;

    opcode.group = op_group::directive;

    Directive directive;
    directive.type = directive_type::load_const_float;
    directive.operand[0] = make_reg_operand(dst,ir_reg_type::dst);
    directive.operand[1] = make_pool_operand(pool_slot);
    directive.operand[2] = make_decimal_operand(decimal);
    directive.size = 3;

    opcode.directive = directive;

    return node->next;
}

OpcodeNode* lower_push_float_arg(Block& block, OpcodeNode* node)
{
    auto& directive = node->value.directive;
    const auto src = directive.operand[0].reg;

    const auto alloc_stack = make_directive_imm1(directive_type::alloc_stack,8);
    directive = alloc_stack;

    const auto addr = make_addr(make_spec_reg_slot(spec_reg::sp),0);
    const auto store = make_store(src,addr,store_type::sf);
    node = insert_after(block.list,node,store);

    return node->next;
}
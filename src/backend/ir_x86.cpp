void insert_mov_reg2(Block& block, OpcodeNode* node, RegSlot dst, RegSlot src, reg_type type)
{
    Opcode opcode;
    opcode.unary_reg2 = make_unary_reg2(dst,src,type == reg_type::gpr_t? unary_reg_op::mov_gpr_reg : unary_reg_op::mov_fpr_reg);
    opcode.group = op_group::unary_reg2;

    insert_at(block.list,node,opcode);
}

template<typename type>
OpcodeNode* lower_reg3(Block& block, OpcodeNode* node, const RegThree<type>& reg3, RegTwoDst<type>* reg2, 
    op_group new_group, reg_type rtype, const u64 commutative_set) 
{
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

    // add dst, v1, v2
    // -> mov dst, v1
    // -> add dst, v2
    else
    {
        insert_mov_reg2(block,node,dst,v1,rtype);

        reg2->dst.ir = dst;
        reg2->src.ir = v2;       
    }

    reg2->type = reg3.type;
    node->value.group = new_group;

    return node->next;
}


// TODO: Handle large imm's and opcodes with no immediate encoding
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


OpcodeNode* set_from_flag(Block& block, OpcodeNode* node, RegSlot dst, cmp_sign_op type)
{
    Opcode set;
    set.set_from_flag = make_unary_reg1(dst,type);
    set.group = op_group::set_from_flag;

    node = insert_after(block.list,node,set);

    return node->next;    
}

OpcodeNode* lower_reg3_cmp_flag(Block& block, OpcodeNode* node)
{
    auto& opcode = node->value;

    const auto& cmp = opcode.cmp_gpr3;
    const auto dst = cmp.dst.ir;
    const auto v1 = cmp.v1.ir;
    const auto v2 = cmp.v2.ir;

    const auto type = cmp.type;

    // cmpsgt dst,v1,v2
    // -> cmp_flags v1, v2
    // -> setsgt dst
    opcode.reg2_src = make_reg2_src(v1,v2,reg_two_src::cmp_flags_gpr);
    opcode.group = op_group::reg2_src;  

    return set_from_flag(block,node,dst,type);
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

    return set_from_flag(block,node,dst,type);
}



OpcodeNode* rewrite_x86_opcode(Interloper& itl, Function& func, Block& block,OpcodeNode* node)
{
    UNUSED(itl); UNUSED(func);

    auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::arith_gpr3:
        { 
            return lower_reg3(block,node,opcode.arith_gpr3, &opcode.arith_gpr2, op_group::arith_gpr2,reg_type::gpr_t,ARITH_BIN_COMMUTATIVE);
        }

        case op_group::arith_imm3:
        {
            return lower_imm3(block,node,opcode.arith_imm3,&opcode.arith_imm2,op_group::arith_imm2);
        }

        case op_group::shift_imm3:
        {
            return lower_imm3(block,node,opcode.shift_imm3,&opcode.shift_imm2,op_group::shift_imm2);
        }

        case op_group::cmp_gpr3:
        {
            return lower_reg3_cmp_flag(block,node);
        }

        case op_group::cmp_imm3:
        {
            return lower_imm3_cmp_flag(block,node);
        }

        case op_group::implicit: break;
        case op_group::branch_label: break;
        case op_group::branch_cond: break;
        case op_group::mov_gpr_imm: break;
        case op_group::mov_fpr_imm: break;
        case op_group::directive: break;
        case op_group::unary_reg2: break;
        case op_group::lea: break;
        case op_group::addrof: break;
        case op_group::load: break;
        case op_group::load_struct: break;
        case op_group::store: break;
        case op_group::store_struct: break;
        default: 
        {
            dump_ir(itl,func,itl.symbol_table);
            unimplemented("rewrite x86 group: %d",opcode.group);
            break;
        } 
    }

    return node->next;
}


void rewrite_x86_func(Interloper& itl, Function& func)
{
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        OpcodeNode* node = block.list.start;

        while(node)
        {
            node = rewrite_x86_opcode(itl,func,block,node);
        }
    }
}

void rewrite_x86_ir(Interloper& itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        rewrite_x86_func(itl,func);   
    }
}
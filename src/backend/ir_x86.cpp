OpcodeNode* lower_x86_cond_branch(Block& block, OpcodeNode* node)
{
    auto& branch = node->value.branch_cond;
    const auto slot = branch.src.ir;
    const auto cond = branch.type;
    const auto label = branch.label;

    Opcode opcode;
    opcode.group = op_group::branch_cond_flag;
    opcode.branch_cond_flag = BranchCondFlag { cond,label };
    node->value = opcode;

    Opcode test;
    test.reg2_src = make_reg2_src(slot,slot,reg_two_src::test);
    test.group = op_group::reg2_src;
    node = insert_at(block.list,node,test);
            
    return node->next;
}


OpcodeNode* rewrite_x86_opcode(Interloper& itl, Function& func, Block& block,OpcodeNode* node)
{
    UNUSED(itl);

    auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::arith_gpr3:
        { 
            // Do not have to lower add
            if(opcode.arith_gpr3.type == arith_bin_op::add_t)
            {
                return node->next;
            }

            return lower_reg3(block,node,opcode.arith_gpr3, &opcode.arith_gpr2, op_group::arith_gpr2,reg_type::gpr_t,ARITH_GPR_COMMUTATIVE);
        }

        case op_group::arith_fpr3:
        { 
            return lower_reg3(block,node,opcode.arith_fpr3, &opcode.arith_fpr2, op_group::arith_fpr2,reg_type::float_t,ARITH_FPR_COMMUTATIVE);
        }

        case op_group::shift_reg3:
        {
            return lower_reg3(block,node,opcode.shift_reg3, &opcode.shift_reg2, op_group::shift_reg2,reg_type::gpr_t,0);
        }

        case op_group::arith_imm3:
        {
            switch(opcode.arith_imm3.type)
            {
                case arith_bin_op::add_t:  return node->next;
                case arith_bin_op::mul_t: return lower_no_imm(func,block,node);
                default: return lower_imm3(block,node,opcode.arith_imm3,&opcode.arith_imm2,op_group::arith_imm2);
            } 
        }

        case op_group::shift_imm3:
        {
            return lower_imm3(block,node,opcode.shift_imm3,&opcode.shift_imm2,op_group::shift_imm2);
        }

        case op_group::cmp_gpr3:
        {
            return lower_reg3_cmp_flag(block,node,reg_type::gpr_t);
        }

        case op_group::cmp_fpr3:
        {
            return lower_reg3_cmp_flag(block,node,reg_type::float_t);
        }

        case op_group::cmp_imm3:
        {
            return lower_imm3_cmp_flag(block,node);
        }

        case op_group::branch_cond:
        {
            return lower_x86_cond_branch(block,node);
        }

        case op_group::branch_cond_flag: break;
        case op_group::implicit: break;
        case op_group::branch_label: break;
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
        case op_group::branch_reg: break;
        case op_group::arith_gpr2: break;
        case op_group::arith_fpr2: break;
        case op_group::shift_reg2: break;
        case op_group::shift_imm2: break;
        case op_group::sign_extend: break;
        case op_group::reg2_src: break;
        case op_group::imm2_src: break;
        case op_group::set_from_flag: break;
        case op_group::arith_imm2: break;
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
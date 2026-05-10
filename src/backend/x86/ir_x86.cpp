OpcodeNode* lower_x86_cond_branch_ir(Block& block, OpcodeNode* node)
{
    auto& branch = node->value.branch_cond;
    const auto slot = branch.src.ir;
    const auto cond = branch.type;
    const auto label = branch.label;

    node->value = Opcode(BranchCondFlag { cond,label });

    const auto test = Opcode(make_reg2_src(slot,slot,reg_two_src::test));
    node = insert_at(block.list,node,test);
            
    return node->next;
}


template<typename op_type,op_group group>
OpcodeNode* lower_x86_fixed_ir(Block& block, OpcodeNode* node, const RegThree<op_type,group>& reg, x86_fixed_type fixed)
{
    const auto dst = reg.dst.ir;
    const auto v1 = reg.v1.ir;
    const auto v2 = reg.v2.ir;

    insert_mov_reg2_at(block,node,dst,v1,reg_type::gpr);

    node->value = Opcode(make_reg2_dst<op_group::x86_fixed>(dst,v2,fixed));

    return node->next;
}

OpcodeNode* rewrite_x86_opcode(Interloper& itl, Function& func, Block& block,OpcodeNode* node)
{
    auto& opcode = node->value;

    switch(opcode.group)
    {
        case op_group::arith_gpr3:
        {
            switch(opcode.arith_gpr3.type)
            {
                case arith_bin_op::add_t: 
                {
                    return lower_reg3_opt_ir<op_group::arith_gpr2>(func,block,node,opcode.arith_gpr3,reg_type::gpr,ARITH_GPR_COMMUTATIVE);
                }

                case arith_bin_op::udiv_t: return lower_x86_fixed_ir(block,node,opcode.arith_gpr3,x86_fixed_type::udiv);
                case arith_bin_op::sdiv_t: return lower_x86_fixed_ir(block,node,opcode.arith_gpr3,x86_fixed_type::sdiv);
                case arith_bin_op::smod_t: return lower_x86_fixed_ir(block,node,opcode.arith_gpr3,x86_fixed_type::smod);
                case arith_bin_op::umod_t: return lower_x86_fixed_ir(block,node,opcode.arith_gpr3,x86_fixed_type::umod);

                default: return lower_reg3_ir<op_group::arith_gpr2>(func,block,node,opcode.arith_gpr3,reg_type::gpr,ARITH_GPR_COMMUTATIVE);
            }

            break;
        }

        case op_group::mov_fpr_imm:
        {
            return lower_fpr_const_ir(itl,block,node);
        }

        case op_group::arith_fpr3:
        { 
            return lower_reg3_ir<op_group::arith_fpr2>(func,block,node,opcode.arith_fpr3,reg_type::fpr,ARITH_FPR_COMMUTATIVE);
        }


        case op_group::shift_reg3:
        {
            switch(opcode.shift_reg3.type)
            {
                case shift_op::lsr: return lower_x86_fixed_ir(block,node,opcode.shift_reg3,x86_fixed_type::lsr);
                case shift_op::lsl: return lower_x86_fixed_ir(block,node,opcode.shift_reg3,x86_fixed_type::lsl);
                case shift_op::asr: return lower_x86_fixed_ir(block,node,opcode.shift_reg3,x86_fixed_type::asr);
            }

            break;
        }

        case op_group::arith_imm3:
        {
            switch(opcode.arith_imm3.type)
            {
                case arith_bin_op::add_t: return lower_imm3_opt_ir<op_group::arith_imm2,op_group::arith_gpr3>(func,block,node,opcode.arith_imm3);
                case arith_bin_op::mul_t: return lower_no_imm_ir(func,block,node);
                default: return lower_imm3_ir<op_group::arith_imm2,op_group::arith_gpr3>(func,block,node,opcode.arith_imm3);
            } 

            break;
        }

        case op_group::shift_imm3:
        {
            return lower_imm3_ir<op_group::shift_imm2,op_group::shift_reg3>(func,block,node,opcode.shift_imm3);
        }

        case op_group::cmp_gpr3:
        {
            return lower_reg3_cmp_flag_ir<op_group::set_from_flag_gpr,cmp_sign_op,reg_type::gpr>(block,node,opcode.cmp_gpr3);
        }

        case op_group::cmp_fpr3:
        {
            return lower_reg3_cmp_flag_ir<op_group::set_from_flag_fpr,comparison_op,reg_type::fpr>(block,node,opcode.cmp_fpr3);
        }

        case op_group::cmp_imm3:
        {
            return lower_imm3_cmp_flag_ir(block,node);
        }

        case op_group::branch_cond:
        {
            return lower_x86_cond_branch_ir(block,node);
        }

        case op_group::unary_reg2:
        {
            switch(opcode.unary_reg2.type)
            {
                case unary_reg2_op::bitwise_not: return lower_unary_reg2_ir(block,node,unary_reg1_op::bitwise_not);  
                default: return node->next;
            }
        }

        case op_group::directive:
        {
            switch(opcode.directive.type)
            {
                case directive_type::push_float_arg: return lower_push_float_arg_ir(block,node);
                default: return node->next;
            }
        }



        case op_group::branch_cond_flag: break;
        case op_group::implicit: break;
        case op_group::branch_label: break;
        case op_group::mov_gpr_imm: break;
        case op_group::unary_reg1: break;
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
        case op_group::reg1_src: break;
        case op_group::imm2_src: break;
        case op_group::set_from_flag_gpr: break;
        case op_group::set_from_flag_fpr: break;
        case op_group::arith_imm2: break;
        case op_group::x86_fixed: break;
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
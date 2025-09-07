OpcodeNode* rewrite_x86_cond_branch(Block& block, OpcodeNode* node, b32 if_true)
{
    const auto slot = node->value.v[0];
    const auto cond = node->value.v[1];

    node->value = make_op(op_type::test,cond,cond);
    node = insert_after(block.list,node,make_op(if_true? op_type::jne : op_type::je,slot));
            
    return node->next;
}

OpcodeNode* rewrite_x86_fixed(Block& block, OpcodeNode* node, op_type op)
{
    const auto dst = node->value.v[0];
    insert_at(block.list,node,make_op(op_type::mov_reg,dst,node->value.v[1]));
    node->value = make_op(op,dst,node->value.v[2]);

    return node->next;
}

// TODO: we need a mechanism for rewriting large imm
// on RISC ISA
OpcodeNode* rewrite_x86_opcode(Interloper& itl, Function& func, Block& block,OpcodeNode* node)
{
    const auto& opcode = node->value;

    switch(opcode.op)
    {
        case op_type::cmpslt_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setslt);
        }

        case op_type::cmpsle_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setsle);
        }

        case op_type::cmpsgt_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setsgt);
        }

        case op_type::cmpsge_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setsge);
        }

        case op_type::cmpult_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setult);
        }

        case op_type::cmpule_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setule);
        }

        case op_type::cmpugt_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setugt);
        }

        case op_type::cmpuge_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setuge);
        }

        case op_type::cmpugt_imm:
        {
            return rewrite_cmp_flag_imm(block,node,op_type::setugt);
        }

        case op_type::cmpeq_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::seteq);
        }

        case op_type::cmpne_reg:
        {
            return rewrite_cmp_flag_reg(block,node,op_type::setne);
        }

        case op_type::cmpne_imm:
        {
            return rewrite_cmp_flag_imm(block,node,op_type::setne);
        }

        case op_type::cmpsgt_imm:
        {
            return rewrite_cmp_flag_imm(block,node,op_type::setsgt);
        }

        case op_type::cmpeq_imm:
        {
            return rewrite_cmp_flag_imm(block,node,op_type::seteq);
        }

        case op_type::add_reg: 
        {
            // Can rewrite to two addresses without an extra instr
            if(node->value.v[0] == node->value.v[1] || node->value.v[0] == node->value.v[2])
            {
                return rewrite_reg3_two_commutative(block,node,op_type::add_reg2,reg_type::gpr_t);
            }

            return node->next;
        }

        case op_type::add_imm:
        {
            if(node->value.v[0] == node->value.v[1])
            {
                return rewrite_imm3_two(block,node,op_type::add_imm2);
            }

            return node->next;
        }

        case op_type::and_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::and_reg2,reg_type::gpr_t);
        }

        case op_type::lsl_imm:
        {
            return rewrite_imm3_two(block,node,op_type::lsl_imm2);
        }

        case op_type::lsr_imm:
        {
            return rewrite_imm3_two(block,node,op_type::lsr_imm2);
        }

        case op_type::and_imm:
        {
            return rewrite_imm3_two(block,node,op_type::and_imm2);
        }

        case op_type::xor_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::xor_reg2,reg_type::gpr_t);
        }

        case op_type::xor_imm: 
        {
            return rewrite_imm3_two(block,node,op_type::xor_imm2);
        } 

        case op_type::or_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::or_reg2,reg_type::gpr_t);
        }

        case op_type::not_reg:
        {
            return rewrite_reg2_one(block,node,op_type::not_reg1);
        }

        case op_type::lsl_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::lsl_x86);
        }

        case op_type::asr_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::asr_x86);
        }

        case op_type::lsr_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::lsr_x86);
        }

        case op_type::sub_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::sub_reg2,reg_type::gpr_t);
        }

        case op_type::sub_imm:
        {
            return rewrite_imm3_two(block,node,op_type::sub_imm2);
        }

        case op_type::udiv_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::udiv_x86);
        }

        case op_type::sdiv_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::sdiv_x86);
        }

        case op_type::umod_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::umod_x86);
        }

        case op_type::smod_reg:
        {
            return rewrite_x86_fixed(block,node,op_type::smod_x86);
        }

        case op_type::mul_reg:
        {
            return rewrite_reg3_two_commutative(block,node,op_type::mul_reg2,reg_type::gpr_t);
        }

        case op_type::mul_imm:
        {
            return rewrite_no_imm(func,block,node,op_type::mul_reg);
        }

        case op_type::bnc:
        {
            return rewrite_x86_cond_branch(block,node,false);
        }

        case op_type::bc:
        {
            return rewrite_x86_cond_branch(block,node,true);
        }

        case op_type::addf_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::addf_reg2,reg_type::float_t);
        }
    
        case op_type::subf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::subf_reg2,reg_type::float_t);
        }

        case op_type::mulf_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::mulf_reg2,reg_type::float_t);
        }

        case op_type::divf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::divf_reg2,reg_type::float_t);
        }

        case op_type::movf_imm:
        {
            // dump float in the const pool table so we can do a relative load
            const auto pool_slot = push_const_pool(itl.const_pool,pool_type::var,&opcode.v[1].decimal,sizeof(f64));
            node->value = make_op(op_type::load_const_float,opcode.v[0],make_raw_operand(pool_slot.handle),opcode.v[1]);
            return node->next;
        }

        case op_type::cmpflt_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setflt);
        }

        case op_type::cmpfle_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setfle);
        }

        case op_type::cmpfgt_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setfgt);
        }

        case op_type::cmpfge_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setfge);
        }

        case op_type::cmpfeq_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setfeq);
        }

        case op_type::cmpfne_reg:
        {
            return rewrite_cmp_flag_float(block,node,op_type::setfne);
        }

        case op_type::push_float_arg:
        {
            const auto src = node->value.v[0];

            node->value = make_raw_op(op_type::alloc_stack,8);
            node = insert_after(block.list,node,make_addr_op(op_type::sf,src,make_spec_operand(spec_reg::sp),make_spec_operand(spec_reg::null),1,0));
            break;
        }

        // Anything that has not been written will get caught by the emitter
        default: break;
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
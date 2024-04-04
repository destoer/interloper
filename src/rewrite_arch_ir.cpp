
// Do a top level rewrite of the IR opcode so
// The machine code translator is basically a 1 to 1 mapping
// outside of optimal instruction selection

ListNode* rewrite_reg3_two_commutative(Block& block, ListNode* node,op_type type)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // add dst, dst, v2
    // -> add dst, v2
    if(dst == v1)
    {
        node->opcode = Opcode(type,dst,v2,0);
    }

    // add dst, v1, dst
    // -> add dst, v1
    else if(dst == v2)
    {
        node->opcode = Opcode(type,dst,v1,0);
    }

    // add dst, v1, v2
    // -> mov dst, v1
    // -> add dst, v2
    else
    {
        node->opcode = Opcode(op_type::mov_reg,dst,v1,0);
        node = insert_after(block.list,node,Opcode(type,dst,v2,0));
    }

    return node->next;
}

ListNode* rewrite_imm3_two(Block& block, ListNode* node,op_type type)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto imm = node->opcode.v[2];

    // add dst, dst, imm
    // -> add dst, imm
    if(dst == v1)
    {
        node->opcode = make_op(type,dst,imm);
    }

    // add dst, v1, imm
    // -> mov dst, v1
    // -> add dst, imm
    else
    {
        node->opcode = Opcode(op_type::mov_reg,dst,v1,0);
        node = insert_after(block.list,node,make_op(type,dst,imm));
    }

    return node->next;
}

ListNode* rewrite_reg3_two(Function& func, Block& block, ListNode* node,op_type type)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // sub dst, dst, v2
    // -> sub dst, v2
    if(dst == v1)
    {
        node->opcode = Opcode(type,dst,v2,0);
    }

    // sub dst, v1, dst
    // -> mov t0, v1
    // -> sub t0, dst
    // -> mov dst, t0
    else if(dst == v2)
    {
        const auto tmp = new_tmp(func,GPR_SIZE);
        node->opcode = Opcode(op_type::mov_reg,tmp.handle,v1,0);
        node = insert_after(block.list,node,Opcode(type,tmp.handle,v2,0));
        node = insert_after(block.list,node,Opcode(op_type::mov_reg,dst,tmp.handle,0));
    }

    // sub dst, v1, v2
    // -> mov dst, v1
    // -> sub dst, v2
    else
    {
        node->opcode = Opcode(op_type::mov_reg,dst,v1,0);
        node = insert_after(block.list,node,Opcode(type,dst,v2,0));
    }

    return node->next;
}

ListNode* rewrite_reg2_one(Block& block, ListNode* node,op_type type)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];

    node->opcode = Opcode(op_type::mov_reg,dst,v1,0);
    node = insert_after(block.list,node,make_op(type,dst));

    return node->next;
}

void assert_bound(u64 v,u64 min, u64 max)
{
    assert(in_range(v,min,max));
}

// TODO: this assumes we have no access to the instruction
void emit_popm(Interloper& itl, Block& block, ListNode* node, u32 bitset)
{
    UNUSED(itl);

    for(s32 i = MACHINE_REG_SIZE - 1; i >= 0; i--)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,Opcode(op_type::pop,i,0,0));
            node = node->next;
        }
    }
}

void emit_pushm(Interloper& itl, Block& block, ListNode* node,u32 bitset)
{
    UNUSED(itl);

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,Opcode(op_type::push,i,0,0));
            node = node->next;
        }
    }
}

ListNode* x86_fixed_arith_oper(Interloper& itl, Function& func,Block& block, ListNode* node, op_type type, bool is_unsigned)
{
    // div dst, v1 , v2    
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // move in numerator
    insert_lock(itl,func,block,node,sym_from_idx(RAX_IR),false); 
    node->opcode = make_op(op_type::mov_reg,RAX_IR,v1);

    // make sure rdx is free and cannot be used for allocation
    node = insert_lock(itl,func,block,node,sym_from_idx(RDX_IR),true);          

    if(is_unsigned)
    {
        // sign extend rax into rdx
        node = insert_after(block.list,node,make_op(op_type::mov_imm,RDX_IR,0));
    }

    else
    {
        // sign extend rax into rdx
        node = insert_after(block.list,node,make_op(op_type::cqo));
    }

    // perform the operation!
    // NOTE: the operation should unlock both registers
    node = insert_after(block.list,node,make_op(type,dst,v2));

    return node->next;
}



ListNode* x86_shift(Interloper& itl, Function& func,Block& block, ListNode* node, op_type type)
{
    // lsl dst, v1 , v2    
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // rewrite to
    // replace rcx, v2
    // mov dst, v1
    // lsl dst, rcx

    insert_lock(itl,func,block,node,sym_from_idx(RCX_IR),false); 

    node->opcode = make_op(op_type::mov_reg,RCX_IR,v2);
    node = insert_after(block.list,node,make_op(op_type::mov_reg,dst,v1));
    node = insert_after(block.list,node,make_op(type,dst,RCX_IR));

    return node->next;
}

ListNode* mul_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_fixed_arith_oper(itl,func,block,node,op_type::mul_x86,false);
}

ListNode* udiv_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_fixed_arith_oper(itl,func,block,node,op_type::udiv_x86,true);
}

ListNode* sdiv_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_fixed_arith_oper(itl,func,block,node,op_type::sdiv_x86,false);
}


ListNode* umod_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_fixed_arith_oper(itl,func,block,node,op_type::umod_x86,true);
}

ListNode* smod_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_fixed_arith_oper(itl,func,block,node,op_type::smod_x86,false);
}

ListNode* lsl_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_shift(itl,func,block,node,op_type::lsl_x86);
}

ListNode* asr_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_shift(itl,func,block,node,op_type::asr_x86);
}

ListNode* lsr_x86(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    return x86_shift(itl,func,block,node,op_type::lsr_x86);
}



ListNode* rewrite_cmp_flag_reg(Block& block, ListNode* node, op_type set)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // cmpsgt dst,v1,v2
    // -> cmp_flags v1, v2
    // -> setsgt dst
    node->opcode = make_op(op_type::cmp_flags,v1,v2);
    node = insert_after(block.list,node,make_op(set,dst));

    return node->next;
}

ListNode* rewrite_cmp_flag_imm(Block& block, ListNode* node, op_type set)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto imm = node->opcode.v[2];

    // cmpsgt dst,v1,imm
    // -> cmp_flags_imm v1, imm
    // -> setsgt dst
    node->opcode = make_op(op_type::cmp_flags_imm,v1,imm);
    node = insert_after(block.list,node,make_op(set,dst));

    return node->next;
}

ListNode* rewrite_no_imm(Function& func, Block& block,ListNode* node, op_type type)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto imm = node->opcode.v[2];

    // mul dst, v1, imm
    // -> mov t0, imm
    // -> mul dst, v1, t0
    const auto tmp = new_tmp(func,GPR_SIZE);
    node->opcode = make_op(op_type::mov_imm,tmp.handle,imm);
    node = insert_after(block.list,node,make_op(type,dst,v1,tmp.handle));

    // NOTE: another writing pass has to happen on this opcode
    return node;   
}

ListNode* rewrite_x86_cond_branch(Block& block, ListNode* node, b32 if_true)
{
    const u32 handle = node->opcode.v[0];
    const u32 cond = node->opcode.v[1];

    node->opcode = make_op(op_type::test,cond,cond);
    node = insert_after(block.list,node,make_op(if_true? op_type::jne : op_type::je,handle));
            
    return node->next;
}

// TODO: we need a mechanism for rewriting large imm
// on RISC ISA
ListNode* rewrite_three_address_code(Interloper& itl, Function& func, Block& block,ListNode* node)
{
    const auto& opcode = node->opcode;
    const auto& info = info_from_op(opcode);


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
            return rewrite_reg3_two_commutative(block,node,op_type::add_reg2);
        }

        case op_type::add_imm:
        {
            return rewrite_imm3_two(block,node,op_type::add_imm2);
        }

        case op_type::and_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::and_reg2);
        }

        case op_type::lsl_imm:
        {
            return rewrite_imm3_two(block,node,op_type::lsl_imm2);
        }

        case op_type::and_imm:
        {
            return rewrite_imm3_two(block,node,op_type::and_imm2);
        }

        case op_type::xor_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::xor_reg2);
        }

        case op_type::xor_imm: 
        {
            return rewrite_imm3_two(block,node,op_type::xor_imm2);
        } 

        case op_type::or_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::or_reg2);
        }

        case op_type::not_reg:
        {
            return rewrite_reg2_one(block,node,op_type::not_reg1);
        }

        case op_type::lsl_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return lsl_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::asr_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return asr_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::lsr_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return lsr_x86(itl,func,block,node); 
                }
            }
            break;
        }

        case op_type::sub_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::sub_reg2);
        }

        case op_type::sub_imm:
        {
            return rewrite_imm3_two(block,node,op_type::sub_imm2);
        }

        case op_type::udiv_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return udiv_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::sdiv_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return sdiv_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::umod_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return umod_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::smod_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return smod_x86(itl,func,block,node);
                }
            }
            break;
        }

        case op_type::mul_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return mul_x86(itl,func,block,node);
                }
            }
            break;
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
            return rewrite_reg3_two_commutative(block,node,op_type::addf_reg2);
        }
    
        case op_type::subf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::subf_reg2);
        }

        case op_type::mulf_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::mulf_reg2);
        }

        case op_type::divf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::divf_reg2);
        }

        case op_type::cvt_fi: break;
        case op_type::cvt_if: break;

        case op_type::call: break;
        case op_type::call_reg: break;
        case op_type::b: break;
        case op_type::b_reg: break;

        case op_type::mov_imm: break;
        case op_type::movf_imm: break;
        case op_type::mov_reg: break;

        case op_type::lb: break;
        case op_type::lh: break;
        case op_type::lw: break;
        case op_type::ld: break;

        case op_type::lsb: break;
        case op_type::lsh: break;
        case op_type::lsw: break;

        case op_type::sb: break;
        case op_type::sh: break;
        case op_type::sw: break;
        case op_type::sd: break;

        case op_type::lea: break;

        case op_type::sxb: break;
        case op_type::sxh: break;
        case op_type::sxw: break;

        case op_type::ret: break;
        case op_type::syscall: break;

        // TODO: any reloading directives may have to be rewritten during the 1st reg alloc pass
        // or any loads by here
        // if [reg, imm] referencing is not possible on the target arch (for how large the imm is)
        // i.e rewrite all the imm -> 0 or whatever bound is acceptable
        // and hard bake the addr into a register with an add
        // this should not pose a problem on x86 however as we should be able to insert arbitary offsetting
        // if need be

        default:
        {
            if(!is_directive(opcode.op))
            {
                printf("[REWRITE TAC]: unknown opcode: %s\n",info.fmt_string.buf);
                assert(false); 
            }
            break;
        }
    }

    return node->next;
}

// TODO: when these grow we should probbably move them elsewhere
ListNode* rewrite_x86_opcode(Interloper& itl, Function& func,Block& block, ListNode* node)
{
    // TODO: rewrite specific opcodes that aern't aviable in any form on x86 such as regm

    // crush the opcode down to two address code
    // TODO: when we change this over to code generation 
    // we want to save crushing opcodes we dont need to such as add
    ListNode* next = rewrite_three_address_code(itl,func,block,node);

    return next;
}


void rewrite_x86_func(Interloper& itl, Function& func)
{

    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];
        ListNode* node = block.list.start;

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
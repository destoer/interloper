
// Do a top level rewrite of the IR opcode so
// The machine code translator is basically a 1 to 1 mapping
// outside of optimal instruction selection

ListNode* rewrite_reg3_two_commutative(Block& block, ListNode* node,op_type type, b32 is_float)
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
        node->opcode = Opcode(is_float? op_type::movf_reg : op_type::mov_reg,dst,v1,0);
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

ListNode* rewrite_reg3_two(Function& func, Block& block, ListNode* node,op_type type, b32 is_float)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    const op_type mov_op = is_float? op_type::movf_reg : op_type::mov_reg;

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
        const auto tmp = is_float? new_float(func) : new_tmp(func,GPR_SIZE);
        node->opcode = Opcode(mov_op,tmp.handle,v1,0);
        node = insert_after(block.list,node,Opcode(type,tmp.handle,v2,0));
        node = insert_after(block.list,node,Opcode(mov_op,dst,tmp.handle,0));
    }

    // sub dst, v1, v2
    // -> mov dst, v1
    // -> sub dst, v2
    else
    {
        node->opcode = Opcode(mov_op,dst,v1,0);
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
ListNode* emit_popm(Interloper& itl, Block& block, ListNode* node, u32 bitset)
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

    return node;
}

ListNode* emit_pushm(Interloper& itl, Block& block, ListNode* node,u32 bitset)
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

    return node;
}

ListNode* emit_popm_float(Interloper& itl, Block& block, ListNode* node, u32 bitset)
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
            node = insert_at(block.list,node,Opcode(op_type::lf,i,sp,offset));
            node = node->next;
            offset -= FLOAT_SIZE;
        }
    }

    node = insert_at(block.list,node,make_op(op_type::add_imm2,sp,size));
    node = node->next;

    return node;
}

void emit_pushm_float(Interloper& itl, Block& block, ListNode* node,u32 bitset)
{
    if(!bitset)
    {
        return;
    }

    const u32 size = popcount(bitset) * FLOAT_SIZE;
    u32 offset = size - FLOAT_SIZE;

    const u32 sp = arch_sp(itl.arch);

    node = insert_at(block.list,node,make_op(op_type::sub_imm2,sp,size));
    node = node->next;

    for(u32 i = 0; i < MACHINE_REG_SIZE; i++)
    {
        if(is_set(bitset,i))
        {
            node = insert_at(block.list,node,Opcode(op_type::sf,i,sp,offset));
            node = node->next;
            offset -= FLOAT_SIZE;
        }
    }
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


ListNode* rewrite_cmp_flag_float(Block& block, ListNode* node, op_type set)
{
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // cmpfgt dst,v1,v2
    // -> cmp_flags_float v1, v2
    // -> setugt dst
    node->opcode = make_op(op_type::cmp_flags_float,v1,v2);
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

ListNode* rewrite_x86_fixed(Block& block, ListNode* node, op_type op)
{
    const auto dst = node->opcode.v[0];
    insert_at(block.list,node,make_op(op_type::mov_reg,dst,node->opcode.v[1]));
    node->opcode = make_op(op,dst,node->opcode.v[2]);

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
            return rewrite_reg3_two_commutative(block,node,op_type::add_reg2,false);
        }

        case op_type::add_imm:
        {
            return rewrite_imm3_two(block,node,op_type::add_imm2);
        }

        case op_type::and_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::and_reg2,false);
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
            return rewrite_reg3_two_commutative(block,node,op_type::xor_reg2,false);
        }

        case op_type::xor_imm: 
        {
            return rewrite_imm3_two(block,node,op_type::xor_imm2);
        } 

        case op_type::or_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::or_reg2,false);
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
                    return rewrite_x86_fixed(block,node,op_type::lsl_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::asr_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::lsr_x86);
                }
            }
            break;
        }

        case op_type::sub_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::sub_reg2,false);
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
                    return rewrite_x86_fixed(block,node,op_type::udiv_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::sdiv_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::umod_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::smod_x86);
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
                    return rewrite_x86_fixed(block,node,op_type::mul_x86);
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
            return rewrite_reg3_two_commutative(block,node,op_type::addf_reg2,true);
        }
    
        case op_type::subf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::subf_reg2,true);
        }

        case op_type::mulf_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::mulf_reg2,true);
        }

        case op_type::divf_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::divf_reg2,true);
        }

        case op_type::movf_imm:
        {
            const f64 v = bit_cast_to_f64(opcode.v[1]);

            // dump float in the const pool table so we can do a relative load
            const auto pool_slot = push_const_pool(itl.const_pool,pool_type::var,&v,sizeof(f64));
            node->opcode = make_op(op_type::load_const_float,opcode.v[0],pool_slot.handle,opcode.v[1]);
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
            const auto src = node->opcode.v[0];

            node->opcode = make_op(op_type::alloc_stack,8);
            node = insert_after(block.list,node,make_op(op_type::sf,src,SP_IR,0));
            break;
        }

        case op_type::cvt_fi: break;
        case op_type::cvt_if: break;
        case op_type::cmp_flags_float: break;

        case op_type::call: break;
        case op_type::call_reg: break;
        case op_type::b: break;
        case op_type::b_reg: break;

        case op_type::mov_imm: break;
        case op_type::mov_reg: break;

        case op_type::movf_reg: break;
        case op_type::lf: break;
        case op_type::sf: break;

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
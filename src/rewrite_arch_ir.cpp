
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

ListNode* x86_fixed_oper(Block& block, ListNode* node, op_type type, u32 out_reg)
{
    // div dst, v1 , v2    
    const auto dst = node->opcode.v[0];
    const auto v1 = node->opcode.v[1];
    const auto v2 = node->opcode.v[2];

    // move in numerator
    node->opcode = make_op(op_type::replace_reg,RAX_IR,v1);

    // make sure rdx is free
    node = insert_after(block.list,node,make_op(op_type::evict_reg,RDX_IR));            

    // sign extend rax into rdx
    node = insert_after(block.list,node,make_op(op_type::cqo));
    
    // perform the operation!
    node = insert_after(block.list,node,make_op(type,v2));

    // copy out the result
    node = insert_after(block.list,node,make_op(op_type::mov_reg,dst,out_reg));

    return node->next;
}


ListNode* mul_x86(Block& block, ListNode* node)
{
    return x86_fixed_oper(block,node,op_type::mul_x86,RAX_IR);
}

ListNode* div_x86(Block& block, ListNode* node)
{
    return x86_fixed_oper(block,node,op_type::div_x86,RAX_IR);
}


// TODO: we need a mechanism for rewriting large imm
// on RISC ISA
ListNode* rewrite_three_address_code(Interloper& itl, Function& func, Block& block,ListNode* node)
{
    const auto& opcode = node->opcode;
    const auto& info = info_from_op(opcode);

    // TODO: need to improve this to take advantage of the fact some operands are commutive
    // but ah well
    switch(opcode.op)
    {

        case op_type::add_reg: 
        {
            return rewrite_reg3_two_commutative(block,node,op_type::add_reg2);
        }

        case op_type::sub_reg: 
        {
            return rewrite_reg3_two(func,block,node,op_type::sub_reg2);
        }

        case op_type::div_reg:
        {
            switch(itl.arch)
            {
                case arch_target::x86_64_t:
                {
                    return div_x86(block,node);
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
                    return mul_x86(block,node);
                }
            }
            break;
        }

    /*
        case op_type::mul_reg:
        {
            return rewrite_reg3_two_commutative(block,node,op_type::mul_reg2);
        }
    */
        case op_type::call: break;
        case op_type::mov_imm: break;
        case op_type::mov_reg: break;
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
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            rewrite_x86_func(itl,func);
        }
    }
}
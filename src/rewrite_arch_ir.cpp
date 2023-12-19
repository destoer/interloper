
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

// TODO: we need a mechanism for rewriting large imm
// on RISC ISA
ListNode* rewrite_three_address_code(Interloper& itl, Function& func, Block& block,ListNode* node)
{
    UNUSED(itl); UNUSED(func); UNUSED(node); UNUSED(block);

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

        case op_type::call: break;
        case op_type::mov_imm: break;
        case op_type::mov_reg: break;
        case op_type::ret: break;
        case op_type::swi: break;

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
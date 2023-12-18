
// Do a top level rewrite of the IR opcode so
// The machine code translator is basically a 1 to 1 mapping
// outside of optimal instruction selection

ListNode* rewrite_three_address_code(Interloper& itl, Function& func, Block& block,ListNode* node)
{
    UNUSED(itl); UNUSED(func); UNUSED(node); UNUSED(block);

    const auto& opcode = node->opcode;
    const auto& info = info_from_op(opcode);

    // TODO: need to improve this to take advantage of the fact some operands are commutive
    // but ah well
    switch(info.group)
    {
        case op_group::reg_t:
        {
            switch(info.args)
            {

                

                default:
                {
                    assert(false);
                    break;
                }
            }
            break;
        }

        case op_group::regm_t:
        {
            assert(false);
            break;
        }

        case op_group::imm_t:
        {
            switch(info.args)
            {
                // no need to rewrite 2 operand instruction
                case 2:
                {
                    break;
                }
                
                default:
                {
                    assert(false);
                    break;
                }
            }
            break;
        }

        case op_group::store_t:
        {
            assert(false);
            break;
        }

        case op_group::load_t:
        {
            assert(false);
            break;
        }

        // No operands, there should be nothing to crush?
        case op_group::implicit_t:
        {
            break;
        }

        case op_group::branch_t:
        {
            assert(false);
            break;
        }


        case op_group::branch_reg_t:
        {
            assert(false);
            break;
        }

        case op_group::slot_t:
        {
            switch(opcode.op)
            {
                // information directive dont care
                case op_type::alloc_slot: break;


                default:
                {
                    assert(false);
                }
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
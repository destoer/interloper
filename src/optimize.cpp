#include <interloper.h>




struct RegValue
{
    u64 value = 0;
    SymSlot slot_value = {SYMBOL_NO_SLOT};

    b32 known_value = false;
    b32 known_slot = false;

    u32 uses = 0;
};

using ValueTable = HashTable<SymSlot, RegValue>;

// does not require an special handling
bool reg_local(const Reg& reg)
{
    return !is_aliased(reg) && reg.kind != reg_kind::global;
}

bool is_known_value(const RegValue& v1, const Reg& reg)
{
    return is_local_reg(reg) && v1.known_value;
}

bool is_known_value(Interloper& itl, Function& func,ValueTable& table, SymSlot slot)
{
    auto v1_opt = lookup(table,slot);

    if(!v1_opt)
    {
        return false;
    }

    auto& v1 = *v1_opt;
    auto& reg = reg_from_slot(itl,func,slot);

    return is_known_value(v1,reg);
}

// ignore slot inline for now
/*
bool is_known_slot(const RegValue& v1, const Reg& reg)
{
    return is_local_reg(reg) && v1.known_slot;
}

bool is_known_slot(ValueTable& table, SymSlot slot)
{
    return is_known_slot(v1,reg);
}
*/


// NOTE: slot invalidation is done in a common place outside this
void update_value(ValueTable& table, SymSlot slot, u64 ans)
{
    // cant prop special regs
    if(!is_var(slot))
    {
        return;
    }

    auto v1_opt = lookup(table,slot);

    if(v1_opt)
    {
        auto& v1 = *v1_opt;
        v1.known_value = true; 
        v1.value = ans;
    }

    // first time
    else
    {
        RegValue v1;
        v1.known_value = true;
        v1.value = ans;

        add(table,slot,v1);
    }
}

void invalidate_value(ValueTable& table, SymSlot slot)
{
    auto v1_opt = lookup(table,slot);

    if(v1_opt)
    {
        auto& v1 = *v1_opt;
        v1.known_value = false; 
    }    
}

void mark_use(ValueTable& table, SymSlot slot)
{
    if(!is_var(slot))
    {
        return;
    }

    auto v1_opt = lookup(table,slot);

    if(v1_opt)
    {
        auto& v1 = *v1_opt;
        v1.uses++;
    }

    // first time
    else
    {   
        RegValue v1;
        v1.uses++;
        add(table,slot,v1);
    }
}

u64 known_value(ValueTable& table, SymSlot slot)
{
    auto v1_opt = lookup(table,slot);

    return v1_opt->value;
}

Opcode mov_imm(const SymSlot dst, u64 v1)
{
    return Opcode(op_type::mov_imm,dst.handle,v1,0);
}

// update slot value and insert into instr
void inline_value(ListNode* node, SymSlot dst, ValueTable& reg_value, u64 ans)
{
    update_value(reg_value,dst,ans);
    node->opcode = mov_imm(dst,ans);    
}

void propagate_values(Interloper& itl, Function& func, Block& block, ValueTable& reg_value)
{
    ListNode *node = block.list.start;

    // forward pass to attempt to propagate values
    while(node)
    {
        auto& info = info_from_op(node->opcode);

        bool removed = false;

        // check if we known both operands of our instruction
        bool can_compute_dst = false;
        const bool is_dst = info.type[0] == arg_type::dst_reg;
        const auto dst = sym_from_idx(node->opcode.v[0]);

        // saved values 
        // NOTE: only valid when can_compute_dst is true
        u64 v1 = 0;
        u64 v2 = 0;
        b32 v2_known = false;
        b32 v1_known = false;

        UNUSED(v1); UNUSED(v2);

        if(is_dst)
        {
            const auto s1 = sym_from_idx(node->opcode.v[1]);
            const auto s2 = sym_from_idx(node->opcode.v[2]);

            // reg3
            if(info.args == 3 && info.group == op_group::reg_t)
            {
                v1_known = is_known_value(itl,func,reg_value,s1);
                v2_known = is_known_value(itl,func,reg_value,s2);

                can_compute_dst = v1_known && v2_known;

                if(v1_known)
                {
                    v1 = known_value(reg_value,s1);
                }

                if(v2_known)
                {
                    v2 = known_value(reg_value,s2);
                }
            }

            // reg2
            else if(info.args == 2 && info.group == op_group::reg_t)
            {
                v1_known = is_known_value(itl,func,reg_value,s1);
                can_compute_dst = v1_known;
                
                if(v1_known)
                {
                    v1 = known_value(reg_value,s1);
                }
            }


            // imm3
            else if(info.args == 3 && info.group == op_group::imm_t)
            {
                v1_known = is_known_value(itl,func,reg_value,s1);
                can_compute_dst = v1_known;

                if(v1_known)
                {
                    v1 = known_value(reg_value,s1);
                    v2 = node->opcode.v[2];
                }
            }

        }

        else
        {

            const auto s1 = sym_from_idx(node->opcode.v[1]);
            const auto s2 = sym_from_idx(node->opcode.v[2]);

            UNUSED(s2);

            // branch
            if(info.args == 2 && info.group == op_group::branch_t)
            {
                v1_known = is_known_value(itl,func,reg_value,s1);
   
                if(v1_known)
                {
                    v1 = known_value(reg_value,s1);
                }          
            }
        }

        switch(node->opcode.op)
        {
            case op_type::mov_imm:
            {
                if(is_var(dst))
                {
                    update_value(reg_value,dst,node->opcode.v[1]);
                    can_compute_dst = true;
                }
                break;
            }

            case op_type::mov_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1);
                }
                break;
            }

            case op_type::add_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 + v2);
                }

                // we can still simplify one operand known
                else if(v2_known)
                {
                    node->opcode = Opcode(op_type::add_imm,node->opcode.v[0],node->opcode.v[1],v2);
                }

                else if(v1_known)
                {
                    node->opcode = Opcode(op_type::add_imm,node->opcode.v[0],node->opcode.v[2],v1);
                }

                break;
            }

            case op_type::add_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 + v2);
                }

                break;
            }

            case op_type::sub_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 - v2);
                }

                // we can still simplify one operand known
                else if(v2_known)
                {
                    node->opcode = Opcode(op_type::sub_imm,node->opcode.v[0],node->opcode.v[1],v2);
                }

                break;
            }

            case op_type::sub_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 - v2);
                }
                break;
            }

            case op_type::mul_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 * v2);
                }
                break;
            }

            case op_type::mul_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 * v2);
                }
                break;        
            }
        
            case op_type::div_reg:
            {
                if(can_compute_dst)
                {
                    if(v2 != 0)
                    {
                        inline_value(node,dst,reg_value,v1 / v2);
                    }

                    else
                    {
                        can_compute_dst = false;
                    }
                }
                break;
            }
        
            case op_type::and_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 & v2);
                }

                else if(v1_known)
                {
                    node->opcode = Opcode(op_type::and_imm,node->opcode.v[0],node->opcode.v[2],v1);
                }

                else if(v2_known)
                {
                    node->opcode = Opcode(op_type::and_imm,node->opcode.v[0],node->opcode.v[1],v2);
                }

                break;
            }

            case op_type::and_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 & v2);
                }

                break;
            }

            // signed compare
            case op_type::cmpsgt_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,s64(v1) > s64(v2));
                }
                break;                
            }

            case op_type::cmpslt_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,s64(v1) < s64(v2));
                }
                break;
            }

            case op_type::cmpsle_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,s64(v1) <= s64(v2));
                }
                break;
            }

            case op_type::cmpsgt_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,s64(v1) > s64(v2));
                }
                break;
            }

            case op_type::cmpsge_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,s64(v1) >= s64(v2));
                }
                break;
            }

            // unsigned compare
            case op_type::cmpugt_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 > v2);
                }
                break;                
            }

            case op_type::cmpult_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 < v2);
                }
                break;
            }

            case op_type::cmpule_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 <= v2);
                }
                break;
            }

            case op_type::cmpugt_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 > v2);
                }
                break;
            }

            case op_type::cmpuge_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 >= v2);
                }
                break;
            }


            // compare equality
            case op_type::cmpeq_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 == v2);
                }
                break;
            }

            case op_type::cmpne_reg:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 != v2);
                }
                break;
            }

            case op_type::cmpeq_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 == v2);
                }
                break;
            }

            case op_type::cmpne_imm:
            {
                if(can_compute_dst)
                {
                    inline_value(node,dst,reg_value,v1 != v2);
                }
                break;
            }

            case op_type::ret:
            {
                // once we return nothing else should be executed in the block
                node->next = nullptr;
                break;
            }

            case op_type::bc:
            {
                if(can_compute_dst)
                {
                    // allways taken
                    if(v1)
                    {
                        
                        // remove fall
                        remove_block_exit(func,block.block_slot,block_from_idx(block.block_slot.handle + 1));
                        node->opcode.op = op_type::b;
                    }

                    // never taken
                    else
                    {
                        // remove target
                        const auto target_label = label_from_idx(node->opcode.v[0]);
                        const auto exit = block_from_label(itl,target_label);
                        remove_block_exit(func,block.block_slot,exit);

                        node = remove(block.list,node);
                        removed = true;
                    }
                }
                break;
            }

            case op_type::bnc:
            {
                if(can_compute_dst)
                {
                    // never taken
                    if(v1)
                    {
                        // remove target
                        const auto target_label = label_from_idx(node->opcode.v[0]);
                        const auto exit = block_from_label(itl,target_label);
                        remove_block_exit(func,block.block_slot,exit);

                        node = remove(block.list,node);
                        removed = true;              
                    }

                    // allways taken
                    else
                    {
                        // remove fall
                        remove_block_exit(func,block.block_slot,block_from_idx(block.block_slot.handle + 1));

                        node->opcode.op = op_type::b;
                    }
                }
                break;
            } 
        
            default: 
            {
                // if we have not actually computed a value than we no longer know it
                can_compute_dst = false;
                break;
            }
        }

        // no longer know the result of the instr
        // must invalidate the value
        if(is_dst && is_var(dst) && !can_compute_dst)
        {
            invalidate_value(reg_value,dst);
        }

        if(!removed)
        {
            // info has changed repull it
            auto& post_info = info_from_op(node->opcode);

            // mark any uses (NOTE: we do this after inlining has happend)
            for(u32 a = 0; a < post_info.args; a++)
            {
                if(post_info.type[a] == arg_type::src_reg)
                {
                    mark_use(reg_value,sym_from_idx(node->opcode.v[a]));
                }
            }

            // inline any src values we can

            // finally 
            // if dst written
            // invalidate known slot values
            if(post_info.type[0] == arg_type::dst_reg)
            {

            }

            node = node->next;
        }
    }
}

void remove_dead_stores(Interloper& itl, Function& func, Block& block, ValueTable& reg_value)
{
    ListNode *node = block.list.start;

    //printf("L%d\n",block.label_slot.handle);

    while(node)
    {
        const auto opcode = node->opcode;
        const auto& info = info_from_op(opcode);

        if(info.type[0] == arg_type::dst_reg)
        {
            const auto dst = sym_from_idx(opcode.v[0]);
            const auto v1_opt = lookup(reg_value,dst);

            if(v1_opt)
            {
                auto& v1 = *v1_opt;
                auto& ir_reg = reg_from_slot(itl,func,dst);

                bool used_beyond = contains(block.live_out,dst); 

                // result of instruciton unused anywhere
                if(v1.uses == 0 && is_local_reg(ir_reg) && !used_beyond)
                {
                    //disass_opcode_sym(node->opcode,itl.symbol_table);
                    
                    node = remove(block.list,node);
                    continue;
                }
            }
        }


        node = node->next;
    }
}

void optimise_block(Interloper& itl, Function& func, Block& block)
{
    ValueTable reg_value = make_table<SymSlot,RegValue>();

    // first propagate values through the block
    propagate_values(itl,func,block,reg_value);

    // pass to remove dead stores arising from prior propagation
    // making sure there is no usage beyond the block here
    remove_dead_stores(itl,func,block,reg_value);

    destroy_table(reg_value);
}

void optimise_func(Interloper& itl, Function& func)
{
    // for now this is just peephole optimiser
    for(u32 b = 0; b < count(func.emitter.program); b++)
    {
        auto& block = func.emitter.program[b];

        optimise_block(itl,func,block);
    }
}


void optimise_ir(Interloper &itl)
{   
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            optimise_func(itl,func);
        }
    }
}
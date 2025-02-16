#include <interloper.h>


struct ConstValue
{
    u64 x = 0;
    b32 known = false;
};


struct RegValue
{
    ConstValue value;
    u32 uses = 0;
};

struct InstrOper
{
    ConstValue v1;
    ConstValue v2;
    b32 can_compute_dst = false;
};

ConstValue make_const_value(u64 v)
{
    return {v,true};
}

using ValueTable = HashTable<SymSlot, RegValue>;



// NOTE: slot invalidation is done in a common place outside this
void update_value(Interloper& itl, Function& func,ValueTable& table, SymSlot slot, u64 ans)
{
    // must be a local value to keep its result
    if(is_var(slot))
    {
        auto& reg = reg_from_slot(itl,func,slot);

        if(!is_local_reg(reg))
        {
            return;
        }
    }

    else
    {
        return;
    }

    auto r1_opt = lookup(table,slot);

    if(r1_opt)
    {
        auto& r1 = *r1_opt;
        r1.value = make_const_value(ans);
    }

    // first time
    else
    {
        RegValue r1;
        r1.value = make_const_value(ans);

        add(table,slot,r1);
    }
}

void invalidate_value(ValueTable& table, SymSlot slot)
{
    auto r1_opt = lookup(table,slot);

    if(r1_opt)
    {
        auto& r1 = *r1_opt;
        r1.value.known = false; 
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

ConstValue known_value(Interloper& itl, ValueTable& table, SymSlot slot)
{
    auto r1_opt = lookup(table,slot);

    if(r1_opt)
    {
        return r1_opt->value;
    }

    else if(is_sym(slot))
    {
        const auto sym = sym_from_slot(itl.symbol_table,slot);

        // const symbol thats builtin directly inline it
        if(sym.reg.kind == reg_kind::constant && is_builtin(sym.type))
        {
            const u64 v = builtin_from_const(itl,sym.type,pool_slot_from_idx(sym.reg.offset),0);
            return make_const_value(v);
        }       
    }

    return {};
}

Opcode mov_imm(const SymSlot dst, u64 v1)
{
    return Opcode(op_type::mov_imm,dst.handle,v1,0);
}

InstrOper get_instr_operands(Interloper& itl,ValueTable& reg_value,const ListNode* node)
{
    // check if we known both operands of our instruction
    bool can_compute_dst = false;

    const auto info = info_from_op(node->opcode);
    const bool is_dst = is_arg_dst(info.type[0]);

    // we optimize TAC this should not be present
    assert(info.type[0] != arg_type::dst_src_reg);

    // saved values 
    // NOTE: only valid when can_compute_dst is true
    ConstValue v1;
    ConstValue v2;

    if(is_dst)
    {
        const auto s1 = sym_from_idx(node->opcode.v[1]);
        const auto s2 = sym_from_idx(node->opcode.v[2]);

        // reg3
        if(info.args == 3 && info.group == op_group::reg_t)
        {
            v1 = known_value(itl,reg_value,s1);
            v2 = known_value(itl,reg_value,s2);

            can_compute_dst = v1.known && v2.known;
        }

        // reg2
        else if(info.args == 2 && info.group == op_group::reg_t)
        {
            v1 = known_value(itl,reg_value,s1);
            can_compute_dst = v1.known;
        }


        // imm3
        else if(info.args == 3 && info.group == op_group::imm_t)
        {
            v1 = known_value(itl,reg_value,s1);
            
            if(v1.known)
            {
                can_compute_dst = true;
                v2 = make_const_value(node->opcode.v[2]);
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
            v1 = known_value(itl,reg_value,s1);     
        }
    }

    return {v1,v2,can_compute_dst};
}

// NOTE: order of operands must not matter for this
void inline_commuative_v1(ListNode* node, op_type imm_op, u64 v1)
{
    node->opcode.op = imm_op;
    node->opcode.v[1] = node->opcode.v[2];
    node->opcode.v[2] = v1;
}

void inline_v2_internal(ListNode* node, op_type imm_op, u64 v2)
{
    node->opcode.op = imm_op;
    node->opcode.v[2] = v2;
}

void inline_v2(ListNode* node, op_type imm_op, const ConstValue& v2)
{
    if(v2.known)
    {
        inline_v2_internal(node,imm_op,v2.x);
    }
}

void inline_commuative_single(ListNode* node, op_type imm_op, const ConstValue &v1, const ConstValue& v2)
{
    if(v1.known)
    {
        inline_commuative_v1(node,imm_op,v1.x);
    }

    else if(v2.known)
    {
        inline_v2_internal(node,imm_op,v2.x);
    }    
}

void simplify_mul_imm(ListNode* node)
{
    switch(node->opcode.v[2])
    {
        case 0:
        {
            // x * 0 = 0
            node->opcode = Opcode(op_type::mov_imm,node->opcode.v[0],0,0);
            break;
        }

        case 1:
        {
            // x * 1 = x;
            node->opcode = Opcode(op_type::mov_reg,node->opcode.v[0],node->opcode.v[1],0);
        }

        default:
        {
            // TODO: power of two

            break;
        }
    }
}

void simplify_add_imm(ListNode* node)
{
    // x + 0 = x
    if(node->opcode.v[2] == 0)
    {
        node->opcode = Opcode(op_type::mov_reg,node->opcode.v[0],node->opcode.v[1],0);
    }
}

void simplify_sub_imm(ListNode* node)
{
    // x - 0 = x
    if(node->opcode.v[2] == 0)
    {
        node->opcode = Opcode(op_type::mov_reg,node->opcode.v[0],node->opcode.v[1],0);
    }
}


std::pair<ListNode*,b32> inline_instruction(Interloper& itl, Function& func,Block& block, ValueTable& reg_value, const InstrOper& oper, ListNode* node)
{
    // NOTE: this doesn't mean that we will compute the dst
    // just that we know both the operands on the instruction
    const auto can_compute_dst = oper.can_compute_dst;

    // inline these here for convience
    const u64 v1 = oper.v1.x;
    const u64 v2 = oper.v2.x;

    ConstValue ans;

    ListNode* next = node->next;
    b32 removed = false;

    // full answer needed
    if(can_compute_dst)
    {
        switch(node->opcode.op)
        {
            case op_type::mov_reg: 
            {
                ans = make_const_value(v1);
                break;
            }

            case op_type::add_reg:
            {
                ans = make_const_value(v1 + v2);
                break;
            }

            case op_type::sub_reg:
            {
                ans = make_const_value(v1 - v2);
                break;
            }

            case op_type::mul_reg:
            {
                ans = make_const_value(v1 * v2);
                break;
            }

            case op_type::udiv_reg:
            {
                if(v2 != 0)
                {
                    ans = make_const_value(u64(v1) / u64(v2));
                }
                break;
            }

            case op_type::sdiv_reg:
            {
                if(v2 != 0)
                {
                    ans = make_const_value(s64(v1) / s64(v2));
                }
                break;
            }

            case op_type::lsl_reg:
            {
                ans = make_const_value(v1 << v2);
                break;                
            }

            case op_type::lsl_imm:
            {
                ans = make_const_value(v1 << v2);
                break;
            }

            case op_type::and_reg:
            {
                ans = make_const_value(v1 & v2);
                break;
            }

            case op_type::or_reg:
            {
                ans = make_const_value(v1 | v2);
                break;
            }

            case op_type::xor_reg:
            {
                ans = make_const_value(v1 ^ v2);
                break;
            }

            case op_type::not_reg:
            {
                ans = make_const_value(~v1);
                break;
            }

            case op_type::add_imm:
            {
                ans = make_const_value(v1 + v2);
                break;
            }

            case op_type::mul_imm:
            {
                ans = make_const_value(v1 * v2);
                break;
            }

            case op_type::and_imm:
            {
                ans = make_const_value(v1 & v2);
                break;
            }

            case op_type::cmpsgt_imm:
            {
                ans = make_const_value(s64(v1) > s64(v2));
                break;                
            }

            case op_type::cmpslt_reg:
            {
                ans = make_const_value(s64(v1) < s64(v2));
                break;
            }

            case op_type::cmpsle_reg:
            {
                ans = make_const_value(s64(v1) <= s64(v2));
                break;
            }

            case op_type::cmpsgt_reg:
            {
                ans = make_const_value(s64(v1) > s64(v2));
                break;
            }

            case op_type::cmpsge_reg:
            {
                ans = make_const_value(s64(v1) >= s64(v2));
                break;
            }

            // unsigned compare
            case op_type::cmpugt_imm:
            {
                ans = make_const_value(v1 > v2);
                break;                
            }

            case op_type::cmpult_reg:
            {
                ans = make_const_value(v1 < v2);
                break;
            }

            case op_type::cmpule_reg:
            {
                ans = make_const_value(v1 <= v2);
                break;
            }

            case op_type::cmpugt_reg:
            {
                ans = make_const_value(v1 > v2);
                break;
            }

            case op_type::cmpuge_reg:
            {
                ans = make_const_value(v1 >= v2);
                break;
            }


            // compare equality
            case op_type::cmpeq_reg:
            {
                ans = make_const_value(v1 == v2);
                break;
            }

            case op_type::cmpne_reg:
            {
                ans = make_const_value(v1 == v2);
                break;
            }

            case op_type::cmpeq_imm:
            {
                ans = make_const_value(v1 == v2);
                break;
            }

            case op_type::cmpne_imm:
            {
                ans = make_const_value(v1 != v2);
                break;
            }

            default: break;
        }
    }

    // partial operands can still optimise
    else
    {
        switch(node->opcode.op)
        {
            case op_type::mov_imm:
            {   
                ans = make_const_value(node->opcode.v[1]);
                break;
            }

            case op_type::add_reg:
            {
                inline_commuative_single(node,op_type::add_imm,oper.v1,oper.v2);

                if(node->opcode.op == op_type::add_imm)
                {
                    simplify_add_imm(node);
                }
                break;
            }

            case op_type::sub_reg:
            {
                inline_v2(node,op_type::sub_imm,oper.v2);

                if(node->opcode.op == op_type::sub_imm)
                {
                    simplify_sub_imm(node);
                }
                break;
            }

            case op_type::add_imm:
            {
                simplify_add_imm(node);
                break;
            }

            case op_type::sub_imm:
            {
                simplify_sub_imm(node);
                break;
            }

            case op_type::mul_reg:
            {
                inline_commuative_single(node,op_type::mul_imm,oper.v1,oper.v2);

                // simplify based on value
                if(node->opcode.op == op_type::mul_imm)
                {
                    simplify_mul_imm(node);
                }

                break;
            }

            case op_type::mul_imm:
            {
                simplify_mul_imm(node);
                break;
            }


            case op_type::and_reg:
            {
                inline_commuative_single(node,op_type::and_imm,oper.v1,oper.v2);
                break;
            }

            case op_type::lsl_imm:
            {
                // shift zero (i.e nothing)
                // conv to mov
                if(node->opcode.v[2] == 0)
                {
                    node->opcode = make_op(op_type::mov_reg,node->opcode.v[0],node->opcode.v[1]);
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

                        next = remove(block.list,node);
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

                        next = remove(block.list,node);
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

            default: break;
        }
    }

    // update the dst results
    if(!removed)
    {
        const auto info = info_from_op(node->opcode);
        const b32 is_dst = is_arg_dst(info.type[0]);
        const auto dst = sym_from_idx(node->opcode.v[0]);

        // we managed to compute the result 
        // update the value and directly insert it as a instruction
        if(ans.known)
        {
            update_value(itl,func,reg_value,dst,ans.x);
            node->opcode = Opcode(op_type::mov_imm,dst.handle,ans.x,0);
        }

        // invalid value if dst
        else if(is_dst && is_var(dst))
        {
            invalidate_value(reg_value,dst);
        }
    }

    return std::pair{next,removed};
}

void update_var_tracking(Interloper& itl, Function& func, ValueTable& reg_value,ListNode* node)
{
    UNUSED(itl); UNUSED(func);

    // info has changed repull it
    auto& post_info = info_from_op(node->opcode);

    // mark any uses (NOTE: we do this after inlining has happend)
    for(u32 a = 0; a < post_info.args; a++)
    {
        if(is_arg_src(post_info.type[a]))
        {
            mark_use(reg_value,sym_from_idx(node->opcode.v[a]));
        }
    }
}

void propagate_values(Interloper& itl, Function& func, Block& block, ValueTable& reg_value)
{
    ListNode *node = block.list.start;

    // forward pass to attempt to propagate values
    while(node)
    {
        const auto oper = get_instr_operands(itl,reg_value,node);
        const auto [next,removed] = inline_instruction(itl,func,block,reg_value,oper,node);

        if(!removed)
        {
            update_var_tracking(itl,func,reg_value,node);
        }

        node = next;
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

        if(is_arg_dst(info.type[0]))
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
    auto start = std::chrono::high_resolution_clock::now();
    
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        optimise_func(itl,func);
    }

    auto end = std::chrono::high_resolution_clock::now();

    itl.optimise_time = std::chrono::duration<double, std::milli>(end-start).count();
}
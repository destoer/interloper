
void rewrite_directive_regs(Directive& directive, const ConstIrRegSpan& regs)
{
    u32 src = 0;
    u32 dst = 0;
    u32 dst_src = 0;

    for(auto& operand : directive.operand)
    {
        switch(operand.type)
        {
            case directive_operand_type::dst_src:
            {
                operand = make_reg_operand(regs.dst_src[dst_src++], ir_reg_type::dst_src);
                break;
            }

            case directive_operand_type::dst:
            {
                operand = make_reg_operand(regs.dst[dst++], ir_reg_type::dst);
                break;
            }

            case directive_operand_type::src:
            {
                operand = make_reg_operand(regs.src[src++], ir_reg_type::src);
                break;
            }

            default: break;
        }
    }
}

void rewrite_mov_gpr_imm(MovGprImm& mov, const ConstIrRegSpan& regs)
{
    mov.dst.ir = regs.dst[0];
}

template<typename op_type, op_group group>
void rewrite_unary_reg2(UnaryReg2<op_type,group>& unary, const ConstIrRegSpan& regs)
{
    unary.dst.ir = regs.dst[0];
    unary.src.ir = regs.src[0];
}

void rewrite_branch_cond(BranchCond& branch, const ConstIrRegSpan& regs)
{
    branch.src.ir = regs.src[0];
}

template<typename op_type,op_group group>
void rewrite_reg3(RegThree<op_type,group>& reg3, const ConstIrRegSpan& regs)
{
    reg3.dst.ir = regs.dst[0];
    reg3.v1.ir = regs.src[0];
    reg3.v2.ir = regs.src[1];
}

void rewrite_opcode(Opcode& opcode, const ConstIrRegSpan& regs)
{
    switch(opcode.group)
    {
        case op_group::directive: rewrite_directive_regs(opcode.directive,regs); break;
        case op_group::mov_gpr_imm: rewrite_mov_gpr_imm(opcode.mov_gpr_imm,regs); break;
        case op_group::unary_reg2: rewrite_unary_reg2(opcode.unary_reg2,regs); break;
        case op_group::branch_cond: rewrite_branch_cond(opcode.branch_cond,regs); break;
        case op_group::arith_fpr3: rewrite_reg3(opcode.arith_fpr3,regs); break;

        default:
        {
            unimplemented("[Rewrite opcode]: Invalid group: %s",OP_GROUP_NAMES[u32(opcode.group)]);
            break;
        }
    }
}

// TODO: For obvious reasons we can speed this up by using the same TMP across the instruction.
// but we will leave this for now as it will not affect correctness
void rewrite_spill_span_arg(Interloper& itl, Function& func, Block& block, OpcodeNode* node, 
    const ConstSpan<RegSlot>& span,  Span<RegSlot> &rewritten, reg_arg_kind kind, u32 arg)
{
    const auto& slot = span[arg];
    auto& reg = reg_from_slot(itl.symbol_table,func, slot);

    const auto tmp = copy_reg_unaliased(func,reg);
    rewritten[arg] = tmp;

    if(is_arg_src(kind))
    {
        const auto opcode = make_reload_slot_into(tmp,slot);
        insert_at(block.list,node,opcode);
    }

    if(is_arg_dst(kind))
    {
        const auto opcode = make_spill_slot(tmp);
        insert_after(block.list,node,opcode);         
    }
}

bool rewrite_aliased_span(Interloper& itl, Function& func, Block& block, OpcodeNode* node, 
    const ConstSpan<RegSlot>& span,  Span<RegSlot> &rewritten, reg_arg_kind kind)
{
    bool modified = false;

    for(u32 arg = 0; arg < span.size; arg++)
    {
        const auto& slot = span[arg];

        if(is_special_reg(slot))
        {
            continue;
        }

        auto& reg = reg_from_slot(itl.symbol_table,func, slot);

        if(!is_aliased(reg))
        {
            continue;
        }

        modified = true;

        rewrite_spill_span_arg(itl,func,block,node,span,rewritten,kind,arg);
    }

    return modified;
}


// rewrite aliased variables to be locals we can actually allocate.
void correct_aliased_block(Interloper& itl, Function& func, Block& block, OpcodeNode* node)
{
    auto& opcode = node->value;

    IrRegSpanStorage reg_span_storage;
    const auto reg = opcode_ir_reg_span(opcode,itl.reg_span);
    IrRegSpan reg_span_rewritten = copy_ir_reg_span(reg_span_storage,reg);

    bool modified = false;

    modified |= rewrite_aliased_span(itl,func,block,node,reg.src,reg_span_rewritten.src,reg_arg_kind::src);
    modified |= rewrite_aliased_span(itl,func,block,node,reg.dst,reg_span_rewritten.dst,reg_arg_kind::dst);
    modified |= rewrite_aliased_span(itl,func,block,node,reg.dst_src,reg_span_rewritten.dst_src,reg_arg_kind::dst_src);

    if(modified)
    {
        rewrite_opcode(opcode,reg_span_rewritten);
    }
}

void correct_aliased_regs(Interloper& itl, Function& func)
{
    for(auto& block: func.emitter.program)
    {
        OpcodeNode* node = block.list.start;

        while(node)
        {
            correct_aliased_block(itl,func,block,node);
            node = node->next;
        }
    }
}
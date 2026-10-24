
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

template<typename type,const bool IS_LOAD, const bool STRUCT_ADDR,op_group group>
void rewrite_addr(AddrOpcode<type,IS_LOAD,STRUCT_ADDR,group>& addr_op, const ConstIrRegSpan& regs)
{
    u32 dst = 0;
    u32 src = 0;

    if constexpr(IS_LOAD)
    {
        addr_op.v1.ir = regs.dst[dst++];
    }

    if constexpr(!IS_LOAD)
    {
        addr_op.v1.ir = regs.src[src++];
    }

    if constexpr(!STRUCT_ADDR)
    {
        addr_op.addr_ir.base = regs.src[src++];
    }

    addr_op.addr_ir.index = regs.src[src++];
}

template<typename type, op_group group>
void rewrite_imm3(ImmThree<type,group>& imm, const ConstIrRegSpan& regs)
{
    imm.src.ir = regs.src[0];
    imm.dst.ir = regs.dst[0];
}

void rewrite_branch_reg(BranchReg& branch, const ConstIrRegSpan& regs)
{
    branch.src.ir = regs.src[0];
}

void rewrite_branch_cmp(BranchCmp& branch, const ConstIrRegSpan& regs)
{
    branch.v1.ir = regs.src[0];
    branch.v2.ir = regs.src[1];
}

void rewrite_branch_cmp_imm(BranchCmpImm&  branch, const ConstIrRegSpan& regs)
{
    branch.src.ir = regs.src[0];
}

template<typename type,op_group group>
void rewrite_imm2_dst(ImmTwoDst<type,group>& imm, const ConstIrRegSpan& regs)
{
    imm.dst.ir = regs.dst_src[0];
}

void rewrite_imm2_src(ImmTwoSrc& imm, const ConstIrRegSpan& regs)
{
    imm.src.ir = regs.src[0];
}


template<typename type,op_group group>
void rewrite_reg2_dst(RegTwoDst<type,group>& reg, const ConstIrRegSpan& regs)
{
    reg.src.ir = regs.src[0];
    reg.dst.ir = regs.dst_src[0];
}

void rewrite_mov_fpr_imm(MovFprImm& mov_imm, const ConstIrRegSpan& regs)
{
    mov_imm.dst.ir = regs.dst[0];
}

template<typename op_type,op_group group>
void rewrite_unary_reg1(UnaryReg1<op_type,group>& unary, const ConstIrRegSpan& regs)
{
    unary.dst.ir = regs.dst[0]; 
}

void rewrite_reg2_src(RegTwoSrc& reg, const ConstIrRegSpan& regs)
{
    reg.v1.ir = regs.src[0];
    reg.v2.ir = regs.src[1];
}

void rewrite_reg1_src(RegOneSrc& reg, const ConstIrRegSpan& regs)
{
    reg.src.ir = regs.src[0];
}

void rewrite_reg1_dst(RegOneDst& reg, const ConstIrRegSpan& regs)
{
    reg.dst.ir = regs.dst[0];
}

void rewrite_opcode(Opcode& opcode, const ConstIrRegSpan& regs)
{
    switch(opcode.group)
    {
        case op_group::implicit: break;
        case op_group::branch_label: break;
        case op_group::branch_cmp_flag: break;
        case op_group::branch_cmp_imm: rewrite_branch_cmp_imm(opcode.branch_cmp_imm,regs); break;
        case op_group::branch_reg: rewrite_branch_reg(opcode.branch_reg,regs); break;
        case op_group::branch_cmp: rewrite_branch_cmp(opcode.branch_cmp,regs); break;
        case op_group::directive: rewrite_directive_regs(opcode.directive,regs); break;
        case op_group::mov_gpr_imm: rewrite_mov_gpr_imm(opcode.mov_gpr_imm,regs); break;
        case op_group::unary_reg2: rewrite_unary_reg2(opcode.unary_reg2,regs); break;
        case op_group::sign_extend: rewrite_unary_reg2(opcode.sign_extend,regs); break;
        case op_group::branch_cond: rewrite_branch_cond(opcode.branch_cond,regs); break;
        case op_group::arith_fpr3: rewrite_reg3(opcode.arith_fpr3,regs); break;
        case op_group::arith_gpr3: rewrite_reg3(opcode.arith_gpr3,regs); break;
        case op_group::shift_reg3: rewrite_reg3(opcode.shift_reg3,regs); break;
        case op_group::shift_reg2: rewrite_reg2_dst(opcode.shift_reg2,regs); break;
        case op_group::arith_imm2: rewrite_imm2_dst(opcode.arith_imm2,regs); break;
        case op_group::cmp_fpr3: rewrite_reg3(opcode.cmp_fpr3,regs); break;
        case op_group::cmp_gpr3: rewrite_reg3(opcode.cmp_gpr3,regs); break;
        case op_group::arith_gpr2: rewrite_reg2_dst(opcode.arith_gpr2,regs); break;
        case op_group::arith_fpr2: rewrite_reg2_dst(opcode.arith_fpr2,regs); break;
        case op_group::shift_imm3: rewrite_imm3(opcode.shift_imm3,regs); break;
        case op_group::shift_imm2: rewrite_imm2_dst(opcode.shift_imm2,regs); break;
        case op_group::store: rewrite_addr(opcode.store,regs); break;
        case op_group::load: rewrite_addr(opcode.load,regs); break;
        case op_group::load_struct: rewrite_addr(opcode.load_struct,regs); break;
        case op_group::store_struct: rewrite_addr(opcode.store_struct,regs); break;
        case op_group::lea: rewrite_addr(opcode.lea,regs); break;
        case op_group::addrof: rewrite_addr(opcode.addrof,regs); break;
        case op_group::arith_imm3: rewrite_imm3(opcode.arith_imm3,regs); break;
        case op_group::cmp_imm3: rewrite_imm3(opcode.cmp_imm3,regs); break;
        case op_group::mov_fpr_imm: rewrite_mov_fpr_imm(opcode.mov_fpr_imm,regs); break;
        case op_group::unary_reg1: rewrite_unary_reg1(opcode.unary_reg1,regs); break;
        case op_group::set_from_flag_gpr: rewrite_unary_reg1(opcode.set_from_flag_gpr, regs); break;
        case op_group::set_from_flag_fpr: rewrite_unary_reg1(opcode.set_from_flag_fpr, regs); break;
        case op_group::reg2_src: rewrite_reg2_src(opcode.reg2_src,regs); break;
        case op_group::reg1_src: rewrite_reg1_src(opcode.reg1_src,regs); break;
        case op_group::reg1_dst: rewrite_reg1_dst(opcode.reg1_dst,regs); break;
        case op_group::imm2_src: rewrite_imm2_src(opcode.imm2_src,regs); break;
        case op_group::x86_fixed: rewrite_reg2_dst(opcode.x86_fixed,regs); break;
    }
}

// TODO: For obvious reasons we can speed this up by using the same TMP across the instruction.
// but we will leave this for now as it will not affect correctness
void rewrite_spill_span_arg(Function& func, Block& block, OpcodeNode* node, const Reg& reg,  RegSlot* target, reg_arg_kind kind)
{
    const auto reg_slot = reg.reg_slot;

    // This will invalidate the held registers
    const auto tmp = copy_reg_unaliased(func,reg);
    *target = tmp;

    if(is_arg_src(kind))
    {
        const auto opcode = make_reload_slot_to(tmp,reg_slot);
        insert_at(block.list,node,opcode);
    }

    if(is_arg_dst(kind))
    {
        const auto opcode = make_spill_slot_to(tmp,reg_slot);
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

        rewrite_spill_span_arg(func,block,node,reg,&rewritten[arg],kind);
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
    if(itl.stack_alloc)
    {
        return;
    }

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
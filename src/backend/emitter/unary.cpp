template<typename op_type, op_group group>
ConstIrRegSpan unary_reg2_reg_span(const UnaryReg2<op_type,group>& unary, IrRegSpan& span)
{
    span.src[0] = unary.src.ir;
    span.src.size = 1;

    span.dst[0] = unary.dst.ir;
    span.dst.size = 1;

    return span;    
}

template<typename op_type,op_group group>
ConstIrRegSpan unary_reg1_reg_span(const UnaryReg1<op_type,group>& unary, IrRegSpan& span)
{
    span.dst[0] = unary.dst.ir;
    span.dst.size = 1;

    return span;    
}


template<typename op_type,op_group group>
UnaryReg2<op_type,group> make_unary_reg2(RegSlot dst, RegSlot src, op_type type)
{
    UnaryReg2<op_type,group> unary;
    unary.dst.ir = dst;
    unary.src.ir = src;
    unary.type = type;

    return unary;
}

template<op_group group,typename op_type>
UnaryReg1<op_type,group> make_unary_reg1(RegSlot dst, op_type type)
{
    UnaryReg1<op_type,group> unary;
    unary.dst.ir = dst;
    unary.type = type;

    return unary;
}

template<op_group group, typename op_type>
void emit_unary_reg2_opcode(Interloper& itl, Function& func, RegSlot dst, RegSlot src, op_type type)
{
    const auto opcode = Opcode(make_unary_reg2<op_type,group>(dst,src,type),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}

void emit_unary_reg_two(Interloper& itl, Function& func, RegSlot dst, RegSlot src, unary_reg2_op type)
{
    emit_unary_reg2_opcode<op_group::unary_reg2>(itl,func,dst,src,type);
}

void emit_sign_extend(Interloper& itl, Function& func, RegSlot dst, RegSlot src,  sign_extend_op type)
{
    emit_unary_reg2_opcode<op_group::sign_extend>(itl,func,dst,src,type);
}

void not_reg(Interloper& itl,Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg2_op::bitwise_not);
}

void mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg2_op::mov_gpr_reg);
}

RegSlot copy_reg(Interloper& itl, Function& func, RegSlot src)
{
    return opcode_res1_gpr(itl,func,src,mov_reg);
}

void mov_float(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg2_op::mov_fpr_reg);
}

void cvt_fi(Interloper& itl, Function& func, RegSlot dst, RegSlot v1)
{
    emit_unary_reg_two(itl,func,dst,v1,unary_reg2_op::cvt_fi);
}

void cvt_if(Interloper& itl, Function& func, RegSlot dst, RegSlot v1)
{
    emit_unary_reg_two(itl,func,dst,v1,unary_reg2_op::cvt_if);
}


Opcode make_mov_reg_lowered_instr(lowered_reg_t dst, lowered_reg_t src, reg_type type)
{
    UnaryReg2<unary_reg2_op,op_group::unary_reg2> unary;
    unary.dst.reg = dst;
    unary.src.reg = src;
    unary.type = type == reg_type::gpr? unary_reg2_op::mov_gpr_reg : unary_reg2_op::mov_fpr_reg; 

    return Opcode(unary,opcode_state::lowered);
}


Opcode make_mov_gpr_lowered_instr(lowered_reg_t dst, lowered_reg_t src)
{
    return make_mov_reg_lowered_instr(dst,src,reg_type::gpr);
}

Opcode mov_reg_ir(RegSlot dst, RegSlot src, reg_type rtype)
{
    const auto type = rtype == reg_type::gpr? unary_reg2_op::mov_gpr_reg : unary_reg2_op::mov_fpr_reg;
    return Opcode(make_unary_reg2<unary_reg2_op,op_group::unary_reg2>(dst,src,type),opcode_state::ir);
}
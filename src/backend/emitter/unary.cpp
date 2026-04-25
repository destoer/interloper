template<typename op_type>
ConstRegSpan unary_reg2_reg_span(const UnaryReg2<op_type>& unary, RegSpan& span)
{
    span.src[0] = unary.src.ir;
    span.src.size = 1;

    span.dst[0] = unary.dst.ir;
    span.dst.size = 1;

    return span;    
}

template<typename op_type>
UnaryReg2<op_type> make_unary_reg2(RegSlot dst, RegSlot src, op_type type)
{
    UnaryReg2<op_type> unary;
    unary.dst.ir = dst;
    unary.src.ir = src;
    unary.type = type;

    return unary;
}

void emit_unary_reg_two(Interloper& itl, Function& func, RegSlot dst, RegSlot src, unary_reg_op type)
{
    Opcode opcode;
    opcode.group = op_group::unary_reg2;

    opcode.unary_reg2 = make_unary_reg2(dst,src,type);

    emit_block_func(itl,func,opcode);
}

void emit_sign_extend(Interloper& itl, Function& func, RegSlot dst, RegSlot src,  sign_extend_op type)
{
    Opcode opcode;
    opcode.group = op_group::sign_extend;

    opcode.sign_extend = make_unary_reg2(dst,src,type);
    emit_block_func(itl,func,opcode);
}

void not_reg(Interloper& itl,Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg_op::bitwise_not);
}

void mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg_op::mov_gpr_reg);
}

RegSlot copy_reg(Interloper& itl, Function& func, RegSlot src)
{
    return opcode_res1(itl,func,src,mov_reg);
}

void mov_float(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_unary_reg_two(itl,func,dst,src,unary_reg_op::mov_fpr_reg);
}
template<typename type>
ConstRegSpan reg3_reg_span(const RegThree<type>& reg, RegSpan& span)
{
    span.src[0] = reg.v1.ir;
    span.src[1] = reg.v2.ir;
    span.src.size = 2;

    span.dst[0] = reg.dst.ir;
    span.dst.size = 1;

    return span;
}


template<typename op_type>
RegThree<op_type> make_reg3(RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    RegThree<op_type> reg_three;
    reg_three.type = type;
    reg_three.dst.ir = dst;
    reg_three.v1.ir = v1;
    reg_three.v2.ir = v2;

    return reg_three;
}


void emit_shift_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, shift_op type)
{
    Opcode opcode;
    opcode.group = op_group::shift_reg3;

    opcode.shift_reg3 = make_reg3(dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}



void emit_gpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::arith_gpr3;

    opcode.arith_gpr3 = make_reg3(dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}

void emit_fpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, fpr_arith type)
{
    Opcode opcode;
    opcode.group = op_group::arith_fpr3;

    opcode.arith_fpr3 = make_reg3(dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}

void sub(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_gpr_reg3(itl,func,dst,v1,v2,arith_bin_op::sub_t);
}

RegSlot sub_res(Interloper& itl,Function& func, RegSlot v1, RegSlot v2)
{
    return opcode_res2(itl,func,v1,v2,sub);
}


void subf(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_fpr_reg3(itl,func,dst,v1,v2,fpr_arith::sub_t);
}
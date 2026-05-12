template<typename type,op_group group>
ConstIrRegSpan reg3_reg_span(const RegThree<type,group>& reg, IrRegSpan& span)
{
    span.src[0] = reg.v1.ir;
    span.src[1] = reg.v2.ir;
    span.src.size = 2;

    span.dst[0] = reg.dst.ir;
    span.dst.size = 1;

    return span;
}


template<op_group group, typename op_type>
RegTwoDst<op_type,group> make_reg2_dst(RegSlot dst, RegSlot v1,op_type type)
{
    RegTwoDst<op_type,group> reg_two;
    reg_two.type = type;
    reg_two.dst.ir = dst;
    reg_two.src.ir = v1;

    return reg_two;
}

RegTwoSrc make_reg2_src(RegSlot v1, RegSlot v2,reg_two_src type)
{
    RegTwoSrc reg_two;
    reg_two.type = type;
    reg_two.v1.ir = v1;
    reg_two.v2.ir = v2;

    return reg_two;
}


RegOneSrc make_reg1_src(RegSlot src, reg1_src_type type)
{
    RegOneSrc reg_one;
    reg_one.type = type;
    reg_one.src.ir = src;

    return reg_one;
}

Opcode make_reg1_src_lowered_instr(lowered_reg_t src, reg1_src_type type)
{
    RegOneSrc reg_one;
    reg_one.type = type;
    reg_one.src.reg = src;

    return make_lowered_instr(reg_one);
}

Opcode make_reg1_dst_lowered_instr(lowered_reg_t dst, reg1_dst_type type)
{
    RegOneDst reg_one;
    reg_one.type = type;
    reg_one.dst.reg = dst;

    return make_lowered_instr(reg_one);
}

template<typename type,op_group group>
ConstIrRegSpan reg2_dst_reg_span(const RegTwoDst<type,group>& reg, IrRegSpan& span)
{
    span.src[0] = reg.src.ir;
    span.src.size = 1;

    span.dst_src[0] = reg.dst.ir;
    span.dst_src.size = 1;

    return span;
}

ConstIrRegSpan reg2_src_reg_span(const RegTwoSrc& reg, IrRegSpan& span)
{
    span.src[0] = reg.v1.ir;
    span.src[1] = reg.v2.ir;
    span.src.size = 2;

    return span;
}

ConstIrRegSpan reg1_src_reg_span(const RegOneSrc& reg, IrRegSpan& span)
{
    span.src[0] = reg.src.ir;
    span.src.size = 1;

    return span;
}

ConstIrRegSpan reg1_dst_reg_span(const RegOneDst& reg, IrRegSpan& span)
{
    span.dst[0] = reg.dst.ir;
    span.dst.size = 1;

    return span;
}


template<op_group group,typename op_type>
RegThree<op_type,group> make_reg3(RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    RegThree<op_type,group> reg_three;
    reg_three.type = type;
    reg_three.dst.ir = dst;
    reg_three.v1.ir = v1;
    reg_three.v2.ir = v2;

    return reg_three;
}

template<op_group group,typename op_type>
void emit_reg3_opcode(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    const auto reg3 = make_reg3<group>(dst,v1,v2,type);
    emit_block_func(itl,func,reg3);
}



void emit_shift_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, shift_op type)
{
    emit_reg3_opcode<op_group::shift_reg3>(itl,func,dst,v1,v2,type);
}


void emit_gpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, arith_bin_op type)
{
    emit_reg3_opcode<op_group::arith_gpr3>(itl,func,dst,v1,v2,type);
}

void emit_fpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, fpr_arith type)
{
    emit_reg3_opcode<op_group::arith_fpr3>(itl,func,dst,v1,v2,type);
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
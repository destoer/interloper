template<typename type,op_group group>
ConstRegSpan reg3_reg_span(const RegThree<type,group>& reg, RegSpan& span)
{
    span.src[0] = reg.v1.ir;
    span.src[1] = reg.v2.ir;
    span.src.size = 2;

    span.dst[0] = reg.dst.ir;
    span.dst.size = 1;

    return span;
}


template<typename op_type,op_group group>
RegTwoDst<op_type,group> make_reg2_dst(RegSlot dst, RegSlot v1,op_type type)
{
    RegTwoDst<op_type,group> reg_two;
    reg_two.type = type;
    reg_two.dst.ir = dst;
    reg_two.src.ir = v1;

    return reg_two;
}


template<typename op_type,op_group group>
void make_reg2_dst_opcode(Opcode& opcode, RegTwoDst<op_type,group>* reg2,RegSlot dst, RegSlot v1,op_type type)
{
    opcode.group = group;
    *reg2 = make_reg2_dst<op_type,group>(dst,v1,type);
}


RegTwoSrc make_reg2_src(RegSlot v1, RegSlot v2,reg_two_src type)
{
    RegTwoSrc reg_two;
    reg_two.type = type;
    reg_two.v1.ir = v1;
    reg_two.v2.ir = v2;

    return reg_two;
}

template<typename type,op_group group>
ConstRegSpan reg2_dst_reg_span(const RegTwoDst<type,group>& reg, RegSpan& span)
{
    span.src[0] = reg.src.ir;
    span.src.size = 1;

    span.dst_src[0] = reg.dst.ir;
    span.dst_src.size = 1;

    return span;
}

ConstRegSpan reg2_src_reg_span(const RegTwoSrc& reg, RegSpan& span)
{
    span.src[0] = reg.v1.ir;
    span.src[1] = reg.v2.ir;
    span.src.size = 2;

    return span;
}


template<typename op_type,op_group group>
RegThree<op_type,group> make_reg3(RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    RegThree<op_type,group> reg_three;
    reg_three.type = type;
    reg_three.dst.ir = dst;
    reg_three.v1.ir = v1;
    reg_three.v2.ir = v2;

    return reg_three;
}

template<typename op_type,op_group group>
void make_reg3_opcode(Opcode& opcode, RegThree<op_type,group>* reg3, RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    opcode.group = group;
    *reg3 = make_reg3<op_type,group>(dst,v1,v2,type);
}


template<typename op_type,op_group group>
void emit_reg3_opcode(Interloper& itl, Function& func, Opcode& opcode, RegThree<op_type,group>* reg3, RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    make_reg3_opcode(opcode,reg3,dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}




void emit_shift_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, shift_op type)
{
    Opcode opcode;
    emit_reg3_opcode(itl,func,opcode,&opcode.shift_reg3,dst,v1,v2,type);
}


void emit_gpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, arith_bin_op type)
{
    Opcode opcode;
    emit_reg3_opcode(itl,func,opcode,&opcode.arith_gpr3,dst,v1,v2,type);
}

void emit_fpr_reg3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, fpr_arith type)
{
    Opcode opcode;
    emit_reg3_opcode(itl,func,opcode,&opcode.arith_fpr3,dst,v1,v2,type);
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
template<typename op_type, op_group group>
ConstRegSpan unary_reg2_reg_span(const UnaryReg2<op_type,group>& unary, RegSpan& span)
{
    span.src[0] = unary.src.ir;
    span.src.size = 1;

    span.dst[0] = unary.dst.ir;
    span.dst.size = 1;

    return span;    
}

template<typename op_type,op_group group>
ConstRegSpan unary_reg1_reg_span(const UnaryReg1<op_type,group>& unary, RegSpan& span)
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

template<typename op_type,op_group group>
UnaryReg1<op_type,group> make_unary_reg1(RegSlot dst, op_type type)
{
    UnaryReg1<op_type,group> unary;
    unary.dst.ir = dst;
    unary.type = type;

    return unary;
}

template<typename op_type,op_group group>
void make_unary_reg1_opcode(Opcode& opcode, UnaryReg1<op_type,group>* unary, RegSlot dst, op_type type)
{
    opcode.group = group;
    *unary = make_unary_reg1<op_type,group>(dst,type);
}

template<typename op_type,op_group group>
void emit_unary_reg2_opcode(Interloper& itl, Function& func, Opcode& opcode, UnaryReg2<op_type,group>* unary2, RegSlot dst, RegSlot src, op_type type)
{
    opcode.group = group;
    *unary2 = make_unary_reg2<op_type,group>(dst,src,type);
    emit_block_func(itl,func,opcode);
}

void emit_unary_reg_two(Interloper& itl, Function& func, RegSlot dst, RegSlot src, unary_reg2_op type)
{
    Opcode opcode;
    emit_unary_reg2_opcode(itl,func,opcode,&opcode.unary_reg2,dst,src,type);
}

void emit_sign_extend(Interloper& itl, Function& func, RegSlot dst, RegSlot src,  sign_extend_op type)
{
    Opcode opcode;
    emit_unary_reg2_opcode(itl,func,opcode,&opcode.sign_extend,dst,src,type);
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
    return opcode_res1(itl,func,src,mov_reg);
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
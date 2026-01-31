template<typename type>
ConstRegSpan imm_three_reg_span(const ImmThree<type> imm, RegSpan& span)
{
    span.src[0] = imm.src.ir;
    span.src.size = 1;

    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

template<typename op_type>
ImmThree<op_type> make_imm_three(RegSlot dst, RegSlot src, u64 imm, op_type type)
{
    ImmThree<op_type> imm_three;
    imm_three.type = type;
    imm_three.dst.ir = dst;
    imm_three.src.ir = src;
    imm_three.imm = imm;

    return imm_three;
}

void emit_gpr_imm_three(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::arith_imm_three;

    opcode.arith_imm_three = make_imm_three(dst,src,imm,type);
    emit_block_func(itl,func,opcode);
}

void add_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    if(imm == 0)
    {
        mov_reg(itl,func,dst,src);
        return;
    }

    emit_gpr_imm_three(itl,func,dst,src,imm,arith_bin_op::add_t);
}

RegSlot add_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    add_imm(itl,func,tmp,src,imm);

    return tmp;
}
ConstRegSpan gpr_imm_three_reg_span(const GprImmThree imm, RegSpan& span)
{
    span.src[0] = imm.src.ir;
    span.src.size = 1;

    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

void emit_gpr_imm_three(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::gpr_imm_three;

    GprImmThree imm_three;
    imm_three.type = type;
    imm_three.dst.ir = dst;
    imm_three.src.ir = src;
    imm_three.imm = imm;

    opcode.gpr_imm_three = imm_three;
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
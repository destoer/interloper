ConstRegSpan mov_gpr_imm_reg_span(const MovGprImm& mov_imm, RegSpan& reg)
{
    u32 dst = 0;
    reg.dst[dst++] = mov_imm.dst.ir;
    reg.dst.size = dst;

    reg.src.size = 0;

    return reg;
}

ConstRegSpan mov_fpr_imm_reg_span(const MovFprImm& mov_imm, RegSpan& reg)
{
    u32 dst = 0;
    reg.dst[dst++] = mov_imm.dst.ir;
    reg.dst.size = dst;

    reg.src.size = 0;

    return reg;
}


Opcode make_mov_imm(RegSlot dst, u64 imm)
{
    MovGprImm mov_imm;
    mov_imm.dst.ir = dst;
    mov_imm.imm = imm;

    Opcode opcode;
    opcode.group = op_group::mov_gpr_imm;
    opcode.mov_gpr_imm = mov_imm;

    return opcode;
}

void mov_imm(Interloper& itl, Function& func, RegSlot dst, u64 imm)
{
    const auto opcode = make_mov_imm(dst,imm);
    emit_block_func(itl,func,opcode);
}

void movf_imm(Interloper& itl, Function& func, RegSlot dst, f64 imm)
{
    MovFprImm mov_imm;
    mov_imm.dst.ir = dst;
    mov_imm.imm = imm;

    Opcode opcode;
    opcode.group = op_group::mov_fpr_imm;
    opcode.mov_fpr_imm = mov_imm;

    emit_block_func(itl,func,opcode);
}

RegSlot movf_imm_res(Interloper& itl, Function& func, u64 imm)
{
    return opcode_res1(itl,func,imm,movf_imm);
}

RegSlot mov_imm_res(Interloper& itl, Function& func, u64 imm)
{
    return opcode_res1(itl,func,imm,mov_imm);
}

RegSlot imm_zero(Interloper& itl, Function& func)
{
    return mov_imm_res(itl,func,0);
}
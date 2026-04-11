

void emit_cmp_imm3(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, cmp_sign_op type)
{
    Opcode opcode;
    opcode.group = op_group::cmp_imm3;

    opcode.cmp_imm3 = make_imm3(dst,src,imm,type);
    emit_block_func(itl,func,opcode);
}

void cmp_eq_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_cmp_imm3(itl,func,dst,src,imm,cmp_sign_op::eq);
}


void emit_cmp_gpr3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, cmp_sign_op type)
{
    Opcode opcode;
    opcode.group = op_group::cmp_gpr3;

    opcode.cmp_gpr3 = make_reg3(dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}



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


void emit_cmp_gpr3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, cmp_sign_op type)
{
    Opcode opcode;
    opcode.group = op_group::cmp_gpr3;

    opcode.cmp_gpr3 = make_reg3(dst,v1,v2,type);
    emit_block_func(itl,func,opcode);
}

void emit_cmp_fpr3(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, comparison_op type)
{
    Opcode opcode;
    opcode.group = op_group::cmp_fpr3;

    opcode.cmp_fpr3 = make_reg3(dst,v1,v2,type);
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


void cmp_ne_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_cmp_imm3(itl,func,dst,src,imm,cmp_sign_op::ne);
}

void cmp_ugt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_cmp_imm3(itl,func,dst,src,imm,cmp_sign_op::ugt);
}

void cmp_sgt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_cmp_imm3(itl,func,dst,src,imm,cmp_sign_op::sgt);
}

RegSlot cmp_eq_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    return opcode_res2(itl,func,src,imm,cmp_eq_imm);
}


RegSlot cmp_ne_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    return opcode_res2(itl,func,src,imm,cmp_ne_imm);
}


void cmp_ne(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_cmp_gpr3(itl,func,dst,v1,v2,cmp_sign_op::ne);
}

RegSlot cmp_ne_res(Interloper& itl,Function& func,RegSlot v1, RegSlot v2)
{
    return opcode_res2(itl,func,v1,v2,cmp_ne);
}
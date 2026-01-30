void mov_imm(Interloper& itl, Function& func, RegSlot dst, u64 imm)
{
    UNUSED(itl);

    handle_dst_storage(itl,func,dst);
    MovGprImm mov_imm;
    mov_imm.dst_ir = dst;
    mov_imm.imm = imm;

    Opcode opcode;
    opcode.group = op_group::mov_gpr_imm;
    opcode.mov_gpr_imm = mov_imm;

    emit_block_func(func,opcode);
}

RegSlot mov_imm_res(Interloper& itl, Function& func, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    mov_imm(itl,func,tmp,imm);

    return tmp;
}

RegSlot imm_zero(Interloper& itl, Function& func)
{
    return mov_imm_res(itl,func,0);
}
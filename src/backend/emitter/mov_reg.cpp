ConstRegSpan mov_reg_reg_span(const MovReg& mov, RegSpan& span)
{
    span.src[0] = mov.src.ir;
    span.src.size = 1;

    span.dst[0] = mov.dst.ir;
    span.dst.size = 1;

    return span;    
}

void emit_mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src,mov_reg_type type)
{
    if(dst == src)
    {
        return;
    }

    Opcode opcode;
    opcode.group = op_group::mov_reg;
    MovReg move_reg;
    move_reg.type = type;
    move_reg.dst.ir = dst;
    move_reg.src.ir = src;
    opcode.mov_reg = move_reg;

    emit_block_func(itl,func,opcode);
}

void mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_mov_reg(itl,func,dst,src,mov_reg_type::gpr);
}

RegSlot copy_reg(Interloper& itl, Function& func, RegSlot src)
{
    const auto dst = new_tmp(func,GPR_SIZE);
    emit_mov_reg(itl,func,dst,src,mov_reg_type::gpr);

    return dst;
}

void mov_float(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_mov_reg(itl,func,dst,src,mov_reg_type::fpr);
}
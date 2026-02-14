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

template<typename type>
ConstRegSpan reg_three_reg_span(const RegThree<type> reg, RegSpan& span)
{
    reg.src[0] = reg.v1.ir;
    reg.src[1] = reg.v2.ir;
    span.src.size = 2;

    span.dst[0] = reg.dst.ir;
    span.dst.size = 1;

    return span;
}


template<typename op_type>
RegThree<op_type> make_reg_three(RegSlot dst, RegSlot v1, RegSlot v2, op_type type)
{
    RegThree<op_type> reg_three;
    reg_three.type = type;
    reg_three.dst.ir = dst;
    reg_three.v1.ir = v1;
    reg_three.v2.ir = v2;

    return reg_three;
}


void emit_gpr_imm_three(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::arith_imm_three;

    opcode.arith_imm_three = make_imm_three(dst,src,imm,type);
    emit_block_func(itl,func,opcode);
}

void emit_gpr_reg_three(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::arith_reg_three;

    opcode.arith_reg_three = make_reg_three(dst,v1,v2,type);
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

void umod_imm(Interloper& itl, Function& func, RegSlot dst,RegSlot src,u64 imm)
{
    if(is_pow2(imm))
    {
        const u32 mask = imm - 1;
        and_imm(itl,func,dst,src,mask);
    }

    else
    {
        // just emulate this instr as no arch is likely to have it
        const auto v2 = mov_imm_res(itl,func,imm);
        emit_gpr_reg_three(itl,func,dst,src,v2,arith_bin_op::umod_t);
    }
}


void udiv_imm(Interloper& itl, Function& func, RegSlot dst,RegSlot src,u64 imm)
{
    if(is_pow2(imm))
    {
        const u32 shift = log2(imm);

        if(shift == 0)
        {
            mov_reg(itl,func,dst,src);
        } 

        else 
        {
            lsr_imm(itl,func,dst,src,shift);
        }
    }

    else
    {
        // just emulate this instr as no arch is likely to have it
        const auto v2 = mov_imm_res(itl,func,imm);
        emit_gpr_imm_three(itl,func,dst,src,v2,arith_op_type::udiv_t);
    }
}

RegSlot udiv_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm)
{
    return opcode_res2(itl,func,src,imm,udiv_imm);
}


RegSlot add_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    return opcode_res2(itl,func,src,imm,add_imm);
}
template<typename type>
ConstRegSpan imm3_reg_span(const ImmThree<type>& imm, RegSpan& span)
{
    span.src[0] = imm.src.ir;
    span.src.size = 1;

    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

template<typename type>
ConstRegSpan imm2_dst_reg_span(const ImmTwoDst<type>& imm, RegSpan& span)
{
    span.src.size = 0;

    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

ConstRegSpan imm2_src_reg_span(const ImmTwoSrc& imm, RegSpan& span)
{
    span.dst.size = 0;

    span.src[0] = imm.src.ir;
    span.src.size = 1;

    return span;
}


template<typename op_type>
ImmThree<op_type> make_imm3(RegSlot dst, RegSlot src, u64 imm, op_type type)
{
    ImmThree<op_type> imm_three;
    imm_three.type = type;
    imm_three.dst.ir = dst;
    imm_three.src.ir = src;
    imm_three.imm = imm;

    return imm_three;
}

ImmTwoSrc make_imm2_src(RegSlot src, u64 imm,imm_two_src type)
{
    ImmTwoSrc imm_two;
    imm_two.type = type;
    imm_two.src.ir = src;
    imm_two.imm = imm;

    return imm_two;
}

void emit_gpr_imm3(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, arith_bin_op type)
{
    Opcode opcode;
    opcode.group = op_group::arith_imm3;

    opcode.arith_imm3 = make_imm3(dst,src,imm,type);
    emit_block_func(itl,func,opcode);
}


void emit_shift_imm3(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, shift_op type)
{
    Opcode opcode;
    opcode.group = op_group::shift_imm3;

    opcode.shift_imm3 = make_imm3(dst,src,imm,type);
    emit_block_func(itl,func,opcode);
}

void add_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    if(imm == 0)
    {
        mov_reg(itl,func,dst,src);
        return;
    }

    emit_gpr_imm3(itl,func,dst,src,imm,arith_bin_op::add_t);
}

void sub_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    if(imm == 0)
    {
        mov_reg(itl,func,dst,src);
        return;
    }

    emit_gpr_imm3(itl,func,dst,src,imm,arith_bin_op::sub_t);
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
        emit_gpr_reg3(itl,func,dst,src,v2,arith_bin_op::umod_t);
    }
}

void lsr_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_shift_imm3(itl,func,dst,src,imm,shift_op::lsr);
}

void lsl_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_shift_imm3(itl,func,dst,src,imm,shift_op::lsl);
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
        emit_gpr_reg3(itl,func,dst,src,v2,arith_bin_op::udiv_t);
    }
}

void mul_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    // Not sure if we should have a higher level function for this?
    if(is_pow2(imm))
    {
        const u32 shift = log2(imm);

        if(shift == 0)
        {
            mov_reg(itl,func,dst,src);
        } 

        else 
        {
            lsl_imm(itl,func,dst,src,shift);
        }
    }

    else
    {
        emit_gpr_imm3(itl,func,dst,src,imm,arith_bin_op::mul_t);
    }
}

RegSlot mul_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm)
{
    return opcode_res2(itl,func,src,imm,mul_imm);
}

RegSlot udiv_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm)
{
    return opcode_res2(itl,func,src,imm,udiv_imm);
}


RegSlot add_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    return opcode_res2(itl,func,src,imm,add_imm);
}

RegSlot sub_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    return opcode_res2(itl,func,src,imm,sub_imm);
}

void xor_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_gpr_imm3(itl,func,dst,src,imm,arith_bin_op::xor_t);
}

void and_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_gpr_imm3(itl,func,dst,src,imm,arith_bin_op::and_t);
}
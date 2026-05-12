template<typename type, op_group group>
ConstIrRegSpan imm3_reg_span(const ImmThree<type,group>& imm, IrRegSpan& span)
{
    span.src[0] = imm.src.ir;
    span.src.size = 1;

    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

template<typename type,op_group group>
ConstIrRegSpan imm2_dst_reg_span(const ImmTwoDst<type,group>& imm, IrRegSpan& span)
{
    span.dst[0] = imm.dst.ir;
    span.dst.size = 1;

    return span;
}

ConstIrRegSpan imm2_src_reg_span(const ImmTwoSrc& imm, IrRegSpan& span)
{
    span.src[0] = imm.src.ir;
    span.src.size = 1;

    return span;
}


template<typename op_type,op_group group>
ImmThree<op_type,group> make_imm3(RegSlot dst, RegSlot src, u64 imm, op_type type)
{
    ImmThree<op_type,group> imm_three;
    imm_three.type = type;
    imm_three.dst.ir = dst;
    imm_three.src.ir = src;
    imm_three.imm = imm;

    return imm_three;
}

template<op_group group,typename op_type>
void emit_imm3_opcode(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, op_type type)
{
    const auto opcode = Opcode(make_imm3<op_type,group>(dst,src,imm,type));
    emit_block_func(itl,func,opcode);
}

ImmTwoSrc make_imm2_src(RegSlot src, u64 imm,imm_two_src type)
{
    ImmTwoSrc imm_two;
    imm_two.type = type;
    imm_two.src.ir = src;
    imm_two.imm = imm;

    return imm_two;
}


template<op_group group, typename op_type>
ImmTwoDst<op_type,group> make_imm2_dst(RegSlot dst, u64 imm,op_type type)
{
    ImmTwoDst<op_type,group> imm_two;
    imm_two.type = type;
    imm_two.dst.ir = dst;
    imm_two.imm = imm;

    return imm_two;
}

ArithImm2 make_arith_imm2(RegSlot dst, u64 imm,arith_bin_op type)
{
    return make_imm2_dst<op_group::arith_imm2>(dst,imm,type);
}


Opcode make_lowered_arith_imm2_instr(lowered_reg_t dst, u64 imm, arith_bin_op type)
{;
    ArithImm2 instr;
    instr.dst.reg = dst;
    instr.imm = imm;
    instr.type = type;

    return make_lowered_instr(instr);
}

void emit_gpr_imm3(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, arith_bin_op type)
{
    emit_imm3_opcode<op_group::arith_imm3>(itl,func,dst,src,imm,type);
}


void emit_shift_imm3(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm, shift_op type)
{
    emit_imm3_opcode<op_group::shift_imm3>(itl,func,dst,src,imm,type);
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
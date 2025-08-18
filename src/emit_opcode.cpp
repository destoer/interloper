

void add(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::add_reg>(itl,func,dst,v1,v2);
}

void mov_imm(Interloper& itl, Function& func, RegSlot dst, u64 imm)
{
    emit_imm1<op_type::mov_imm>(itl,func,dst,imm);
}

RegSlot mov_imm_res(Interloper& itl, Function& func, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    mov_imm(itl,func,tmp,imm);

    return tmp;
}

void movf_imm(Interloper& itl, Function& func, RegSlot dst, f64 v1)
{
    emit_fp_imm1<op_type::movf_imm>(itl,func,dst,v1);
}

RegSlot movf_imm_res(Interloper& itl, Function& func, f64 v1)
{
    const auto dst = new_float(func);

    emit_fp_imm1<op_type::movf_imm>(itl,func,dst,v1);

    return dst;
}

void addf(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::addf_reg>(itl,func,dst,v1,v2);
}

void subf(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::subf_reg>(itl,func,dst,v1,v2);
}

void mulf(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::mulf_reg>(itl,func,dst,v1,v2);
}

void divf(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::divf_reg>(itl,func,dst,v1,v2);
}

void cvt_fi(Interloper& itl, Function& func, RegSlot dst, RegSlot v1)
{
    emit_reg2<op_type::cvt_fi>(itl,func,dst,v1);
}

void cvt_if(Interloper& itl, Function& func, RegSlot dst, RegSlot v1)
{
    emit_reg2<op_type::cvt_if>(itl,func,dst,v1);
}

RegSlot add_res(Interloper& itl,Function& func, RegSlot v1, RegSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    add(itl,func,tmp,v1,v2);

    return tmp;
}

void sub(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::sub_reg>(itl,func,dst,v1,v2);
}

RegSlot sub_res(Interloper& itl,Function& func, RegSlot v1, RegSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    sub(itl,func,tmp,v1,v2);

    return tmp;
}

void mul(Interloper& itl, Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::mul_reg>(itl,func,dst,v1,v2);
}

RegSlot mul_res(Interloper& itl, Function& func, RegSlot v1, RegSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    mul(itl,func,tmp,v1,v2);

    return tmp;
}

void asr(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::asr_reg>(itl,func,dst,v1,v2);
}

void lsr(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::lsr_reg>(itl,func,dst,v1,v2);
}

void lsr_imm(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, s32 v2)
{
    emit_imm2<op_type::lsr_imm>(itl,func,dst,v1,v2);
}

RegSlot lsr_imm_res(Interloper& itl,Function& func, RegSlot v1, s32 v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    lsr_imm(itl,func,tmp,v1,v2);

    return tmp;
}


void lsl(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::lsl_reg>(itl,func,dst,v1,v2);
}

void lsl_imm(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, s32 v2)
{
    emit_imm2<op_type::lsl_imm>(itl,func,dst,v1,v2);
}

RegSlot lsl_imm_res(Interloper& itl,Function& func, RegSlot v1, s32 v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    lsl_imm(itl,func,tmp,v1,v2);

    return tmp;
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
        emit_imm2<op_type::mul_imm>(itl,func,dst,src,imm);
    }
}

RegSlot mul_imm_res(Interloper& itl, Function& func, RegSlot src,u32 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    mul_imm(itl,func,tmp,src,imm);

    return tmp;   
}

void udiv_imm(Interloper& itl, Function& func, RegSlot dst,RegSlot src,u32 imm)
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
        emit_reg3<op_type::udiv_reg>(itl,func,dst,src,v2);
    }
}

RegSlot udiv_imm_res(Interloper& itl, Function& func, RegSlot src,u32 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    udiv_imm(itl,func,tmp,src,imm);

    return tmp;   
}

void not_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::not_reg>(itl,func,dst,src);
}

void call(Interloper& itl,Function& func, LabelSlot slot)
{
    func.leaf_func = false;
    spill_func_bounds(itl,func);

    emit_label1<op_type::call>(itl,func,slot);
}

void call_reg(Interloper& itl,Function& func, RegSlot slot)
{
    func.leaf_func = false;
    spill_func_bounds(itl,func);

    emit_reg1<op_type::call_reg>(itl,func,slot);
}

void branch_reg(Interloper& itl, Function&func, RegSlot target)
{
    emit_branch_reg<op_type::b_reg>(itl,func,target);
}

void ret(Interloper& itl, Function& func)
{
    add_func_exit(func,cur_block(func));
    emit_implicit<op_type::ret>(itl,func);
}

void syscall(Interloper& itl, Function& func)
{
    func.leaf_func = false;
    emit_implicit<op_type::syscall>(itl,func);
}

void sign_extend_byte(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::sxb>(itl,func,dst,src);
}

void sign_extend_half(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::sxh>(itl,func,dst,src);
}

void sign_extend_word(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::sxw>(itl,func,dst,src);
}

void mov_reg(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::mov_reg>(itl,func,dst,src);
}

void mov_unlock(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::mov_unlock>(itl,func,dst,src);
}

void mov_float(Interloper& itl, Function& func, RegSlot dst, RegSlot src)
{
    emit_reg2<op_type::movf_reg>(itl,func,dst,src);
}

void and_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::and_imm>(itl,func,dst,src,imm);
}

void xor_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::xor_imm>(itl,func,dst,src,imm);
}

void add_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::add_imm>(itl,func,dst,src,imm);
}

RegSlot add_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    add_imm(itl,func,tmp,src,imm);

    return tmp;
}

void sub_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::sub_imm>(itl,func,dst,src,imm);
}

void cmp_signed_gt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::cmpsgt_imm>(itl,func,dst,src,imm);
}

void cmp_unsigned_gt_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::cmpugt_imm>(itl,func,dst,src,imm);
}

void store_byte(Interloper& itl,Function& func, RegSlot src, RegSlot addr, u64 imm)
{
    emit_store<op_type::sb>(itl,func,src,addr,imm);
}

void store_half(Interloper& itl,Function& func, RegSlot src, RegSlot addr, u64 imm)
{
    emit_store<op_type::sh>(itl,func,src,addr,imm);
}


void store_word(Interloper& itl,Function& func, RegSlot src, RegSlot addr, u64 imm)
{
    emit_store<op_type::sw>(itl,func,src,addr,imm);
}

void store_double(Interloper& itl,Function& func, RegSlot src, RegSlot addr, u64 imm)
{
    emit_store<op_type::sd>(itl,func,src,addr,imm);
}

void store_float(Interloper& itl,Function& func, RegSlot src, RegSlot addr, u64 imm)
{
    emit_store<op_type::sf>(itl,func,src,addr,imm);
}

void load_byte(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lb>(itl,func,dst,addr,imm);
}

void load_half(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lh>(itl,func,dst,addr,imm);
}

void load_word(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lw>(itl,func,dst,addr,imm);
}

void load_double(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::ld>(itl,func,dst,addr,imm);
}

void load_float(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lf>(itl,func,dst,addr,imm);
}



void load_signed_byte(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lsb>(itl,func,dst,addr,imm);
}

void load_signed_half(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lsh>(itl,func,dst,addr,imm);
}

void load_signed_word(Interloper& itl,Function& func, RegSlot dst, RegSlot addr, u64 imm)
{
    emit_load<op_type::lsw>(itl,func,dst,addr,imm);
}

void lea(Interloper& itl,Function& func, RegSlot dst, RegSlot ptr, u32 offset)
{
    emit_load<op_type::lea>(itl,func,dst,ptr,offset);
}

RegSlot lea_res(Interloper& itl,Function& func, RegSlot ptr, u32 offset)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    lea(itl,func,tmp,ptr,offset);

    return tmp;
}



// copy gpr sized reg
RegSlot copy_reg(Interloper& itl, Function& func, RegSlot src)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    mov_reg(itl,func,tmp,src);

    return tmp;
}

void push_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    emit_reg1<op_type::push_arg>(itl,func,src);
}

void push_float_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    emit_reg1<op_type::push_float_arg>(itl,func,src);
}


void cmp_eq_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::cmpeq_imm>(itl,func,dst,src,imm);
}

void cmp_eq(Interloper& itl, Function& func,RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::cmpeq_reg>(itl,func,dst,v1,v2);
}

RegSlot cmp_eq_res(Interloper& itl, Function& func, RegSlot v1, RegSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_eq(itl,func,tmp,v1,v2);

    return tmp;   
}

void cmp_ne_imm(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    emit_imm2<op_type::cmpne_imm>(itl,func,dst,src,imm);
}

void cmp_ne(Interloper& itl, Function& func,RegSlot dst, RegSlot v1, RegSlot v2)
{
    emit_reg3<op_type::cmpne_reg>(itl,func,dst,v1,v2);
}

RegSlot cmp_ne_res(Interloper& itl, Function& func, RegSlot v1, RegSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_ne(itl,func,tmp,v1,v2);

    return tmp;   
}

RegSlot cmp_eq_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_eq_imm(itl,func,tmp,src,imm);

    return tmp;    
}

RegSlot cmp_ne_imm_res(Interloper& itl, Function& func, RegSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_ne_imm(itl,func,tmp,src,imm);

    return tmp;    
}

RegSlot imm_zero(Interloper& itl, Function& func)
{
    return mov_imm_res(itl,func,0);
}
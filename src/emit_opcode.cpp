

void add(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    emit_reg3<op_type::add_reg>(itl,func,dst,v1,v2);
}

SymSlot add_res(Interloper& itl,Function& func, SymSlot v1, SymSlot v2)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    add(itl,func,tmp,v1,v2);

    return tmp;
}

void sub(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    emit_reg3<op_type::sub_reg>(itl,func,dst,v1,v2);
}

void asr(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    emit_reg3<op_type::asr_reg>(itl,func,dst,v1,v2);
}

void lsr(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    emit_reg3<op_type::lsr_reg>(itl,func,dst,v1,v2);
}

void lsl(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    emit_reg3<op_type::lsl_reg>(itl,func,dst,v1,v2);
}

void not_reg(Interloper& itl, Function& func, SymSlot dst, SymSlot src)
{
    emit_reg2<op_type::not_reg>(itl,func,dst,src);
}

void call(Interloper& itl,Function& func, LabelSlot slot, b32 save_regs = false)
{
    if(save_regs)
    {
        spill_rv(itl,func);
    }

    spill_func_bounds(itl,func);

    emit_label1<op_type::call>(itl,func,slot);
}

void call_reg(Interloper& itl,Function& func, SymSlot slot, b32 save_regs = false)
{
    if(save_regs)
    {
        spill_rv(itl,func);
    }

    spill_func_bounds(itl,func);

    emit_reg1<op_type::call_reg>(itl,func,slot);
}

void branch_reg(Interloper& itl, Function&func, SymSlot target)
{
    emit_reg1<op_type::b_reg>(itl,func,target);
}

void ret(Interloper& itl, Function& func)
{
    add_func_exit(func,cur_block(func));

    spill_func_bounds(itl,func);
    emit_implicit<op_type::ret>(itl,func);
}

void syscall(Interloper& itl, Function& func, u32 syscall)
{
    emit_imm0<op_type::swi>(itl,func,syscall);
}

void sign_extend_byte(Interloper& itl, Function& func, SymSlot dst, SymSlot src)
{
    emit_reg2<op_type::sxb>(itl,func,dst,src);
}

void sign_extend_half(Interloper& itl, Function& func, SymSlot dst, SymSlot src)
{
    emit_reg2<op_type::sxh>(itl,func,dst,src);
}

void sign_extend_word(Interloper& itl, Function& func, SymSlot dst, SymSlot src)
{
    emit_reg2<op_type::sxw>(itl,func,dst,src);
}

void mov_reg(Interloper& itl, Function& func, SymSlot dst, SymSlot src)
{
    emit_reg2<op_type::mov_reg>(itl,func,dst,src);
}


void and_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::and_imm>(itl,func,dst,src,imm);
}

void mul_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::mul_imm>(itl,func,dst,src,imm);
}

void xor_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::xor_imm>(itl,func,dst,src,imm);
}

SymSlot mul_imm_res(Interloper& itl, Function& func, SymSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    mul_imm(itl,func,tmp,src,imm);

    return tmp;
}

void add_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::add_imm>(itl,func,dst,src,imm);
}

SymSlot add_imm_res(Interloper& itl, Function& func, SymSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    add_imm(itl,func,tmp,src,imm);

    return tmp;
}

void sub_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::sub_imm>(itl,func,dst,src,imm);
}

void cmp_signed_gt_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::cmpsgt_imm>(itl,func,dst,src,imm);
}

void cmp_unsigned_gt_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::cmpugt_imm>(itl,func,dst,src,imm);
}

void store_byte(Interloper& itl,Function& func, SymSlot src, SymSlot addr, u64 imm)
{
    emit_store<op_type::sb>(itl,func,src,addr,imm);
}

void store_half(Interloper& itl,Function& func, SymSlot src, SymSlot addr, u64 imm)
{
    emit_store<op_type::sh>(itl,func,src,addr,imm);
}


void store_word(Interloper& itl,Function& func, SymSlot src, SymSlot addr, u64 imm)
{
    emit_store<op_type::sw>(itl,func,src,addr,imm);
}

void store_double(Interloper& itl,Function& func, SymSlot src, SymSlot addr, u64 imm)
{
    emit_store<op_type::sd>(itl,func,src,addr,imm);
}

void load_byte(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lb>(itl,func,dst,addr,imm);
}

void load_half(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lh>(itl,func,dst,addr,imm);
}

void load_word(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lw>(itl,func,dst,addr,imm);
}

void load_double(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::ld>(itl,func,dst,addr,imm);
}

void load_signed_byte(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lsb>(itl,func,dst,addr,imm);
}

void load_signed_half(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lsh>(itl,func,dst,addr,imm);
}

void load_signed_word(Interloper& itl,Function& func, SymSlot dst, SymSlot addr, u64 imm)
{
    emit_load<op_type::lsw>(itl,func,dst,addr,imm);
}

// TODO: this doesn't support 64 bit values
void mov_imm(Interloper& itl, Function& func, SymSlot dst, u64 imm)
{
    emit_imm1<op_type::mov_imm>(itl,func,dst,imm);
}

SymSlot mov_imm_res(Interloper& itl, Function& func, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    mov_imm(itl,func,tmp,imm);

    return tmp;
}

void lea(Interloper& itl,Function& func, SymSlot dst, SymSlot ptr, u32 offset)
{
    emit_load<op_type::lea>(itl,func,dst,ptr,offset);
}

SymSlot lea_res(Interloper& itl,Function& func, SymSlot ptr, u32 offset)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    lea(itl,func,tmp,ptr,offset);

    return tmp;
}



// copy gpr sized reg
SymSlot copy_reg(Interloper& itl, Function& func, SymSlot src)
{
    const auto tmp = new_tmp(func,GPR_SIZE);

    mov_reg(itl,func,tmp,src);

    return tmp;
}

void push_arg(Interloper& itl, Function& func, SymSlot src)
{
    emit_reg1<op_type::push_arg>(itl,func,src);
}


void cmp_eq_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::cmpeq_imm>(itl,func,dst,src,imm);
}

void cmp_ne_imm(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    emit_imm2<op_type::cmpne_imm>(itl,func,dst,src,imm);
}

SymSlot cmp_eq_imm_res(Interloper& itl, Function& func, SymSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_eq_imm(itl,func,tmp,src,imm);

    return tmp;    
}

SymSlot cmp_ne_imm_res(Interloper& itl, Function& func, SymSlot src, u64 imm)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    cmp_ne_imm(itl,func,tmp,src,imm);

    return tmp;    
}

SymSlot imm_zero(Interloper& itl, Function& func)
{
    return mov_imm_res(itl,func,0);
}
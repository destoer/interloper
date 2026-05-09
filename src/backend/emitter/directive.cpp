ConstIrRegSpan directive_reg_span(const Directive& directive, IrRegSpan& reg)
{
    for(u32 i = 0; i < directive.size; i++)
    {
        const auto& oper = directive.operand[i];

        switch(oper.type)
        {
            case directive_operand_type::dst_src:
            {
                reg.dst_src[reg.dst_src.size++] = oper.ir_reg;
                break;
            }

            case directive_operand_type::dst:
            {
                reg.dst[reg.dst.size++] = oper.ir_reg;
                break;
            }

            case directive_operand_type::src:
            {
                reg.src[reg.src.size++] = oper.ir_reg;
                break;
            }

            default: break;
        }
    }

    return reg;
}

DirectiveOperand make_reg_operand(RegSlot slot,ir_reg_type rtype)
{
    DirectiveOperand oper;
    oper.ir_reg = slot;
    oper.type = directive_operand_type(rtype);

    return oper;
}


DirectiveOperand make_lowered_reg_operand(lowered_reg_t reg)
{
    DirectiveOperand oper;
    oper.reg = reg;
    oper.type = directive_operand_type::lowered_reg;

    return oper;
}


DirectiveOperand make_decimal_operand(f64 decimal)
{
    DirectiveOperand oper;
    oper.decimal = decimal;
    oper.type = directive_operand_type::decimal;

    return oper;
}

DirectiveOperand make_imm_operand(u64 imm)
{
    DirectiveOperand oper;
    oper.imm = imm;
    oper.type = directive_operand_type::imm;

    return oper;
}

DirectiveOperand make_reg_set_operand(u64 set)
{
    DirectiveOperand oper;
    oper.reg_set = set;
    oper.type = directive_operand_type::reg_set;

    return oper;
}


DirectiveOperand make_label_operand(LabelSlot slot)
{
    DirectiveOperand oper;
    oper.label = slot;
    oper.type = directive_operand_type::label;

    return oper;
}

DirectiveOperand make_spec_operand(spec_reg reg)
{
    return make_reg_operand(make_spec_reg_slot(reg),ir_reg_type::directive);
}

DirectiveOperand make_pool_operand(PoolSlot slot)
{
    DirectiveOperand oper;
    oper.pool = slot;
    oper.type = directive_operand_type::pool;

    return oper;
}


Opcode make_directive_one(directive_type type,const DirectiveOperand& v1)
{
    Directive directive;
    directive.type = type;
    directive.operand[directive.size++] = v1;

    return Opcode(directive);
}

Opcode make_directive_two(directive_type type,const DirectiveOperand& v1,const DirectiveOperand& v2)
{
    Directive directive;
    directive.type = type;
    directive.operand[directive.size++] = v1;
    directive.operand[directive.size++] = v2;

    return Opcode(directive);
}

Opcode make_directive_three(directive_type type,const DirectiveOperand& v1,const DirectiveOperand& v2, const DirectiveOperand& v3)
{
    Directive directive;
    directive.type = type;
    directive.operand[directive.size++] = v1;
    directive.operand[directive.size++] = v2;
    directive.operand[directive.size++] = v3;

    return Opcode(directive);
}

void emit_directive_reg1(Interloper& itl, Function& func, directive_type type, const DirectiveReg& reg)
{
    const auto opcode = make_directive_one(type,make_reg_operand(reg.slot,reg.type));
    emit_block_func(itl,func,opcode);
}

void emit_directive_reg2(Interloper& itl, Function& func, directive_type type, const DirectiveReg& v1, const DirectiveReg& v2)
{
    const auto opcode = make_directive_two(type,make_reg_operand(v1.slot,v1.type),make_reg_operand(v2.slot,v2.type));
    emit_block_func(itl,func,opcode);  
}


void emit_directive_imm1(Interloper& itl, Function& func, directive_type type, u64 imm)
{
    const auto opcode = make_directive_one(type,make_imm_operand(imm));
    emit_block_func(itl,func,opcode);
}

void emit_directive_reg_imm2(Interloper& itl, Function& func, directive_type type, const DirectiveReg& v1, u64 v2, u64 v3)
{
    const auto opcode = make_directive_three(type,make_reg_operand(v1.slot,v1.type), make_imm_operand(v2), make_imm_operand(v3));
    emit_block_func(itl,func,opcode);
}

void emit_directive_reg_imm1(Interloper& itl, Function& func, directive_type type, const DirectiveReg& v1, u64 v2)
{
    const auto opcode = make_directive_two(type,make_reg_operand(v1.slot,v1.type),make_imm_operand(v2));
    emit_block_func(itl,func,opcode);    
}

void emit_directive_reg_set(Interloper& itl, Function& func, directive_type type, u64 set)
{
    const auto opcode = make_directive_one(type, make_reg_set_operand(set));
    emit_block_func(itl,func,opcode);     
}


void alloc_local_array(Interloper& itl, Function& func, RegSlot dst, const u64 size, const u64 count)
{
    const DirectiveReg dst_reg = {dst,ir_reg_type::dst};
    emit_directive_reg_imm2(itl,func,directive_type::alloc_local_array,dst_reg,size,count);
}

void alloc_global_array(Interloper& itl, Function& func, RegSlot dst, const u64 alloc_idx)
{
    const DirectiveReg dst_reg = {dst,ir_reg_type::dst};
    emit_directive_reg_imm1(itl,func,directive_type::alloc_global_array,dst_reg,alloc_idx);
}


void clean_args(Interloper& itl, Function& func, u64 args)
{
    if(args != 0)
    {
        emit_directive_imm1(itl,func,directive_type::clean_args,args);
    }
}

void unlock_reg_set(Interloper& itl, Function& func, u64 set)
{
    if(set != 0)
    {
        emit_directive_reg_set(itl,func,directive_type::unlock_reg_set,set);
    }
}


void lock_reg_set(Interloper& itl, Function& func, u64 set)
{
    if(set != 0)
    {
        emit_directive_reg_set(itl,func,directive_type::lock_reg_set,set);
    }
}

void lock_reg(Interloper& itl, Function& func, spec_reg reg)
{
    const DirectiveReg v1 = {make_spec_reg_slot(reg),ir_reg_type::directive};
    emit_directive_reg1(itl,func,directive_type::lock_reg,v1);
}


void push_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    const DirectiveReg v1 = {src,ir_reg_type::src};
    emit_directive_reg1(itl,func,directive_type::push_arg,v1);
}

void push_float_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    const DirectiveReg v1 = {src,ir_reg_type::src};
    emit_directive_reg1(itl,func,directive_type::push_float_arg,v1);
}


void reload_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    const DirectiveReg v1 = {reg.slot,ir_reg_type::directive};
    emit_directive_reg1(itl,func,directive_type::reload_slot,v1);
}

void spill_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    const DirectiveReg v1 = {reg.slot,ir_reg_type::directive};
    emit_directive_reg1(itl,func,directive_type::spill_slot,v1);
}

void mov_unlock(Interloper& itl, Function& func, RegSlot dst, spec_reg spec)
{
    const DirectiveReg v1 = {dst,ir_reg_type::dst};
    const DirectiveReg v2 = {make_spec_reg_slot(spec),ir_reg_type::src};
    emit_directive_reg2(itl,func,directive_type::mov_unlock,v1,v2);
}

void pool_addr(Interloper& itl, Function& func, RegSlot dst, PoolSlot pool_slot, u32 offset)
{
    const auto opcode = make_directive_three(directive_type::pool_addr,
        make_reg_operand(dst,ir_reg_type::dst), make_pool_operand(pool_slot), make_imm_operand(offset)
    );

    emit_block_func(itl,func,opcode);
}

RegSlot pool_addr_res(Interloper& itl, Function& func, PoolSlot pool_slot, u32 offset)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    pool_addr(itl,func,tmp,pool_slot,offset);

    return tmp;
}

void alloc_stack(Interloper& itl, Function& func, u32 size)
{
    emit_directive_imm1(itl,func,directive_type::alloc_stack,size);
}

void alloc_slot(Interloper& itl, Function& func, RegSlot src, bool forced)
{
    const auto opcode = make_directive_two(directive_type::alloc_slot, make_reg_operand(src,ir_reg_type::directive),make_imm_operand(forced));
    emit_block_func(itl,func,opcode);  
}

void load_func_addr(Interloper& itl, Function& func, RegSlot dst, LabelSlot label)
{
    const auto opcode = make_directive_two(directive_type::load_func_addr, make_reg_operand(dst,ir_reg_type::dst),make_label_operand(label));
    emit_block_func(itl,func,opcode);  
}

Opcode make_spill(lowered_reg_t reg, RegSlot slot, u64 stack_offset)
{
    const auto reg_oper = make_lowered_reg_operand(reg);
    const auto slot_oper = make_reg_operand(slot,ir_reg_type::directive);
    const auto opcode = make_directive_three(directive_type::spill,reg_oper,slot_oper,make_imm_operand(stack_offset));

    return opcode;
}

Opcode make_load(lowered_reg_t reg, RegSlot slot, u64 stack_offset)
{
    const auto reg_oper = make_lowered_reg_operand(reg);
    const auto slot_oper = make_reg_operand(slot,ir_reg_type::directive);
    const auto opcode = make_directive_three(directive_type::load,reg_oper,slot_oper,make_imm_operand(stack_offset));

    return opcode;
}
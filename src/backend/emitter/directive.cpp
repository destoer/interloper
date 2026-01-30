void emit_directive(Interloper& itl, Function& func, const Directive& directive)
{
    UNUSED(itl);

    Opcode opcode;
    opcode.group = op_group::directive;
    opcode.directive = directive;

    emit_block_func(func,opcode);
}

DirectiveOperand make_reg_operand(RegSlot slot)
{
    DirectiveOperand oper;
    oper.reg = slot;
    oper.type = directive_operand_type::reg;

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

DirectiveOperand make_label_operand(LabelSlot slot)
{
    DirectiveOperand oper;
    oper.label = slot;
    oper.type = directive_operand_type::label;

    return oper;
}
DirectiveOperand make_spec_operand(spec_reg reg)
{
    return make_reg_operand(make_spec_reg_slot(reg));
}

void emit_directive_reg1(Interloper& itl, Function& func, directive_type type, RegSlot reg)
{
    Directive directive;
    directive.type = type;
    directive.operand[0] = make_reg_operand(reg);
    emit_directive(itl,func,directive);  
}

void emit_directive_imm1(Interloper& itl, Function& func, directive_type type, u64 imm)
{
    Directive directive;
    directive.type = type;
    directive.operand[0] = make_imm_operand(imm);
    emit_directive(itl,func,directive);  
}

void clean_args(Interloper& itl, Function& func, u64 args)
{
    emit_directive_imm1(itl,func,directive_type::clean_args,args);
}

void unlock_reg_set(Interloper& itl, Function& func, u64 set)
{
    emit_directive_imm1(itl,func,directive_type::unlock_reg_set,set);
}



void push_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    handle_src_storage(itl,func,src);
    emit_directive_reg1(itl,func,directive_type::push_arg,src);
}

void reload_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    emit_directive_reg1(itl,func,directive_type::reload_slot,reg.slot);
}

void spill_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    emit_directive_reg1(itl,func,directive_type::spill_slot,reg.slot);
}
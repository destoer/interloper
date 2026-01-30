void emit_directive(Interloper& itl, Function& func, const Directive& directive)
{
    UNUSED(itl);

    Opcode opcode;
    opcode.group = op_group::directive;
    opcode.directive = directive;

    emit_block_func(func,opcode);
}

inline DirectiveOperand make_reg_operand(RegSlot slot)
{
    DirectiveOperand oper;
    oper.reg = slot;
    oper.type = directive_operand_type::reg;

    return oper;
}

void emit_directive_reg1(Interloper& itl, Function& func, directive_type type, RegSlot reg)
{
    Directive directive;
    directive.type = type;
    directive.operand[0] = make_reg_operand(reg);
    emit_directive(itl,func,directive);  
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
const RegSpan directive_reg_span(const Directive& directive, RegBuffer& reg)
{
    reg.src.size = 0;
    reg.dst.size = 0;

    for(const auto& oper : directive.operand)
    {
        switch(oper.type)
        {
            case directive_operand_type::dst_src:
            {
                reg.dst[reg.dst.size++] = oper.reg;
                reg.src[reg.src.size++] = oper.reg;
                break;
            }


            case directive_operand_type::dst:
            {
                reg.dst[reg.dst.size++] = oper.reg;
                break;
            }

            case directive_operand_type::src:
            {
                reg.src[reg.src.size++] = oper.reg;
                break;
            }

            default: break;
        }
    }

    return reg;
}

void emit_directive(Interloper& itl, Function& func, const Directive& directive)
{
    Opcode opcode;
    opcode.group = op_group::directive;
    opcode.directive = directive;

    emit_block_func(itl,func,opcode);
}

DirectiveOperand make_reg_operand(RegSlot slot,ir_reg_type reg_type)
{
    DirectiveOperand oper;
    oper.reg = slot;
    oper.type = directive_operand_type(reg_type);

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
    return make_reg_operand(make_spec_reg_slot(reg),ir_reg_type::directive);
}

void emit_directive_reg1(Interloper& itl, Function& func, directive_type type, RegSlot reg, ir_reg_type reg_type)
{
    Directive directive;
    directive.type = type;
    directive.operand[0] = make_reg_operand(reg,reg_type);
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

void lock_reg(Interloper& itl, Function& func, spec_reg reg)
{
    emit_directive_reg1(itl,func,directive_type::lock_reg,make_spec_reg_slot(reg),ir_reg_type::directive);
}


void push_arg(Interloper& itl, Function& func, ArgPass& pass, RegSlot src)
{
    pass.arg_clean++;
    emit_directive_reg1(itl,func,directive_type::push_arg,src,ir_reg_type::src);
}

void reload_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    emit_directive_reg1(itl,func,directive_type::reload_slot,reg.slot,ir_reg_type::directive);
}

void spill_slot(Interloper& itl, Function& func, const Reg& reg)
{
    if(stored_in_mem(reg))
    {
        return;
    }

    emit_directive_reg1(itl,func,directive_type::spill_slot,reg.slot,ir_reg_type::src);
}
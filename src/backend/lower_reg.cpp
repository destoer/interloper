void lower_directive(Directive& directive, const ConstLoweredRegSpan& regs)
{
    u32 src = 0;
    u32 dst = 0;
    u32 dst_src = 0;

    for(auto& operand : directive.operand)
    {
        switch(operand.type)
        {
            case directive_operand_type::dst_src:
            {
                operand = make_lowered_reg_operand(regs.dst_src[dst_src++]);
                break;
            }

            case directive_operand_type::dst:
            {
                operand = make_lowered_reg_operand(regs.dst_src[dst++]);
                break;
            }

            case directive_operand_type::src:
            {
                operand = make_lowered_reg_operand(regs.src[src++]);
                break;
            }

            default: break;
        }
    }
}

void lower_mov_gpr_imm(MovGprImm& mov, const ConstLoweredRegSpan& regs)
{
    mov.dst.reg = regs.dst[0];
}

template<typename op_type, op_group group>
void lower_unary_reg2(UnaryReg2<op_type,group>& unary, const ConstLoweredRegSpan& regs)
{
    unary.dst.reg = regs.dst[0];
    unary.src.reg = regs.src[0];
}

void lower_opcode(Opcode& opcode, const ConstLoweredRegSpan& regs)
{
    UNUSED(regs);

    switch(opcode.group)
    {
        case op_group::directive:
        {
            lower_directive(opcode.directive,regs);
            break;
        }

        case op_group::mov_gpr_imm:
        {
            lower_mov_gpr_imm(opcode.mov_gpr_imm,regs);
            break;
        }

        case op_group::unary_reg2:
        {
            lower_unary_reg2(opcode.unary_reg2,regs);
            break;
        }

        case op_group::implicit: break;
        case op_group::branch_label: break;

        default:
        {
            unimplemented("Lower registers for %d",u32(opcode.group));
        }
    }

    opcode.lowered = true;
}
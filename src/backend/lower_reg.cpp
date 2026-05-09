void lower_directive(Opcode& opcode, const ConstLoweredRegSpan& regs)
{
    u32 src = 0;
    u32 dst = 0;
    u32 dst_src = 0;

    auto& directive = opcode.directive;

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

void lower_opcode(Opcode& opcode, const ConstLoweredRegSpan& regs)
{
    UNUSED(regs);

    switch(opcode.group)
    {
        case op_group::directive:
        {
            lower_directive(opcode,regs);
            break;
        }

        default:
        {
            unimplemented("Lower registers for %d",u32(opcode.group));
        }
    }

    opcode.lowered = true;
}
void allocate_and_rewrite_opcode(LinearAlloc& alloc, Block& block, OpcodeNode* node);

void lower_directive_regs(Directive& directive, const ConstLoweredRegSpan& regs)
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

OpcodeNode* lower_directive_pass1(LinearAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& directive = node->value.directive;

    switch(directive.type)
    {
        case directive_type::load_const_float: 
        {
            assert(false);
            break;
        }

        case directive_type::live_var:
        {
            assert(false);
            break;
        }

        case directive_type::lock_reg:
        {
            assert(false);
            break;
        }

        case directive_type::mov_unlock:
        {
            assert(false);
            break;
        }

        case directive_type::lock_reg_set:
        {
            assert(false);
            break;
        }

        case directive_type::unlock_reg_set:
        {
            assert(false);
            break;
        }

        case directive_type::load_func_addr:
        {
            assert(false);
            break;
        }
        
        case directive_type::reload_slot:
        {
            assert(false);
            break;
        }

        case directive_type::spill_slot:
        {
            assert(false);
            break;
        }

        case directive_type::push_arg:
        {
            assert(false);
            break;
        }

        case directive_type::push_float_arg:
        {
            assert(false);
            break;
        }

    
        case directive_type::clean_args:
        {
            assert(false);
            break;
        }

        case directive_type::alloc_stack:
        {
            assert(false);
            break;
        }

        case directive_type::alloc_slot:
        {
            const auto slot = directive.operand[0].ir_reg;
            auto& reg = reg_from_slot(slot,alloc);

            const b32 force_lower = directive.operand[1].imm;

            log_reg(alloc.print,*alloc.table,"alloc slot: %r : %s\n",slot,force_lower? "forced" : "unforced");

            // explicitly force a stack alloc now
            if(force_lower && reg.segment != reg_segment::global)
            {
                stack_reserve_reg(alloc.stack_alloc,reg);
            }

            node = remove(block.list,node);  
            break;       
        }

        case directive_type::alloc_local_array:
        {
            assert(false);
            break;
        }

        case directive_type::alloc_global_array:
        {
            assert(false);
            break;
        }

        case directive_type::spill_func_bounds:
        {
            assert(false);
            break;
        }

        case directive_type::pool_addr:
        {
            assert(false);
            break;
        }

        default:
        {
            allocate_and_rewrite_opcode(alloc,block,node);
            node = node->next;
            break;
        }
    }

    return node;
}



template<typename op_type,const bool IS_LOAD, op_group group>
void lower_addr_struct(LinearAlloc& alloc, AddrOpcode<op_type,IS_LOAD,true,group>& addr, const ConstLoweredRegSpan& regs)
{    
    const u32 scale = addr.addr_ir.scale;
    const u32 offset = addr.addr_ir.offset;
    const auto base = addr.addr_ir.base;

    if constexpr(IS_LOAD)
    {
        addr.v1.reg = regs.dst[0];
    }

    addr.addr.index = regs.src[0];
    addr.addr.scale = scale;
    addr.addr.offset = offset;

    addr.addr.base_ir = base;

    auto& reg = reg_from_slot(addr.addr.base_ir,alloc);

    // add the stack offset, so this correctly offset for when we fully rewrite this
    if(is_local_reg(reg))
    {
        addr.addr.offset += alloc.stack_alloc.stack_offset;
    }
}

template<typename op_type,const bool IS_LOAD, op_group group>
void lower_addr_pointer(AddrOpcode<op_type,IS_LOAD,false,group>& addr, const ConstLoweredRegSpan& regs)
{    
    const u32 scale = addr.addr_ir.scale;
    const u32 offset = addr.addr_ir.offset;

    if constexpr(IS_LOAD)
    {
        addr.v1.reg = regs.dst[0];
    }

    addr.addr.index = regs.src[0];
    addr.addr.base = regs.src[1];
    addr.addr.scale = scale;
    addr.addr.offset = offset;
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

void lower_opcode(LinearAlloc& alloc, Opcode& opcode, const ConstLoweredRegSpan& regs)
{
    switch(opcode.group)
    {
        case op_group::directive:
        {
            lower_directive_regs(opcode.directive,regs);
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

        case op_group::addrof:
        {
            // -> <addrof> <alloced reg> <slot> <stack offset>
            // -> lea <alloced reg> <sp + whatever>
            const auto base = opcode.addrof.addr_ir.base;
            const auto dst = regs.dst[0];
            auto& reg = reg_from_slot(base,alloc);

            log_reg(alloc.print,*alloc.table,"addrof %r <- %r\n",dst,base);

            if(is_reg_mem_unallocated(reg))
            {
                assert(stored_in_mem(reg));
                stack_reserve_reg(alloc.stack_alloc,reg);
            }

            lower_addr_struct(alloc,opcode.addrof,regs);
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
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

OpcodeNode* lower_directive_pass2(Interloper& itl, LinearAlloc& alloc,Block& block, OpcodeNode* node)
{
    UNUSED(itl); UNUSED(alloc); UNUSED(block); UNUSED(node);
    assert(false);
}

OpcodeNode* lower_directive_pass1(Interloper& itl, LinearAlloc& alloc,Block& block, OpcodeNode* node)
{
    auto& directive = node->value.directive;

    switch(directive.type)
    {
        case directive_type::load_const_float: 
        {
            // grab the offset we want
            const auto pool_slot = directive.operand[1].pool;
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            const auto dst = directive.operand[0].ir_reg;
            const auto addr = make_addr(make_spec_reg_slot(spec_reg::const_seg),section.offset);
            node->value = Opcode(make_addr_op<Load>(dst,addr,load_type::lf));

            allocate_and_rewrite_opcode(alloc,block,node);
            return node->next;
        }

        case directive_type::live_var:
        {
            const auto slot = directive.operand[0].ir_reg;
            auto& ir_reg = reg_from_slot(slot,alloc);

            // issue a reload
            if(is_reg_locally_allocated(ir_reg))
            {
                reload_reg(alloc,block,node,slot,ir_reg.local_reg,insertion_type::before);
            }

            return remove(block.list,node);
        }

        case directive_type::lock_reg:
        {
            const auto reg = directive.operand[0].ir_reg;
            lock_special_reg(alloc,block,node,reg.spec);

            return remove(block.list,node);
        }

        case directive_type::mov_unlock:
        {
            const auto reg = directive.operand[1].ir_reg;
            const auto dst = directive.operand[0].ir_reg;
            unlock_special_reg(alloc,reg.spec);

            node->value = mov_reg_ir(dst,reg,reg_type::gpr);

            allocate_and_rewrite_opcode(alloc,block,node);
            return node->next;
        }

        case directive_type::lock_reg_set:
        {
            const auto set = directive.operand[0].reg_set;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(set,r))
                {
                    lock_reg(alloc.gpr,r);
                }
            }

            return remove(block.list,node);
        }

        case directive_type::unlock_reg_set:
        {
            const auto set = directive.operand[0].reg_set;

            for(u32 r = 0; r < MACHINE_REG_SIZE; r++)
            {
                if(is_set(set,r))
                {
                    unlock_reg(alloc.gpr,r);
                }
            }

            return remove(block.list,node);
        }

        case directive_type::load_func_addr:
        {
            // Just rewrite the dst for now
            allocate_and_rewrite_opcode(alloc,block,node);
            return node->next;
        }
        
        case directive_type::reload_slot:
        {
            const auto slot = directive.operand[0].ir_reg;
            reload_slot(alloc,block,node,slot);

            return remove(block.list,node);
        }

        case directive_type::spill_slot:
        {
            const auto slot = directive.operand[0].ir_reg;
            spill(alloc,block,node,slot,insertion_type::before);

            return remove(block.list,node);
        }

        case directive_type::push_arg:
        {
            node->value =  make_reg1_src(directive.operand[0].ir_reg,reg1_src_type::push);

            // adjust opcode for reg alloc
            allocate_and_rewrite_opcode(alloc,block,node);

            // variables now have to be accessed at a different offset
            // until this is corrected by clean call
            alloc.stack_alloc.stack_offset += GPR_SIZE;

            return node->next;
        }

        case directive_type::clean_args:
        {
            // clean up args
            const auto stack_clean = GPR_SIZE * directive.operand[0].imm;

            node->value = Opcode(make_arith_imm2(make_spec_reg_slot(spec_reg::sp),stack_clean,arith_bin_op::add_t));
            alloc.stack_alloc.stack_offset -= stack_clean;

            // adjust opcode for reg alloc
            allocate_and_rewrite_opcode(alloc,block,node);

            log(alloc.stack_alloc.print,"clean args: %x\n",stack_clean);

            return node->next;
        }

        case directive_type::alloc_stack:
        {
            const u32 size = directive.operand[0].imm;

            // nothing to do
            if(size == 0)
            {
                return remove(block.list,node);
            }

            node->value = Opcode(make_arith_imm2(make_spec_reg_slot(spec_reg::sp),size,arith_bin_op::sub_t));
            allocate_and_rewrite_opcode(alloc,block,node);

            alloc.stack_alloc.stack_offset += size;

            if(alloc.stack_alloc.print)
            {
                printf("allocate stack %x\n",size);
            }

            return node->next;
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

            return remove(block.list,node);  
        }

        case directive_type::alloc_local_array:
        {
            const u32 size = directive.operand[1].imm;
            const u32 count = directive.operand[2].imm;
            const auto reg = directive.operand[0].ir_reg;

            const u32 offset = allocate_stack_array(alloc.stack_alloc,*alloc.table,reg.sym_slot,size,count);

            const auto directive = make_directive_two(directive_type::alloc_local_array,make_reg_operand(reg,ir_reg_type::dst),make_imm_operand(offset));
            node->value = Opcode(directive);
            allocate_and_rewrite_opcode(alloc,block,node);

            return node->next;
        }

        case directive_type::alloc_global_array:
        {
            assert(false);
            break;
        }

        case directive_type::spill_func_bounds:
        {
            // clear our any caller saved regs
            save_caller_saved_regs(alloc,block,node);
            return remove(block.list,node);
        }

        default:
        {
            allocate_and_rewrite_opcode(alloc,block,node);
            return node->next;
        }
    }

    return node;
}
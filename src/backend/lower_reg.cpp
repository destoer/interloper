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
            assert(false);
            break;
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

        case directive_type::push_float_arg:
        {
            assert(false);
            break;
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

template<typename op_type, op_group group>
void lower_unary_reg1(UnaryReg1<op_type,group>& unary, const ConstLoweredRegSpan& regs)
{
    unary.dst.reg = regs.dst[0];
}


void lower_reg1_src(RegOneSrc& reg1, const ConstLoweredRegSpan& regs)
{
    reg1.src.reg = regs.src[0];
}

void lower_reg2_src(RegTwoSrc& reg2, const ConstLoweredRegSpan& regs)
{
    reg2.v1.reg = regs.src[0];
    reg2.v2.reg = regs.src[1];
}

template<typename op_type,op_group group>
void lower_reg2_dst(RegTwoDst<op_type,group>& reg2, const ConstLoweredRegSpan& regs)
{
    reg2.dst.reg = regs.dst_src[0];
    reg2.src.reg = regs.src[0];
}

template<typename op_type, op_group group>
void lower_imm2_dst(ImmTwoDst<op_type,group>& imm2, const ConstLoweredRegSpan& regs)
{
    imm2.dst.reg = regs.dst[0];
}


void lower_imm2_src(ImmTwoSrc& imm2, const ConstLoweredRegSpan& regs)
{
    imm2.src.reg = regs.src[0];
}

void lower_branch_reg(BranchReg& branch, const ConstLoweredRegSpan& regs)
{
    branch.src.reg = regs.src[0];
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


        case op_group::branch_reg:
        {
            lower_branch_reg(opcode.branch_reg,regs);
            break;
        }

        case op_group::shift_imm2:
        {
            lower_imm2_dst(opcode.shift_imm2,regs);
            break;
        }

        case op_group::arith_imm2:
        {
            lower_imm2_dst(opcode.arith_imm2,regs);
            break;
        }

        case op_group::imm2_src:
        {
            lower_imm2_src(opcode.imm2_src,regs);
            break;
        }

        case op_group::arith_gpr2:
        {
            lower_reg2_dst(opcode.arith_gpr2,regs);
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

        case op_group::reg2_src:
        {
            lower_reg2_src(opcode.reg2_src,regs);
            break;
        }

        case op_group::reg1_src:
        {
            lower_reg1_src(opcode.reg1_src,regs);
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

        case op_group::lea:
        {
            lower_addr_pointer(opcode.lea,regs);
            break;
        }

        case op_group::load_struct:
        {
            lower_addr_struct(alloc,opcode.load_struct,regs);
            break;            
        }

        case op_group::store_struct:
        {
            lower_addr_struct(alloc,opcode.store_struct,regs);
            break;            
        }

        case op_group::load:
        {
            lower_addr_pointer(opcode.load,regs);
            break;
        }

        case op_group::store:
        {
            lower_addr_pointer(opcode.store,regs);
            break;
        }

        case op_group::set_from_flag_gpr:
        {
            lower_unary_reg1(opcode.set_from_flag_gpr,regs);
            break;
        }

        case op_group::implicit: break;
        case op_group::branch_label: break;
        case op_group::branch_cond_flag: break;

        default:
        {
            unimplemented("Lower registers for %d",u32(opcode.group));
        }
    }

    opcode.lowered = true;
}
void allocate_and_rewrite_opcode(LinearAlloc& alloc, Block& block, OpcodeNode* node);

#include "x86/lower_reg_x86.cpp"
#include "lower_directive.cpp"


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

template<typename op_type,op_group group>
void lower_reg3(RegThree<op_type,group>& reg3, const ConstLoweredRegSpan& regs)
{
    reg3.dst.reg = regs.dst[0];
    reg3.v1.reg = regs.src[0];
    reg3.v2.reg = regs.src[1];
}

template<typename op_type, op_group group>
void lower_imm2_dst(ImmTwoDst<op_type,group>& imm2, const ConstLoweredRegSpan& regs)
{
    imm2.dst.reg = regs.dst[0];
}

template<typename op_type, op_group group>
void lower_imm3(ImmThree<op_type,group>& imm3, const ConstLoweredRegSpan& regs)
{
    imm3.dst.reg = regs.dst[0];
    imm3.src.reg = regs.src[0];
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

        case op_group::arith_imm3:
        {
            lower_imm3(opcode.arith_imm3,regs);
            break;
        }

        case op_group::imm2_src:
        {
            lower_imm2_src(opcode.imm2_src,regs);
            break;
        }

        case op_group::arith_gpr3:
        {
            lower_reg3(opcode.arith_gpr3,regs);
            break;
        }

        case op_group::arith_gpr2:
        {
            lower_reg2_dst(opcode.arith_gpr2,regs);
            break;
        }

        case op_group::arith_fpr2:
        {
            lower_reg2_dst(opcode.arith_fpr2,regs);
            break;
        }

        case op_group::mov_gpr_imm:
        {
            lower_mov_gpr_imm(opcode.mov_gpr_imm,regs);
            break;
        }

        case op_group::unary_reg1:
        {
            lower_unary_reg1(opcode.unary_reg1,regs);
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

        case op_group::set_from_flag_fpr:
        {
            lower_unary_reg1(opcode.set_from_flag_fpr,regs);
            break;
        }

        case op_group::x86_fixed:
        {
            lower_reg2_dst(opcode.x86_fixed,regs);
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
#include "opcode.h"

void reload_slot(Interloper& itl, Function& func, const Reg& dst);
void spill_slot(Interloper& itl, Function& func, const Reg& src);
ConstRegSpan opcode_reg_span(const Opcode& opcode, RegSpan& reg);

void handle_src_storage(Interloper& itl, Function& func, RegSlot src)
{
    if(is_special_reg(src))
    {
        return;
    }

    auto& reg = reg_from_slot(itl.symbol_table,func,src);

    if(is_aliased(reg))
    {
        reload_slot(itl,func,reg);
    }
}

void handle_dst_storage(Interloper& itl, Function& func, RegSlot dst_slot)
{
    if(is_special_reg(dst_slot))
    {
        return;
    }

    auto& reg = reg_from_slot(itl.symbol_table,func,dst_slot);

    if(is_aliased(reg))
    {
        spill_slot(itl,func,reg);
    }
}

const ConstRegSpan blank_reg_span(RegSpan& reg)
{
    reg.dst.size = 0;
    reg.src.size = 0;

    return reg;
}

// NOTE: these are the bottom level emitter only use directly if you need to gen code yourself
OpcodeNode* emit_block_internal(Interloper& itl, Function& func,BlockSlot block_slot, const Opcode& opcode)
{
    const auto reg = opcode_reg_span(opcode,itl.reg_span);

    for(const auto& src: reg.src)
    {
        handle_src_storage(itl,func,src);
    }

    auto& block = block_from_slot(func,block_slot);

    auto &list = block.list;
    append(list,opcode);
    
    for(const auto& dst: reg.dst)
    {
        handle_dst_storage(itl,func,dst);
    }

    return list.finish;    
}

OpcodeNode* emit_block_func(Interloper& itl, Function& func,const Opcode& opcode)
{
    return emit_block_internal(itl,func,cur_block(func),opcode);
}

template<typename T, typename OPCODE_FUNC>
RegSlot opcode_res1(Interloper& itl,Function& func, const T& arg, const OPCODE_FUNC& opcode_func)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    opcode_func(itl,func,tmp,arg);

    return tmp;
}


template<typename T, typename Y, typename OPCODE_FUNC>
RegSlot opcode_res2(Interloper& itl,Function& func, const T& v1, const Y& v2, const OPCODE_FUNC& opcode_func)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    opcode_func(itl,func,tmp,v1,v2);

    return tmp;
}


#include "emitter/directive.cpp"
#include "emitter/mov_imm.cpp"
#include "emitter/arith_reg.cpp"
#include "emitter/arith_imm.cpp"
#include "emitter/branch.cpp"
#include "emitter/implicit.cpp"
#include "emitter/addr.cpp"
#include "emitter/unary.cpp"
#include "emitter/cmp.cpp"

ConstRegSpan opcode_reg_span(const Opcode& opcode, RegSpan& reg)
{
    if(opcode.lowered)
    {
        return blank_reg_span(reg);
    }

    switch(opcode.group)
    {
        case op_group::implicit: return blank_reg_span(reg); 
        case op_group::branch_label: return blank_reg_span(reg); 
        case op_group::branch_reg: return branch_reg_span(opcode.branch_reg,reg);
        case op_group::branch_cond: return branch_cond_reg_span(opcode.branch_cond,reg);
        case op_group::directive: return directive_reg_span(opcode.directive,reg); 
        case op_group::mov_gpr_imm: return mov_gpr_imm_reg_span(opcode.mov_gpr_imm,reg); 
        case op_group::mov_fpr_imm: return mov_fpr_imm_reg_span(opcode.mov_fpr_imm,reg); 
        case op_group::arith_imm3: return imm3_reg_span(opcode.arith_imm3,reg);
        case op_group::arith_gpr3: return reg3_reg_span(opcode.arith_gpr3,reg);
        case op_group::arith_gpr2: return unary_reg2_reg_span(opcode.arith_gpr2,reg);
        case op_group::arith_fpr3: return reg3_reg_span(opcode.arith_fpr3,reg);
        case op_group::shift_imm3: return imm3_reg_span(opcode.shift_imm3,reg);
        case op_group::shift_reg3: return reg3_reg_span(opcode.shift_reg3,reg);
        case op_group::load: return addr_opcode_reg_span(opcode.load,reg);
        case op_group::load_struct: return addr_opcode_reg_span(opcode.load_struct,reg);
        case op_group::store: return addr_opcode_reg_span(opcode.store,reg);
        case op_group::store_struct: return addr_opcode_reg_span(opcode.store_struct,reg);   
        case op_group::lea: return addr_opcode_reg_span(opcode.lea,reg);
        case op_group::addrof: return addr_opcode_reg_span(opcode.addrof,reg);  
        case op_group::unary_reg2: return unary_reg2_reg_span(opcode.unary_reg2,reg);
        case op_group::sign_extend: return unary_reg2_reg_span(opcode.sign_extend,reg);
        case op_group::cmp_imm3: return imm3_reg_span(opcode.cmp_imm3,reg);
        case op_group::cmp_gpr3: return reg3_reg_span(opcode.cmp_gpr3,reg);
        case op_group::cmp_fpr3: return reg3_reg_span(opcode.cmp_fpr3,reg);
    }

    return reg;
}


void destroy_block(Block& block)
{
    destroy_arr(block.entry);
    destroy_arr(block.exit);

    destroy_set(block.live_in);
    destroy_set(block.live_out);
    destroy_set(block.def);
    destroy_set(block.use);

    destroy_arr(block.links);
}

void destroy_emitter(IrEmitter& emitter)
{
    for(u32 b = 0; b < count(emitter.program); b++)
    {
        destroy_block(emitter.program[b]);
    }

    destroy_arr(emitter.program);
}
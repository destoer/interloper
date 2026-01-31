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


void handle_storage(Interloper& itl, Function& func, const ConstRegSpan& reg)
{
    for(const auto& src: reg.src)
    {
        handle_src_storage(itl,func,src);
    }

    for(const auto& dst: reg.dst)
    {
        handle_dst_storage(itl,func,dst);
    }
}

const ConstRegSpan blank_reg_span(RegSpan& reg)
{
    reg.dst.size = 0;
    reg.src.size = 0;

    return reg;
}

// NOTE: these are the bottom level emitter only use directly if you need to gen code yourself
OpcodeNode* emit_block_internal(Function& func,BlockSlot block_slot, const Opcode& opcode)
{
    auto& block = block_from_slot(func,block_slot);

    auto &list = block.list;
    append(list,opcode);
    
    return list.finish;    
}

OpcodeNode* emit_block_func(Interloper& itl, Function& func,const Opcode& opcode)
{
    const auto reg = opcode_reg_span(opcode,itl.reg_span);
    handle_storage(itl,func,reg);

    return emit_block_internal(func,cur_block(func),opcode);
}


#include "emitter/directive.cpp"
#include "emitter/mov_gpr_imm.cpp"
#include "emitter/mov_reg.cpp"
#include "emitter/arith.cpp"
#include "emitter/branch.cpp"
#include "emitter/implicit.cpp"
#include "emitter/addr.cpp"

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
        case op_group::directive: return directive_reg_span(opcode.directive,reg); 
        case op_group::mov_gpr_imm: return mov_gpr_imm_reg_span(opcode.mov_gpr_imm,reg); 
        case op_group::arith_imm_three: return imm_three_reg_span(opcode.arith_imm_three,reg);
        case op_group::take_addr: return addr_opcode_reg_span(opcode.take_addr,reg); 
        case op_group::load: return addr_opcode_reg_span(opcode.load,reg); 
        case op_group::mov_reg: return mov_reg_reg_span(opcode.mov_reg,reg);
    }

    return reg;
}
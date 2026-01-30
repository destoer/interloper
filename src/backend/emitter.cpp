#include "opcode.h"

void reload_slot(Interloper& itl, Function& func, const Reg& dst);
void spill_slot(Interloper& itl, Function& func, const Reg& src);

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


// NOTE: these are the bottom level emitter only use directly if you need to gen code yourself
OpcodeNode* emit_block_internal(Function& func,BlockSlot block_slot, const Opcode& opcode)
{
    auto& block = block_from_slot(func,block_slot);

    auto &list = block.list;
    append(list,opcode);
    
    return list.finish;    
}

OpcodeNode* emit_block_func(Function& func,const Opcode& opcode)
{
    return emit_block_internal(func,cur_block(func),opcode);
}


#include "emitter/directive.cpp"
#include "emitter/mov_gpr_imm.cpp"
#include "emitter/branch.cpp"
#include "emitter/implicit.cpp"
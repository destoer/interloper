#include <reg.cpp>

List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).end;    
}



// NOTE: this is the bottom level emitter
void emit_block_internal(Function& func,BlockSlot block_slot, op_type type, u32 v1, u32 v2, u32 v3)
{
    const Opcode opcode(type,v1,v2,v3);

    auto& block = block_from_slot(func,block_slot);
    auto &list = block.list;
    append(list,opcode);    
}

constexpr OpInfo opcode_info_from_type(op_type type)
{
    const u32 IDX = u32(type);

    const auto OP_INFO = OPCODE_TABLE[IDX];

    return OP_INFO;    
}

// emitter for reg_t 3
template<const op_type type>
void emit_reg3(Function& func, DstSlot dst, SrcSlot v1, SrcSlot v2)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.type[2] == arg_type::dst_reg);

    emit_block_internal(func,cur_block(func),type,dst.handle,v1.handle,v2.handle);
}


template<const op_type type>
void emit_label1(Function& func, LabelSlot slot)
{
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::label);

    emit_block_internal(func,cur_block(func),type,slot.handle,0,0);
}

#include <emit_opcode.cpp>
#include <reg.cpp>

List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).end;    
}




constexpr OpInfo opcode_info_from_type(op_type type)
{
    const u32 IDX = u32(type);

    const auto OP_INFO = OPCODE_TABLE[IDX];

    return OP_INFO;    
}

// NOTE: we could have handled this with higher level checks at hte point we request the symbol
// from the table and strongly type src and dst slots, but it seems too error prone,
// even though the current solution is a bit heavyweight

void handle_src_storage(Interloper& itl, Function& func, SymSlot src_slot)
{
    UNUSED(itl); UNUSED(func); UNUSED(src_slot);
}

void handle_dst_storage(Interloper& itl, Function& func, SymSlot dst_slot)
{
    UNUSED(itl); UNUSED(func); UNUSED(dst_slot);
}

// NOTE: this is the bottom level emitter
void emit_block_internal(Function& func,BlockSlot block_slot, op_type type, u32 v1, u32 v2, u32 v3)
{
    const Opcode opcode(type,v1,v2,v3);

    auto& block = block_from_slot(func,block_slot);
    auto &list = block.list;
    append(list,opcode);    
}


template<const op_type type>
void emit_implicit(Interloper& itl,Function& func)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);
    static_assert(OP_INFO.args == 0);

    emit_block_internal(func,cur_block(func),type,0,0,0);
}

// emitter for reg_t 3
template<const op_type type>
void emit_reg3(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.type[2] == arg_type::src_reg);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,v1);
    handle_src_storage(itl,func,v2);

    emit_block_internal(func,cur_block(func),type,dst.handle,v1.handle,v2.handle);

    handle_dst_storage(itl,func,dst);
}


template<const op_type type>
void emit_reg2(Interloper& itl,Function& func, SymSlot dst, SymSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.args == 2);

    handle_src_storage(itl,func,src);

    emit_block_internal(func,cur_block(func),type,dst.handle,src.handle,0);

    handle_dst_storage(itl,func,dst);
}

template<const op_type type>
void emit_reg1(Interloper& itl, Function& func, SymSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::src_reg);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,src.handle,0,0);

    handle_src_storage(itl,func,src);
}



template<const op_type type>
void emit_store(Interloper& itl, Function& func, SymSlot src, SymSlot ptr, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::src_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,ptr);
    handle_src_storage(itl,func,src);

    emit_block_internal(func,cur_block(func),type,src.handle,ptr.handle,imm);    
}

template<const op_type type>
void emit_load(Interloper& itl, Function& func, SymSlot dst, SymSlot ptr, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,ptr);
    
    emit_block_internal(func,cur_block(func),type,dst.handle,ptr.handle,imm);    

    handle_dst_storage(itl,func,dst);
}

template<const op_type type>
void emit_imm1(Interloper& itl, Function& func, SymSlot dst, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::imm);
    static_assert(OP_INFO.args == 2);

    emit_block_internal(func,cur_block(func),type,dst.handle,imm,0);    

    handle_dst_storage(itl,func,dst);
}

template<const op_type type>
void emit_imm0(Interloper& itl, Function& func, u32 imm)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::imm);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,imm,0,0);    
}

template<const op_type type>
void emit_imm2(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);
    static_assert(OP_INFO.type[1] == arg_type::src_reg);
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,src);

    emit_block_internal(func,cur_block(func),type,dst.handle,src.handle,imm);    

    handle_dst_storage(itl,func,dst);
}

template<const op_type type>
void emit_label1(Interloper& itl,Function& func, LabelSlot slot)
{
    UNUSED(itl);

    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::label);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,slot.handle,0,0);
}

#include <emit_opcode.cpp>
#include <directive.cpp>
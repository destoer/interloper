#include <reg.cpp>

void reload_slot(Interloper& itl, Function& func, const Reg& reg);

List& get_cur_list(IrEmitter& emitter)
{
    return emitter.program[count(emitter.program)-1].list; 
}

ListNode* get_cur_end(IrEmitter& emitter)
{
    return get_cur_list(emitter).finish;    
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

constexpr OpInfo opcode_three_info(op_type type)
{
    const u32 IDX = u32(type);

    const auto OP_INFO = OPCODE_TABLE[IDX];

    return OP_INFO;    
}

// NOTE: we could have handled this with higher level checks at the point we request the symbol
// from the table and strongly type src and dst slots, but it seems too error prone,
// even though the current solution is a bit heavyweight

void handle_src_storage(Interloper& itl, Function& func, SymSlot src_slot, b32 is_float)
{
    if(!is_var(src_slot))
    {
        return;
    }

    auto& reg = reg_from_slot(itl.symbol_table,func,src_slot);

    if(is_aliased(reg))
    {
        reload_slot(itl,func,reg);
    }

    UNUSED(is_float);

#ifdef VERIFY_FLOAT
    if(!is_float)
    {
        assert(!(reg.flags & REG_FLOAT));
    }

    else
    {
        assert(reg.flags & REG_FLOAT);
    }
#endif
}

void handle_dst_storage(Interloper& itl, Function& func, SymSlot dst_slot, b32 is_float)
{
    if(!is_var(dst_slot))
    {
        return;
    }

    auto& reg = reg_from_slot(itl.symbol_table,func,dst_slot);

    if(is_aliased(reg))
    {
        spill_slot(itl,func,reg);
    }

    UNUSED(is_float);

#ifdef VERIFY_FLOAT
    if(!is_float)
    {
        assert(!(reg.flags & REG_FLOAT));
    }

    else
    {
        reg.flags |= REG_FLOAT;
    } 
#endif
}



// NOTE: these are the bottom level emitter only use directly if you need to gen code yourself
ListNode* emit_block_internal(Function& func,BlockSlot block_slot, op_type type, u64 v1, u64 v2, u64 v3)
{
    const Opcode opcode(type,v1,v2,v3);

    auto& block = block_from_slot(func,block_slot);
    auto &list = block.list;
    append(list,opcode);

    return list.finish;    
}

ListNode* emit_block_internal_slot(Function& func,BlockSlot block_slot, op_type type, SymSlot v1, SymSlot v2, SymSlot v3)
{
    return emit_block_internal(func,block_slot,type,v1.handle,v2.handle,v3.handle);
}


template<const op_type type>
void emit_branch_reg(Interloper& itl, Function& func, SymSlot v1)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::branch_reg_t);

    static_assert(OP_INFO.type[0] == arg_type::src_reg);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,v1.handle,0,0);    
}

template<const op_type type>
void emit_implicit(Interloper& itl,Function& func)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);
    static_assert(OP_INFO.args == 0);

    emit_block_internal(func,cur_block(func),type,0,0,0);
}



template<const op_type type>
void emit_reg2(Interloper& itl,Function& func, SymSlot dst, SymSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::reg_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.args == 2);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[1]));

    emit_block_internal(func,cur_block(func),type,dst.handle,src.handle,0);

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

// emitter for reg_t 3
template<const op_type type>
void emit_reg3(Interloper& itl,Function& func, SymSlot dst, SymSlot v1, SymSlot v2)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::reg_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(is_arg_src_const(OP_INFO.type[2]));
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,v1,is_arg_float_const(OP_INFO.type[1]));
    handle_src_storage(itl,func,v2,is_arg_float_const(OP_INFO.type[2]));

    emit_block_internal(func,cur_block(func),type,dst.handle,v1.handle,v2.handle);

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_reg1(Interloper& itl, Function& func, SymSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::reg_t);

    static_assert(is_arg_src_const(OP_INFO.type[0]));
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,src.handle,0,0);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[0]));
}



template<const op_type type>
void emit_store(Interloper& itl, Function& func, SymSlot src, SymSlot ptr, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::store_t);

    static_assert(is_arg_src_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,ptr,is_arg_float_const(OP_INFO.type[2]));
    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[1]));

    emit_block_internal(func,cur_block(func),type,src.handle,ptr.handle,imm);    
}

template<const op_type type>
void emit_load(Interloper& itl, Function& func, SymSlot dst, SymSlot ptr, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::load_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,ptr,is_arg_float_const(OP_INFO.type[1]));
    
    emit_block_internal(func,cur_block(func),type,dst.handle,ptr.handle,imm);    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_imm1(Interloper& itl, Function& func, SymSlot dst, u64 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(OP_INFO.type[1] == arg_type::imm);
    static_assert(OP_INFO.args == 2);

    emit_block_internal(func,cur_block(func),type,dst.handle,imm,0);    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_imm0(Interloper& itl, Function& func, u64 imm)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(OP_INFO.type[0] == arg_type::imm);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,imm,0,0);    
}

template<const op_type type>
void emit_imm2(Interloper& itl, Function& func, SymSlot dst, SymSlot src, u64 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[1]));

    emit_block_internal(func,cur_block(func),type,dst.handle,src.handle,imm);    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_fp_imm1(Interloper& itl, Function& func, SymSlot dst, f64 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(OP_INFO.type[0] == arg_type::dst_float);
    static_assert(OP_INFO.type[1] == arg_type::imm);
    static_assert(OP_INFO.args == 2);

    emit_block_internal(func,cur_block(func),type,dst.handle,bit_cast_from_f64(imm),0);    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_label1(Interloper& itl,Function& func, LabelSlot slot)
{
    UNUSED(itl);

    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.type[0] == arg_type::label);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,slot.handle,0,0);
}

#include <emit_opcode.cpp>
#include <directive.cpp>
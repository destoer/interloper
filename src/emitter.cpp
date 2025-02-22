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

Operand make_reg_operand(RegSlot slot)
{
    Operand oper;
    oper.reg = slot;
    oper.type = operand_type::reg;

    return oper;
}

Operand make_decimal_operand(f64 decimal)
{
    Operand oper;
    oper.decimal = decimal;
    oper.type = operand_type::decimal;

    return oper;
}

Operand make_imm_operand(u64 imm)
{
    Operand oper;
    oper.imm = imm;
    oper.type = operand_type::imm;

    return oper;
}

Operand make_label_operand(LabelSlot slot)
{
    Operand oper;
    oper.label = slot;
    oper.type = operand_type::label;

    return oper;
}

Operand make_raw_operand(u64 value)
{
    Operand oper;
    oper.raw = value;
    oper.type = operand_type::raw;

    return oper;
}

static const Operand BLANK_OPERAND = make_raw_operand(0);

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

void handle_src_storage(Interloper& itl, Function& func, RegSlot src_slot, b32 is_float)
{
    if(src_slot.kind == reg_kind::spec)
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

void handle_dst_storage(Interloper& itl, Function& func, RegSlot dst_slot, b32 is_float)
{
    if(dst_slot.kind == reg_kind::spec)
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
ListNode* emit_block_internal(Function& func,BlockSlot block_slot, op_type type, Operand v1, Operand v2, Operand v3)
{
    const Opcode opcode = {type,v1,v2,v3};

    auto& block = block_from_slot(func,block_slot);
    auto &list = block.list;
    append(list,opcode);

    return list.finish;    
}

ListNode* emit_block_internal_slot(Function& func,BlockSlot block_slot, op_type type, RegSlot v1, RegSlot v2, RegSlot v3)
{
    return emit_block_internal(func,block_slot,type,make_reg_operand(v1),make_reg_operand(v2),make_reg_operand(v3));
}


template<const op_type type>
void emit_branch_reg(Interloper& itl, Function& func, RegSlot v1)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::branch_reg_t);

    static_assert(OP_INFO.type[0] == arg_type::src_reg);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,make_reg_operand(v1),BLANK_OPERAND,BLANK_OPERAND);    
}

template<const op_type type>
void emit_implicit(Interloper& itl,Function& func)
{
    UNUSED(itl);

    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);
    static_assert(OP_INFO.args == 0);

    emit_block_internal(func,cur_block(func),type,BLANK_OPERAND,BLANK_OPERAND,BLANK_OPERAND);
}



template<const op_type type>
void emit_reg2(Interloper& itl,Function& func, RegSlot dst, RegSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::reg_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.args == 2);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[1]));

    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_reg_operand(src),BLANK_OPERAND);

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

// emitter for reg_t 3
template<const op_type type>
void emit_reg3(Interloper& itl,Function& func, RegSlot dst, RegSlot v1, RegSlot v2)
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

    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_reg_operand(v1),make_reg_operand(v2));

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_reg1(Interloper& itl, Function& func, RegSlot src)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::reg_t);

    static_assert(is_arg_src_const(OP_INFO.type[0]));
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,make_reg_operand(src),BLANK_OPERAND,BLANK_OPERAND);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[0]));
}



template<const op_type type>
void emit_store(Interloper& itl, Function& func, RegSlot src, RegSlot ptr, u32 imm)
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

    emit_block_internal(func,cur_block(func),type,make_reg_operand(src),make_reg_operand(ptr),make_imm_operand(imm));    
}

template<const op_type type>
void emit_load(Interloper& itl, Function& func, RegSlot dst, RegSlot ptr, u32 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::load_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,ptr,is_arg_float_const(OP_INFO.type[1]));
    
    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_reg_operand(ptr),make_imm_operand(imm));    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_imm1(Interloper& itl, Function& func, RegSlot dst, u64 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(OP_INFO.type[1] == arg_type::imm);
    static_assert(OP_INFO.args == 2);

    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_imm_operand(imm),BLANK_OPERAND);    

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

    emit_block_internal(func,cur_block(func),type,make_imm_operand(imm),BLANK_OPERAND,BLANK_OPERAND);    
}

template<const op_type type>
void emit_imm2(Interloper& itl, Function& func, RegSlot dst, RegSlot src, u64 imm)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(is_arg_dst_const(OP_INFO.type[0]));
    static_assert(is_arg_src_const(OP_INFO.type[1]));
    static_assert(OP_INFO.type[2] == arg_type::imm);
    static_assert(OP_INFO.args == 3);

    handle_src_storage(itl,func,src,is_arg_float_const(OP_INFO.type[1]));

    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_reg_operand(src),make_imm_operand(imm));    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_fp_imm1(Interloper& itl, Function& func, RegSlot dst, f64 decimal)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.group == op_group::imm_t);

    static_assert(OP_INFO.type[0] == arg_type::dst_float);
    static_assert(OP_INFO.type[1] == arg_type::imm);
    static_assert(OP_INFO.args == 2);

    emit_block_internal(func,cur_block(func),type,make_reg_operand(dst),make_decimal_operand(decimal),BLANK_OPERAND);    

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}

template<const op_type type>
void emit_label1(Interloper& itl,Function& func, LabelSlot slot)
{
    UNUSED(itl);

    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.type[0] == arg_type::label);
    static_assert(OP_INFO.args == 1);

    emit_block_internal(func,cur_block(func),type,make_label_operand(slot),BLANK_OPERAND,BLANK_OPERAND);
}

#include <emit_opcode.cpp>
#include <directive.cpp>
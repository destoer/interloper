
template<typename type,const bool IS_LOAD, const bool STRUCT_ADDR,op_group group>
ConstIrRegSpan addr_opcode_ir_reg_span(const AddrOpcode<type,IS_LOAD,STRUCT_ADDR,group>& addr_op, IrRegSpan& reg)
{
    if constexpr(IS_LOAD)
    {
        reg.dst[reg.dst.size++] = addr_op.v1.ir;
    }

    // TODO: This should be processed last for compat :P (switch it out later for sanity)
    if constexpr(!IS_LOAD)
    {
        reg.src[reg.src.size++] = addr_op.v1.ir;
    }

    if constexpr(!STRUCT_ADDR)
    {
        reg.src[reg.src.size++] = addr_op.addr_ir.base;
    }

    reg.src[reg.src.size++] = addr_op.addr_ir.index;

    return reg;
}

template<typename T, typename op_type>
T make_addr_op(RegSlot dst,const Addr& addr, op_type type)
{
    T addr_op;
    addr_op.v1.ir = dst;
    addr_op.addr_ir = addr;
    addr_op.type = type;
    
    return addr_op;
}


template<typename T, typename op_type>
Opcode make_lowered_addr_instr(lowered_reg_t v1, lowered_reg_t base,lowered_reg_t index, u32 scale, u32 offset, op_type type)
{
    T addr_op;
    addr_op.addr = {};
    addr_op.v1.reg = v1;
    addr_op.addr.base = base;
    addr_op.addr.index = index;
    addr_op.addr.offset = offset;
    addr_op.addr.scale = scale;
    addr_op.type = type;

    return Opcode(addr_op,opcode_state::lowered);
}

Opcode make_lowered_lea_instr(lowered_reg_t v1, lowered_reg_t base,lowered_reg_t index, u32 scale, u32 offset)
{
    return make_lowered_addr_instr<Lea>(v1,base,index,scale,offset,take_addr_type::take_addr);
}

Opcode make_lowered_load_instr(lowered_reg_t v1, lowered_reg_t base, u32 offset, load_type type)
{
    return make_lowered_addr_instr<Load>(v1,base,u32(spec_reg::null),1,offset,type);
}

Opcode make_lowered_store_instr(lowered_reg_t v1, lowered_reg_t base, u32 offset, store_type type)
{
    return make_lowered_addr_instr<Store>(v1,base,u32(spec_reg::null),1,offset,type);
}

void addrof(Interloper& itl,Function& func, RegSlot dst, const StructAddr& struct_addr)
{
    // mark reg as aliased
    auto& reg = reg_from_slot(itl,func,struct_addr.addr.base);
    reg.flags |= ALIASED;

    const auto opcode = Opcode(make_addr_op<AddrOf>(dst,struct_addr.addr,take_addr_type::take_addr),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}

RegSlot addrof_res(Interloper& itl,Function& func, const StructAddr& struct_addr)
{
    return opcode_res1_gpr(itl,func,struct_addr,addrof);  
}

void lea(Interloper& itl,Function& func, RegSlot dst, const PointerAddr& pointer)
{
    // This has no indexing don't bother
    if(is_null_reg(pointer.addr.index))
    {
        add_imm(itl,func,dst,pointer.addr.base,pointer.addr.offset);
        return;
    }

    const auto opcode = Opcode(make_addr_op<Lea>(dst,pointer.addr,take_addr_type::take_addr),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}

RegSlot lea_res(Interloper& itl,Function& func, const PointerAddr& addr)
{
    return opcode_res1_gpr(itl,func,addr,lea); 
}

void emit_load(Interloper& itl, Function& func, RegSlot dst, const PointerAddr& pointer, load_type type)
{
    const auto opcode = Opcode(make_addr_op<Load>(dst,pointer.addr,type),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}

Opcode make_store(RegSlot src, const Addr& addr, store_type type)
{
    return Opcode(make_addr_op<Store>(src,addr,type),opcode_state::ir);
}

void emit_store(Interloper& itl, Function& func, RegSlot src, const PointerAddr& pointer, store_type type)
{
    const auto store = make_store(src,pointer.addr,type);
    emit_block_func(itl,func,store);
}

void emit_load_struct(Interloper& itl, Function& func, RegSlot dst, const StructAddr& struct_addr, load_type type)
{
    const auto opcode = Opcode(make_addr_op<LoadStruct>(dst,struct_addr.addr,type),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}

void emit_store_struct(Interloper& itl, Function& func, RegSlot src, const StructAddr& struct_addr, store_type type)
{
    const auto opcode = Opcode(make_addr_op<StoreStruct>(src,struct_addr.addr,type),opcode_state::ir);
    emit_block_func(itl,func,opcode);
}
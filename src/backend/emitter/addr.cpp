
template<typename type,const bool IS_LOAD, const bool STRUCT_ADDR>
ConstRegSpan addr_opcode_reg_span(const AddrOpcode<type,IS_LOAD,STRUCT_ADDR>& addr_op, RegSpan& reg)
{
    reg.dst.size = 0;
    reg.src.size = 0;

    if constexpr(IS_LOAD)
    {
        reg.dst[reg.dst.size++] = addr_op.v1.ir;
    }

    else
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

void addrof(Interloper& itl,Function& func, RegSlot dst, const StructAddr& struct_addr)
{
    // mark reg as aliased
    auto& reg = reg_from_slot(itl,func,struct_addr.addr.base);
    reg.flags |= ALIASED;

    Opcode opcode;
    opcode.group = op_group::addrof;

    opcode.addrof = make_addr_op<AddrOf>(dst,struct_addr.addr,take_addr::addrof);
    emit_block_func(itl,func,opcode);
}

void lea(Interloper& itl,Function& func, RegSlot dst, const PointerAddr& pointer)
{
    // This has no indexing don't bother
    if(is_null_reg(pointer.addr.index))
    {
        add_imm(itl,func,dst,pointer.addr.base,pointer.addr.offset);
        return;
    }

    Opcode opcode;
    opcode.group = op_group::lea;

    opcode.lea = make_addr_op<Lea>(dst,pointer.addr,take_addr::lea);
    emit_block_func(itl,func,opcode);
}

RegSlot lea_res(Interloper& itl,Function& func, const PointerAddr& addr)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    lea(itl,func,tmp,addr);

    return tmp;
}

void emit_load(Interloper& itl, Function& func, RegSlot dst, const PointerAddr& pointer, load_type type)
{
    Opcode opcode;
    opcode.group = op_group::load;

    opcode.load = make_addr_op<Load>(dst,pointer.addr,type);;
    emit_block_func(itl,func,opcode);
}

void emit_store(Interloper& itl, Function& func, RegSlot src, const PointerAddr& pointer, store_type type)
{
    Opcode opcode;
    opcode.group = op_group::store;

    opcode.store = make_addr_op<Store>(src,pointer.addr,type);
    emit_block_func(itl,func,opcode);
}
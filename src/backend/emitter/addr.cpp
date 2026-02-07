
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

RegSlot addrof_res(Interloper& itl,Function& func, const StructAddr& struct_addr)
{
    return opcode_res1(itl,func,struct_addr,addrof);  
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
    return opcode_res1(itl,func,addr,lea); 
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

void emit_load_struct(Interloper& itl, Function& func, RegSlot dst, const StructAddr& struct_addr, load_type type)
{
    Opcode opcode;
    opcode.group = op_group::load_struct;

    opcode.load_struct = make_addr_op<LoadStruct>(dst,struct_addr.addr,type);;
    emit_block_func(itl,func,opcode);
}

void emit_store_struct(Interloper& itl, Function& func, RegSlot src, const StructAddr& struct_addr, store_type type)
{
    Opcode opcode;
    opcode.group = op_group::store_struct;

    opcode.store_struct = make_addr_op<StoreStruct>(src,struct_addr.addr,type);
    emit_block_func(itl,func,opcode);
}
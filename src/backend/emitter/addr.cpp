const RegSpan take_addr_reg_span(const TakeAddr& take_addr, RegBuffer& reg)
{
    u32 dst = 0;
    reg.dst[dst++] = take_addr.dst_ir;
    reg.dst.size = dst;

    u32 src = 0;
    if(take_addr.type != take_addr_type::addrof)
    {
        reg.src[src++] = take_addr.addr_ir.base;
        reg.src[src++] = take_addr.addr_ir.index;
    }

    reg.src.size = src;

    return reg;
}

void emit_take_addr(Interloper& itl, Function& func, RegSlot dst_slot,const Addr& addr, take_addr_type type)
{
    Opcode opcode;
    opcode.group = op_group::take_addr;

    TakeAddr take_addr;
    take_addr.dst_ir = dst_slot;
    take_addr.addr_ir = addr;
    take_addr.type = type;
    
    opcode.take_addr = take_addr;
    emit_block_func(itl,func,opcode);
}

void addrof(Interloper& itl,Function& func, RegSlot dst, StructAddr struct_addr)
{
    // mark reg as aliased
    auto& reg = reg_from_slot(itl,func,struct_addr.addr.base);
    reg.flags |= ALIASED;

    emit_take_addr(itl,func,dst,struct_addr.addr, take_addr_type::addrof);
}
ConstRegSpan take_addr_reg_span(const TakeAddr& take_addr, RegSpan& reg)
{
    u32 dst = 0;
    reg.dst[dst++] = take_addr.dst.ir;
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

void emit_take_addr(Interloper& itl, Function& func, RegSlot dst,const Addr& addr, take_addr_type type)
{
    Opcode opcode;
    opcode.group = op_group::take_addr;

    TakeAddr take_addr;
    take_addr.dst.ir = dst;
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

void lea(Interloper& itl,Function& func, RegSlot dst, PointerAddr pointer)
{
    // This has no indexing don't bother
    if(is_null_reg(pointer.addr.index))
    {
        add_imm(itl,func,dst,pointer.addr.base,pointer.addr.offset);
        return;
    }

    emit_take_addr(itl,func,dst,pointer.addr, take_addr_type::lea);
}

RegSlot lea_res(Interloper& itl,Function& func, PointerAddr addr)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    lea(itl,func,tmp,addr);

    return tmp;
}


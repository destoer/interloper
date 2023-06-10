

// NOTE: this is less strictly checked than emitters for instructions
template<const op_type type>
void emit_directive_dst1(Interloper& itl,Function& func, SymSlot dst, u32 v2, u32 v3)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_info_from_type(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);    

    emit_block_internal(func,cur_block(func),type,dst.handle,v2,v3);

    handle_dst_storage(itl,func,dst);
}


// TODO: we probably need a way to override if this counts for aliasing
// if this is just for internal use, i.e for returning a struct out of a function
// we know that it will be used saftely...
void addrof(Interloper& itl,Function& func, SymSlot dst, SymSlot src)
{
    // TODO: move this over to a better system, just keep the old one for now
    auto& reg = reg_from_slot(itl.symbol_table,func,src);
    reg.aliased = true;    

    emit_directive_dst1<op_type::addrof>(itl,func,dst,src.handle,0);
}


SymSlot addrof_res(Interloper& itl, Function& func, SymSlot src)
{
    const auto tmp = new_tmp_ptr(func);
    addrof(itl,func,tmp,src);

    return tmp;
}

void spill_rv(Interloper& itl, Function& func)
{
    emit_implicit<op_type::spill_rv>(itl,func);
}

void clean_args(Interloper& itl, Function& func, u32 v)
{
    emit_imm0<op_type::clean_args>(itl,func,v);
}

void save_regs(Interloper& itl, Function& func)
{
    emit_implicit<op_type::save_regs>(itl,func);
}

void restore_regs(Interloper& itl, Function& func)
{
    emit_implicit<op_type::restore_regs>(itl,func);
}

void emit_exit_block(Interloper& itl, Function& func)
{
    emit_implicit<op_type::exit_block>(itl,func);
}

void pool_addr(Interloper& itl, Function& func, SymSlot dst_slot, PoolSlot pool_slot)
{
    emit_directive_dst1<op_type::pool_addr>(itl,func,dst_slot,pool_slot.handle,0);
}

SymSlot pool_addr_res(Interloper& itl, Function& func, PoolSlot pool_slot)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    pool_addr(itl,func,tmp,pool_slot);

    return tmp;
}

void free_slot(Interloper& itl,Function& func, SymSlot slot)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),op_type::free_slot,slot.handle,0,0);
}


void free_fixed_array(Interloper& itl,Function& func,SymSlot src,u32 size,u32 count)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),op_type::free_fixed_array,src.handle,size,count);
}

void alloc_slot(Interloper& itl,Function& func, const Reg& reg, b32 force_alloc)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),op_type::alloc_slot,reg.slot.handle,force_alloc,0);
}

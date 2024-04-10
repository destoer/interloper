

// NOTE: this is less strictly checked than emitters for instructions
template<const op_type type>
void emit_directive_dst1(Interloper& itl,Function& func, SymSlot dst, u32 v2, u32 v3)
{
    // sanity checking fmt
    constexpr auto OP_INFO = opcode_three_info(type);

    static_assert(OP_INFO.type[0] == arg_type::dst_reg);    

    emit_block_internal(func,cur_block(func),type,dst.handle,v2,v3);

    handle_dst_storage(itl,func,dst,is_arg_float_const(OP_INFO.type[0]));
}



void addrof(Interloper& itl,Function& func, SymSlot dst, SymSlot src, u32 offset = 0)
{
    // mark reg as aliased
    auto& reg = reg_from_slot(itl,func,src);
    reg.flags |= ALIASED;

    emit_directive_dst1<op_type::addrof>(itl,func,dst,src.handle,offset);
}

SymSlot addrof_res(Interloper& itl, Function& func, SymSlot src, u32 offset = 0)
{
    const auto tmp = new_tmp_ptr(func);
    addrof(itl,func,tmp,src,offset);

    return tmp;
}

void clean_args(Interloper& itl, Function& func, u32 v)
{
    emit_imm0<op_type::clean_args>(itl,func,v);
}

void spill_all(Interloper& itl, Function& func)
{
    emit_implicit<op_type::spill_all>(itl,func);
}

void spill_func_bounds(Interloper& itl, Function& func)
{
    emit_implicit<op_type::spill_func_bounds>(itl,func);
}


void emit_exit_block(Interloper& itl, Function& func)
{
    emit_implicit<op_type::exit_block>(itl,func);
}

void pool_addr(Interloper& itl, Function& func, SymSlot dst_slot, PoolSlot pool_slot, u32 offset)
{
    emit_directive_dst1<op_type::pool_addr>(itl,func,dst_slot,pool_slot.handle,offset);
}

SymSlot pool_addr_res(Interloper& itl, Function& func, PoolSlot pool_slot, u32 offset)
{
    const auto tmp = new_tmp(func,GPR_SIZE);
    pool_addr(itl,func,tmp,pool_slot,offset);

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

ListNode* alloc_slot(Interloper& itl,Function& func, const SymSlot slot, b32 force_alloc)
{
    UNUSED(itl);
    return emit_block_internal(func,cur_block(func),op_type::alloc_slot,slot.handle,force_alloc,0);
}

ListNode* alloc_stack(Interloper& itl, Function& func, u32 size)
{
    UNUSED(itl);
    return emit_block_internal(func,cur_block(func),op_type::alloc_stack,size,0,0);    
}


void reload_slot(Interloper& itl, Function& func, const Reg& reg)
{
    UNUSED(itl);

    if(!stored_in_mem(reg))
    {
        emit_block_internal(func,cur_block(func),op_type::reload_slot,reg.slot.handle,0,0);
    }
}

void spill_slot(Interloper& itl, Function& func, const Reg& reg)
{
    UNUSED(itl);

    if(!stored_in_mem(reg))
    {
        emit_block_internal(func,cur_block(func),op_type::spill_slot,reg.slot.handle,0,0);
    }
}

void load_func_addr(Interloper& itl, Function& func, SymSlot dst, LabelSlot label)
{
    UNUSED(itl);

    emit_block_internal(func,cur_block(func),op_type::load_func_addr,dst.handle,label.handle,0);
}

void load_struct_internal(Interloper& itl, Function& func, op_type type,SymSlot dst, AddrSlot addr_slot)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),type,dst.handle,addr_slot.slot.handle,addr_slot.offset);
}

void store_struct_internal(Interloper& itl, Function& func, op_type type,SymSlot src, AddrSlot addr_slot)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),type,src.handle,addr_slot.slot.handle,addr_slot.offset);
}


void load_struct_u64(Interloper& itl, Function& func, SymSlot dst, AddrSlot addr_slot)
{
    load_struct_internal(itl,func,op_type::load_struct_u64,dst,addr_slot);
}

SymSlot load_struct_u64_res(Interloper& itl, Function& func, AddrSlot addr_slot)
{
    const auto dst = new_tmp(func,GPR_SIZE);
    load_struct_u64(itl,func,dst,addr_slot);

    return dst;
}

void write_struct_u64(Interloper& itl, Function& func, SymSlot src, AddrSlot addr_slot)
{
    store_struct_internal(itl,func,op_type::store_struct_u64,src,addr_slot);
}

void lock_reg(Interloper& itl, Function& func, SymSlot reg)
{
    const u32 machine_reg = special_reg_to_reg(itl.arch,reg);

    func.locked_set |= (1 << machine_reg);

    UNUSED(itl);
    emit_block_internal(func,cur_block(func),op_type::lock_reg,reg.handle,0,0);
}

ListNode* insert_lock(Interloper& itl,Function& func,Block& block, ListNode* node, SymSlot reg, bool after)
{
    const auto lock = make_op(op_type::lock_reg,reg.handle);

    const u32 machine_reg = special_reg_to_reg(itl.arch,reg);

    func.locked_set |= (1 << machine_reg);

    if(after)
    {
        return insert_after(block.list,node,lock);
    }

    else
    {
        return insert_at(block.list,node,lock);
    }
}

void unlock_reg(Interloper& itl, Function& func, SymSlot reg)
{
    UNUSED(itl);
    emit_block_internal(func,cur_block(func),op_type::unlock_reg,reg.handle,0,0);
}
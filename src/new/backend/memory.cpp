Addr make_addr(RegSlot base, u32 offset)
{
    return {base,make_spec_reg_slot(spec_reg::null),1,offset};
}

// Do not use directly, unless rescaling manually afterwards
Addr make_indexed_addr(RegSlot base, RegSlot index, u32 scale, u32 offset)
{
    return {base,index,scale,offset};
}

AddrSlot make_pointer_addr(RegSlot base,u32 offset)
{
    return {make_addr(base,offset),false};
}

AddrSlot make_indexed_pointer_addr(RegSlot base,RegSlot index, u32 scale,u32 offset)
{
    return {make_indexed_addr(base,index,scale,offset),false};
}

AddrSlot make_struct_addr(RegSlot base, u32 offset)
{
    return {make_addr(base,offset),true};
}

AddrSlot generate_indexed_pointer(Interloper& itl, Function& func, RegSlot base, RegSlot index, u32 scale, u32 offset)
{
    if(scale > GPR_SIZE || !is_pow2(scale))
    {
        index = mul_imm_res(itl,func,index,scale);
        scale = 1;
    }

    return make_indexed_pointer_addr(base,index,scale,offset);
}

void collapse_struct_addr(Interloper& itl, Function& func, RegSlot dst_slot, const AddrSlot struct_slot)
{
    if(struct_slot.struct_addr)
    {
        StructAddr struct_addr = {struct_slot.addr};
        addrof(itl,func,dst_slot,struct_addr);
        return;
    }

    PointerAddr pointer = {struct_slot.addr};
    lea(itl,func,dst_slot,pointer);
}

RegSlot collapse_struct_addr_res(Interloper& itl, Function& func, const AddrSlot struct_slot)
{
    const RegSlot tmp = new_tmp(func,GPR_SIZE);
    collapse_struct_addr(itl,func,tmp,struct_slot);

    return tmp;
}

// This doesn't require a copy, if this is just a straight pointer just return the base
RegSlot collapse_struct_addr_oper(Interloper& itl, Function& func, const AddrSlot struct_slot)
{
    if(!struct_slot.struct_addr && struct_slot.addr.offset == 0 && is_null_reg(struct_slot.addr.index))
    {
        return struct_slot.addr.base;
    }

    return collapse_struct_addr_res(itl,func,struct_slot);
}

TypedReg collapse_typed_struct_res(Interloper& itl, Function& func, const TypedAddr& struct_slot)
{
    const auto ptr = collapse_struct_addr_res(itl,func,struct_slot.addr_slot);
    return TypedReg {ptr,struct_slot.type};
}


void collapse_struct_offset(Interloper& itl, Function& func, AddrSlot* struct_slot)
{
    *struct_slot = make_pointer_addr(collapse_struct_addr_res(itl,func,*struct_slot),0);
}


static void load_ptr_addr(Interloper &itl,Function& func,RegSlot dst_slot,PointerAddr addr,u32 size, b32 is_signed, b32 is_float)
{
    if(is_float)
    {
        load_float(itl,func,dst_slot,addr);
    }

    else if(is_signed)
    {
        switch(size)
        {
            case 1:
            {
                load_signed_byte(itl,func,dst_slot,addr);
                break;
            }

            case 2: 
            {
                load_signed_half(itl,func,dst_slot,addr);
                break;
            }

            case 4:
            {
                load_signed_word(itl,func,dst_slot,addr);
                break;
            }

            case 8:
            {
                load_double(itl,func,dst_slot,addr);
                break;
            }

            default: assert(false);
        }        
    }

    else
    {
        switch(size)
        {
            case 1:
            {
                load_byte(itl,func,dst_slot,addr);
                break;
            }

            case 2: 
            {
                load_half(itl,func,dst_slot,addr);
                break;
            }

            case 4:
            {
                load_word(itl,func,dst_slot,addr);
                break;
            }

            case 8:
            {
                load_double(itl,func,dst_slot,addr);
                break;
            }

            default: assert(false);
        }
    }
}

void load_struct(Interloper &itl,Function& func,RegSlot dst_slot,StructAddr addr,u32 size, b32 is_signed, b32 is_float)
{
    if(is_float)
    {
        load_struct_internal(itl,func,op_type::load_struct_f64,dst_slot,addr);  
    }

    else if(is_signed)
    {
        const op_type SIGNED_LOAD_STRUCT_TABLE[] = {op_type::load_struct_s8,op_type::load_struct_s16,op_type::load_struct_s32,op_type::load_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,SIGNED_LOAD_STRUCT_TABLE[idx],dst_slot,addr);     
    }

    else
    {
        const op_type UNSIGNED_LOAD_STRUCT_TABLE[] = {op_type::load_struct_u8,op_type::load_struct_u16,op_type::load_struct_u32,op_type::load_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,UNSIGNED_LOAD_STRUCT_TABLE[idx],dst_slot,addr);     
    }
}

void load_addr_slot(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot addr_slot, u32 size, b32 sign, b32 is_float)
{
    if(addr_slot.struct_addr)
    {
        const StructAddr struct_addr = {addr_slot.addr};
        load_struct(itl,func,dst_slot,struct_addr,size,sign,is_float);
    }

    else
    {
        const PointerAddr pointer_addr = {addr_slot.addr};
        load_ptr_addr(itl,func,dst_slot,pointer_addr,size,sign,is_float);
    }
}

RegSlot load_addr_gpr_res(Interloper &itl,Function &func,AddrSlot addr_slot)
{
    RegSlot tmp = new_tmp(func,GPR_SIZE);
    load_addr_slot(itl,func,tmp,addr_slot,GPR_SIZE,false,false);

    return tmp;
}

void do_addr_copy(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot src_addr, u32 size)
{
    switch(dst_slot.kind)
    {
        case reg_kind::sym:
        case reg_kind::tmp:
        {
            const auto addr = make_struct_addr(dst_slot,0);
            ir_memcpy(itl,func,addr,src_addr,size);
            break;
        }

        case reg_kind::spec:
        {
            const auto spec = dst_slot.spec;

            switch(spec)
            { 
                // copy into hidden pointer
                case spec_reg::rv_struct:
                {
                    const auto dst_addr = make_pointer_addr(make_sym_reg_slot(func.sig.args[0]),0);
                    ir_memcpy(itl,func,dst_addr,src_addr,size);
                    break;
                }

                default: assert(false);
            }
        }
    }
}

void do_addr_load(Interloper &itl,Function &func,RegSlot dst_slot,const TypedAddr& src_addr)
{
    const u32 size = type_size(itl,src_addr.type);

    if(is_array(src_addr.type))
    {
        if(is_runtime_size(src_addr.type))
        {
            do_addr_copy(itl,func,dst_slot,src_addr.addr_slot,size);
        }

        // fixed size array, the pointer is the array
        else
        {
            collapse_struct_addr(itl,func,dst_slot,src_addr.addr_slot);
        }
    }

    else if(is_struct(src_addr.type))
    {
        do_addr_copy(itl,func,dst_slot,src_addr.addr_slot,size);
    }

    else if(size <= GPR_SIZE)
    {
        const b32 sign = is_signed(src_addr.type);
        const b32 fp = is_float(src_addr.type);

        load_addr_slot(itl,func,dst_slot,src_addr.addr_slot,size,sign,fp);
    }            

    else
    {
        assert(false);
    }
}

void do_ptr_load(Interloper &itl,Function &func,RegSlot dst_slot,const TypedReg& reg, u32 offset = 0)
{
    const TypedAddr src_addr = typed_addr_from_reg(reg,offset); 
   do_addr_load(itl,func,dst_slot,src_addr);
}

static void store_ptr_addr(Interloper &itl,Function& func,RegSlot src_slot,PointerAddr addr,u32 size, b32 is_float)
{
    if(is_float)
    {
        store_float(itl,func,src_slot,addr);
    }

    else
    {
        switch(size)
        {
            case 1:
            {
                store_byte(itl,func,src_slot,addr);
                break;
            }

            case 2: 
            {
                store_half(itl,func,src_slot,addr);
                break;
            }

            case 4:
            {
                store_word(itl,func,src_slot,addr);
                break;
            }

            case 8:
            {
                store_double(itl,func,src_slot,addr);
                break;
            }

            default: assert(false);
        }
    }    
}


void store_ptr(Interloper &itl,Function& func,RegSlot src_slot,RegSlot ptr,u32 offset,u32 size, b32 is_float)
{
    const PointerAddr pointer = {make_addr(ptr,offset)};
    store_ptr_addr(itl,func,src_slot,pointer,size,is_float);
}


void store_struct(Interloper &itl,Function& func,RegSlot src_slot,StructAddr addr,u32 size, b32 is_float)
{
    if(is_float)
    {
        load_struct_internal(itl,func,op_type::store_struct_f64,src_slot,addr);
    }

    else
    {
        const op_type STORE_STRUCT_TABLE[] = {op_type::store_struct_u8,op_type::store_struct_u16,op_type::store_struct_u32,op_type::store_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,STORE_STRUCT_TABLE[idx],src_slot,addr);
    }      
}

void store_addr_slot(Interloper &itl,Function &func,RegSlot src_slot,AddrSlot addr_slot, u32 size,b32 is_float)
{
    if(addr_slot.struct_addr)
    {
        const StructAddr struct_addr = {addr_slot.addr};
        store_struct(itl,func,src_slot,struct_addr,size,is_float);
    }

    else
    {
        const PointerAddr pointer_addr = {addr_slot.addr}; 
        store_ptr_addr(itl,func,src_slot,pointer_addr,size,is_float);
    }
}


void do_addr_store(Interloper &itl,Function &func,RegSlot src_slot,const TypedAddr& dst)
{
    const u32 size = type_size(itl,dst.type);

    if(size <= GPR_SIZE)
    {
        const b32 fp = is_float(dst.type);
        store_addr_slot(itl,func,src_slot,dst.addr_slot,size,fp);
    }

    // large copy
    else
    {      
        const auto src_addr = make_struct_addr(src_slot,0);
        ir_memcpy(itl,func,dst.addr_slot,src_addr,size);        
    } 
}

void do_ptr_store(Interloper &itl,Function &func,RegSlot src_slot,const TypedReg& reg, u32 offset = 0)
{
    const TypedAddr addr = {make_pointer_addr(reg.slot,offset),reg.type};
    do_addr_store(itl,func,src_slot,addr);
}


// we dont want the 2nd stage IR handling how things need to be copied
// as it does not have the information required easily accessible
void compile_move(Interloper &itl, Function &func, const TypedReg& dst, const TypedReg& src)
{
    // check the operation is even legal

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underlying type if its a pointer
    if(is_trivial_copy(dst.type) && is_trivial_copy(src.type))
    {
        if(is_float(dst.type))
        {
            mov_float(itl,func,dst.slot,src.slot);
        }

        else
        {
            mov_reg(itl,func,dst.slot,src.slot);
        }
    }

    else if(is_vla(dst.type) && is_array(src.type))
    {
        RegSlot addr_slot;
        
        switch(dst.slot.kind)
        {
            case reg_kind::tmp:
            case reg_kind::sym:
            {
                const StructAddr struct_addr = {make_addr(dst.slot,0)};

                addr_slot = addrof_res(itl,func,struct_addr);
                break;
            }

            case reg_kind::spec:
            {
                switch(dst.slot.spec)
                {
                    case spec_reg::rv_struct:
                    {
                        addr_slot = make_sym_reg_slot(func.sig.args[0]);
                        break;
                    }

                    default: assert(false);
                }

                break;
            }
        }

        const RegSlot data_slot = load_arr_data(itl,func,src);
        store_ptr(itl,func,data_slot,addr_slot,0,GPR_SIZE,false);

        const RegSlot len_slot = load_arr_len(itl,func,src);
        store_ptr(itl,func,len_slot,addr_slot,GPR_SIZE,GPR_SIZE,false);
    }

    // requires special handling to move
    else if(is_struct(dst.type) && is_struct(src.type))
    {
        switch(dst.slot.kind)
        {
            case reg_kind::sym:
            case reg_kind::tmp:
            {
                const auto src_addr = make_struct_addr(src.slot,0);
                const auto dst_addr = make_struct_addr(dst.slot,0);

                ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst.type));
                break;
            }

            case reg_kind::spec:
            {
                switch(dst.slot.spec)
                {
                    // copy out the structure using the hidden pointer in the first arg
                    case spec_reg::rv_struct:
                    {
                        const auto src_addr = make_struct_addr(src.slot,0);
                        const auto dst_addr = make_pointer_addr(make_sym_reg_slot(func.sig.args[0]),0);

                        ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst.type));
                        break;
                    }

                    default: assert(false);
                }
                break;
            }
        }
    }

    else
    {
        assert(false);
    }
}


AddrSlot make_addr(RegSlot slot, u32 offset)
{
    return {slot,offset,false};
}

AddrSlot make_struct_addr(RegSlot slot, u32 offset)
{
    return {slot,offset,true};
}


AddrSlot take_addr(Interloper& itl, Function& func, RegSlot src, u32 offset)
{
    const auto src_ptr = addrof_res(itl,func,src,offset);
    return make_addr(src_ptr,0);
}

// get back a complete pointer
RegSlot collapse_offset(Interloper& itl,Function&func, RegSlot addr_slot, u32 *offset)
{
    if(*offset)
    {
        const RegSlot final_addr = add_imm_res(itl,func,addr_slot,*offset);
        *offset = 0;

        return final_addr;
    }
    
    else
    {
        return addr_slot;
    }
}


void collapse_struct_offset(Interloper& itl, Function& func, AddrSlot* struct_slot)
{
    if(struct_slot->struct_addr)
    {
        *struct_slot = take_addr(itl,func,struct_slot->slot,struct_slot->offset);
    }

    else
    {
        struct_slot->slot = collapse_offset(itl,func,struct_slot->slot,&struct_slot->offset);
    }
}


void load_ptr(Interloper &itl,Function& func,RegSlot dst_slot,RegSlot addr_slot,u32 offset,u32 size, b32 is_signed, b32 is_float)
{
    if(is_float)
    {
        load_float(itl,func,dst_slot,addr_slot,offset);
    }

    else if(is_signed)
    {
        switch(size)
        {
            case 1:
            {
                load_signed_byte(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 2: 
            {
                load_signed_half(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 4:
            {
                load_signed_word(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 8:
            {
                load_double(itl,func,dst_slot,addr_slot,offset);
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
                load_byte(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 2: 
            {
                load_half(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 4:
            {
                load_word(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            case 8:
            {
                load_double(itl,func,dst_slot,addr_slot,offset);
                break;
            }

            default: assert(false);
        }
    }
}

void load_struct(Interloper &itl,Function& func,RegSlot dst_slot,AddrSlot addr_slot,u32 size, b32 is_signed, b32 is_float)
{
    if(is_float)
    {
        load_struct_internal(itl,func,op_type::load_struct_f64,dst_slot,addr_slot);  
    }

    else if(is_signed)
    {
        const op_type SIGNED_LOAD_STRUCT_TABLE[] = {op_type::load_struct_s8,op_type::load_struct_s16,op_type::load_struct_s32,op_type::load_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,SIGNED_LOAD_STRUCT_TABLE[idx],dst_slot,addr_slot);     
    }

    else
    {
        const op_type UNSIGNED_LOAD_STRUCT_TABLE[] = {op_type::load_struct_u8,op_type::load_struct_u16,op_type::load_struct_u32,op_type::load_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,UNSIGNED_LOAD_STRUCT_TABLE[idx],dst_slot,addr_slot);     
    }
}

void load_addr_slot(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot addr_slot, u32 size, b32 sign, b32 is_float)
{
    if(addr_slot.struct_addr)
    {
        load_struct(itl,func,dst_slot,addr_slot,size,sign,is_float);
    }

    else
    {
        load_ptr(itl,func,dst_slot,addr_slot.slot,addr_slot.offset,size,sign,is_float);
    }
}

Option<itl_error> do_addr_copy(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot src_addr, u32 size)
{
    switch(dst_slot.kind)
    {
        case reg_kind::sym:
        case reg_kind::tmp:
        {
            const auto dst_addr = make_struct_addr(dst_slot,0);
            const auto memcpy_err = ir_memcpy(itl,func,dst_addr,src_addr,size);
            if(!!memcpy_err)
            {
                return memcpy_err;
            }

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
                    const auto dst_addr = make_addr(make_sym_reg_slot(func.sig.args[0]),0);
                    const auto memcpy_err = ir_memcpy(itl,func,dst_addr,src_addr,size);
                    if(!!memcpy_err)
                    {
                        return memcpy_err;
                    }
                    break;
                }

                default: assert(false);
            }
        }
    }

    return option::none;
}

Option<itl_error> do_addr_load(Interloper &itl,Function &func,RegSlot dst_slot,AddrSlot src_addr, const Type* type)
{
    const u32 size = type_size(itl,type);

    if(is_array(type))
    {
        if(is_runtime_size(type))
        {
            const auto copy_err = do_addr_copy(itl,func,dst_slot,src_addr,size);
            if(!!copy_err)
            {
                return copy_err;
            }
        }

        // fixed size array, the pointer is the array
        else
        {
            collapse_struct_offset(itl,func,&src_addr);
            mov_reg(itl,func,dst_slot,src_addr.slot);
        }
    }

    else if(is_struct(type))
    {
        const auto copy_err = do_addr_copy(itl,func,dst_slot,src_addr,size);
        if(!!copy_err)
        {
            return copy_err;
        }
    }

    else if(size <= GPR_SIZE)
    {
        const b32 sign = is_signed(type);
        const b32 fp = is_float(type);

        load_addr_slot(itl,func,dst_slot,src_addr,size,sign,fp);
    }            

    else
    {
        assert(false);
    }

    return option::none;
}

Option<itl_error> do_ptr_load(Interloper &itl,Function &func,RegSlot dst_slot,const TypedReg& reg, u32 offset = 0)
{
    const auto src_addr = make_addr(reg.slot,offset);
    return do_addr_load(itl,func,dst_slot,src_addr,reg.type);
}



void store_ptr(Interloper &itl,Function& func,RegSlot src_slot,RegSlot dst_addr,u32 offset,u32 size, b32 is_float)
{
    if(is_float)
    {
        store_float(itl,func,src_slot,dst_addr,offset);
    }

    else
    {
        switch(size)
        {
            case 1:
            {
                store_byte(itl,func,src_slot,dst_addr,offset);
                break;
            }

            case 2: 
            {
                store_half(itl,func,src_slot,dst_addr,offset);
                break;
            }

            case 4:
            {
                store_word(itl,func,src_slot,dst_addr,offset);
                break;
            }

            case 8:
            {
                store_double(itl,func,src_slot,dst_addr,offset);
                break;
            }

            default: assert(false);
        }
    }    
}


void store_struct(Interloper &itl,Function& func,RegSlot src_slot,AddrSlot dst_addr,u32 size, b32 is_float)
{
    if(is_float)
    {
        load_struct_internal(itl,func,op_type::store_struct_f64,src_slot,dst_addr);
    }

    else
    {
        const op_type STORE_STRUCT_TABLE[] = {op_type::store_struct_u8,op_type::store_struct_u16,op_type::store_struct_u32,op_type::store_struct_u64};
        const u32 idx = log2(size);

        assert(idx <= 3);

        load_struct_internal(itl,func,STORE_STRUCT_TABLE[idx],src_slot,dst_addr);
    }      
}

void store_addr_slot(Interloper &itl,Function &func,RegSlot src_slot,AddrSlot dst_addr, u32 size,b32 is_float)
{
    if(dst_addr.struct_addr)
    {
        store_struct(itl,func,src_slot,dst_addr,size,is_float);
    }

    else
    {
        store_ptr(itl,func,src_slot,dst_addr.slot,dst_addr.offset,size,is_float);
    }
}


Option<itl_error> do_addr_store(Interloper &itl,Function &func,RegSlot src_slot,AddrSlot dst_addr, const Type* type)
{
    const u32 size = type_size(itl,type);

    if(size <= GPR_SIZE)
    {
        const b32 fp = is_float(type);
        store_addr_slot(itl,func,src_slot,dst_addr,size,fp);
        return option::none;
    }

    // large copy
    else
    {      
        const auto src_addr = make_struct_addr(src_slot,0);
        return ir_memcpy(itl,func,dst_addr,src_addr,size);        
    } 
}

Option<itl_error> do_ptr_store(Interloper &itl,Function &func,RegSlot src_slot,const TypedReg& reg, u32 offset = 0)
{
    const auto dst_addr = make_addr(reg.slot,offset);
    return do_addr_store(itl,func,src_slot,dst_addr,reg.type);
}
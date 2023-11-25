
AddrSlot make_addr(SymSlot slot, u32 offset)
{
    return {slot,offset,false};
}

AddrSlot make_struct_addr(SymSlot slot, u32 offset)
{
    return {slot,offset,true};
}


AddrSlot take_addr(Interloper& itl, Function& func, SymSlot src, u32 offset)
{
    const auto src_ptr = addrof_res(itl,func,src,offset);
    return make_addr(src_ptr,0);
}

// get back a complete pointer
SymSlot collapse_offset(Interloper& itl,Function&func, SymSlot addr_slot, u32 *offset)
{
    if(*offset)
    {
        const SymSlot final_addr = add_imm_res(itl,func,addr_slot,*offset);
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


void load_ptr(Interloper &itl,Function& func,SymSlot dst_slot,SymSlot addr_slot,u32 offset,u32 size, b32 is_signed)
{
    if(is_signed)
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

void load_struct(Interloper &itl,Function& func,SymSlot dst_slot,AddrSlot addr_slot,u32 size, b32 is_signed)
{
    if(is_signed)
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

void load_addr_slot(Interloper &itl,Function &func,SymSlot dst_slot,AddrSlot addr_slot, u32 size, b32 sign)
{
    if(addr_slot.struct_addr)
    {
        load_struct(itl,func,dst_slot,addr_slot,size,sign);
    }

    else
    {
        load_ptr(itl,func,dst_slot,addr_slot.slot,addr_slot.offset,size,sign);
    }
}

void do_addr_load(Interloper &itl,Function &func,SymSlot dst_slot,AddrSlot src_addr, const Type* type)
{
    const u32 size = type_size(itl,type);

    if(is_array(type))
    {
        // TODO: we want to reduce the number of copies this requires
        // for passing to a function
        // we should probably pass in a special slot that tells us to just directly push things
        if(is_runtime_size(type))
        {
            const auto dst_addr = make_struct_addr(dst_slot,0);
            ir_memcpy(itl,func,dst_addr,src_addr,VLA_SIZE);
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
        const auto dst_addr = make_struct_addr(dst_slot,0);

        ir_memcpy(itl,func,dst_addr,src_addr,size);
    }

    else if(size <= GPR_SIZE)
    {
        const b32 sign = is_signed(type);

        load_addr_slot(itl,func,dst_slot,src_addr,size,sign);
    }            

    else
    {
        assert(false);
    }
}

void do_ptr_load(Interloper &itl,Function &func,SymSlot dst_slot,SymSlot ptr_slot, const Type* type, u32 offset = 0)
{
    const auto src_addr = make_addr(ptr_slot,offset);
    do_addr_load(itl,func,dst_slot,src_addr,type);
}



void store_ptr(Interloper &itl,Function& func,SymSlot src_slot,SymSlot dst_addr,u32 offset,u32 size)
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


void store_struct(Interloper &itl,Function& func,SymSlot src_slot,AddrSlot dst_addr,u32 size)
{
    const op_type STORE_STRUCT_TABLE[] = {op_type::store_struct_u8,op_type::store_struct_u16,op_type::store_struct_u32,op_type::store_struct_u64};
    const u32 idx = log2(size);

    assert(idx <= 3);

    load_struct_internal(itl,func,STORE_STRUCT_TABLE[idx],src_slot,dst_addr);      
}

void store_addr_slot(Interloper &itl,Function &func,SymSlot src_slot,AddrSlot dst_addr, u32 size)
{
    if(dst_addr.struct_addr)
    {
        store_struct(itl,func,src_slot,dst_addr,size);
    }

    else
    {
        store_ptr(itl,func,src_slot,dst_addr.slot,dst_addr.offset,size);
    }
}


void do_addr_store(Interloper &itl,Function &func,SymSlot src_slot,AddrSlot dst_addr, const Type* type)
{
    const u32 size = type_size(itl,type);

    if(size <= GPR_SIZE)
    {
        store_addr_slot(itl,func,src_slot,dst_addr,size);
    }

    // large copy
    else
    {      
        const auto src_addr = make_struct_addr(src_slot,0);
        ir_memcpy(itl,func,dst_addr,src_addr,size);        
    } 
}

void do_ptr_store(Interloper &itl,Function &func,SymSlot src_slot,SymSlot ptr_slot, const Type* type, u32 offset = 0)
{
    const auto dst_addr = make_addr(ptr_slot,offset);
    do_addr_store(itl,func,src_slot,dst_addr,type);
}
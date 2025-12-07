#include <interloper.h>

RegSlot load_arr_data(Interloper& itl,Function& func,const TypedAddr& src)
{
    if(is_runtime_size(src.type))
    {
        return load_addr_gpr_res(itl,func,src.addr_slot);
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return collapse_struct_addr_res(itl,func,src.addr_slot);
    }
}

RegSlot load_arr_data(Interloper& itl,Function& func,const TypedReg& reg)
{
    if(is_runtime_size(reg.type))
    {
        const auto addr_slot = make_struct_addr(reg.slot,0);
        return load_addr_gpr_res(itl,func,addr_slot);
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return reg.slot;
    }
}


RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_data(itl,func,typed_reg(sym));
}


RegSlot load_arr_len(Interloper& itl,Function& func,const TypedReg& reg)
{
    if(is_runtime_size(reg.type))
    {
        const auto addr_slot = make_struct_addr(reg.slot,GPR_SIZE);
        return load_addr_gpr_res(itl,func,addr_slot);
    }

    ArrayType* array_type = (ArrayType*)reg.type;

    return mov_imm_res(itl,func,array_type->size);   
}


RegSlot load_arr_len(Interloper& itl,Function& func,const TypedAddr& src)
{
    if(is_runtime_size(src.type))
    {
        AddrSlot addr_copy = src.addr_slot;
        addr_copy.addr.offset += GPR_SIZE;
        return load_addr_gpr_res(itl,func,addr_copy);
    }

    ArrayType* array_type = (ArrayType*)src.type;
    return mov_imm_res(itl,func,array_type->size);   
}

RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_len(itl,func,typed_reg(sym));
}



// NOTE: this has to be a vla
void store_arr_data(Interloper& itl, Function& func, RegSlot slot, RegSlot data)
{
    if(is_special_reg(slot))
    {
        switch(slot.spec)
        {
            case spec_reg::rv_struct:
            {
                store_ptr(itl,func,data,make_sym_reg_slot(func.sig.args[0]),0,GPR_SIZE,false);
                break;
            }

            default: assert(false);
        }
    }

    else
    {
        const auto dst_addr = StructAddr { make_addr(slot,0) };
        store_struct(itl,func,data,dst_addr,GPR_SIZE,false);
    }
}

void store_arr_len(Interloper& itl, Function& func, RegSlot slot,RegSlot len)
{
    if(is_special_reg(slot))
    {
        switch(slot.spec)
        {
            case spec_reg::rv_struct:
            {
                store_ptr(itl,func,len,make_sym_reg_slot(func.sig.args[0]),GPR_SIZE,GPR_SIZE,false);
                break;
            }

            default: assert(false);
        }
    }

    else
    {
        const auto dst_addr = StructAddr { make_addr(slot,GPR_SIZE) };
        store_struct(itl,func,len,dst_addr,GPR_SIZE,false);
    }
}


// TODO: we need to pass in a slot + offset for storing data copies...
#include "error.h"
#include "ir.h"

void do_addr_store(Interloper &itl,Function &func,const TypedReg& src,const TypedAddr& dst);

void make_any(Interloper& itl,Function& func, const AddrSlot& addr, const TypedReg& reg)
{
    auto& rtti = itl.rtti_cache;
    const u32 base_offset = addr.addr.offset;

    // store type
    auto any_type_addr = TypedAddr{addr, itl.byte_ptr_type};
    any_type_addr.addr_slot.addr.offset = base_offset + rtti.any_type_offset;

    // acquire a copy of the typing information from the const pool
    const TypedReg rtti_ptr = {acquire_rtti(itl,func,reg.type), itl.byte_ptr_type};

    do_addr_store(itl,func,rtti_ptr,any_type_addr);

    // store data.
    auto any_data_addr = TypedAddr{addr, itl.byte_ptr_type};
    any_data_addr.addr_slot.addr.offset = base_offset + rtti.any_data_offset;

    const bool stored_extern = !is_trivial_copy(reg.type);

    // goes directly in the pointer
    if(!stored_extern)
    {
        // store data
        do_addr_store(itl,func,reg,any_data_addr);
    } 

    // allready in memory just store a the pointer to it
    else if(is_vla(reg.type))
    {
        const StructAddr struct_addr = {make_addr(reg.slot,0)};
        const auto arr_ptr = TypedReg{addrof_res(itl,func,struct_addr), itl.byte_ptr_type};

        do_addr_store(itl,func,arr_ptr,any_data_addr);
    }

    else
    {
        compile_panic(itl,itl_error::invalid_expr,"Compile any for unhandled type: %t",reg.type);
    }

    // store extern flag
    auto any_stored_extern_addr = TypedAddr{addr, itl.byte_ptr_type};
    any_stored_extern_addr.addr_slot.addr.offset = base_offset + rtti.any_stored_extern_offset;

    const auto stored_extern_reg = TypedReg { mov_imm_res(itl,func,stored_extern), itl.bool_type };
    do_addr_store(itl,func,stored_extern_reg,any_stored_extern_addr);


}


void compile_any_internal(Interloper& itl, Function& func, AstNode* arg_node, const Option<AddrSlot>& addr)
{
    const auto& rtti = itl.rtti_cache;

    // push const str as fixed size array
    if(arg_node->type == ast_type::string)
    {
        StringNode* string_node = (StringNode*)arg_node;

        const u32 size = string_node->string.size;
        auto rtype = make_array(itl,make_builtin(itl,builtin_type::c8_t,true),size);

        // push the data offset
        const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,string_node->string);
        const RegSlot addr_slot = pool_addr_res(itl,func,pool_slot,0);

        const TypedReg reg = {addr_slot,rtype};

        if(!addr)
        {
            // alloc the struct size for our copy
            alloc_stack(itl,func,rtti.any_struct_size);

            const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);
            const auto addr_slot = make_pointer_addr(SP_SLOT, 0);
            make_any(itl,func,addr_slot,reg);
        }

        else
        {
            make_any(itl,func,*addr,reg);
        }

        return;
    }


    // compile our arg and figure out what we have
    auto arg_reg = compile_oper(itl,func,arg_node);

    // is allready an any just copy the struct
    if(is_any(itl,arg_reg.type))
    {
        const u32 stack_size = rtti.any_struct_size;

        if(!addr)
        {
            // alloc the struct size for our copy
            alloc_stack(itl,func,stack_size);

            const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);

            // need to save SP as it will get pushed last
            const RegSlot dst_ptr = copy_reg(itl,func,SP_SLOT);
            const auto dst_addr = make_pointer_addr(dst_ptr,0);

            const auto src_addr = make_struct_addr(arg_reg.slot,0);

            ir_memcpy(itl,func,dst_addr,src_addr,stack_size);
        }

        else
        {
            unimplemented("Any with unhandled storage");
        }

        return;
    }


    // Plain variable
    const u32 stack_size = rtti.any_struct_size;

    if(!addr)
    {
        // alloc the struct size for our copy
        alloc_stack(itl,func,stack_size);

        const RegSlot SP_SLOT = make_spec_reg_slot(spec_reg::sp);
        const auto dst_addr = make_pointer_addr(SP_SLOT,0);
        make_any(itl,func,dst_addr,arg_reg);
    }

    else
    {
        make_any(itl,func,*addr,arg_reg);
    }
}

// return total size including data
u32 compile_any(Interloper& itl, Function& func, AstNode* arg_node)
{
    // for now this just always takes size of the any struct
    const u32 size = align_val(itl.rtti_cache.any_struct_size,GPR_SIZE);

    // Handle stack alloc and store itself caller will handle deallocation of stack
    compile_any_internal(itl,func,arg_node,option::none);
    return size;
}


void compile_any_arr(Interloper& itl, Function& func, AstNode* arg_node, const Option<AddrSlot>& addr)
{
    // storage allocation handled by caller
    compile_any_internal(itl,func,arg_node,addr);
}
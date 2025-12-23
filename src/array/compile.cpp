

void compile_struct_decl_default(Interloper& itl, Function& func, const Struct& structure,AddrSlot addr_slot);


void default_construct_arr(Interloper& itl, Function& func,ArrayType* type, AddrSlot addr_slot)
{
    // VLA just setup the struct
    if(!is_fixed_array(type))
    {
        const auto zero = mov_imm_res(itl,func,0);

        store_addr_slot(itl,func,zero,addr_slot,GPR_SIZE,false);
        addr_slot.addr.offset += GPR_SIZE;

        store_addr_slot(itl,func,zero,addr_slot,GPR_SIZE,false);
        addr_slot.addr.offset += GPR_SIZE;

        return;
    }

    switch(type->contained_type->kind)
    {
        // We have arrays, just nest this
        case type_class::array_t:
        {
            ArrayType* next_type = (ArrayType*)type->contained_type;

            for(u32 i = 0; i < type->size; i++)
            {
                auto sub_addr = addr_slot;
                sub_addr.addr.offset += (i * next_type->sub_size);

                default_construct_arr(itl,func,next_type,sub_addr);
            }

            break;
        }

        case type_class::struct_t:
        {
            // TODO: replace this with a zero_mem primitive
            // if the struct has no initializers
            const auto structure = struct_from_type(itl.struct_table,type->contained_type);

            auto struct_addr = addr_slot;

            for(u32 i = 0; i < type->size; i++)
            {
                compile_struct_decl_default(itl,func,structure,struct_addr);
                struct_addr.addr.offset += structure.size;
            }

            break;
        }

        // Final plain values
        default:
        {
            const RegSlot ptr = collapse_struct_addr_res(itl,func,addr_slot);
            ir_zero(itl,func,ptr,type->size * type->sub_size);
            break;
        }
    }
}

void compile_array_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& array)
{

    // allocate fixed array if needed, and initialize it to its data pointer
    if(is_fixed_array(array.type))
    {
        const auto [arr_size,arr_count] = calc_arr_allocation(itl,array.type);

        // we have the allocation information now complete it
        switch(array.reg.segment)
        {
            case reg_segment::local:
            {
                const auto opcode = make_op(op_type::alloc_local_array,make_reg_operand(array.reg.slot),make_imm_operand(arr_size),make_imm_operand(arr_count));
                emit_block_func(func,opcode);
                break;
            }
            // just dump addr
            case reg_segment::global:
            {
                const u32 alloc_idx = allocate_global_array(itl.global_alloc,itl.symbol_table,array.reg.slot.sym_slot,arr_size,arr_count);
                const auto opcode = make_op(op_type::alloc_global_array,make_reg_operand(array.reg.slot),make_imm_operand(alloc_idx));
                emit_block_func(func,opcode);
                break;
            }

            // constants should not go through this function!
            case reg_segment::constant:
            {
                assert(false);
                break;
            }
        }
    }

    // Default init
    if(!decl_node->expr)
    {
        const auto addr_slot = make_pointer_addr(array.reg.slot,0);
        default_construct_arr(itl,func,(ArrayType*)array.type,addr_slot);   
    }

    // has an initializer
    else if(decl_node->expr->type != ast_type::no_init)
    {
        unimplemented("Array initializer");
    }
}


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

void compile_array_init(Interloper& itl, Function& func, AstNode* node,ArrayType* type, AddrSlot *addr_slot);

void compile_array_initializer_list(Interloper& itl, Function& func, InitializerListNode* init_list,ArrayType* type, AddrSlot *addr_slot)
{
    switch(type->contained_type->kind)
    {
        // Handle sub array
        case type_class::array_t:
        {
            // normal types
            for(AstNode* node : init_list->list)
            {
                compile_array_init(itl,func,node,(ArrayType*)type->contained_type,addr_slot);
            }
            break;
        }

        case type_class::struct_t:
        {
            unimplemented("Compile Struct array initializer");
        }

        default:
        {
            // we are getting to the value assigns!
            Type* base_type = type->contained_type;
            const u32 size = type_size(itl,base_type);

            // normal types
            for(AstNode* node : init_list->list)
            {
                auto reg = compile_oper(itl,func,node);

                const TypedAddr dst_addr = {*addr_slot,base_type};
                do_addr_store(itl,func,reg,dst_addr);
                addr_slot->addr.offset += size;
            }

            break;
        }
    }
}

void compile_array_init(Interloper& itl, Function& func, AstNode* node,ArrayType* type, AddrSlot *addr_slot)
{
    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            compile_array_initializer_list(itl,func,(InitializerListNode*)node,type,addr_slot);
            break;
        } 

        case ast_type::string:
        {
            unimplemented("String intializer");
        }

        // Do nothing.
        case ast_type::no_init:
        {
            break;
        }

        default:
        {
            auto reg = compile_oper(itl,func,node);

            const TypedAddr dst_addr = {*addr_slot,(Type*)type};
            do_addr_store(itl,func,reg,dst_addr);
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

    // VLA allocate the struct
    else
    {
        alloc_slot(itl,func,array.reg.slot,true);
    }

    auto addr_slot = is_runtime_size(array.type)? make_struct_addr(array.reg.slot,0) : make_pointer_addr(array.reg.slot,0);

    // Default init
    if(!decl_node->expr)
    {
        // VLA is a struct on the stack and must be treated as such
        default_construct_arr(itl,func,(ArrayType*)array.type,addr_slot);
        return;  
    }

    compile_array_init(itl,func,decl_node->expr,(ArrayType*)array.type,&addr_slot);
}

TypedAddr compile_pointer_index(Interloper& itl, Function& func, IndexNode* index, RegSlot ptr_slot)
{
    AstNode* subscript_expr = index->indexes[0];

    Type* indexed_type = index->node.expr_type;

    const u32 size = type_size(itl,indexed_type);

    if(subscript_expr->known_value)
    {
        const u32 offset = *subscript_expr->known_value * size;
        return TypedAddr {make_pointer_addr(ptr_slot,offset),indexed_type};        
    }

    const auto subscript = compile_oper(itl,func,subscript_expr);
    const AddrSlot addr = generate_indexed_pointer(itl,func,ptr_slot,subscript.slot,size,0);

    return TypedAddr {addr,indexed_type};    
}

Addr rescale_addr(Interloper& itl, Function& func, Addr addr)
{
    if(addr.scale > GPR_SIZE || !is_pow2(addr.scale))
    {
        addr.index = mul_imm_res(itl,func,addr.index,addr.scale);
        addr.scale = 1;
    }

    return addr;
}


Addr collapse_array_addr(Interloper& itl, Function& func, Addr addr)
{
    rescale_addr(itl,func,addr);

    PointerAddr pointer = {addr};
    const RegSlot base = lea_res(itl,func,pointer);

    return make_addr(base,0);
}

TypedAddr compile_array_index(Interloper& itl, Function& func, IndexNode* index, ArrayType* array_type, RegSlot ptr_slot)
{
    const u32 indexes = count(index->indexes);

    Addr addr = make_addr(ptr_slot,0);

    for(u32 i = 0; i < indexes; i++)
    {
        const bool last_index = i == indexes - 1;
        
        // perform the indexing operation
        const u32 size = array_type->sub_size;

        AstNode* subscript = index->indexes[i];

        if(!subscript->known_value)
        {
            if(!is_null_reg(addr.index))
            {
                addr = collapse_array_addr(itl,func,addr);
            }

            const auto src = compile_oper(itl,func,index->indexes[i]);
            addr.index = src.slot;
            addr.scale = size;
        }

        else
        {
            addr.offset += *subscript->known_value * size;
        }


        const bool contains_arr = is_array(array_type->contained_type);

        // vla and indexing is not finished need to load data ptr
        if(is_runtime_size(array_type))
        {
            // this is not the last index
            if(contains_arr && !last_index)
            {
                const auto tmp = new_tmp_ptr(func);

                PointerAddr pointer = {rescale_addr(itl,func,addr)};
                load_ptr_addr(itl,func,tmp,pointer,GPR_SIZE,false,false);

                addr = make_addr(tmp,0);
            }
        }

        // goto next subscript
        if(contains_arr)
        {
            array_type = (ArrayType*)index_arr(array_type);
        }
    }

    // Final crush of the scale
    AddrSlot pointer = {rescale_addr(itl,func,addr)};

    // return pointer to accessed type
    // NOTE: this can give out a fixed array pointer.
    // this needs conversion by the host into a VLA, this is not obtainable by
    // taking a pointer to an array,
    return TypedAddr{pointer,index->node.expr_type};     
}

TypedAddr index_arr(Interloper& itl, Function& func, IndexNode* index)
{
    auto& array = sym_from_slot(itl.symbol_table,index->sym_slot);

    switch(index->type)
    {
        case index_type::pointer:
        {
            return compile_pointer_index(itl,func,index,array.reg.slot);
        }

        case index_type::array:
        {
            const auto ptr_slot = load_arr_data(itl,func,array);
            return compile_array_index(itl,func,index,(ArrayType*)array.type,ptr_slot);
        }
    }

    assert(false);
}

void compile_index(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    const auto index = index_arr(itl,func,(IndexNode*)expr);

    // fixed array needs conversion by host
    if(is_fixed_array(index.type))
    {
        collapse_struct_addr(itl,func,dst_slot,index.addr_slot);
        return;
    }
    
    do_addr_load(itl,func,dst_slot,index);
}


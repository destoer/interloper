#include <interloper.h>

RegResult index_pointer(Interloper& itl,Function& func,RegSlot ptr_slot,
    RegSlot dst_slot,IndexNode* index_node,PointerType* type)
{
    const u32 indexes = count(index_node->indexes);

    if(indexes != 1)
    {
        return compile_error(itl,itl_error::array_type_error,"[COMPILE]: expected single index for pointer\n");
    }

    Type* plain = type->contained_type;

    const u32 size = type_size(itl,plain);

    const auto res = compile_oper(itl,func,index_node->indexes[0]);
    if(!res)
    {
        return res;
    }
    
    auto subscript = *res;

    if(!is_integer(subscript.type))
    {
        return compile_error(itl,itl_error::int_type_error,"[COMPILE]: expected integeral expr for array subscript got %s\n",
            type_name(itl,subscript.type).buf); 
    }

    const RegSlot offset = mul_imm_res(itl,func,subscript.slot,size);  

    add(itl,func,dst_slot,ptr_slot,offset);
    return TypedReg{dst_slot,(Type*)type};
}

// indexes off a given type + ptr
RegResult index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, RegSlot ptr_slot, RegSlot dst_slot)
{
    // standard array index
    if(!is_array(type))
    {
        return compile_error(itl,itl_error::array_type_error,"[COMPILE]: '%s' is not an array got type %s\n",arr_name.buf,type_name(itl,type).buf);
    }

    RegSlot last_slot = ptr_slot;

    ArrayType* array_type = (ArrayType*)type;

    Type* accessed_type = nullptr;

    const u32 indexes = count(index_node->indexes);

    for(u32 i = 0; i < indexes; i++)
    {

        const auto res = compile_oper(itl,func,index_node->indexes[i]);
        if(!res)
        {
            return res;
        }

        const auto subscript = *res;

        if(!is_integer(subscript.type))
        {
            return compile_error(itl,itl_error::int_type_error,"[COMPILE]: expected integeral expr for array subscript got %s\n",
                type_name(itl,subscript.type).buf);
        }
        
        const bool last_index = i == indexes - 1;
        

        // perform the indexing operation

        const u32 size = array_type->sub_size;

        const RegSlot mul_slot = mul_imm_res(itl,func,subscript.slot,size);   

        const RegSlot add_slot = last_index? dst_slot : new_tmp(func,GPR_SIZE);
        add(itl,func,add_slot,last_slot,mul_slot);

        last_slot = add_slot;

        // vla and indexing insnt finished need to load data ptr
        if(is_runtime_size(type))
        {
            // this is not the last index
            if(is_array(array_type->contained_type) && !last_index)
            {
                const auto tmp = new_tmp_ptr(func);

                load_ptr(itl,func,tmp,last_slot,0,GPR_SIZE,false,false);

                last_slot = tmp;
            }
        }

        // handle typing on next index

        // goto next subscript
        if(is_array(array_type->contained_type))
        {
            array_type = (ArrayType*)index_arr(array_type);

            if(last_index)
            {
                accessed_type = (Type*)array_type;
            }

        }

        // this last index will give us our actual values
        else
        {
            // this is the last index i.e it is fine to get back a contained type
            // that is plain!
            if(last_index)
            {
                accessed_type = index_arr(array_type);
            }

            else 
            {
                return compile_error(itl,itl_error::out_of_bounds,"Out of bounds indexing for array %s (%d:%d)\n",arr_name.buf,i,indexes);                         
            }
        } 
        
    }

    // return pointer to accessed type
    // NOTE: this can give out a fixed array pointer.
    // this needs conversion by the host into a VLA, this is not obtainable by
    // taking a pointer to an array,
    return TypedReg{dst_slot,make_reference(itl,accessed_type)}; 
}

// TODO: these multidimensional handwave vla's

RegSlot load_arr_data(Interloper& itl,Function& func,const TypedAddr& addr_slot)
{
    if(is_runtime_size(addr_slot.type))
    {
        return load_struct_u64_res(itl,func,addr_slot.addr);
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return collapse_struct_res(itl,func,addr_slot.addr);
    }
}

RegSlot load_arr_data(Interloper& itl,Function& func,const TypedReg& reg)
{
    if(is_runtime_size(reg.type))
    {
        const auto addr_slot = make_struct_addr(reg.slot,0);
        return load_struct_u64_res(itl,func,addr_slot);
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return reg.slot;
    }
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
        const auto dst_addr = make_struct_addr(slot,0);
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
        const auto dst_addr = make_struct_addr(slot,GPR_SIZE);
        store_struct(itl,func,len,dst_addr,GPR_SIZE,false);
    }
}

RegSlot load_arr_len(Interloper& itl,Function& func,const TypedReg& reg)
{
    if(is_runtime_size(reg.type))
    {
        const auto addr_slot = make_struct_addr(reg.slot,GPR_SIZE);
        return load_struct_u64_res(itl,func,addr_slot);
    }

    ArrayType* array_type = (ArrayType*)reg.type;

    return mov_imm_res(itl,func,array_type->size);   
}


RegSlot load_arr_len(Interloper& itl,Function& func,const TypedAddr& addr_slot)
{
    if(is_runtime_size(addr_slot.type))
    {
        AddrSlot addr_copy = addr_slot.addr;
        addr_copy.offset += GPR_SIZE;
        return load_struct_u64_res(itl,func,addr_copy);
    }

    ArrayType* array_type = (ArrayType*)addr_slot.type;
    return mov_imm_res(itl,func,array_type->size);   
}

RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_data(itl,func,typed_reg(sym));
}

RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_len(itl,func,typed_reg(sym));
}

RegResult index_arr(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    IndexNode* index_node = (IndexNode*)node;

    const auto arr_name = index_node->name;

    const auto arr_ptr = get_sym(itl.symbol_table,arr_name);

    if(!arr_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: array '%s' used before declaration\n",arr_name.buf);     
    }

    const auto arr = *arr_ptr;

    if(is_array(arr.type))
    {
        // get the initial data ptr
        const RegSlot data_slot = load_arr_data(itl,func,arr);

        return index_arr_internal(itl,func,index_node,arr_name,arr.type,data_slot,dst_slot);
    }

    else if(is_pointer(arr.type))
    {
        return index_pointer(itl,func,arr.reg.slot,dst_slot,index_node,(PointerType*)arr.type);
    }

    else
    {
        return compile_error(itl,itl_error::array_type_error,"[COMPILE]: expected array or pointer for index got %s\n",type_name(itl,arr.type));        
    }
}

TypeResult read_arr(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    auto index_res = index_arr(itl,func,node,new_tmp_ptr(func));
    if(!index_res)
    {
        return index_res.error();
    }

    auto index = *index_res;

    index.type = deref_pointer(index.type);

    // fixed array needs conversion by host
    if(is_fixed_array(index.type))
    {
        mov_reg(itl,func,dst_slot,index.slot);
        return index.type;
    }

    const auto err = do_ptr_load(itl,func,dst_slot,index);
    if(!!err)
    {
        return *err;
    }

    return index.type;
}


// TODO: our type checking for our array assigns has to be done out here to ensure locals are type checked correctly
Option<itl_error> write_arr(Interloper &itl,Function &func,AstNode *node,const TypedReg& src)
{
    auto index_res = index_arr(itl,func,node,new_tmp_ptr(func));
    if(!index_res)
    {
        return index_res.error();
    }

    auto index = *index_res;

    // convert fixed size pointer..
    if(is_array(src.type))
    {
        unimplemented("array write");
    }

    else
    {
        // deref of pointer
        index.type = deref_pointer(index.type);

        const auto store_err = do_ptr_store(itl,func,src.slot,index);
        if(!!store_err)
        {
            return *store_err;
        }

        return check_assign(itl,index.type,src.type);
    }
}

Option<itl_error> assign_vla_initializer(Interloper& itl, Function& func, RecordNode* list, AddrSlot* addr_slot, ArrayType* type)
{
    const u32 node_len = count(list->nodes);

    if(node_len != 2)
    {
        return compile_error(itl,itl_error::missing_initializer,"vla initializer expects 2 initializers {POINTER,SIZE}");
    }

    const auto data_res = compile_oper(itl,func,list->nodes[0]);
    if(!data_res)
    {
        return data_res.error();
    }

    const auto ptr = *data_res;

    const auto ptr_assign_err = check_assign(itl,make_reference(itl,type->contained_type),ptr.type);
    if(!!ptr_assign_err)
    {
        return ptr_assign_err;
    }

    store_addr_slot(itl,func,ptr.slot,*addr_slot,GPR_SIZE,false);
    addr_slot->offset += GPR_SIZE;

    const auto len_res = compile_oper(itl,func,list->nodes[1]);
    if(!len_res)
    {
        return len_res.error();
    }

    const auto len = *len_res;

    const auto len_assign_err = check_assign(itl,make_builtin(itl,GPR_SIZE_TYPE),len.type);
    if(!!len_assign_err)
    {
        return len_assign_err;
    }

    store_addr_slot(itl,func,len.slot,*addr_slot,GPR_SIZE,false);
    addr_slot->offset += GPR_SIZE;
    return option::none;
}

Option<itl_error> traverse_string_initializer_internal(Interloper& itl,Function& func,AstNode* node,AddrSlot* addr_slot, ArrayType* next_arr)
{
    LiteralNode* literal_node = (LiteralNode*)node;
    const String literal = literal_node->literal;        

    if(!is_string(next_arr))
    {
        return compile_error(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,(Type*)next_arr).buf);
    }

    if(is_runtime_size(next_arr))
    {
        if(is_const_string(next_arr))
        {
            // we can set this up directly from the const pool
            const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,literal);

            const RegSlot arr_data = pool_addr_res(itl,func,pool_slot,0);
            store_addr_slot(itl,func,arr_data,*addr_slot,GPR_SIZE,false);

            addr_slot->offset += GPR_SIZE;

            const RegSlot arr_size = mov_imm_res(itl,func,literal.size);
            store_addr_slot(itl,func,arr_size,*addr_slot,GPR_SIZE,false);

            addr_slot->offset += GPR_SIZE;
            return option::none;
        }

        else
        {
            return compile_error(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla\n");
        }
    }

    // fixed sized array
    else
    {
        return compile_error(itl,itl_error::string_type_error,"cannot assign string literal to fixed sized array\n");
    }
}

Option<itl_error> traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,AddrSlot* addr_slot, ArrayType* type)
{
    const u32 node_len = count(list->nodes);

    // this just gets the first node size
    if(type->size == DEDUCE_SIZE)
    {
        type->size = node_len;
    }

    else if(is_runtime_size(type))
    {
        return assign_vla_initializer(itl,func,list,addr_slot,type);
    }

    // next type is a sub array
    if(is_array(type->contained_type))
    {
        ArrayType* next_arr = (ArrayType*)type->contained_type;

        const u32 count = type->size;

        // type check the actual ammount is right
        // TODO: this should allow not specifing the full ammount but for now just keep it simple
        if(count != node_len)
        {
            return compile_error(itl,itl_error::missing_initializer,"array %s expects %d initializers got %d\n",
                type_name(itl,(Type*)type).buf,count,node_len);
        }        

        for(u32 n = 0; n < node_len; n++)
        {
            AstNode* node = list->nodes[n];

            // descend each sub initializer until we hit one containing values
            // for now we are just gonna print them out, and then we will figure out how to emit the inialzation code
            switch(node->type)
            {
                case ast_type::initializer_list:
                {
                    const auto traverse_err = traverse_arr_initializer_internal(itl,func,(RecordNode*)node,addr_slot,next_arr);
                    if(!!traverse_err)
                    {
                        return *traverse_err;
                    }
                    break;
                }

                case ast_type::string:
                {
                    return traverse_string_initializer_internal(itl,func,node,addr_slot,next_arr);
                }

                // handle an array (this should fufill the current "depth req in its entirety")
                default:
                {
                    unimplemented("arr initializer with array");
                    break;            
                }
            }
        }
    }

    // we are getting to the value assigns!
    else
    {  
        Type* base_type = type->contained_type;

        const u32 size = type_size(itl,base_type);

        // seperate loop incase we need to handle initializers
        if(is_struct(base_type))
        {
            for(u32 i = 0; i < node_len; i++)
            {

                // struct initalizer
                if(list->nodes[i]->type == ast_type::initializer_list)
                {
                    const auto structure = struct_from_type(itl.struct_table,base_type);
                    const auto struct_err = traverse_struct_initializer(itl,func,(RecordNode*)list->nodes[i],*addr_slot,structure);
                    if(!!struct_err)
                    {
                        return struct_err;
                    }
                }

                // allready finished struct
                else
                {
                    auto res = compile_oper(itl,func,list->nodes[i]);
                    if(!res)
                    {
                        return res.error();
                    }

                    auto reg = *res;

                    const auto assign_err = check_assign_init(itl,base_type,reg.type);
                    if(!!assign_err)
                    {
                        return assign_err;
                    }

                    const auto store_err = do_addr_store(itl,func,reg.slot,*addr_slot,base_type);
                    if(!!store_err)
                    {
                        return store_err;
                    }
                }

                addr_slot->offset += size;
            }
        }

        // normal types
        else
        {
            for(u32 i = 0; i < node_len; i++)
            {
                auto res = compile_oper(itl,func,list->nodes[i]);
                if(!res)
                {
                    return res.error();
                }

                auto reg = *res;

                const auto assign_err = check_assign_init(itl,base_type,reg.type);
                if(!!assign_err)
                {
                    return assign_err;
                }

                const auto store_err = do_addr_store(itl,func,reg.slot,*addr_slot,base_type);
                if(!!store_err)
                {
                    return store_err;
                }
                addr_slot->offset += size;
            }
        }           
    }

    return option::none;   
}

// for stack allocated arrays i.e ones with fixed sizes at the top level of the decl
std::pair<u32,u32> calc_arr_allocation(Interloper& itl, Symbol& sym)
{
    UNUSED(itl);

    b32 done = false;
    
    Type* type = sym.type;
    u32 count = 0;
    u32 size = 0;

    while(!done)
    {
        switch(type->kind)
        {
            case type_class::pointer_t:
            {
                size = GPR_SIZE;

                // whatever is pointed too is responsible for handling its own allocation
                // because it comes from somewhere else we are done!
                done = true;
                break;
            }

            case type_class::array_t:
            {
                ArrayType* array_type = (ArrayType*)type;

                if(is_runtime_size(array_type))
                {
                    size = GPR_SIZE * 2;
                    done = true;
                }

                else
                {
                    count = accumulate_count(count,array_type->size);
                    type = index_arr(type);
                }
                break;
            }

            default:
            {
                size = type_size(itl,type);

                done = true;
                break;
            }
        }
    }

    if(size > GPR_SIZE)
    {
        count = gpr_count(count * size);
        size = GPR_SIZE;
    }   

    return std::pair{size,count};
}

Option<itl_error> traverse_arr_initializer(Interloper& itl,Function& func,AstNode *node,AddrSlot addr_slot, Type* type)
{
    RecordNode* list = (RecordNode*)node;

    return traverse_arr_initializer_internal(itl,func,list,&addr_slot,(ArrayType*)type);
}


void store_const_string(Interloper& itl, Function& func, const String& literal, const RegSlot arr_slot)
{
    // we can set this up directly from the const pool
    const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,literal);

    const RegSlot arr_data = pool_addr_res(itl,func,pool_slot,0);
    store_arr_data(itl,func,arr_slot,arr_data);

    const RegSlot arr_size = mov_imm_res(itl,func,literal.size);
    store_arr_len(itl,func,arr_slot,arr_size);
}

Option<itl_error> compile_arr_assign(Interloper& itl, Function& func, AstNode* node, const TypedReg& arr)
{
    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            // initialize the actual struct
            if(is_runtime_size(arr.type))
            {
                auto addr_slot = make_struct_addr(arr.slot,0);
                return assign_vla_initializer(itl,func,(RecordNode*)node,&addr_slot,(ArrayType*)arr.type);
            }

            else 
            {
                const RegSlot ptr_slot = load_arr_data(itl,func,arr);
                const auto addr_slot = make_addr(ptr_slot,0);
                return traverse_arr_initializer(itl,func,node,addr_slot,arr.type);
            }
            break;
        }

        case ast_type::string:
        {
            // TODO: we need to add different hanlding for const strings

            if(!is_string(arr.type))
            {
                return compile_error(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,arr.type).buf);
            }

            ArrayType* array_type = (ArrayType*)arr.type;

            LiteralNode* literal_node = (LiteralNode*)node;
            const String literal = literal_node->literal;

            if(is_runtime_size(array_type))
            {
                if(is_const_string(arr.type))
                {
                    store_const_string(itl,func,literal,arr.slot);
                }

                else
                {
                    return compile_error(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla %s\n",type_name(itl,arr.type).buf);
                }
            }

            // fixed sized array
            else
            {
                return compile_error(itl,itl_error::string_type_error,"cannot assign string literal to fixed sized array\n");
            }

            return option::none;
        }
    
        // arbitary expression
        default:
        {
            // compile expr
            auto res = compile_oper(itl,func,node);
            if(!res)
            {
                return res.error();
            }

            auto reg = *res;

            const auto assign_err = check_assign_init(itl,arr.type,reg.type);
            if(!!assign_err)
            {
                return *assign_err;
            }
            
            return compile_move(itl,func,arr,reg);
        }
    }
}

Option<itl_error> default_construct_arr(Interloper& itl, Function& func,ArrayType* type, AddrSlot addr_slot)
{
    if(is_fixed_array(type))
    {
        // this has not been inited by traverse_arr_initializer
        if(type->size == DEDUCE_SIZE)
        {
            return compile_error(itl,itl_error::missing_initializer,"auto sized array does not have an initializer\n");
        }

        if(is_array(type->contained_type))
        {
            ArrayType* next_type = (ArrayType*)type->contained_type;

            for(u32 i = 0; i < type->size; i++)
            {
                auto sub_addr = addr_slot;
                sub_addr.offset += (i * next_type->sub_size);

                const auto recur_err = default_construct_arr(itl,func,next_type,sub_addr);
                if(!!recur_err)
                {
                    return *recur_err;
                }
            }
        }

        // default construct each arr
        else if(is_struct(type->contained_type))
        {
            // TODO: replace this with a zero_mem primitive
            // if the struct has no initalizers
            const auto structure = struct_from_type(itl.struct_table,type->contained_type);

            auto struct_addr = addr_slot;

            for(u32 i = 0; i < type->size; i++)
            {
                // TODO: just default construct it for now!
                const auto struct_err = compile_struct_decl_default(itl,func,structure,struct_addr);
                if(!!struct_err)
                {
                    return struct_err;
                }
                struct_addr.offset += structure.size;
            }
        }

        // final plain values
        else
        {
            const RegSlot ptr = collapse_struct_res(itl,func,addr_slot);
            return ir_zero(itl,func,ptr,type->size * type->sub_size);
        }
    }

    // vla just setup the struct
    else
    {
        const auto zero = mov_imm_res(itl,func,0);

        store_addr_slot(itl,func,zero,addr_slot,GPR_SIZE,false);
        addr_slot.offset += GPR_SIZE;

        store_addr_slot(itl,func,zero,addr_slot,GPR_SIZE,false);
        addr_slot.offset += GPR_SIZE;
    }

    return option::none;       
}

Option<itl_error> compile_arr_decl(Interloper& itl, Function& func, const DeclNode *decl_node, SymSlot slot)
{
    // This allocation needs to happen before we initialize the array but we dont have all the information yet
    // so we need to finish it up later
    OpcodeNode* alloc = alloc_slot(itl,func,make_sym_reg_slot(slot),true);

    // has an initalizer
    if(decl_node->expr)
    {
        auto& array = sym_from_slot(itl.symbol_table,slot);

        if(decl_node->expr->type != ast_type::no_init)
        {
            const auto assign_err = compile_arr_assign(itl,func,decl_node->expr,typed_reg(array));
            if(!!assign_err)
            {
                return assign_err;
            }
        }

        // now we have deduced every size, we need to reinit the sub size...
        if(is_fixed_array(array.type))
        {
            init_arr_sub_sizes(itl,(Type*)array.type);
        }
    }

    // default construct
    else 
    {
        auto& array = sym_from_slot(itl.symbol_table,slot);

        ArrayType* array_type = (ArrayType*)array.type;

        if(!is_fixed_array(array.type))
        {
            const auto addr_slot = make_struct_addr(array.reg.slot,0);
            const auto construct_err = default_construct_arr(itl,func,array_type,addr_slot);
            if(!!construct_err)
            {
                return construct_err;
            }
        }

        else
        {
            const auto addr_slot = make_addr(array.reg.slot,0);
            const auto construct_err = default_construct_arr(itl,func,array_type,addr_slot);
            if(!!construct_err)
            {
                return construct_err;
            }
        }
    }

    auto& array = sym_from_slot(itl.symbol_table,slot);

    const auto [arr_size,arr_count] = calc_arr_allocation(itl,array);

    // allocate fixed array if needed, and initalize it to its data pointer
    if(is_fixed_array(array.type))
    {
        // we have the allocation information now complete it
        switch(array.reg.segment)
        {
            case reg_segment::local:
            {
                alloc->value = make_op(op_type::alloc_local_array,make_reg_operand(array.reg.slot),make_imm_operand(arr_size),make_imm_operand(arr_count));
                break;
            }
            // just dump addr
            case reg_segment::global:
            {
                const u32 alloc_idx = allocate_global_array(itl.global_alloc,itl.symbol_table,slot,arr_size,arr_count);
                alloc->value = make_op(op_type::alloc_global_array,make_reg_operand(array.reg.slot),make_imm_operand(alloc_idx));
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
    
    return option::none;
}

TypeResult slice_array_addr(Interloper& itl, Function& func, SliceNode* slice_node, RegSlot dst_slot, TypedAddr arr)
{
    if(!is_array(arr.type))
    {
        return compile_error(itl,itl_error::array_type_error,"[COMPILE]: expected array or pointer for slice got %s\n",type_name(itl,arr.type).buf);       
    }

    RegSlot data_slot = load_arr_data(itl,func,arr);
    RegSlot slice_lower = make_spec_reg_slot(spec_reg::null);

    // Lower is populated add to data
    if(slice_node->lower)
    {
        const auto res = compile_oper(itl,func,slice_node->lower);
        if(!res)
        {
            return res.error();
        }

        const auto index = *res;

        slice_lower = index.slot;

        if(!is_integer(index.type))
        {
            return compile_error(itl,itl_error::array_type_error,"[COMPILE]: expected integer for slice lower bound got %s\n",type_name(itl,index.type).buf);   
        }

        const RegSlot offset_slot = mul_imm_res(itl,func,index.slot,type_size(itl,index_arr(arr.type)));
        data_slot = add_res(itl,func,data_slot,offset_slot);
    }

    store_arr_data(itl,func,dst_slot,data_slot);

    RegSlot data_len = make_spec_reg_slot(spec_reg::null);
    
    // Upper is populated set the length
    if(slice_node->upper)
    {
        const auto res = compile_oper(itl,func,slice_node->upper);
        if(!res)
        {
            return res.error();
        }

        const auto upper = *res;

        if(!is_integer(upper.type))
        {
            return compile_error(itl,itl_error::array_type_error,"[COMPILE]: expected integer for slice upper bound got %s\n",
                type_name(itl,upper.type).buf);      
        }

        data_len = upper.slot;
    }

    else
    {
        data_len = load_arr_len(itl,func,arr);
    }
    
    // Sub lower slice if present
    if(slice_node->lower)
    {
        data_len = sub_res(itl,func,data_len,slice_lower);
    }

    store_arr_len(itl,func,dst_slot,data_len);

    // If array type is not runtime size make it!
    if(!is_runtime_size(arr.type))
    {
        auto copy_arr_type = (ArrayType*)copy_type(itl,arr.type);
        copy_arr_type->size = RUNTIME_SIZE;

        return (Type*)copy_arr_type;
    }

    return arr.type;
}

TypeResult slice_array(Interloper& itl, Function& func,SliceNode* slice_node, RegSlot dst_slot)
{
    const auto arr_name = slice_node->name;
    const auto arr_ptr = get_sym(itl.symbol_table,arr_name);

    if(!arr_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: array '%s' used before declaration\n",arr_name.buf);      
    }

    const auto arr_sym = *arr_ptr;
    const auto arr = typed_addr(arr_sym);

    return slice_array_addr(itl,func,slice_node,dst_slot,arr);
}
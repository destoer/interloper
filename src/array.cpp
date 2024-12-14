#include <interloper.h>

std::pair<Type*,SymSlot> index_pointer(Interloper& itl,Function& func,SymSlot ptr_slot,SymSlot dst_slot,IndexNode* index_node,PointerType* type)
{
    const u32 indexes = count(index_node->indexes);

    if(indexes != 1)
    {
        panic(itl,itl_error::array_type_error,"[COMPILE]: expected single index for pointer\n");
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};  
    }

    Type* plain = type->contained_type;

    const u32 size = type_size(itl,plain);

    const auto [subscript_type,subscript_slot] = compile_oper(itl,func,index_node->indexes[0]);
    if(!is_integer(subscript_type))
    {
        panic(itl,itl_error::int_type_error,"[COMPILE]: expected integeral expr for array subscript got %s\n",type_name(itl,subscript_type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};  
    }

    const SymSlot offset = mul_imm_res(itl,func,subscript_slot,size);  

    add(itl,func,dst_slot,ptr_slot,offset);
    return std::pair{(Type*)type,dst_slot};
}

// indexes off a given type + ptr
std::pair<Type*,SymSlot> index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, SymSlot ptr_slot, SymSlot dst_slot)
{
    // standard array index
    if(!is_array(type))
    {
        panic(itl,itl_error::array_type_error,"[COMPILE]: '%s' is not an array got type %s\n",arr_name.buf,type_name(itl,type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};  
    }

    SymSlot last_slot = ptr_slot;

    ArrayType* array_type = (ArrayType*)type;

    Type* accessed_type = nullptr;

    const u32 indexes = count(index_node->indexes);

    for(u32 i = 0; i < indexes; i++)
    {

        const auto [subscript_type,subscript_slot] = compile_oper(itl,func,index_node->indexes[i]);
        if(!is_integer(subscript_type))
        {
            panic(itl,itl_error::int_type_error,"[COMPILE]: expected integeral expr for array subscript got %s\n",type_name(itl,subscript_type).buf);
            return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};  
        }
        
        const bool last_index = i == indexes - 1;
        

        // perform the indexing operation

        const u32 size = array_type->sub_size;

        const SymSlot mul_slot = mul_imm_res(itl,func,subscript_slot,size);   

        const SymSlot add_slot = last_index? dst_slot : new_tmp(func,GPR_SIZE);
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
                panic(itl,itl_error::out_of_bounds,"Out of bounds indexing for array %s (%d:%d)\n",arr_name.buf,i,indexes);
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};                          
            }
        } 
        
    }

    // return pointer to accessed type
    // NOTE: this can give out a fixed array pointer.
    // this needs conversion by the host into a VLA, this is not obtainable by
    // taking a pointer to an array,
    return std::pair{make_pointer(itl,accessed_type),dst_slot}; 
}

// TODO: these multidimensional handwave vla's

SymSlot load_arr_data(Interloper& itl,Function& func,SymSlot slot, const Type* type)
{
    if(is_runtime_size(type))
    {
        const auto addr_slot = make_struct_addr(slot,0);
        return load_struct_u64_res(itl,func,addr_slot);
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return slot;
    }
}

// NOTE: this has to be a vla
void store_arr_data(Interloper& itl, Function& func, SymSlot slot, SymSlot data)
{
    const auto addr_slot = make_struct_addr(slot,0);
    write_struct_u64(itl,func,data,addr_slot);
}

void store_arr_len(Interloper& itl, Function& func, SymSlot slot,SymSlot len)
{
    const auto addr_slot = make_struct_addr(slot,GPR_SIZE);
    write_struct_u64(itl,func,len,addr_slot);
}

SymSlot load_arr_len(Interloper& itl,Function& func,SymSlot slot, const Type* type)
{
    if(is_runtime_size(type))
    {
        const auto addr_slot = make_struct_addr(slot,GPR_SIZE);
        return load_struct_u64_res(itl,func,addr_slot);
    }

    ArrayType* array_type = (ArrayType*)type;

    return mov_imm_res(itl,func,array_type->size);   
}

SymSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_data(itl,func,sym.reg.slot,sym.type);
}

SymSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym)
{
    return load_arr_len(itl,func,sym.reg.slot,sym.type);
}

std::pair<Type*, SymSlot> index_arr(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    IndexNode* index_node = (IndexNode*)node;

    const auto arr_name = index_node->name;

    const auto arr_ptr = get_sym(itl.symbol_table,arr_name);

    if(!arr_ptr)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: array '%s' used before declaration\n",arr_name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};       
    }

    const auto arr = *arr_ptr;

    if(is_array(arr.type))
    {
        // get the initial data ptr
        const SymSlot data_slot = load_arr_data(itl,func,arr);

        return index_arr_internal(itl,func,index_node,arr_name,arr.type,data_slot,dst_slot);
    }

    else if(is_pointer(arr.type))
    {
        return index_pointer(itl,func,arr.reg.slot,dst_slot,index_node,(PointerType*)arr.type);
    }

    else
    {
        panic(itl,itl_error::array_type_error,"[COMPILE]: expected array or pointer for index got %s\n",type_name(itl,arr.type));
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};          
    }
}

Type* read_arr(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_tmp_ptr(func));

    type = deref_pointer(type);

    // fixed array needs conversion by host
    if(is_fixed_array(type))
    {
        mov_reg(itl,func,dst_slot,addr_slot);
        return type;
    }

    do_ptr_load(itl,func,dst_slot,addr_slot,type);
    return type;
}


// TODO: our type checking for our array assigns has to be done out here to ensure locals are type checked correctly
void write_arr(Interloper &itl,Function &func,AstNode *node,Type* write_type, SymSlot slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_tmp_ptr(func));


    // convert fixed size pointer..
    if(is_array(write_type))
    {
        unimplemented("array write");
    }

    else
    {
        // deref of pointer
        type = deref_pointer(type);

        do_ptr_store(itl,func,slot,addr_slot,type);

        check_assign(itl,type,write_type);
    }
}

void assign_vla_initializer(Interloper& itl, Function& func, RecordNode* list, AddrSlot* addr_slot, ArrayType* type)
{
    const u32 node_len = count(list->nodes);

    if(node_len != 2)
    {
        panic(itl,itl_error::missing_initializer,"vla initializer expects 2 initializers {POINTER,SIZE}");
        return;
    }

    const auto [ptr_type,ptr_slot] = compile_oper(itl,func,list->nodes[0]);
    check_assign(itl,make_pointer(itl,type->contained_type),ptr_type);

    store_addr_slot(itl,func,ptr_slot,*addr_slot,GPR_SIZE,false);
    addr_slot->offset += GPR_SIZE;

    const auto [size_type,size_slot] = compile_oper(itl,func,list->nodes[1]);
    check_assign(itl,make_builtin(itl,GPR_SIZE_TYPE),size_type);

    store_addr_slot(itl,func,size_slot,*addr_slot,GPR_SIZE,false);
    addr_slot->offset += GPR_SIZE;
}

void traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,AddrSlot* addr_slot, ArrayType* type)
{
    if(itl.error)
    {
        return;
    }

    const u32 node_len = count(list->nodes);

    // this just gets the first node size
    if(type->size == DEDUCE_SIZE)
    {
        type->size = node_len;
    }

    else if(is_runtime_size(type))
    {
        assign_vla_initializer(itl,func,list,addr_slot,type);
        return;
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
            panic(itl,itl_error::missing_initializer,"array %s expects %d initializers got %d\n",type_name(itl,(Type*)type).buf,count,node_len);
            return;
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
                    traverse_arr_initializer_internal(itl,func,(RecordNode*)node,addr_slot,next_arr);
                    break;
                }

                case ast_type::string:
                {
                    LiteralNode* literal_node = (LiteralNode*)node;
                    const String literal = literal_node->literal;        

                    if(!is_string(next_arr))
                    {
                        panic(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,(Type*)next_arr).buf);
                        return;
                    }


                    if(is_runtime_size(next_arr))
                    {
                        if(is_const_string(next_arr))
                        {
                            // we can set this up directly from the const pool
                            const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,literal);

                            const SymSlot arr_data = pool_addr_res(itl,func,pool_slot,0);
                            store_addr_slot(itl,func,arr_data,*addr_slot,GPR_SIZE,false);

                            addr_slot->offset += GPR_SIZE;

                            const SymSlot arr_size = mov_imm_res(itl,func,literal.size);
                            store_addr_slot(itl,func,arr_size,*addr_slot,GPR_SIZE,false);

                            addr_slot->offset += GPR_SIZE;
                        }

                        else
                        {
                            panic(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla\n");
                        }
                    }

                    // fixed sized array
                    else
                    {
                        panic(itl,itl_error::string_type_error,"cannot assign string literal to fixed sized array\n");
                        return;
                    }

                    break;
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
                    traverse_struct_initializer(itl,func,(RecordNode*)list->nodes[i],*addr_slot,structure);
                }

                // allready finished struct
                else
                {
                    auto [rtype,reg] = compile_oper(itl,func,list->nodes[i]);
                    check_assign_init(itl,base_type,rtype);

                    do_addr_store(itl,func,reg,*addr_slot,base_type);
                }

                addr_slot->offset += size;
            }
        }

        // normal types
        else
        {
            for(u32 i = 0; i < node_len; i++)
            {
                auto [rtype,reg] = compile_oper(itl,func,list->nodes[i]);
                check_assign_init(itl,base_type,rtype);

                do_addr_store(itl,func,reg,*addr_slot,base_type);
                addr_slot->offset += size;
            }
        }           
    }   
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
        switch(type->type_idx)
        {
            case POINTER:
            {
                size = GPR_SIZE;

                // whatever is pointed too is responsible for handling its own allocation
                // because it comes from somewhere else we are done!
                done = true;
                break;
            }

            case ARRAY:
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

void traverse_arr_initializer(Interloper& itl,Function& func,AstNode *node,AddrSlot addr_slot, Type* type)
{
    RecordNode* list = (RecordNode*)node;

    traverse_arr_initializer_internal(itl,func,list,&addr_slot,(ArrayType*)type);
}


void store_const_string(Interloper& itl, Function& func, const String& literal, const SymSlot arr_slot)
{
    // we can set this up directly from the const pool
    const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,literal);

    const SymSlot arr_data = pool_addr_res(itl,func,pool_slot,0);
    store_arr_data(itl,func,arr_slot,arr_data);

    const SymSlot arr_size = mov_imm_res(itl,func,literal.size);
    store_arr_len(itl,func,arr_slot,arr_size);
}

void compile_arr_assign(Interloper& itl, Function& func, AstNode* node, const SymSlot arr_slot, Type* type)
{
    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            // initialize the actual struct
            if(is_runtime_size(type))
            {
                auto addr_slot = make_struct_addr(arr_slot,0);
                assign_vla_initializer(itl,func,(RecordNode*)node,&addr_slot,(ArrayType*)type);
            }

            else 
            {
                const SymSlot ptr_slot = load_arr_data(itl,func,arr_slot,type);
                const auto addr_slot = make_addr(ptr_slot,0);
                traverse_arr_initializer(itl,func,node,addr_slot,type);
            }
            break;
        }

        case ast_type::string:
        {
            // TODO: we need to add different hanlding for const strings

            if(!is_string(type))
            {
                panic(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,type).buf);
                return;
            }

            ArrayType* array_type = (ArrayType*)type;

            LiteralNode* literal_node = (LiteralNode*)node;
            const String literal = literal_node->literal;

            if(is_runtime_size(array_type))
            {
                if(is_const_string(type))
                {
                    store_const_string(itl,func,literal,arr_slot);
                }

                else
                {
                    panic(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla\n");
                }
            }

            // fixed sized array
            else
            {
                panic(itl,itl_error::string_type_error,"cannot assign string literal to fixed sized array\n");
                return;
            }

            break;           
        }
    
        // arbitary expression
        default:
        {
            // compile expr
            auto [rtype,slot] = compile_oper(itl,func,node);
              
            check_assign_init(itl,type,rtype);
            
            if(itl.error)
            {
                return;
            }

            compile_move(itl,func,arr_slot,slot,type,rtype);

            break;
        }
    }
}

void default_construct_arr(Interloper& itl, Function& func,ArrayType* type, AddrSlot addr_slot)
{
    if(is_fixed_array(type))
    {
        // this has not been inited by traverse_arr_initializer
        if(type->size == DEDUCE_SIZE)
        {
            panic(itl,itl_error::missing_initializer,"auto sized array does not have an initializer\n");
            return;
        }

        if(is_array(type->contained_type))
        {
            ArrayType* next_type = (ArrayType*)type->contained_type;

            for(u32 i = 0; i < type->size; i++)
            {
                auto sub_addr = addr_slot;
                sub_addr.offset += (i * next_type->sub_size);

                default_construct_arr(itl,func,next_type,sub_addr);
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
                struct_addr.offset += structure.size;

                // TODO: just default construct it for now!
                compile_struct_decl_default(itl,func,structure,struct_addr);
            }
        }

        // final plain values
        else
        {
            collapse_struct_offset(itl,func,&addr_slot);
            ir_zero(itl,func,addr_slot.slot,type->size * type->sub_size);
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
}

void compile_arr_decl(Interloper& itl, Function& func, const DeclNode *decl_node, SymSlot slot)
{
    // This allocation needs to happen before we initialize the array but we dont have all the information yet
    // so we need to finish it up later
    ListNode* alloc = alloc_slot(itl,func,slot,true);

    // has an initalizer
    if(decl_node->expr)
    {
        auto& array = sym_from_slot(itl.symbol_table,slot);

        if(decl_node->expr->type != ast_type::no_init)
        {
            compile_arr_assign(itl,func,decl_node->expr,array.reg.slot,array.type);
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
            default_construct_arr(itl,func,array_type,addr_slot);
        }

        else
        {
            const auto addr_slot = make_addr(array.reg.slot,0);
            default_construct_arr(itl,func,array_type,addr_slot);
        }
    }

    if(itl.error)
    {
        return;
    }

    else
    {
        auto& array = sym_from_slot(itl.symbol_table,slot);

        const auto [arr_size,arr_count] = calc_arr_allocation(itl,array);

        // allocate fixed array if needed, and initalize it to its data pointer
        if(is_fixed_array(array.type))
        {
            // we have the allocation information now complete it
            switch(array.reg.kind)
            {
                case reg_kind::local:
                {
                    alloc->opcode = Opcode(op_type::alloc_local_array,array.reg.slot.handle,arr_size,arr_count);
                    break;
                }

                case reg_kind::tmp:
                {
                    alloc->opcode = Opcode(op_type::alloc_local_array,array.reg.slot.handle,arr_size,arr_count);
                    break;
                }

                // just dump addr
                case reg_kind::global:
                {
                    const u32 alloc_idx = allocate_global_array(itl.global_alloc,itl.symbol_table,array.reg.slot,arr_size,arr_count);
                    alloc->opcode = Opcode(op_type::alloc_global_array,array.reg.slot.handle,alloc_idx,0);
                    break;
                }

                // constants should not go through this function!
                case reg_kind::constant:
                {
                    assert(false);
                    break;
                }
            }
        }
    }
}
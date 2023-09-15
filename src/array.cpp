
// indexes off a given type + ptr
std::pair<Type*,SymSlot> index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, SymSlot ptr_slot, SymSlot dst_slot)
{

    if(!is_array(type))
    {
        panic(itl,itl_error::array_type_error,"[COMPILE]: '%s' is not an array got type %s\n",arr_name.buf,type_name(itl,type).buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};  
    }

    const u32 indexes = count(index_node->indexes);

    SymSlot last_slot = ptr_slot;

    
    ArrayType* array_type = (ArrayType*)type;

    Type* accessed_type = nullptr;

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

                load_ptr(itl,func,tmp,last_slot,0,GPR_SIZE,false);

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
        return load_struct_u64_res(itl,func,slot,0);
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
    write_struct_u64(itl,func,data,slot,0);
}

void store_arr_len(Interloper& itl, Function& func, SymSlot slot,SymSlot len)
{
    write_struct_u64(itl,func,len,slot,GPR_SIZE);
}

SymSlot load_arr_len(Interloper& itl,Function& func,SymSlot slot, const Type* type)
{
    if(is_runtime_size(type))
    {
        return load_struct_u64_res(itl,func,slot,GPR_SIZE);
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

    
    // get the initial data ptr
    const SymSlot data_slot = load_arr_data(itl,func,arr);

    return index_arr_internal(itl,func,index_node,arr_name,arr.type,data_slot,dst_slot);
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

void copy_fixed_size_string(Interloper& itl, Function& func,SymSlot addr_slot, ArrayType *array_type, const String& literal)
{
    if(array_type->size < literal.size)
    {
        panic(itl,itl_error::out_of_bounds,"expected array of atleast size %d got %d\n",literal.size,array_type->size);
        return;
    }

    // copy into array
    const auto base_type = array_type->contained_type;
    const auto rtype = make_builtin(itl,builtin_type::c8_t);

    for(u32 i = 0; i < literal.size; i++)
    {
        const SymSlot slot = mov_imm_res(itl,func,literal[i]);
        check_assign_init(itl,base_type,rtype);

        do_ptr_store(itl,func,slot,addr_slot,rtype,i);
    }    
}

void traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,const SymSlot addr_slot, ArrayType* type, u32* offset)
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

    else if(type->size == RUNTIME_SIZE)
    {
        panic(itl,itl_error::array_type_error,"cannot assign initalizer to vla\n");
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
                    traverse_arr_initializer_internal(itl,func,(RecordNode*)node,addr_slot,next_arr,offset);
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

                            const SymSlot arr_data = pool_addr_res(itl,func,pool_slot);
                            store_ptr(itl,func,arr_data,addr_slot,0 + *offset,GPR_SIZE);

                            const SymSlot arr_size = mov_imm_res(itl,func,literal.size);
                            store_ptr(itl,func,arr_size,addr_slot,GPR_SIZE + *offset,GPR_SIZE);

                            *offset += VLA_SIZE;
                        }

                        else
                        {
                            panic(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla\n");
                        }
                    }

                    // fixed sized array
                    else
                    {
                        copy_fixed_size_string(itl,func,addr_slot,next_arr,literal);
                        *offset += next_arr->size * sizeof(char);
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
                    traverse_struct_initializer(itl,func,(RecordNode*)list->nodes[i],addr_slot,structure,*offset);
                }

                // allready finished struct
                else
                {
                    auto [rtype,reg] = compile_oper(itl,func,list->nodes[i]);
                    check_assign_init(itl,base_type,rtype);

                    do_ptr_store(itl,func,reg,addr_slot,base_type,*offset);
                }

                *offset = *offset + size;
            }
        }

        // normal types
        else
        {
            for(u32 i = 0; i < node_len; i++)
            {
                auto [rtype,reg] = compile_oper(itl,func,list->nodes[i]);
                check_assign_init(itl,base_type,rtype);

                do_ptr_store(itl,func,reg,addr_slot,base_type,*offset);
                *offset = *offset + size;
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

void traverse_arr_initializer(Interloper& itl,Function& func,AstNode *node,const SymSlot addr_slot, Type* type)
{
    RecordNode* list = (RecordNode*)node;

    u32 idx = 0;
    traverse_arr_initializer_internal(itl,func,list,addr_slot,(ArrayType*)type,&idx);
}


void compile_arr_assign(Interloper& itl, Function& func, AstNode* node, const SymSlot arr_slot, Type* type)
{
    switch(node->type)
    {
        case ast_type::initializer_list:
        {
            const SymSlot addr_slot = load_arr_data(itl,func,arr_slot,type);
            traverse_arr_initializer(itl,func,node,addr_slot,type);
            break;
        }

        case ast_type::string:
        {
            LiteralNode* literal_node = (LiteralNode*)node;
            const String literal = literal_node->literal;

            // TODO: we need to add different hanlding for const strings

            if(!is_string(type))
            {
                panic(itl,itl_error::string_type_error,"expected string got %s\n",type_name(itl,type).buf);
                return;
            }


            ArrayType* array_type = (ArrayType*)type;


            // handle auto sizing
            if(array_type->size == DEDUCE_SIZE)
            {
                array_type->size = literal.size;
            }


            if(array_type->size == RUNTIME_SIZE)
            {
                if(is_const_string(type))
                {
                    // we can set this up directly from the const pool
                    const PoolSlot pool_slot = push_const_pool_string(itl.const_pool,literal);

                    const SymSlot arr_data = pool_addr_res(itl,func,pool_slot);
                    store_arr_data(itl,func,arr_slot,arr_data);

                    const SymSlot arr_size = mov_imm_res(itl,func,literal.size);
                    store_arr_len(itl,func,arr_slot,arr_size);
                }

                else
                {
                    panic(itl,itl_error::const_type_error,"cannot assign string literal to mutable vla\n");
                }
            }

            // fixed sized array
            else
            {
                const SymSlot addr_slot = load_arr_data(itl,func,arr_slot,type);
                copy_fixed_size_string(itl,func,addr_slot,array_type,literal);
            }

            break;           
        }
    
        // arbitary expression
        default:
        {
            // compile expr
            auto [rtype,slot] = compile_oper(itl,func,node);
            
            if(is_array(rtype))
            {
                compile_move(itl,func,arr_slot,slot,type,rtype);
                check_assign_init(itl,type,rtype);
            }

            break;
        }
    }
}

void compile_arr_decl(Interloper& itl, Function& func, const DeclNode *decl_node, Symbol& array)
{
    // This allocation needs to happen before we initialize the array but we dont have all the information yet
    // so we need to finish it up later
    alloc_slot(itl,func,array.reg,true);
    ListNode* alloc = get_cur_end(func.emitter);


    // has an initalizer
    // TODO: need to handle dumping this as a constant if the array is constant
    // rather than runtime setup
    if(decl_node->expr)
    {
        compile_arr_assign(itl,func,decl_node->expr,array.reg.slot,array.type);
    }

    else 
    {
        // default construct
        if(!is_fixed_array(array.type))
        {
            const auto addr = addrof_res(itl,func,array.reg.slot);
            const auto zero = mov_imm_res(itl,func,0);

            store_ptr(itl,func,zero,addr,0,GPR_SIZE);
            store_ptr(itl,func,zero,addr,GPR_SIZE,GPR_SIZE);
        }

    }

    if(itl.error)
    {
        return;
    }

    if(array.reg.count == RUNTIME_SIZE)
    {
        unimplemented("DECL VLA");
    }

    // this has not been inited by traverse_arr_initializer
    else if(array.reg.count == DEDUCE_SIZE)
    {
        panic(itl,itl_error::missing_initializer,"auto sized array %s does not have an initializer\n",array.name.buf);
        return;
    }

    else
    {
        const auto [arr_size,arr_count] = calc_arr_allocation(itl,array);

        if(is_fixed_array(array.type))
        {
            // we have the allocation information now complete it
            switch(array.reg.kind)
            {
                case reg_kind::local:
                {
                    alloc->opcode = Opcode(op_type::alloc_fixed_array,array.reg.slot.handle,arr_size,arr_count);
                    break;
                }

                case reg_kind::tmp:
                {
                    alloc->opcode = Opcode(op_type::alloc_fixed_array,array.reg.slot.handle,arr_size,arr_count);
                    break;
                }

                case reg_kind::global:
                {
                    assert(false);
                    break;
                }

                case reg_kind::constant:
                {
                    assert(false);
                    break;
                }
            }
        }

        else
        {

        }
    }
}
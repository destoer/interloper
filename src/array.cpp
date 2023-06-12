
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

        /*
        const u32 count = type.dimensions[i];
        // this just works for single dimension VLA
        
        if(count == RUNTIME_SIZE)
        {
            unimplemented("index VLA");
        }
        else
        */
        {
            const u32 size = array_type->sub_size;

            const SymSlot mul_slot = mul_imm_res(itl,func,subscript_slot,size);   

            const bool last_index = i == indexes - 1;

            const SymSlot add_slot = last_index? dst_slot : new_tmp(func,GPR_SIZE);
            add(itl,func,add_slot,last_slot,mul_slot);

            last_slot = add_slot;


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
        const SymSlot addr = addrof_res(itl,func,slot);

        const SymSlot dst_slot = new_tmp(func,GPR_SIZE);
        load_ptr(itl,func,dst_slot,addr,0,GPR_SIZE,false);

        return dst_slot;
    }

    // fixed size, array ptr is stored in its own slot!
    else
    {
        return slot;
    }
}

SymSlot load_arr_len(Interloper& itl,Function& func,SymSlot slot, const Type* type)
{
    UNUSED(slot);

    if(is_runtime_size(type))
    {
        const SymSlot addr = addrof_res(itl,func,slot);

        const SymSlot dst_slot = new_tmp(func,GPR_SIZE);
        load_ptr(itl,func,dst_slot,addr,GPR_SIZE,GPR_SIZE,false);

        return dst_slot;
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

    const auto arr_opt = get_sym(itl.symbol_table,arr_name);

    if(!arr_opt)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: array '%s' used before declaration\n",arr_name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};       
    }

    const auto arr = arr_opt.value();

    
    // get the initial data ptr
    const SymSlot data_slot = load_arr_data(itl,func,arr);

    return index_arr_internal(itl,func,index_node,arr_name,arr.type,data_slot,dst_slot);
}

std::pair<Type*,SymSlot> read_arr(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_tmp_ptr(func));

    type = deref_pointer(type);

    // fixed array needs conversion by host
    if(is_fixed_array(type))
    {
        return std::pair{type,addr_slot};
    }

    do_ptr_load(itl,func,dst_slot,addr_slot,type);
    return std::pair{type,dst_slot};
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
        unimplemented("VLA assign");
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
            // descend each sub initializer until we hit one containing values
            // for now we are just gonna print them out, and then we will figure out how to emit the inialzation code
            if(list->nodes[n]->type == ast_type::initializer_list)
            {
                traverse_arr_initializer_internal(itl,func,(RecordNode*)list->nodes[n],addr_slot,next_arr,offset);
            }

            // handle an array (this should fufill the current "depth req in its entirety")
            else
            {
                unimplemented("arr initializer with array");            
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
                    // TODO: make sure that the allocation information does not go into alloc slot
                    unimplemented("arr alloc size");
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
    // just a straight assign
    if(node->type != ast_type::initializer_list)
    {
        if(node->type == ast_type::string)
        {      

            LiteralNode* literal_node = (LiteralNode*)node;
            const String literal = literal_node->literal;

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
                unimplemented("string vla");
            }

            if(array_type->size < literal.size)
            {
                panic(itl,itl_error::out_of_bounds,"expected array of atleast size %d got %d\n",literal.size,array_type->size);
            }

            const auto base_type = array_type->contained_type;
            const auto rtype = make_builtin(itl,builtin_type::c8_t);

            for(u32 i = 0; i < literal.size; i++)
            {
                const SymSlot slot = mov_imm_res(itl,func,literal[i]);
                check_assign_init(itl,base_type,rtype);

                do_ptr_store(itl,func,slot,addr_slot,rtype,i);
            }           
        }

        else
        {
            unimplemented("single intializer");
        }

        return;
    }

    RecordNode* list = (RecordNode*)node;

    u32 idx = 0;
    traverse_arr_initializer_internal(itl,func,list,addr_slot,(ArrayType*)type,&idx);
}


void compile_arr_decl(Interloper& itl, Function& func, const DeclNode *decl_node, Symbol& array)
{
    // This allocation needs to happen before we initialize the array but we dont have all the information yet
    // so we need to finish it up later
    alloc_slot(itl,func,array.reg,false);
    ListNode* alloc = get_cur_end(func.emitter);


    // has an initalizer
    // TODO: need to handle dumping this as a constant if the array is constant
    // rather than runtime setup
    if(decl_node->expr)
    {
        const SymSlot addr_slot = load_arr_data(itl,func,array);
        traverse_arr_initializer(itl,func,decl_node->expr,addr_slot,array.type);
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

        // we have the allocation information now complete it
        alloc->opcode = Opcode(op_type::alloc_fixed_array,array.reg.slot.handle,arr_size,arr_count);
    }
}
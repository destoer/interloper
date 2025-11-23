
b32 is_constant(const Symbol& sym)
{
    return sym.reg.segment == reg_segment::constant;
}

// NOTE: type checking rules are less strict than in "runtime" expressions
Option<itl_error> check_const_cmp(Interloper& itl, Type* ltype, Type* rtype)
{
    b32 valid = (is_integer(ltype) && is_integer(rtype)) || (is_bool(ltype) && is_bool(rtype));
    
    if(!valid)
    {
        return compile_error(itl,itl_error::const_type_error,"Could not compare types: %s : %s",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }

    return option::none;
}

void clip_const_type(ConstData& data, u32 size)
{
    switch(size)
    {
        case 1: data.v &= 0xff; break;
        case 2: data.v &= 0xffff; break;
        case 4: data.v &= 0xffff'ffff; break;
        case 8: break;

        default: assert(false); break;
    }    
}

Option<itl_error> handle_const_cast(Interloper& itl, Type* new_type, ConstData& data)
{
    const Type* old_type = data.type;

    // assign the new type
    data.type = new_type; 

    // handle side effects of the cast
    // builtin type
    if(is_plain_builtin(old_type) && is_plain_builtin(new_type))
    {
        const auto builtin_old = cast_builtin(old_type);
        const auto builtin_new = cast_builtin(new_type);

        // integer
        // any cast is fine, just make sure to clip if the new type has smaller storage
        if(is_integer(old_type) && is_integer(new_type))
        {
            // larger type -> smaller type
            // truncate value (mask)
            if(builtin_size(builtin_old) > builtin_size(builtin_new))
            {
                clip_const_type(data,builtin_size(builtin_new));
            }

            return option::none;
        }

        // bool to integer
        // no conversion needd
        else if(builtin_old == builtin_type::bool_t && is_integer(new_type))
        {
            return option::none;
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(old_type) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(old_type))
            {
                data.v = s64(data.v) > 0;
            }

            // unsigned
            else
            {
                data.v = u64(data.v) > 0;
            }

            return option::none;
        }        

        else
        {
            return compile_error(itl,itl_error::illegal_cast,"cannot cast %s -> %s",type_name(itl,old_type).buf,type_name(itl,new_type).buf);
        }
    }

    // cast from enum to int is fine
    else if(is_enum(old_type) && is_integer(new_type))
    {
        return option::none;
    }

    // as is integer to enum
    else if(is_integer(old_type) && is_enum(new_type))
    {
        return option::none;
    }

    // cast does nothing just move the reg, its only acknowledgement your doing something screwy
    // NOTE: we will check size accesses on reads to ensure nothing goes wrong doing this
    else if(is_pointer(old_type) && is_pointer(new_type))
    {
        return option::none;
    }

    // pointer to int is illegal in const expr
    else if(is_pointer(old_type) && is_integer(new_type))
    {
        return compile_error(itl,itl_error::illegal_cast,"pointer to int cast is illegal in constant expr %s -> %s",
            type_name(itl,old_type).buf,type_name(itl,new_type).buf);
    }

    // as is integer to pointer
    else if(is_integer(old_type) && is_pointer(new_type))
    {
        return compile_error(itl,itl_error::illegal_cast,"int to pointer cast is illegal in constant expr %s -> %s",
            type_name(itl,old_type).buf,type_name(itl,new_type).buf);
    }

    // fuck knows
    else
    {
        return compile_error(itl,itl_error::illegal_cast,"cannot cast %s -> %s",type_name(itl,old_type).buf,type_name(itl,new_type).buf);     
    }   
}

ConstDataResult compile_const_expression(Interloper& itl, AstNode* node);

Result<std::pair<ConstData,ConstData>,itl_error> const_bin_op(Interloper& itl, BinNode* bin_node)
{
    const auto left_res = compile_const_expression(itl,bin_node->left);
    const auto right_res = compile_const_expression(itl,bin_node->right);
    if(!left_res)
    {
        return left_res.error();
    }

    if(!right_res)
    {
        return left_res.error();
    }
    
    return std::pair{*left_res,*right_res};
}

template<typename FUNC>
ConstDataResult compile_const_bin_op(Interloper& itl, BinNode* bin_node, arith_bin_op arith, FUNC func)
{
    const auto left_res = compile_const_expression(itl,bin_node->left);
    const auto right_res = compile_const_expression(itl,bin_node->right);
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }
    
    const auto left = *left_res;
    const auto right = *right_res;

    const u64 ans = func(left,right);
    auto type_res = effective_arith_type(itl,left.type,right.type,arith);
    if(!type_res)
    {
        return type_res.error();
    }

    return make_const_builtin(ans,*type_res);  
}

template<typename FUNC>
ConstDataResult compile_const_shift_op(Interloper& itl, BinNode* bin_node, FUNC func)
{
    const auto left_res = compile_const_expression(itl,bin_node->left);
    const auto right_res = compile_const_expression(itl,bin_node->right);
    if(!left_res)
    {
        return left_res;
    }

    if(!right_res)
    {
        return right_res;
    }
    
    const auto left = *left_res;
    const auto right = *right_res;

    const u64 ans = func(left,right);

    return make_const_builtin(ans,left.type);  
}

ConstDataResult compile_const_expression(Interloper& itl, AstNode* node)
{
    switch(node->type)
    {
        default:
        {
            return compile_error(itl,itl_error::const_type_error,"unrecognised operation for const expr: %s",AST_INFO[u32(node->type)].name);
        }
    }    
}

ConstValueResult compile_const_int_expression(Interloper& itl, AstNode* node)
{
    const auto data_res = compile_const_expression(itl,node);
    if(!data_res)
    {
        return data_res.error();
    }

    auto data = *data_res;

    // not valid if this is not an int
    if(!is_integer(data.type))
    {
        return compile_error(itl,itl_error::int_type_error,"expected integer for const int expr got %s",type_name(itl,data.type).buf); 
    }

    return ConstValue{data.type,data.v};
}


Result<bool,itl_error> compile_const_bool_expression(Interloper& itl, AstNode* node)
{
    const auto data_res = compile_const_expression(itl,node);
    if(!data_res)
    {
        return data_res.error();
    }

    auto data = *data_res;

    // not valid if this is not an int
    if(!is_bool(data.type))
    {
        return compile_error(itl,itl_error::int_type_error,"expected bool for const bool expr got %s",type_name(itl,data.type).buf);
    }

    return bool(data.v);
}

Option<itl_error> compile_const_struct_list_internal(Interloper& itl,RecordNode* list, const Struct& structure, PoolSlot slot, u32 offset);


Result<u32,itl_error> const_write_in_string(Interloper& itl, LiteralNode* literal_node, Type* type, PoolSlot slot, u32 offset)
{
    if(!is_string(type))
    {
        return compile_error(itl,itl_error::string_type_error,"expected string got %s",type_name(itl,(Type*)type).buf);
    }

    ArrayType* array_type = (ArrayType*)type;

    const String literal = literal_node->literal;

    if(is_runtime_size(array_type))
    {
        // add string into const pool
        const auto data_slot = push_const_pool_string(itl.const_pool,literal);

        // make sure we pull the section after pushing data
        auto& section = pool_section_from_slot(itl.const_pool,slot);

        // write in vla data
        write_const_pool_vla(itl.const_pool,section,offset,data_slot,literal.size);
        offset += VLA_SIZE;
    }

    // fixed size
    else
    {
        return compile_error(itl,itl_error::string_type_error,"cannot assign string literal to fixed sized array");         
    }

    return offset;    
}

Option<itl_error> compile_const_arr_list_internal(Interloper& itl,RecordNode* list, ArrayType* type, PoolSlot slot, u32* offset)
{
    const u32 node_len = count(list->nodes);

    if(is_runtime_size(type))
    {
        return compile_error(itl,itl_error::array_type_error,"cannot assign initializer to vla");
    }

    // next type is a sub array
    if(is_array(type->contained_type))
    {
        ArrayType* next_arr = (ArrayType*)type->contained_type;

        const u32 count = type->size;

        // type check the actual amount is right
        // TODO: this should allow not specifying the full amount but for now just keep it simple
        if(count != node_len)
        {
            return compile_error(itl,itl_error::missing_initializer,"array %s expects %d initializers got %d",
                type_name(itl,(Type*)type).buf,count,node_len);
        }        

        for(u32 n = 0; n < node_len; n++)
        {
            AstNode* node = list->nodes[n];

            // descend each sub initializer until we hit one containing values
            // for now we are just gonna print them out, and then we will figure out how to emit the initialization code
            switch(node->type)
            {
                // handle an array (this should fulfill the current "depth req in its entirety")
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

                    const auto struct_err = compile_const_struct_list_internal(itl,(RecordNode*)list->nodes[i],structure,slot,*offset);
                    if(struct_err)
                    {
                        return struct_err;
                    }
                }

                // allready finished struct
                else
                {
                    auto data_res = compile_const_expression(itl,list->nodes[i]);
                    if(!data_res)
                    {
                        return data_res.error();
                    }

                    auto data = *data_res;

                    const auto assign_err = check_assign_init(itl,base_type,data.type);
                    if(assign_err)
                    {
                        return *assign_err;
                    }

                    data.type = base_type;

                    const auto write_err = write_const_data(itl,slot,*offset,data);
                    if(write_err)
                    {
                        return *write_err;
                    }
                }

                *offset = *offset + size;
            }
        }

        // normal types
        else
        {
            for(u32 i = 0; i < node_len; i++)
            {
                auto data_res = compile_const_expression(itl,list->nodes[i]);
                if(!data_res)
                {
                    return data_res.error();
                }

                auto data = *data_res;


                const auto assign_err = check_assign_init(itl,base_type,data.type);
                if(assign_err)
                {
                    return *assign_err;
                }
                data.type = base_type;

                const auto write_err = write_const_data(itl,slot,*offset,data);
                if(write_err)
                {
                    return *write_err;
                }
                *offset = *offset + size;
            }
        }           
    }

    return option::none; 
}

PoolSlot add_const_vla(Interloper& itl, Symbol& array, PoolSlot data_slot,u64 len)
{
    const auto vla_slot = push_const_pool_vla(itl.const_pool,data_slot,len);

    array.reg.offset = vla_slot.handle;

    return vla_slot;
}

// returns data slot
PoolSlot add_const_fixed_array(Interloper& itl, Symbol& array)
{
    // okay now reinit subsizes now we know the complete type
    init_arr_sub_sizes(itl,(Type*)array.type);
    
    // preallocate the arr data
    const u32 data_size = type_memory_size(itl,array.type);

    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    const auto pointer_slot = push_const_pool_fixed_array(itl.const_pool,data_slot);

    // mark as location
    array.reg.offset = pointer_slot.handle;

    return data_slot;    
}

Option<itl_error> compile_const_arr_initializer_list(Interloper& itl, Symbol& array, AstNode* node)
{
    if(is_runtime_size(array.type))
    {
        return compile_error(itl,itl_error::array_type_error,"Cannot use initializer list with vla");
    }

    // scan each part of initializer quickly
    // and determine the size, so we can figure out how
    // much we need to alloc into the pool 
    // NOTE: we still check that the sizes are the same later
    RecordNode* record_node = (RecordNode*)node;

    auto record_cur = record_node;
    ArrayType* type_cur = (ArrayType*)array.type;

    b32 done = false;

    while(!done)
    {
        const u32 size = count(record_cur->nodes);

        if(type_cur->size == DEDUCE_SIZE)
        {
            type_cur->size = size;
        }

        // can we descend any more?
        const b32 valid = is_array(type_cur->contained_type) && (size != 0) && (record_cur->nodes[0]->type == ast_type::initializer_list);

        done = !valid;

        if(valid)
        {
            type_cur = (ArrayType*)type_cur->contained_type;
            record_cur = (RecordNode*)record_cur->nodes[0];
        }
    }

    // prealloc data inside the const pool for this
    const auto data_slot = add_const_fixed_array(itl,array);

    // actually insert the data into the const pool for the list
    u32 offset = 0;
    return compile_const_arr_list_internal(itl,record_node,(ArrayType*)array.type,data_slot,&offset);
}

Option<itl_error> compile_const_struct_list_internal(Interloper& itl,RecordNode* list, const Struct& structure, PoolSlot slot, u32 offset)
{
    const u32 node_len = count(list->nodes);
    const u32 member_size = count(structure.members);

    if(node_len != member_size)
    {
        return compile_error(itl,itl_error::undeclared,"struct initializer missing initializer expected %d got %d",member_size,node_len);
    }
    
    for(u32 i = 0; i < count(structure.members); i++)
    {
        const auto member = structure.members[i];
    
        // either sub struct OR array member initializer
        switch(list->nodes[i]->type)
        {
            assert(false);
        }
    }

    return option::none;     
}

Option<itl_error> compile_const_struct_initializer_list(Interloper& itl, Symbol& structure, AstNode* node)
{
    RecordNode* record_node = (RecordNode*)node;

    // allocate struct
    const u32 data_size = type_size(itl,structure.type);
    const auto data_slot = reserve_const_pool_section(itl.const_pool,pool_type::var,data_size);

    // assign offset
    structure.reg.offset = data_slot.handle;

    const auto& struct_info = struct_from_type(itl.struct_table,structure.type);

    // do init
    return compile_const_struct_list_internal(itl,record_node,struct_info,data_slot,0);
}


Option<itl_error> compile_constant_initializer(Interloper& itl, Symbol& sym, AstNode* node)
{
    switch(sym.type->kind)
    {
        case type_class::builtin_t:
        {
            const builtin_type type = cast_builtin(sym.type);
            
            switch(type)
            {
                // integers
                case builtin_type::u8_t:
                case builtin_type::u16_t:
                case builtin_type::u32_t:
                case builtin_type::u64_t:
                case builtin_type::s8_t:
                case builtin_type::s16_t:
                case builtin_type::s32_t:
                case builtin_type::s64_t:
                case builtin_type::byte_t:
                case builtin_type::c8_t:
                {
                    // compile expression
                    auto res = compile_const_int_expression(itl,node);
                    if(!res)
                    {
                        return res.error();
                    }

                    auto const_value = *res;

                    // now we know the value we can get an exact type out of the result
                    Value value;
                    value.v = const_value.value;
                    value.sign = is_signed(const_value.type);
                    
                    const_value.type = value_type(itl,value);

                    // push to const pool and save handle as offset for later loading...
                    const auto slot = push_const_pool(itl.const_pool,pool_type::var,&value.v,builtin_size(type));
                    sym.reg.offset = slot.handle;
                    
                    sym.known_value = true;
                    sym.constant_value = value.v;

                    return check_assign_init(itl,sym.type,const_value.type);
                }


                case builtin_type::bool_t:
                {
                    const auto res = compile_const_bool_expression(itl,node);
                    if(!res)
                    {
                        return res.error();
                    }


                    const auto slot = push_const_pool(itl.const_pool,pool_type::var,&res.value(),GPR_SIZE);
                    sym.reg.offset = slot.handle;

                    sym.known_value = true;
                    sym.constant_value = *res;

                    return option::none;                
                }

                case builtin_type::f64_t:
                {
                    assert(false);
                    break;
                }

                // these should not be possible...
                case builtin_type::null_t:
                {
                    return compile_error(itl,itl_error::undefined_type_oper,"null as dst type in constant expression!?");
                }

                case builtin_type::void_t:
                {
                    return compile_error(itl,itl_error::undefined_type_oper,"void as dst type in constant expression!?");
                } 
            }

            break;   
        }

        case type_class::array_t:
        {
            // initializer
            switch(node->type)
            {
                default:
                {
                    unimplemented("arbitrary assign const array");
                    return option::none;
                }
            }
            break;
        }

        case type_class::struct_t:
        {
            switch(node->type)
            {
                default:
                {
                    unimplemented("arbitrary assign const struct");
                    return option::none;
                }            
            }

            break;
        }

        default: assert(false);
    }

    assert(false);
    return option::none;
}


Option<itl_error> compile_constant_decl(Interloper& itl, DeclNode* decl_node, b32 global)
{
    // pull type and name so we can create a symbol
    const auto name = decl_node->name;

    if(symbol_exists(itl.symbol_table,name))
    {
        return compile_error(itl,itl_error::redeclaration,"constant symbol %s redefined",name.buf);
    }

    // force constant
    decl_node->type->is_constant = true;

    // build the typing info
    auto type_res = get_type(itl,decl_node->type);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;

    // add into table
    auto& sym = global? add_global(itl,name,type,true) : add_symbol(itl,name,type);

    // make sure this is marked as constant
    // incase it is declared locally
    sym.reg.segment = reg_segment::constant;

    // compile the expression
    return compile_constant_initializer(itl,sym,decl_node->expr);    
}

Option<itl_error> compile_constant(Interloper& itl, GlobalDeclNode* node)
{
    auto context_guard = switch_context(itl,node->filename,node->name_space,(AstNode*)node);
    return compile_constant_decl(itl,node->decl,true);
}

void add_compiler_constant(Interloper& itl, const String& name, builtin_type builtin, u64 value)
{
    auto type = make_builtin(itl,builtin);

    auto& sym = add_global(itl,name,type,true);

    // push to const pool and save handle as offset for later loading...
    const auto slot = push_const_pool(itl.const_pool,pool_type::var,&value,builtin_size(builtin));
    sym.reg.offset = slot.handle;    
}


void declare_compiler_constants(Interloper& itl)
{
    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            add_compiler_constant(itl,"LITTLE_ENDIAN",builtin_type::bool_t,true);
            break;
        }
    }
}

Option<itl_error> compile_constants(Interloper& itl)
{
    for(u32 c = 0; c < count(itl.constant_decl); c++)
    {
        const auto const_err = compile_constant(itl,itl.constant_decl[c]);
        if(const_err)
        {
            return const_err;
        }
    }

    return option::none;
}
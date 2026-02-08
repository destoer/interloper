Option<itl_error> check_assign_no_const(Interloper& itl,const Type *ltype, const Type *rtype);

builtin_type value_type(const Value& value)
{
    if(value.sign)
    {
        const s64 v = s64(value.v);

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,s64(builtin_min(builtin_type::s8_t)),s64(builtin_max(builtin_type::s8_t))))
        {
            return builtin_type::s8_t;
        }

        else if(in_range(v,s64(builtin_min(builtin_type::s16_t)),s64(builtin_max(builtin_type::s16_t))))
        {
            return builtin_type::s16_t;
        }

        else if(in_range(v,s64(builtin_min(builtin_type::s32_t)),s64(builtin_max(builtin_type::s32_t))))
        {
            return builtin_type::s32_t;
        }

        else
        {
            return builtin_type::s64_t;
        }
    }

    else
    {
        const u64 v = value.v;

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,builtin_min(builtin_type::u8_t),builtin_max(builtin_type::u8_t)))
        {
            return builtin_type::u8_t;
        }

        else if(in_range(v,builtin_min(builtin_type::u16_t),builtin_max(builtin_type::u16_t)))
        {
            return builtin_type::u16_t;
        }

        else if(in_range(v,builtin_min(builtin_type::u32_t),builtin_max(builtin_type::u32_t)))
        {
            return builtin_type::u32_t;
        }

        else
        {
            return builtin_type::u64_t;
        }        
    }    
}


TypeResult effective_arith_type(Interloper& itl,Type *ltype, Type *rtype, arith_bin_type arith)
{
    // builtin type
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        // both integers
        if(is_integer(rtype) && is_integer(ltype))
        {
            const auto builtin_r = cast_builtin(rtype);
            const auto builtin_l = cast_builtin(ltype);

            // return the larger size of the type (promotion)
            return (builtin_size(builtin_l) > builtin_size(builtin_r))? ltype : rtype; 
        }

        // both floats, just a float
        if(is_float(rtype) && is_float(ltype))
        {
            return make_builtin(itl,builtin_type::f64_t);
        }

        if(is_bool(rtype) && is_bool(ltype))
        {
            return make_builtin(itl,builtin_type::bool_t);
        }

        // something else
        else
        {
            return compile_error(itl,itl_error::undefined_type_oper,"arithmetic operation undefined for %t and %t",ltype,rtype);
        }
    }

    // pointer arithmetic is fine
    else if(is_pointer(ltype) && is_integer(rtype))
    {
        if(arith != arith_bin_type::sub_t && arith != arith_bin_type::add_t)
        {
            return compile_error(itl,itl_error::undefined_type_oper,"Pointer arithmetic is only defined for addition and subtraction");     
        }

        return ltype;
    }

    else if(is_pointer(ltype) && is_pointer(rtype) && arith == arith_bin_type::sub_t)
    {
        return make_builtin(itl,GPR_SIZE_TYPE);
    }

    // one or more user defined
    else
    {
        return compile_error(itl,itl_error::undefined_type_oper,"arithmetic operation undefined for %t and %t",ltype,rtype);   
    }
}

TypeResult check_comparison_operation(Interloper& itl,const Type *ltype, const Type *rtype, comparison_op type)
{
    // both are builtin
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        // both integers 
        if(is_integer(rtype) && is_integer(ltype))
        {
            if(is_signed(rtype) != is_signed(ltype))
            {
                return compile_error(itl,itl_error::int_type_error,"logical comparision on different signs %t and %t",ltype,rtype);
            }
        }

        // both float or bool
        else if(!(is_float(rtype) && is_float(ltype)) || (is_bool(ltype) && is_bool(rtype)))
        {
            return compile_error(itl,itl_error::undefined_type_oper,"logical operation undefined for %t and %t",ltype,rtype);
        }
    }

    else if(is_pointer(ltype) && is_pointer(rtype))
    {
        const auto ptr_err = type_check_pointer(itl,ltype,rtype,assign_type::none);
        if(ptr_err)
        {
            return *ptr_err;
        }
    }

    else if(is_enum(ltype) && is_enum(rtype))
    {
        if(type != comparison_op::eq && type != comparison_op::ne)
        {
            return compile_error(itl,itl_error::enum_type_error,"comparision on enums is only defined for '==' and '!='");
        }

        const EnumType* enum_ltype = (EnumType*)ltype;
        const EnumType* enum_rtype = (EnumType*)rtype;

        if(enum_ltype->enum_idx != enum_rtype->enum_idx)
        {
            return compile_error(itl,itl_error::enum_type_error,"expected enum of the same type for comparisons %t : %t",ltype,rtype);
        }
    }

    else if(is_struct(ltype) && is_struct(rtype))
    {
        if(type != comparison_op::eq && type != comparison_op::ne)
        {
            return compile_error(itl,itl_error::struct_error,"comparision on structs is only defined for '==' and '!='");
        }

        const StructType* struct_ltype = (StructType*)ltype;
        const StructType* struct_rtype = (StructType*)rtype;
        
        if(struct_ltype->struct_idx != struct_rtype->struct_idx)
        {
            return compile_error(itl,itl_error::struct_error,"expected Struct of the same type for comparisons %t : %t",ltype,rtype);
        }
    }

    else if(is_array(ltype) && is_array(rtype))
    {
        if(type != comparison_op::eq && type != comparison_op::ne)
        {
            return compile_error(itl,itl_error::array_type_error,"comparision on array is only defined for '==' and '!='");
        }

        const ArrayType* array_ltype = (ArrayType*)ltype;
        const ArrayType* array_rtype = (ArrayType*)rtype;
        
        if(is_array(array_ltype->contained_type) || is_array(array_ltype->contained_type))
        {
            return compile_error(itl,itl_error::array_type_error,"Array comparison must be 1d %t == %t",ltype,rtype);
        }

        if(is_fixed_array(array_ltype) && is_fixed_array(array_rtype) && array_ltype->size != array_rtype->size)
        {
            return compile_error(itl,itl_error::array_type_error,"Array comparison on fixed array of differing sizes %t == %t",ltype,rtype);
        }
        
        const auto err = check_assign_no_const(itl,array_ltype->contained_type,array_rtype->contained_type);
        if(err)
        {
            return *err;
        }
    }


    // no matching operator
    else 
    {
        return compile_error(itl,itl_error::undefined_type_oper,"logical operation on user defined type: %t : %t",ltype,rtype);
    }   

    return make_builtin(itl,builtin_type::bool_t);
}


Option<itl_error> check_const_internal(Interloper&itl, const Type* ltype, const Type* rtype, assign_type type, b32 was_reference)
{

    // const ltype is of no concern if while an arg or initializer (in this instance they are the same thing)
    // i.e first time initialization, it is a problem if this is an assign though
    // but we only really care for assigns on the "top level" if its a pointer
    if(is_const(ltype) && type == assign_type::assign && !was_reference)
    {
        return compile_error(itl,itl_error::const_type_error,"cannot assign rtype to const ltype: %t = %t",ltype,rtype);
    }

    // for an rtype a copy is fine, unless it was a reference in which case
    // the ltype must also be const
    if(is_const(rtype))
    {
        if(!is_const(ltype) && was_reference)
        {
            return compile_error(itl,itl_error::const_type_error,"cannot assign const ref rtype to ltype: %t = %t",ltype,rtype);
        }
    }

    // neither is const is fine in any context
    return option::none;
}

// NOTE: this is expected to be called after main sets of type checking
// so this function assumes that every type is the of the same kind at every level
Option<itl_error> check_const(Interloper&itl, const Type* ltype, const Type* rtype, assign_type type)
{
    b32 done = false;
/*
    // value types can be copied if only the rype is const
    if(is_value_type(rtype) && is_value_type(ltype))
    {
        // can be copied fine
        if(rtype->is_const && !ltype->is_const)
        {
            return;
        }
    }
*/
    // was the type above us a reference?
    b32 was_reference = false;

    // check const specifiers at every level
    while(!done)
    {
        const auto const_err = check_const_internal(itl,ltype,rtype,type,was_reference);
        if(const_err)
        {
            return const_err;
        }

        switch(ltype->kind)
        {
            case type_class::array_t:
            {
                // check sub types
                ltype = index_arr(ltype);
                rtype = index_arr(rtype);

                // array counts as a pointer
                was_reference = true;
                break;
            }

            case type_class::pointer_t:
            {
                // check sub types
                ltype = deref_pointer(ltype);
                rtype = deref_pointer(rtype);

                was_reference = true;
                break;
            }

            case type_class::struct_t:
            {
                done = true;
                break;
            }

            case type_class::enum_t:
            {
                done = true;
                break;
            }

            case type_class::func_pointer_t:
            {
                done = true;
                break;
            }

            case type_class::tuple_t: assert(false); break;

            // check end type
            case type_class::builtin_t:
            {
                done = true;
                break;
            }
        }
    }

    return option::none;
}

b32 is_plain_type(const Type* type)
{
    return !is_array(type) && !is_struct(type);
}

b32 plain_type_equal(const Type* ltype, const Type* rtype)
{
    if(ltype->kind != rtype->kind)
    {
        return false;
    }

    switch(ltype->kind)
    {
        case type_class::struct_t:
        {
            StructType* struct_ltype = (StructType*)ltype;
            StructType* struct_rtype = (StructType*)rtype;

            return struct_ltype->struct_idx == struct_rtype->struct_idx;
        }

        case type_class::builtin_t:
        {
            return cast_builtin(ltype) == cast_builtin(rtype);
        }

        case type_class::enum_t:
        {
            EnumType* enum_ltype = (EnumType*)ltype;
            EnumType* enum_rtype = (EnumType*)rtype; 
            
            return enum_ltype->enum_idx == enum_rtype->enum_idx;
        }

        default:
        {
            assert(false);
            break;
        }
    }
}

b32 type_equal(const Type* ltype, const Type* rtype)
{
    if(ltype->kind != rtype->kind)
    {
        return false;
    }

    switch(ltype->kind)
    {
        case type_class::array_t:
        {
            assert(false);
        }

        case type_class::pointer_t:
        {
            if(ltype->flags != rtype->flags)
            {
                return false;
            }

            return type_equal(deref_pointer(ltype),deref_pointer(rtype));
        }

        default:
        {
            return plain_type_equal(ltype,rtype);
        }
    }
}


bool is_byte_ptr(const Type* type)
{
    return is_pointer(type) && is_builtin_type(deref_pointer(type),builtin_type::byte_t);
}

bool is_flat_array(const Type* type)
{
    return is_array(type) && !is_array(index_arr(type));
}

Option<itl_error> type_check_pointer_nullable(Interloper& itl, const PointerType* ltype, const PointerType* rtype,  assign_type assign_kind)
{
    // Not for an assign we don't care!
    if(assign_kind == assign_type::none)
    {
        return option::none;
    }

    if(ltype->pointer_kind == pointer_type::reference && rtype->pointer_kind == pointer_type::nullable)
    {
        return compile_error(itl,itl_error::pointer_type_error,"Cannot assign a nullable pointer to a reference %t = %t",(Type*)ltype,(Type*)rtype);
    }

    return option::none;
}

Option<itl_error> type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype, assign_type assign_kind)
{
    const auto base_ltype = (PointerType*)ltype;
    const auto base_rtype = (PointerType*)rtype;

    {
        const auto null_err = type_check_pointer_nullable(itl,base_ltype,base_rtype,assign_kind);
        if(null_err)
        {
            return null_err;
        }
    }

    // null rtype auto converted 
    if(is_pointer(ltype) && is_pointer(rtype) && is_builtin_type(deref_pointer(rtype),builtin_type::null_t))
    {
        return option::none;
    }
    
    // any rtype can be assigned to a byte ptr
    if(is_byte_ptr(ltype) && is_pointer(rtype))
    {
        return option::none;
    }


    b32 indirection = true;

    // descend until we hit the base type
    // check they are equal at each step
    while(indirection)
    {
        // types are mismatched we are done
        if(ltype->kind != rtype->kind)
        {
            indirection = false;
        }

        else
        {
            switch(ltype->kind)
            {
                case type_class::pointer_t:
                {
                    const auto null_err = type_check_pointer_nullable(itl,base_ltype,base_rtype,assign_kind);
                    if(null_err)
                    {
                        return null_err;
                    }

                    ltype = deref_pointer(ltype);
                    rtype = deref_pointer(rtype);

                    break;
                }

                case type_class::array_t:
                {
                    if(!is_runtime_size(ltype) || !is_runtime_size(rtype))
                    {
                        return compile_error(itl,itl_error::array_type_error,"Pointer to fixed array");
                    }

                    ltype = index_arr(ltype);
                    rtype = index_arr(rtype);

                    break;
                }

                // is a standard type, struct, enum, builtin etc2
                default:
                {
                    indirection = false;
                    break;
                }
            }
        }
    }

    if(!is_plain(ltype) || !is_plain(rtype))
    {
        return compile_error(itl,itl_error::pointer_type_error,"expected pointer of type %t got %t",(Type*)base_ltype,(Type*)base_rtype);
    }

    // anything else
    else
    {
        // if base types still aren't equal we have a problem!
        if(!plain_type_equal(ltype,rtype))
        {
            return compile_error(itl,itl_error::pointer_type_error,"expected pointer of type %t got %t",(Type*)base_ltype,(Type*)base_rtype);
        }
    }

    return option::none;
}

b32 is_any(Interloper& itl, const Type* type)
{
    if(!itl.rtti_enable)
    {
        return false;
    }

    if(is_struct(type))
    {
        StructType* struct_type = (StructType*)type;

        return struct_type->struct_idx == itl.rtti_cache.any_idx;
    }

    return false;
}

Option<itl_error> check_assign_plain(Interloper& itl, const Type* ltype, const Type* rtype)
{
    // both are builtin
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        const auto builtin_r = cast_builtin(rtype);
        const auto builtin_l = cast_builtin(ltype);

        // both integers
        if(is_integer(ltype) && is_integer(rtype))
        {
            // would narrow (assign is illegal)
            if(builtin_size(builtin_l) < builtin_size(builtin_r))
            {
                return compile_error(itl,itl_error::int_type_error,"narrowing conversion %t = %t",ltype,rtype);
            }

            // unsigned cannot assign to signed
            // TODO: do we want to be this pedantic with integer conversions?
            if(!is_signed(ltype) && is_signed(rtype))
            {
                return compile_error(itl,itl_error::int_type_error,"unsigned = signed (%t = %t)",ltype,rtype);
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            // void is not assignable!
            if(builtin_r == builtin_type::void_t || builtin_l == builtin_type::void_t)
            {
                return compile_error(itl,itl_error::undefined_type_oper,"void assign %t = %t",ltype,rtype);
            }

            // must be same type
            else if(builtin_r != builtin_l)
            {
                return compile_error(itl,itl_error::undefined_type_oper,"invalid assign %t = %t",ltype,rtype);     
            }
        }
    }

    else if(is_struct(ltype) && is_struct(rtype))
    {
        StructType* struct_ltype = (StructType*)ltype;
        StructType* struct_rtype = (StructType*)rtype;

        if(struct_ltype->struct_idx != struct_rtype->struct_idx)
        {
            return compile_error(itl,itl_error::struct_error,"struct assign of different types %t = %t",ltype,rtype);
        }
    }

    else if(is_enum(ltype) && is_enum(rtype))
    {
        EnumType* enum_ltype = (EnumType*)ltype;
        EnumType* enum_rtype = (EnumType*)rtype;

        if(enum_ltype->enum_idx != enum_rtype->enum_idx)
        {
            return compile_error(itl,itl_error::struct_error,"enum assign of different types %t = %t",ltype,rtype);
        }        
    }

    else if(is_func_pointer(ltype) && is_func_pointer(rtype))
    {
        FuncPointerType* func_ltype = (FuncPointerType*)ltype;
        FuncPointerType* func_rtype = (FuncPointerType*)rtype;

        if(count(func_ltype->sig.args) != count(func_rtype->sig.args))
        {
            return compile_error(itl,itl_error::mismatched_args,"func pointers have mismatched arg sizes %t = %t",ltype,rtype);
        }

        if(count(func_ltype->sig.return_type) != count(func_rtype->sig.return_type))
        {
            return compile_error(itl,itl_error::mismatched_args,"func pointers have mismatched return type sizes %t = %t",ltype,rtype);
        }

        // check every type in function pointer is equal
        // implicit conversions are not valid here!

        // check args
        for(u32 a = 0; a < count(func_ltype->sig.args); a++)
        {
            auto& lsym = sym_from_slot(itl.symbol_table,func_ltype->sig.args[a]);
            auto& rsym = sym_from_slot(itl.symbol_table,func_rtype->sig.args[a]);

            if(!type_equal(lsym.type,rsym.type))
            {
                return compile_error(itl,itl_error::mismatched_args,"func pointer arg type %d does not match:\n%t = %t",a,ltype,rtype);
            }
        }

        // check ret type
        for(u32 r = 0; r < count(func_ltype->sig.return_type); r++)
        {
            if(!type_equal(func_ltype->sig.return_type[r],func_rtype->sig.return_type[r]))
            {
                return compile_error(itl,itl_error::mismatched_args,"func pointer return type %d does not match:\n%t = %t",r,ltype,rtype);
            }
        }

    }

    else
    {
        return compile_error(itl,itl_error::undefined_type_oper,"cannot assign %t = %t",ltype,rtype);
    }

    return option::none;
}

Option<itl_error> type_check_array(Interloper& itl, const Type* ltype, const Type* rtype, assign_type type)
{
    // type idx along with the indirection, and contain type
    // must be the same
    if(!is_array(rtype))
    {
        return compile_error(itl,itl_error::array_type_error,"expected array of %t got %t",ltype,rtype);
    }

    // single dimension can trivally convert to a vla
    // for multi dimensional arrays all sizes must match
    if(!is_array(index_arr(ltype)) && !is_array(index_arr(rtype)))
    {
        ArrayType* array_ltype = (ArrayType*)ltype;
        ArrayType* array_rtype = (ArrayType*)rtype;

        if(type != assign_type::arg)
        {
            if(!is_runtime_size(ltype))
            {
                return compile_error(itl,itl_error::array_type_error,"%t = %t, cannot assign to fixed size array",ltype,rtype);
            }
        } 

        // any rtype size is legal in this context if ltype is a vla
        if(!is_runtime_size(ltype))
        {
            // must be same size
            if(array_ltype->size != array_rtype->size)
            {
                return compile_error(itl,itl_error::array_type_error,"expected array of size %d got %d",array_ltype->size,array_rtype->size);
            }
        }

        ltype = index_arr(ltype);
        rtype = index_arr(rtype);              
    } 


    b32 done = false;

    while(!done)
    {
        if(ltype->kind != rtype->kind)
        {
            return compile_error(itl,itl_error::array_type_error,"expected array of underlying type %t got %t",ltype,rtype);
        }

        switch(ltype->kind)
        {
            case type_class::array_t:
            {
                ArrayType* array_ltype = (ArrayType*)ltype;
                ArrayType* array_rtype = (ArrayType*)rtype;

                // for arg passing only
                // valid
                // [3] = [3]

                if(type != assign_type::arg)
                {
                    if(!is_runtime_size(ltype))
                    {
                        return compile_error(itl,itl_error::array_type_error,"%t = %t, cannot assign to fixed size array",ltype,rtype);
                    }
                }

                // must be same size
                if(array_ltype->size != array_rtype->size)
                {
                    // provide better error messagee for vlas
                    if(is_runtime_size(ltype) != is_runtime_size(rtype))
                    {
                        return compile_error(itl,itl_error::array_type_error,"%t = %t, cannot assign different array types beyond 1d",ltype,rtype);                            
                    }

                    return compile_error(itl,itl_error::array_type_error,"expected array of size %d got %d",array_ltype->size,array_rtype->size);
                }
                    

                ltype = index_arr(ltype);
                rtype = index_arr(rtype);                                       
                
                break;
            }

            case type_class::pointer_t:
            {
                assert(false);
            }

            case type_class::builtin_t:
            {
                if(cast_builtin(ltype) != cast_builtin(rtype))
                {
                    return compile_error(itl,itl_error::array_type_error,"Cannot assign array with different underlying type %t != %t",ltype,rtype);
                }
                done = true;
                break;
            }

            default:
            {
                done = true;
            }
        }
    }

    // finally type check the base type!
    return check_assign_plain(itl,ltype,rtype);
}

Option<itl_error> check_assign_internal(Interloper& itl,const Type *ltype, const Type *rtype, assign_type type)
{
    // Assign to any is fine for any type.
    if(is_any(itl,ltype))
    {
        return option::none;
    }

    if(is_plain(rtype) && is_plain(ltype))
    {
        const auto plain_err = check_assign_plain(itl,ltype,rtype);
        if(plain_err)
        {
            return plain_err;
        }
    }

    // check assign by ltype
    else
    {
        if(is_pointer(ltype))
        {
            const auto ptr_err = type_check_pointer(itl,ltype,rtype,type);
            if(ptr_err)
            {
                return ptr_err;
            }
        }

        else if(is_array(ltype))
        {
            const auto array_err = type_check_array(itl,ltype,rtype,type);
            if(array_err)
            {
                return array_err;
            }
        }

        else
        {            
            return compile_error(itl,itl_error::mismatched_args,"cannot assign %t = %t",ltype,rtype);
        }
    }

    if(type == assign_type::no_const)
    {
        return option::none;
    }

    // we know this will descend properly so check const!
    return check_const(itl,ltype,rtype,type);
}

// check ordinary assign
Option<itl_error> check_assign(Interloper& itl,const Type *ltype, const Type *rtype)
{
    return check_assign_internal(itl,ltype,rtype,assign_type::assign);
}

// check ordinary assign
Option<itl_error> check_assign_no_const(Interloper& itl,const Type *ltype, const Type *rtype)
{
    return check_assign_internal(itl,ltype,rtype,assign_type::no_const);
}

Option<itl_error> check_assign_arg(Interloper& itl, const Type* ltype, const Type* rtype)
{
    // args behave the same as initializers
    return check_assign_internal(itl,ltype,rtype,assign_type::arg);
}

Option<itl_error> check_assign_init(Interloper& itl, const Type* ltype, const Type* rtype)
{
    return check_assign_internal(itl,ltype,rtype,assign_type::initializer);
}



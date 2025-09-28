
static constexpr u32 ENUM_SIZE = 4;

u32 type_size(Interloper& itl,const Type *type)
{
    switch(type->kind)
    {
        case type_class::func_pointer_t:
        {
            return GPR_SIZE;
        }

        case type_class::struct_t:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.size;
        }

        case type_class::enum_t:
        {
            const auto& enumeration = enum_from_type(itl.enum_table,type);

            // return size of underyling integeral type
            if(enumeration.underlying_type && is_integer(enumeration.underlying_type))
            {
                return type_size(itl,enumeration.underlying_type);
            }

            return ENUM_SIZE;
        }

        case type_class::array_t:
        {
            if(is_runtime_size(type))
            {
                return GPR_SIZE * 2;
            }

            return GPR_SIZE;            
        }

        case type_class::pointer_t:
        {
            return GPR_SIZE;
        }

        case type_class::tuple_t:
        {
            assert(false);
        }

        case type_class::builtin_t:
        {
            const BuiltinType* underlying = (BuiltinType*)type;
            return builtin_size(underlying->builtin);
        }
    }

    assert(false);
}


u32 array_memory_size(Interloper& itl, const ArrayType* array)
{
    UNUSED(itl);

    if(is_runtime_size(array))
    {
        return GPR_SIZE * 2;
    }
    
    // Fixed array, give back the actual size occupied by the buffer
    return array->sub_size * array->size;
}

u32 type_memory_size(Interloper& itl, const Type* type)
{
    switch(type->kind)
    {
        case type_class::array_t:
        {
            return array_memory_size(itl,(ArrayType*)type);
        }

        default:
        {
            return type_size(itl,type);
        }
    }
}

u32 data_size(Interloper& itl,const Type *type)
{
    switch(type->kind)
    {
        case type_class::struct_t:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.data_size;
        }   

        case type_class::array_t:
        {
            return array_memory_size(itl,(ArrayType*)type);
        }

        default:
        {
            return type_size(itl,type);
        }
    }
}

u32 accumulate_count(u32 count, u32 size)
{
    // if count is zero we are just getting started
    return count == 0? size : count * size;
}


u32 init_arr_sub_sizes_internal(Interloper& itl, Type* type)
{
    /* 
        descend until the bottom is reached mark the size
        return up the sub size and mark it across each level of the type


        do this for any sub indirecitons i.e pointers VLA's
    */

    switch(type->kind)
    {
        case type_class::array_t:
        {
            ArrayType* array_type = (ArrayType*)type;
                
            // VLA
            if(is_runtime_size(type))
            {
                array_type->sub_size = init_arr_sub_sizes_internal(itl,index_arr(type));
                return GPR_SIZE * 2;
            }

            // fixed size
            else
            {
                array_type->sub_size = init_arr_sub_sizes_internal(itl,index_arr(type));
                return array_type->sub_size * array_type->size;
            }
        }

        
        case type_class::pointer_t:
        {
            PointerType* pointer_type = (PointerType*)type; 

            // do for sub type
            if(is_array(pointer_type->contained_type))
            {
                init_arr_sub_sizes(itl,pointer_type->contained_type);
            }

            return GPR_SIZE;
        }

        // plain var mark size and return it!
        default:
        {
            const u32 size = type_size(itl,type);
            return size;
        }
    }
}


void init_arr_sub_sizes(Interloper&itl,Type* type)
{
    init_arr_sub_sizes_internal(itl,type);
}

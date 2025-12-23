
TypeResult type_check_pointer_index(Interloper& itl, IndexNode* index, PointerType* ptr_type)
{
    if(ptr_type->pointer_kind == pointer_type::nullable)
    {
        return compile_error(itl,itl_error::pointer_type_error,"Cannot index a nullable pointer");
    }

    const u32 indexes = count(index->indexes);

    if(indexes != 1)
    {
        return compile_error(itl,itl_error::pointer_type_error,"Expected single index for pointer");
    }

    const auto index_res = type_check_expr(itl,index->indexes[0]);
    if(!index_res)
    {
        return index_res;
    }

    const Type* subscript_type = *index_res;

    if(!is_integer(subscript_type))
    {
        return compile_error(itl,itl_error::int_type_error,"Expected integral expr for pointer subscript got %t",subscript_type);
    }

    return ptr_type->contained_type;
}

TypeResult type_check_array_index_internal(Interloper& itl, IndexNode* index,ArrayType* array_type)
{
    Type* accessed_type = nullptr;

    const u32 indexes = count(index->indexes);

    for(u32 i = 0; i < indexes; i++)
    {
        AstNode* subscript = index->indexes[i];

        const auto subscript_res = type_check_expr(itl,subscript);
        if(!subscript_res)
        {
            return subscript_res;
        }

        const auto subscript_type = *subscript_res;
        if(!is_integer(subscript_type))
        {
            return compile_error(itl,itl_error::int_type_error,"Expected integral expr for array subscript got %t",subscript_type);
        }

        const bool last_index = i == indexes - 1;
        
        if(subscript->known_value)
        {
            const auto subscript_value = *subscript->known_value; 

            // We know both the bounds and subscript check the access is in range.
            if(is_fixed_array(array_type))
            {
                if(subscript_value >= array_type->size)
                {
                    return compile_error(itl,itl_error::out_of_bounds,"Array subscript(%d) [%d] is out of bounds for array of size %d",
                        i,subscript_value,array_type->size);
                }
            }
        }


        if(last_index)
        {
            accessed_type = index_arr(array_type);
        }

        // goto next subscript
        else if(!is_array(array_type->contained_type))
        {
            return compile_error(itl,itl_error::out_of_bounds,"Out of bounds indexing for array %S (%d:%d)",index->name,i,indexes);  
        }

        else
        {
            array_type = (ArrayType*)index_arr(array_type);
        }
    }

    return accessed_type;
}

TypeResult type_check_array_index(Interloper& itl, AstNode* expr)
{
    IndexNode* index = (IndexNode*)expr;

    const auto arr_ptr = get_sym(itl.symbol_table,index->name);

    if(!arr_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"Array '%S' used before declaration",index->name);     
    }

    const auto arr = *arr_ptr;

    index->sym_slot = arr.reg.slot.sym_slot;

    switch(arr.type->kind)
    {
        case type_class::array_t:
        {
            index->type = index_type::array;
            return type_check_array_index_internal(itl,index,(ArrayType*)arr.type);
        }

        case type_class::pointer_t:
        {
            index->type = index_type::pointer;
            return type_check_pointer_index(itl,index,(PointerType*)arr.type);
        }

        default:
        {
            return compile_error(itl,itl_error::array_type_error,"expected array or pointer for index got %t",arr.type);        
        }
    }
}
#include <interloper.h>
#include "array/storage.cpp"
#include "array/checker.cpp"

// for stack allocated arrays i.e ones with fixed sizes at the top level of the decl, We do this to get the base type size
// So we don't have to end up padding this out at higher alignments.
std::pair<u32,u32> calc_arr_allocation(Interloper& itl, const Type* type)
{
    b32 done = false;
    
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
#include <interloper.h>


// TODO: how do we want to internally store properties of builtin types
// i.e sign
// size
// max and minimum value


const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::void_t, false, false, 0, 0, 0},

    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, false, 1, static_cast<u32>(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, false ,2,  static_cast<u32>(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, false ,4,  static_cast<u32>(-(0xffff / 2)), (0xffffffff / 2)},
};



std::string Interloper::type_name(const Type &type)
{
    if(is_builtin(type.type_idx))
    {
        return builtin_type_name(static_cast<builtin_type>(type.type_idx));
    }

    // TODO: make this return properly for user defined types
    else
    {
        return "";
    }
}

// TODO: do we want to pass the operation in here for when we support overloading?
Type Interloper::effective_arith_type(const Type &ltype, const Type &rtype)
{
    // builtin type
    if(is_builtin(rtype.type_idx) && is_builtin(ltype.type_idx))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers
        if(is_integer(builtin_l) && is_integer(builtin_r))
        {
            // return the larger size of the type (promotion)
            return (size(builtin_l) > size(builtin_r))? ltype : rtype; 
        }

        // something else
        else
        {
            printf("arithmetic operation undefined for %s and %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
            exit(1);
        }

    }

    // one or more user defined
    else
    {
        printf("unimplmented: user defined type arithmetic!\n");
        exit(1);        
    }
}

void Interloper::check_assign(const Type &ltype, const Type &rtype)
{
  
    // both are builtin
    if(is_builtin(rtype.type_idx) && is_builtin(ltype.type_idx))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers
        if(is_integer(builtin_l) && is_integer(builtin_r))
        {
            // would narrow (assign is illegal)
            if(size(builtin_l) < size(builtin_r))
            {
                printf("narrowing conversion %s = %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
                exit(1);
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            printf("unimplmented: non integer assign!\n");
            exit(1);            
        }
    }

    // one or more type is user defined
    // here probably the only valid thing is both are the same
    else
    {
        printf("unimplmented: check assign user defined type!\n");
        exit(1);
    }
}
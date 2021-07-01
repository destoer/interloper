#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::void_t, false, false, 0, 0, 0},

    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, static_cast<u32>(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  static_cast<u32>(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  static_cast<u32>(-(0xffff / 2)), (0xffffffff / 2)},
};

// this needs to have more added when we get extra fields
bool same_type(const Type &type1, const Type &type2)
{
    return 
        type1.type_idx == type2.type_idx;
}

u32 Interloper::type_size(const Type &type)
{
    if(is_builtin(type.type_idx))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_type_info[type.type_idx].size;
    }
    
    else
    {
        puts("user defined type size\n");
        exit(1);
    }
}


std::string Interloper::type_name(const Type &type)
{
    if(is_builtin(type.type_idx))
    {
        return builtin_type_name(static_cast<builtin_type>(type.type_idx));
    }

    // TODO: make this return properly for user defined types
    else
    {
        return "undefined_type";
    }
}

// TODO: do we want to pass the operation in here for when we support overloading?
Type Interloper::effective_arith_type(const Type &ltype, const Type &rtype)
{
    if(same_type(ltype,rtype))
    {
        return ltype;
    }

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
    // if we have the same types we dont care
    // TODO: an additonal earlier check might be needed on the dst
    // if we add const specifiers
    if(same_type(ltype,rtype))
    {
        return;
    }


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

// start here
// we need to implement proper stores and loads 
// for each type first
void Interloper::handle_cast(const Type &old_type, const Type &new_type)
{
    // we dont care if we have the same type
    // i.e this cast does nothing
    if(same_type(old_type,new_type))
    {
        return;
    }



    // handle side effects of the cast
    // builtin type
    if(is_builtin(old_type.type_idx) && is_builtin(new_type.type_idx))
    {
        const auto builtin_old = static_cast<builtin_type>(old_type.type_idx);
        const auto builtin_new = static_cast<builtin_type>(new_type.type_idx);

        // integer
        if(is_integer(builtin_new) && is_integer(builtin_old))
        {
            // TODO: make sure this is optimised out

            // unsigned -> larger type
            // zero extend 
            // (this is done by default)
            
            
            // signed -> larger type
            // sign extend
            if(is_signed(builtin_old) && is_signed(builtin_old) && 
                size(builtin_old) < size(builtin_new))
            {
                switch(builtin_old)
                {
                    case builtin_type::s8_t: 
                    {
                        emitter.emit(op_type::sxb,reg(emitter.reg_count),reg(emitter.reg_count));
                        break;
                    }

                    case builtin_type::s16_t:
                    {
                        emitter.emit(op_type::sxh,reg(emitter.reg_count),reg(emitter.reg_count));
                        break;
                    }

                    default: assert(false);
                }
            }

            // larger type -> smaller type
            // truncate value (mask)
            else if(size(builtin_old) > size(builtin_new))
            {
                switch(size(builtin_new))
                {
                    case 1: 
                    {
                        emitter.emit(op_type::and_imm,reg(emitter.reg_count),reg(emitter.reg_count),0xff);
                        break;
                    }

                    case 2:  
                    {
                        emitter.emit(op_type::and_imm,reg(emitter.reg_count),reg(emitter.reg_count),0xffff);
                        break;
                    }

                    default: assert(false);
                }
            }

        }

        else
        {
            printf("unimplmented: handle cast non integer type!\n");
            exit(1);
        }
    }

    // probably only pointers are gonna valid for casts here
    else
    {
        printf("unimplmented: handle cast user defined type!\n");
        exit(1);        
    }

}
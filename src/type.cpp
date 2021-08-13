#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::void_t, false, false, 0, 0, 0},

    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, static_cast<u32>(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  static_cast<u32>(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  static_cast<u32>(-(0xffffffff / 2)), (0xffffffff / 2)},

    {builtin_type::bool_t, false, false ,1,  0, 1},
};

// this needs to have more added when we get extra fields
bool same_type(const Type &type1, const Type &type2)
{
    return 
        type1.type_idx == type2.type_idx;
}

u32 Interloper::type_size(const Type &type)
{
    if(is_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_size(static_cast<builtin_type>(type.type_idx));
    }

    else
    {
        puts("user defined type size\n");
        exit(1);
    }
}

u32 Interloper::type_min(const Type &type)
{
    if(is_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_min(static_cast<builtin_type>(type.type_idx));
    }

    else
    {
        puts("user defined type size\n");
        exit(1);
    }
}

u32 Interloper::type_max(const Type &type)
{
    if(is_builtin(type))
    {
        // assume plain type for now i.e no pointers etc
        return builtin_max(static_cast<builtin_type>(type.type_idx));
    }

    else
    {
        puts("user defined type size\n");
        exit(1);
    }
}


std::string Interloper::type_name(const Type &type)
{
    if(is_builtin(type))
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
    // builtin type
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        // both integers
        if(is_integer(rtype) && is_integer(ltype))
        {
            const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
            const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

            // return the larger size of the type (promotion)
            return (builtin_size(builtin_l) > builtin_size(builtin_r))? ltype : rtype; 
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

void Interloper::check_logical_operation(const Type &ltype, const Type &rtype)
{
    // both are builtin
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers 
        if(is_integer(rtype) && is_integer(ltype))
        {
            if(is_signed(rtype) != is_signed(ltype))
            {
                panic("logical comparision on different signs %s and %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
            }
        }

        // both bool
        else if(builtin_r == builtin_type::bool_t && builtin_l == builtin_type::bool_t)
        {
            
        }

        // something else
        else
        {
            panic("logical operation undefined for %s and %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
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
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        const auto builtin_r = static_cast<builtin_type>(rtype.type_idx);
        const auto builtin_l = static_cast<builtin_type>(ltype.type_idx);

        // both integers
        if(is_integer(ltype) && is_integer(rtype))
        {
            // would narrow (assign is illegal)
            if(builtin_size(builtin_l) < builtin_size(builtin_r))
            {
                panic("narrowing conversion %s = %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
            }

            // unsigned cannot assign to signed
            // TODO: do we want to be this pedantic with integer conversions?
            if(!is_signed(builtin_l) && is_signed(builtin_r))
            {
                panic("unsigned = signed (%s = %s)\n",type_name(ltype).c_str(),type_name(rtype).c_str());
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            // void is not assignable!
            if(builtin_r == builtin_type::void_t || builtin_l == builtin_type::void_t)
            {
                panic("void assign %s = %s\n",type_name(ltype).c_str(),type_name(rtype).c_str());
            }

            else
            {
                printf("unimplmented: non integer assign!\n");
                exit(1); 
            }           
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
void Interloper::handle_cast(IrEmitter &emitter,const Type &old_type, const Type &new_type)
{
    // we dont care if we have the same type
    // i.e this cast does nothing
    if(same_type(old_type,new_type))
    {
        return;
    }



    // handle side effects of the cast
    // builtin type
    if(is_builtin(old_type) && is_builtin(new_type))
    {
        const auto builtin_old = static_cast<builtin_type>(old_type.type_idx);
        const auto builtin_new = static_cast<builtin_type>(new_type.type_idx);

        // integer
        if(is_integer(old_type) && is_integer(new_type))
        {
            // TODO: make sure this is optimised out

            // unsigned -> larger type
            // zero extend 
            // (this is done by default)
            
            
            // signed -> larger type
            // sign extend
            if(is_signed(old_type) && is_signed(new_type) && 
                builtin_size(builtin_old) < builtin_size(builtin_new))
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
            else if(builtin_size(builtin_old) > builtin_size(builtin_new))
            {
                switch(builtin_size(builtin_new))
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

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(builtin_new))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(builtin_old) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(builtin_old))
            {
                emitter.emit(op_type::cmpsgt_imm,reg(emitter.reg_count),reg(emitter.reg_count),0);
            }

            // unsigned
            else
            {
                emitter.emit(op_type::cmpugt_imm,reg(emitter.reg_count),reg(emitter.reg_count),0);
            }
        }        

        else
        {
            printf("unimplmented: handle cast builtin illegal %s -> %s\n",type_name(old_type).c_str(),type_name(new_type).c_str());
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
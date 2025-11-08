
void clip_arith_type(Interloper &itl, Function& func,RegSlot dst_slot, RegSlot src_slot, u32 size)
{
    switch(size)
    {
        case 1: 
        {
            and_imm(itl,func,dst_slot,src_slot,0xff);
            break;
        }

        case 2:  
        {
            and_imm(itl,func,dst_slot,src_slot,0xffff);
            break;
        }

        case 4:
        {
            and_imm(itl,func,dst_slot,src_slot,0xffff'ffff);
            break;
        }

        // maximum type -> do nothing
        case 8:
        {
            break;
        }

        default:
        {
            assert(false);
        }
    }    
}

Option<itl_error> handle_cast(Interloper& itl,Function& func, const TypedReg& old_reg, const TypedReg& new_reg)
{
    // handle side effects of the cast
    // builtin type
    if(is_plain_builtin(old_reg.type) && is_plain_builtin(new_reg.type))
    {
        const auto builtin_old = cast_builtin(old_reg.type);
        const auto builtin_new = cast_builtin(new_reg.type);

        // integer
        if(is_integer(old_reg.type) && is_integer(new_reg.type))
        {
            // signed -> larger type
            // sign extend
            if(is_signed(old_reg.type) && is_signed(new_reg.type) && 
                builtin_size(builtin_old) < builtin_size(builtin_new))
            {
                switch(builtin_old)
                {
                    case builtin_type::s8_t: 
                    {
                        sign_extend_byte(itl,func,new_reg.slot,old_reg.slot);
                        break;
                    }

                    case builtin_type::s16_t:
                    {
                        sign_extend_half(itl,func,new_reg.slot,old_reg.slot);
                        break;
                    }

                    case builtin_type::s32_t:
                    {
                        sign_extend_word(itl,func,new_reg.slot,old_reg.slot);
                        break;                        
                    }

                    case builtin_type::s64_t: break;

                    default: crash_and_burn("invalid signed integer upcast");
                }
            }

            // larger type -> smaller type
            // truncate value (mask)
            else if(builtin_size(builtin_old) > builtin_size(builtin_new))
            {
                clip_arith_type(itl,func,new_reg.slot,old_reg.slot,type_size(itl,new_reg.type));
            }

            // cast doesnt do anything (as zero extension is default) but move into a tmp so the IR doesnt break
            else
            {
                mov_reg(itl,func,new_reg.slot,old_reg.slot);
            }

        }

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(new_reg.type))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
            mov_reg(itl,func,new_reg.slot,old_reg.slot);
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(old_reg.type) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(old_reg.type))
            {
                cmp_signed_gt_imm(itl,func,new_reg.slot,old_reg.slot,0);
            }

            // unsigned
            else
            {
                cmp_unsigned_gt_imm(itl,func,new_reg.slot,old_reg.slot,0);
            }
        }        

        else if(is_float(old_reg.type) && is_integer(new_reg.type))
        {
            cvt_fi(itl,func,new_reg.slot,old_reg.slot);
        }

        else if(is_integer(old_reg.type) && is_float(new_reg.type))
        {
            cvt_if(itl,func,new_reg.slot,old_reg.slot);
        }

        else
        {
            return compile_error(itl,itl_error::illegal_cast,"cannot cast %t -> %t",old_reg.type,new_reg.type);
        }
    }

    // these cast do no conversions just move the reg 
    // they are only acknowledgement's your doing something screwy

    // cast from enum to int is fine
    else if(is_enum(old_reg.type) && is_integer(new_reg.type))
    {
        mov_reg(itl,func,new_reg.slot,old_reg.slot);
    }

    // as is integer to enum
    else if(is_integer(old_reg.type) && is_enum(new_reg.type))
    {
        mov_reg(itl,func,new_reg.slot,old_reg.slot);
    }

    // pointer to pointer or integer
    else if(is_pointer(old_reg.type) && (is_pointer(new_reg.type) || is_integer(new_reg.type)))
    {
        mov_reg(itl,func,new_reg.slot,old_reg.slot);
    }


    // integer to pointer
    else if(is_integer(old_reg.type) && (is_pointer(new_reg.type)))
    {
        mov_reg(itl,func,new_reg.slot,old_reg.slot);
    }

    // func pointer cast to pointer or integer is fine
    else if(is_func_pointer(old_reg.type) && (is_pointer(new_reg.type) || is_integer(new_reg.type)))
    {
        mov_reg(itl,func,new_reg.slot,old_reg.slot);
    }

    else if(is_array(old_reg.type) && is_array(new_reg.type))
    {
        if(!is_vla(new_reg.type))
        {
            return compile_error(itl,itl_error::illegal_cast,"Cannot recast to fixed array %t -> %t",old_reg.type,new_reg.type);
        }


        const u32 new_size = type_size(itl,index_arr(new_reg.type));
        const u32 old_size = type_size(itl,index_arr(old_reg.type));

        const auto data_slot = load_arr_data(itl,func,old_reg);
        store_arr_data(itl,func,new_reg.slot,data_slot);

        const auto len_slot = load_arr_len(itl,func,old_reg);
        const auto byte_slot = mul_imm_res(itl,func,len_slot,old_size);
        const auto converted_len = udiv_imm_res(itl,func,byte_slot,new_size);
        store_arr_len(itl,func,new_reg.slot,converted_len);
    }

    // fuck knows
    else
    {
        return compile_error(itl,itl_error::illegal_cast,"cannot cast %t -> %t",old_reg,new_reg);      
    }

    return option::none;
}

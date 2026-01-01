
#include "error.h"
#include <type_traits>
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


u64 cast_const(Interloper& itl, Type* old_type, Type* new_type, u64 value)
{
    const u32 new_size = type_size(itl,new_type);
    const u32 old_size = type_size(itl,old_type);

    if(!is_signed(old_type) && is_signed(new_type))
    {
        switch(old_size)
        {
            case 1: return sign_extend_type<u64>(s8(value)); 
            case 2: return sign_extend_type<u64>(s16(value)); 
            case 4: return sign_extend_type<u64>(s32(value)); 
            default: return value;
        }
    }

    else if(new_size < old_size)
    {
        switch(new_size)
        {
            case 1: return value & 0xff; 
            case 2: return value & 0xffff; 
            case 4: return value & 0xffff'ffff; 
            default: return value;        
        }
    }

    return value;
}

TypeResult type_check_cast(Interloper& itl, AstNode* expr)
{
    CastNode* cast = (CastNode*)expr;
    
    const auto old_type_res = type_check_expr(itl,cast->expr);
    if(!old_type_res)
    {
        return old_type_res;
    }

    Type* old_type = *old_type_res;


    const auto new_type_res = get_type(itl,cast->type);
    if(!new_type_res)
    {
        return new_type_res;
    }

    Type* new_type = *new_type_res;

    if(cast->expr->known_value && is_integer(new_type))
    {
        const u64 value = *cast->expr->known_value;
        cast->node.known_value = cast_const(itl,old_type,new_type,value);
    }

    // handle side effects of the cast
    // builtin type
    if(is_plain_builtin(old_type) && is_plain_builtin(new_type))
    {
        const auto builtin_old = cast_builtin(old_type);
        const auto builtin_new = cast_builtin(new_type);

        // integer
        if(is_integer(old_type) && is_integer(new_type))
        {
            // signed -> larger type
            // sign extend
            if(is_signed(old_type) && is_signed(new_type) && 
                builtin_size(builtin_old) < builtin_size(builtin_new))
            {
                switch(builtin_old)
                {
                    case builtin_type::s8_t: 
                    case builtin_type::s16_t:
                    case builtin_type::s32_t:
                    {
                        cast->oper = cast_oper::sign_extend;
                        break;
                    }

                    case builtin_type::s64_t:
                    {
                        cast->oper = cast_oper::move;
                        break;
                    } 

                    default: crash_and_burn("invalid signed integer upcast");
                }
            }

            // larger type -> smaller type
            // truncate value (mask)
            else if(builtin_size(builtin_old) > builtin_size(builtin_new))
            {
                cast->oper  = cast_oper::clip;
            }

            // cast doesnt do anything (as zero extension is default) but move into a tmp so the IR doesnt break
            else
            {
                cast->oper = cast_oper::move;
            }

        }

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(new_type))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
            cast->oper = cast_oper::move;
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(old_type) && builtin_new == builtin_type::bool_t)
        {
            cast->oper = cast_oper::compare;
        }        

        else if(is_float(old_type) && is_integer(new_type))
        {
            cast->oper = cast_oper::from_float;
        }

        else if(is_integer(old_type) && is_float(new_type))
        {
            cast->oper = cast_oper::to_float;
        }

        else
        {
            return compile_error(itl,itl_error::illegal_cast,"cannot cast %t -> %t",old_type,new_type);
        }
    }

    // these cast do no conversions just move the reg 
    // they are only acknowledgement's your doing something screwy

    // cast from enum to int is fine
    else if(is_enum(old_type) && is_integer(new_type))
    {
        cast->oper = cast_oper::move;
    }

    // as is integer to enum
    else if(is_integer(old_type) && is_enum(new_type))
    {
        cast->oper = cast_oper::move;
    }

    // pointer to pointer or integer
    else if(is_pointer(old_type) && (is_pointer(new_type) || is_integer(new_type)))
    {
        cast->oper = cast_oper::move;
    }


    // integer to pointer
    else if(is_integer(old_type) && (is_pointer(new_type)))
    {
        cast->oper = cast_oper::move;
    }

    // func pointer cast to pointer or integer is fine
    else if(is_func_pointer(old_type) && (is_pointer(new_type) || is_integer(new_type)))
    {
        cast->oper = cast_oper::move;
    }

    else if(is_array(old_type) && is_array(new_type))
    {
        if(!is_vla(new_type))
        {
            return compile_error(itl,itl_error::illegal_cast,"Cannot recast to fixed array %t -> %t",old_type,new_type);
        }

        cast->oper = cast_oper::recast_array;
    }

    // fuck knows
    else
    {
        return compile_error(itl,itl_error::illegal_cast,"cannot cast %t -> %t",old_type,new_type);      
    }

    return new_type;
}

TypeResult type_check_cast_ref(Interloper& itl, AstNode* expr)
{
    CastRefNode* cast = (CastRefNode*)expr;
    
    const auto expr_res = type_check_expr(itl,cast->expr);
    if(!expr_res)
    {
        return expr_res;
    }

    const auto old_pointer = *expr_res;
    if(!is_pointer(old_pointer))
    {
        return compile_error(itl,itl_error::pointer_type_error,"Cast ref requires a pointer got %t",old_pointer);
    }


    return make_reference(itl, deref_pointer(old_pointer));
}

TypedReg compile_oper(Interloper& itl,Function &func,AstNode *node);


void compile_cast_ref(Interloper& itl,Function &func,AstNode *expr, RegSlot dst_slot)
{
    CastRefNode* cast = (CastRefNode*)expr;

    const auto reg = compile_oper(itl,func,cast->expr);
    mov_reg(itl,func,dst_slot,reg.slot);
}

void compile_cast(Interloper& itl,Function &func,AstNode *expr, RegSlot dst_slot)
{
    CastNode* cast = (CastNode*)expr;

    const auto reg =  compile_oper(itl,func,cast->expr);

    switch(cast->oper)
    {
        case cast_oper::move:
        {
            mov_reg(itl,func,dst_slot,reg.slot);
            break;
        }

        case cast_oper::sign_extend:
        {
            BuiltinType* builtin_type = (BuiltinType*)reg.type;

            switch(builtin_type->builtin)
            {
                case builtin_type::s8_t: 
                {
                    sign_extend_byte(itl,func,dst_slot,reg.slot);
                    break;
                }

                case builtin_type::s16_t:
                {
                    sign_extend_half(itl,func,dst_slot,reg.slot);
                    break;
                }

                case builtin_type::s32_t:
                {
                    sign_extend_word(itl,func,dst_slot,reg.slot);
                    break;                        
                }

                default: break;
            }

            break;
        }


        case cast_oper::clip:
        {
            clip_arith_type(itl,func,dst_slot,reg.slot,type_size(itl,cast->node.expr_type));
            break;
        }

        case cast_oper::compare:
        {
            if(is_signed(reg.type))
            {
                cmp_signed_gt_imm(itl,func,dst_slot,reg.slot,0);
            }

            // unsigned
            else
            {
                cmp_unsigned_gt_imm(itl,func,dst_slot,reg.slot,0);
            }
            
            break;
        }

        case cast_oper::to_float:
        {
            cvt_if(itl,func,dst_slot,reg.slot);
            break;
        }

        case cast_oper::from_float:
        {
            cvt_fi(itl,func,dst_slot,reg.slot);
            break;
        }

        case cast_oper::recast_array:
        {
            const u32 new_size = type_size(itl,index_arr(cast->node.expr_type));
            const u32 old_size = type_size(itl,index_arr(reg.type));

            const auto data_slot = load_arr_data(itl,func,reg);
            store_arr_data(itl,func,dst_slot,data_slot);

            const auto len_slot = load_arr_len(itl,func,reg);
            const auto byte_slot = mul_imm_res(itl,func,len_slot,old_size);
            const auto converted_len = udiv_imm_res(itl,func,byte_slot,new_size);
            store_arr_len(itl,func,dst_slot,converted_len);
            break;
        }
    }
}
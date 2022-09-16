#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, u32(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  u32(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  u32(-(0xffffffff / 2)), (0xffffffff / 2)},

    {builtin_type::byte_t, true, false, 1, 0, 0xff},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    {builtin_type::void_t, false, false, 0, 0, 0},

    {builtin_type::null_t, false,false, GPR_SIZE,0,0},
};

void type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype);

b32 is_builtin(const Type* type)
{
    return type->type_idx < BUILTIN_TYPE_SIZE;
}

b32 is_integer(const Type* type)
{
    return is_builtin(type) && builtin_type_info[type->type_idx].is_integer;
}

b32 is_signed(const Type *type)
{
    return is_builtin(type) && builtin_type_info[type->type_idx].is_signed;
}

b32 is_signed_integer(const Type *type)
{
    return is_signed(type) && is_integer(type);
}

b32 is_array(const Type* type)
{
    return type->type_idx == ARRAY;
}

b32 is_struct(const Type* type)
{
    return type->type_idx == STRUCT;
}

b32 is_enum(const Type* type)
{
    return type->type_idx == ENUM;
}

b32 is_pointer(const Type* type)
{
    return type->type_idx == POINTER;
}

b32 is_bool(const Type* type)
{
    return type->type_idx == u32(builtin_type::bool_t);
}

b32 is_trivial_copy(const Type *type)
{
    return is_builtin(type) || is_pointer(type) || is_enum(type);
}

// for arrays
b32 is_runtime_size(const Type* type)
{
    const ArrayType* array_type = (ArrayType*)type;

    return array_type->size == RUNTIME_SIZE;
}

b32 is_fixed_array_pointer(const Type* type)
{
    if(is_pointer(type))
    {
        const PointerType* pointer = (PointerType*)type;

        const Type* contained_type = pointer->contained_type;

        if(is_array(contained_type))
        {
            const ArrayType* array = (ArrayType*)contained_type;
            return array->size != RUNTIME_SIZE;
        }
    }

    return false;
}


b32 is_plain(const Type *type)
{
    return !is_pointer(type) && !is_array(type);
}

b32 is_value_type(const Type* type)
{
    return is_plain(type);
}

b32 is_fixed_array(const Type* type)
{
    if(is_array(type))
    {
        return !is_runtime_size(type);
    }

    return false;
}


u32 builtin_size(builtin_type t)
{
    return builtin_type_info[u32(t)].size;
}

u32 builtin_max(builtin_type t)
{
    return builtin_type_info[u32(t)].max;
}

u32 builtin_min(builtin_type t)
{
    return builtin_type_info[u32(t)].min;
}

const char *builtin_type_name(builtin_type t)
{
    return TYPE_NAMES[static_cast<size_t>(t)];
}

builtin_type cast_builtin(const Type *type)
{
    return builtin_type(type->type_idx);
}

u32 type_size(Interloper& itl,const Type *type)
{
    if(is_builtin(type))
    {
        return builtin_size(builtin_type(type->type_idx));
    }

    else if(is_pointer(type))
    {
        return GPR_SIZE;
    }
    
    // TODO: this doesnt handle VLA
    else if(is_array(type))
    {
        if(is_runtime_size(type))
        {
            return GPR_SIZE * 2;
        }

        return GPR_SIZE;
    }

    else if(is_enum(type))
    {
        return GPR_SIZE;
    }

    // user defined type
    else if(is_struct(type))
    {
        const auto& structure = struct_from_type(itl.struct_table,type);
        return structure.size;
    }

    unimplemented("unhandled type size");
}


const Type* deref_pointer(const Type* type)
{
    const PointerType* pointer_type = (PointerType*)type;
    return pointer_type->contained_type;
}

Type* deref_pointer(Type* type)
{
    PointerType* pointer_type = (PointerType*)type;
    return pointer_type->contained_type;
}



// NOTE: this should probably return any count before the runtime size...
// along with where the first runtime size is

// NOTE: this needs to be reworked to support deduced sizes

std::pair<u32,u32> arr_size(Interloper&itl,const Type* arr_type)
{
    UNUSED(itl); UNUSED(arr_type);
    assert(false);
}


u32 default_value(const Type* type)
{
    UNUSED(type);
    // NOTE: for now every default is zero
    // and as such we dont bother doing any type checking either
    return 0;
}

// type creation helpers
template<typename T>
Type* alloc_type(Interloper& itl, u32 type_idx, b32 is_const)
{
    Type* type = (Type*)allocate(itl.type_allocator,sizeof(T));
    type->type_idx = type_idx;
    type->is_const = is_const;

    return type;
}


Type* make_raw(Interloper& itl, u32 type)
{
    return alloc_type<Type>(itl,type,false);
}

Type* make_builtin(Interloper& itl, builtin_type type)
{
    return alloc_type<Type>(itl,u32(type),false);
}


Type* make_pointer(Interloper& itl,Type* contained_type)
{
    PointerType* pointer_type = (PointerType*)alloc_type<PointerType>(itl,POINTER,false);

    pointer_type->contained_type = contained_type;

    return (Type*)pointer_type;
}


String type_name(Interloper& itl,const Type *type)
{
    StringBuffer prefix;

    StringBuffer compound;

    if(type->is_const)
    {
        push_string(itl.string_allocator,prefix,"const ");
    }

    String plain;

    b32 done = false;

    while(!done)
    {
        switch(type->type_idx)
        {
            case POINTER:
            {
                push_char(itl.string_allocator,compound,'@');

                PointerType* pointer_type = (PointerType*)type;
                type = pointer_type->contained_type;
                break;
            }

            case STRUCT: assert(false);

            case ENUM: assert(false);

            case ARRAY: assert(false);

            // builtin
            default:
            {
                plain = builtin_type_name(builtin_type(type->type_idx));
                done = true;
                break;
            }
        }

    }

    push_string(itl.string_allocator,prefix,plain);

    // null term both strings
    push_char(itl.string_allocator,prefix,'\0');
    push_char(itl.string_allocator,compound,'\0');


    // produce the final string!
    String name = cat_string(itl.string_allocator,make_string(prefix),make_string(compound));

    return name;
}

Type* get_type(Interloper& itl, TypeNode* type_decl,u32 struct_idx_override = INVALID_TYPE)
{
    u32 type_idx = 0;

    if(struct_idx_override != INVALID_TYPE)
    {
        type_idx = STRUCT;
        // add the structure nonsense!
        assert(false);
    }

    else if(type_decl->type_idx == USER_TYPE)
    {
        assert(false);
    }

    else
    {
        type_idx = type_decl->type_idx;
    }

    Type* type = make_raw(itl,type_idx);

    type->is_const = type_decl->is_const;

    // arrays, pointers
    for(u32 c = 0; c < count(type_decl->compound_type); c++)
    {
        AstNode* node = type_decl->compound_type[c];

        switch(node->type)
        {
            // pointer to current type
            case ast_type::ptr_indirection:
            {
                type = make_pointer(itl,type);
                break;
            }

            default:
            {
                panic(itl,"invalid type specifier: %s\n",AST_NAMES[u32(node->type)]);
                return make_builtin(itl,builtin_type::void_t);
            }
        }
    }

    return type;
}

// TODO: this is more restrictive than required atm
b32 def_has_indirection(TypeNode *type_decl)
{
    return count(type_decl->compound_type);
}



// TODO: do we want to pass the operation in here for when we support overloading?
Type* effective_arith_type(Interloper& itl,Type *ltype, Type *rtype)
{
    // builtin type
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        // both integers
        if(is_integer(rtype) && is_integer(ltype))
        {
            const auto builtin_r = builtin_type(rtype->type_idx);
            const auto builtin_l = builtin_type(ltype->type_idx);

            // return the larger size of the type (promotion)
            return (builtin_size(builtin_l) > builtin_size(builtin_r))? ltype : rtype; 
        }

        // something else
        else
        {
            panic(itl,"arithmetic operation undefined for %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return make_builtin(itl,builtin_type::void_t);
        }

    }

    // pointer arithmetic is fine
    else if(is_pointer(ltype) && is_integer(rtype))
    {
        return ltype;
    }


    // one or more user defined
    else
    {
        unimplemented("user defined type arithmetic!\n");       
    }
}

void check_logical_operation(Interloper& itl,const Type *ltype, const Type *rtype, logic_op type)
{
    UNUSED(itl);

    // both are builtin
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        const auto builtin_r = builtin_type(rtype->type_idx);
        const auto builtin_l = builtin_type(ltype->type_idx);

        // both integers 
        if(is_integer(rtype) && is_integer(ltype))
        {
            if(is_signed(rtype) != is_signed(ltype))
            {
                panic(itl,"logical comparision on different signs %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
        }

        // both bool
        else if(builtin_r == builtin_type::bool_t && builtin_l == builtin_type::bool_t)
        {
            
        }

        // something else
        else
        {
            panic(itl,"logical operation undefined for %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }
    }

    else if(is_pointer(ltype) && is_pointer(rtype))
    {
        type_check_pointer(itl,ltype,rtype);
    }

    else if(is_enum(ltype) && is_enum(rtype))
    {
        if(type != logic_op::cmpeq_reg && type != logic_op::cmpne_reg)
        {
            panic(itl,"comparision on enums is only defined for '==' and '!='");
            return;
        }

        if(ltype->type_idx != rtype->type_idx)
        {
            panic(itl,"expected enum of the same type for comparsions %s : %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }
    }

    // no matching operator
    else 
    {
        panic(itl,"logical operation on user defined type: %s : %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }   
}


void check_const_internal(Interloper&itl, const Type* ltype, const Type* rtype, bool is_arg, bool is_initializer)
{
    // handle const
    // TODO: this does not typecheck arrays yet
    if(rtype->is_const)
    {
        if(is_arg)
        {
            // if the ltype is not const and the rtype is not this is illegal
            if(!ltype->is_const)
            {
                panic(itl,"cannot pass const ref to mut ref: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                return;
            }
        }

        // assign
        else
        {
            // ltype is const
            // only valid given an initialisation
            if(ltype->is_const && !is_initializer)
            {
                panic(itl,"cannot to const: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }

            // ltype is not const illegal
            else
            {
                panic(itl,"cannot assign const ref to mut ref: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }  
        }
    }

    
    else if(ltype->is_const)
    {
        // if its an arg or initalizer its fine
        if(!is_initializer && !is_arg)
        {
            panic(itl,"cannot assign to const: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }
    }
}

// NOTE: this is expected to be called after main sets of type checking
// so types should atleast be the same fmt at every level
void check_const(Interloper&itl, const Type* ltype, const Type* rtype, bool is_arg, bool is_initializer)
{
    b32 done = false;

    // value types can be copied if only the rype is const
    if(is_value_type(rtype) && is_value_type(ltype))
    {
        // can be copied fine
        if(rtype->is_const && !ltype->is_const)
        {
            return;
        }
    }


    // check const specifiers at every level
    while(!done)
    {
        switch(ltype->type_idx)
        {
            case ARRAY:
            {
                assert(false);
            }

            case POINTER:
            {
                check_const_internal(itl,ltype,rtype,is_arg,is_initializer);

                // check sub types
                ltype = deref_pointer(ltype);
                rtype = deref_pointer(rtype);
                break;
            }

            case STRUCT:
            {
                assert(false);
            }

            case ENUM:
            {
                assert(false);
            }

            // check end type
            default:
            {
                check_const_internal(itl,ltype,rtype,is_arg,is_initializer);
                done = true;
            }
        }
    }
}

b32 plain_type_equal(const Type* ltype, const Type* rtype)
{
    switch(ltype->type_idx)
    {
        case STRUCT:
        {
            assert(false);
        }

        case ENUM:
        {
            assert(false);
        }

        default:
        {
            return ltype->type_idx == rtype->type_idx;
        }
    }
}



void type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype)
{
    UNUSED(itl);


    b32 indirection = true;

    // descend until we hit the base type
    // check they are equal at each step
    while(indirection)
    {
        // types are mismatched we are done
        if(ltype->type_idx != rtype->type_idx)
        {
            indirection = false;
        }

        else
        {
            switch(ltype->type_idx)
            {
                case POINTER:
                {
                    ltype = deref_pointer(ltype);
                    rtype = deref_pointer(rtype);

                    break;
                }

                case ARRAY:
                {
                    assert(false);
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
        panic(itl,"expected pointer of type %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }

    // anything else
    else
    {
        // anything of NULL is implictly converted
        if(rtype->type_idx == u32(builtin_type::null_t))
        {

        }

        // anything against a ltype of byte is fine!
        else if(ltype->type_idx == u32(builtin_type::byte_t))
        {

        }

        // if base types still aernt equal we have a problem!
        else if(!plain_type_equal(ltype,rtype))
        {
            panic(itl,"expected pointer of type %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }
    }
}




void check_assign(Interloper& itl,const Type *ltype, const Type *rtype, bool is_arg = false, bool is_initializer = false)
{


    // both are builtin
    if(is_builtin(rtype) && is_builtin(ltype))
    {
        const auto builtin_r = builtin_type(rtype->type_idx);
        const auto builtin_l = builtin_type(ltype->type_idx);

        // both integers
        if(is_integer(ltype) && is_integer(rtype))
        {
            // would narrow (assign is illegal)
            if(builtin_size(builtin_l) < builtin_size(builtin_r))
            {
                panic(itl,"narrowing conversion %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }

            // unsigned cannot assign to signed
            // TODO: do we want to be this pedantic with integer conversions?
            if(!is_signed(ltype) && is_signed(rtype))
            {
                panic(itl,"unsigned = signed (%s = %s)\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            // void is not assignable!
            if(builtin_r == builtin_type::void_t || builtin_l == builtin_type::void_t)
            {
                panic(itl,"void assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }

            // same type is fine
            else if(builtin_r == builtin_l)
            {

            }

            else
            {
                unimplemented("non integer assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }           
        }
    }

    else if(is_struct(ltype) && is_struct(rtype))
    {
        assert(false);
    }

    // check assign by ltype
    else
    {
        if(is_pointer(ltype))
        {
            type_check_pointer(itl,ltype,rtype);

            if(itl.error)
            {
                return;
            }
        }

        else if(is_array(ltype))
        {
            assert(false);
        }


        else
        {
            panic(itl,"cannot assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }
    }


    if(!itl.error)
    {
        // we know this will descend properly so check const!
        check_const(itl,ltype,rtype,is_arg,is_initializer);
    }
}

void handle_cast(Interloper& itl,Function& func, u32 dst_slot,u32 src_slot,const Type *old_type, const Type *new_type)
{
    if(itl.error)
    {
        return;
    }

    // handle side effects of the cast
    // builtin type
    if(is_plain(old_type) && is_plain(new_type))
    {
        const auto builtin_old = builtin_type(old_type->type_idx);
        const auto builtin_new = builtin_type(new_type->type_idx);

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
                        emit(func,op_type::sxb,dst_slot,src_slot);
                        break;
                    }

                    case builtin_type::s16_t:
                    {
                        emit(func,op_type::sxh,dst_slot,src_slot);
                        break;
                    }

                    default: panic("invalid signed integer upcast");
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
                        emit(func,op_type::and_imm,dst_slot,src_slot,0xff);
                        break;
                    }

                    case 2:  
                    {
                        emit(func,op_type::and_imm,dst_slot,src_slot,0xffff);
                        break;
                    }

                    default: panic("invalid signed integer downcast");
                }
            }

            // cast doesnt do anything but move into a tmp so the IR doesnt break
            else
            {
                emit(func,op_type::mov_reg,dst_slot,src_slot);
            }

        }

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(new_type))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
            emit(func,op_type::mov_reg,dst_slot,src_slot);
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(old_type) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(old_type))
            {
                emit(func,op_type::cmpsgt_imm,dst_slot,src_slot,0);
            }

            // unsigned
            else
            {
                emit(func,op_type::cmpugt_imm,dst_slot,src_slot,0);
            }
        }        

        else
        {
            unimplemented("handle cast builtin illegal %s -> %s\n",type_name(itl,old_type).buf,type_name(itl,new_type).buf);
        }
    }

    // cast from enum to int is fine
    else if(is_enum(old_type) && is_integer(new_type))
    {
        emit(func,op_type::mov_reg,dst_slot,src_slot);
    }

    // as is integer to enum
    else if(is_integer(old_type) && is_enum(new_type))
    {
        emit(func,op_type::mov_reg,dst_slot,src_slot);
    }

    // cast does nothing just move the reg, its only acknowledgement your doing something screwy
    else if(is_pointer(old_type) && is_pointer(new_type))
    {
        emit(func,op_type::mov_reg,dst_slot,src_slot);
    }

    // probably only pointers are gonna valid for casts here
    else
    {
        unimplemented("handle cast user defined type!\n");        
    }

}

void add_type_decl(Interloper& itl, u32 type_idx, const String& name, type_kind kind)
{
    TypeDecl type_decl;

    type_decl.type_idx = type_idx;
    type_decl.name = name;
    type_decl.kind = kind;

    add(itl.type_table,type_decl.name,type_decl);    
}



void destroy_func(Function& func)
{
    destroy_arr(func.args);
    destroy_arr(func.return_type);
    destroy_emitter(func.emitter);
}
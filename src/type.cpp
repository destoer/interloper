#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, static_cast<u32>(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  static_cast<u32>(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  static_cast<u32>(-(0xffffffff / 2)), (0xffffffff / 2)},

    {builtin_type::byte_t, true, false, 1, 0, 0xff},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    {builtin_type::void_t, false, false, 0, 0, 0},

    {builtin_type::null_t, false,false, GPR_SIZE,0,0},
};

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


Type* make_raw_type(Interloper& itl, u32 type)
{
    return alloc_type<Type>(itl,type,false);
}

Type* make_builtin_type(Interloper& itl, builtin_type type)
{
    return alloc_type<Type>(itl,u32(type),false);
}


String type_name(Interloper& itl,const Type *type)
{
    UNUSED(itl); UNUSED(type);
    assert(false);
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

    Type* type = make_raw_type(itl,type_idx);

    type->is_const = type_decl->is_const;

    // arrays, pointers
    for(u32 c = 0; c < count(type_decl->compound_type); c++)
    {
        assert(false);
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
            return make_builtin_type(itl,builtin_type::void_t);
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
    UNUSED(itl); UNUSED(ltype); UNUSED(rtype); UNUSED(type);
    assert(false);
}

void check_const(Interloper&itl, const Type* ltype, const Type* rtype, bool is_arg, bool is_initializer)
{
    // handle const
    // TODO: this does not typecheck arrays yet
    if(rtype->is_const)
    {
        if(is_arg)
        {
            // if both are value types this is fine as its just a copy
            if(is_value_type(rtype) && is_value_type(rtype))
            {

            }

            // if the ltype is const and the rtype is not this is illegal
            else if(!ltype->is_const)
            {
                panic(itl,"cannot pass const ref to mut ref: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                return;
            }
        }

        else
        {
            // ltype is const
            // only valid given an initialisation
            if(ltype->is_const && !is_initializer)
            {
                panic(itl,"cannot to const: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }

            // ltype is not const, fine given that the rtype is a value type
            else if(!is_value_type(rtype))
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

void check_assign(Interloper& itl,const Type *ltype, const Type *rtype, bool is_arg = false, bool is_initializer = false)
{
    // check const first
    check_const(itl,ltype,rtype,is_arg,is_initializer);



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
        assert(false);
    }
}

void handle_cast(Interloper& itl,Function& func, u32 dst_slot,u32 src_slot,const Type *old_type, const Type *new_type)
{
    if(itl.error)
    {
        return;
    }

    assert(false);
    UNUSED(itl); UNUSED(func); UNUSED(dst_slot); UNUSED(src_slot); UNUSED(old_type); UNUSED(new_type);
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
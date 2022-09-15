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


u32 type_size(const Type *type)
{
    UNUSED(type);
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

Type* get_type(Interloper& itl, TypeNode* type_node)
{
    UNUSED(itl); UNUSED(type_node);
    assert(false);
}

// TODO: do we want to pass the operation in here for when we support overloading?
Type* effective_arith_type(Interloper& itl,const Type *ltype, const Type *rtype)
{
    UNUSED(itl); UNUSED(ltype); UNUSED(rtype);
    assert(false);
}

void check_logical_operation(Interloper& itl,const Type *ltype, const Type *rtype, logic_op type)
{
    UNUSED(itl); UNUSED(ltype); UNUSED(rtype); UNUSED(type);
    assert(false);
}

void check_assign(Interloper& itl,const Type *ltype, const Type *rtype, bool is_arg = false, bool is_initializer = false)
{
    UNUSED(itl); UNUSED(ltype); UNUSED(rtype);
    UNUSED(is_arg); UNUSED(is_initializer);
    assert(false);
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
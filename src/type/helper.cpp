b32 fit_into_s8(s64 v1)
{
    return in_range<s64>(v1,-128,127);
}

b32 fit_into_s32(s64 v1)
{
    return in_range<s64>(v1,s32(-(0xffffffff / 2)),s32(0xffffffff / 2));
}


b32 fit_into_u32(s64 v1)
{
    return in_range<u64>(v1,0,0xffffffff);
}

Option<itl_error> type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype, assign_type assign_kind);
Option<itl_error> parse_def(Interloper& itl, TypeDef& def);

b32 is_builtin(const Type* type)
{
    return type->kind == type_class::builtin_t;
}

b32 is_integer(builtin_type type)
{    
    return builtin_type_info[u32(type)].is_integer;
}

b32 is_integer(const Type* type)
{
    if(!is_builtin(type))
    {
        return false;
    }

    const BuiltinType* underlying = (BuiltinType*)type;
    return is_integer(underlying->builtin);
}

bool is_builtin_type(const Type* type,builtin_type builtin)
{
    if(!is_builtin(type))
    {
        return false;
    }

    const BuiltinType* underlying = (BuiltinType*)type;
    return underlying->builtin == builtin;
}


b32 is_float(const Type* type)
{
    return is_builtin_type(type,builtin_type::f64_t);
}

b32 is_signed(const Type *type)
{
    if(!is_builtin(type))
    {
        return false;
    }

    const BuiltinType* underlying = (BuiltinType*)type;
    return builtin_type_info[u32(underlying->builtin)].is_signed;
}

b32 is_signed_integer(const Type *type)
{
    return is_signed(type) && is_integer(type);
}

b32 is_unsigned_integer(const Type *type)
{
    return !is_signed(type) && is_integer(type);
}


b32 is_array(const Type* type)
{
    return type->kind == type_class::array_t;
}

b32 is_struct(const Type* type)
{
    return type->kind == type_class::struct_t;
}

b32 is_struct_index(const Type* type, u32 idx)
{
    return is_struct(type) && ((StructType*)type)->struct_idx == idx;
}

b32 is_func_pointer(const Type* type)
{
    return type->kind == type_class::func_pointer_t;
}


b32 is_enum(const Type* type)
{
    return type->kind == type_class::enum_t;
}

b32 is_pointer(const Type* type)
{
    return type->kind == type_class::pointer_t;
}

b32 is_reference(const Type* type)
{
    return type->kind == type_class::pointer_t && ((PointerType*)type)->pointer_kind == pointer_type::reference;
}

b32 is_bool(const Type* type)
{
    return is_builtin_type(type,builtin_type::bool_t);
}

b32 is_void(const Type* type)
{
    return is_builtin_type(type,builtin_type::void_t);
}

// for arrays
b32 is_runtime_size(const ArrayType* type)
{
    return type->size == RUNTIME_SIZE;
}

b32 is_runtime_size(const Type* type)
{
    return is_runtime_size((ArrayType*)type);
}

b32 is_fixed_array(const ArrayType* type)
{
    return !is_runtime_size(type);
}

b32 is_fixed_array(const Type* type)
{
    return is_array(type) && !is_runtime_size(type);
}

b32 is_vla(const Type* type)
{
    return is_array(type) && is_runtime_size(type);
}

b32 is_trivial_copy(const Type *type)
{
    return is_builtin(type) || is_pointer(type) || is_enum(type) || is_func_pointer(type) || is_fixed_array(type);
}


b32 is_string(const ArrayType* type)
{
    return is_builtin_type(type->contained_type,builtin_type::c8_t);
}

b32 is_string(const Type* type)
{
    if(is_array(type))
    {
        return is_string((ArrayType*)type);
    }

    return false;
}


b32 is_const(const Type* type)
{
    return type->flags & TYPE_FLAG_CONST;
}

b32 is_const_string(const ArrayType* type)
{
    return is_builtin_type(type->contained_type,builtin_type::c8_t) && is_const((Type*)type->contained_type);
}

b32 is_const_string(const Type* type)
{
    if(is_array(type))
    {
        return is_const_string((ArrayType*)type);
    }

    return false;
}


b32 is_plain(const Type *type)
{
    return !is_pointer(type) && !is_array(type);
}

b32 is_plain_builtin(const Type* type)
{
    return is_plain(type) && is_builtin(type);
}

b32 is_value_type(const Type* type)
{
    return is_plain(type);
}

u32 builtin_size(builtin_type t)
{
    return builtin_type_info[u32(t)].size;
}

u64 builtin_max(builtin_type t)
{
    return builtin_type_info[u32(t)].max;
}

u64 builtin_min(builtin_type t)
{
    return builtin_type_info[u32(t)].min;
}

const char *builtin_type_name(builtin_type t)
{
    return TYPE_NAMES[static_cast<size_t>(t)];
}

builtin_type cast_builtin(const Type *type)
{
    const BuiltinType* underlying = (BuiltinType*)type;
    return underlying->builtin;
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

Type* index_arr(ArrayType* array_type)
{
    return array_type->contained_type;
}

const Type* index_arr(const Type* type)
{
    ArrayType* array_type = (ArrayType*)type;
    return array_type->contained_type;
}

Type* index_arr(Type* type)
{
    ArrayType* array_type = (ArrayType*)type;
    return array_type->contained_type;
}

// gives back first type that isn't an array
Type* contained_arr_type(ArrayType* array_type)
{
    while(is_array(array_type->contained_type))
    {
        array_type = (ArrayType*)array_type->contained_type;
    }

    return array_type->contained_type;
}


// gives back the absolute bottom type
Type* get_plain_type(Type* type)
{
    for(;;)
    {
        switch(type->kind)
        {
            case type_class::array_t:
            {
                type = index_arr(type);
                break;
            }

            case type_class::pointer_t:
            {
                type = deref_pointer(type);
                break;
            }

            default:
            {
                return type; 
            }
        }
    }

    assert(false);
}


// type creation helpers
template<typename T>
Type* alloc_type(Interloper& itl, type_class kind, u32 flags)
{
    Type* type = (Type*)allocate(itl.type_allocator,sizeof(T));
    type->kind = kind;
    type->flags = flags;

    return type;
}


Type* make_builtin(Interloper& itl, builtin_type type, u32 flags = 0)
{
    BuiltinType* builtin = (BuiltinType*)alloc_type<BuiltinType>(itl,type_class::builtin_t,flags);
    builtin->builtin = type;

    return (Type*)builtin;
}

Type* make_pointer(Interloper& itl,Type* contained_type, pointer_type pointer_kind, u32 flags = 0)
{
    PointerType* pointer_type = (PointerType*)alloc_type<PointerType>(itl,type_class::pointer_t,flags);

    pointer_type->contained_type = contained_type;
    pointer_type->pointer_kind = pointer_kind;

    return (Type*)pointer_type;
}

Type* make_reference(Interloper& itl,Type* contained_type, u32 flags = 0)
{
    return make_pointer(itl,contained_type,pointer_type::reference,flags);
}

Type* make_nullable_ptr(Interloper& itl,Type* contained_type, u32 flags = 0)
{
    return make_pointer(itl,contained_type,pointer_type::nullable,flags);   
}


Type* make_struct(Interloper& itl, u32 struct_idx, u32 flags = 0)
{
    StructType* struct_type = (StructType*)alloc_type<StructType>(itl,type_class::struct_t,flags);

    struct_type->struct_idx = struct_idx;

    return (Type*)struct_type;
}

Type* make_enum(Interloper& itl, u32 enum_idx, u32 flags = 0)
{
    EnumType* enum_type = (EnumType*)alloc_type<EnumType>(itl,type_class::enum_t,flags);

    enum_type->enum_idx = enum_idx;

    return (Type*)enum_type;
}


Type* make_array(Interloper& itl, Type* contained_type, u32 size, u32 flags = 0)
{
    ArrayType* array_type = (ArrayType*)alloc_type<ArrayType>(itl,type_class::array_t,flags);

    array_type->size = size;
    array_type->contained_type = contained_type;
    init_arr_sub_sizes(itl,(Type*)array_type);

    return (Type*)array_type;
}

u32 promote_size(u32 size)
{
    if(size < GPR_SIZE)
    {
        return GPR_SIZE;
    }

    return size;
}


DefInfo* lookup_definition(NameSpace* root, const String& name);
void print_namespace_tree(NameSpace* root, u32 depth);

void store_arr_data(Interloper& itl, Function& func, RegSlot slot, RegSlot data);
void store_arr_len(Interloper& itl, Function& func, RegSlot slot,RegSlot len);

RegSlot mul_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm);
RegSlot udiv_imm_res(Interloper& itl, Function& func, RegSlot src,u64 imm);

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},
    {builtin_type::u64_t, true, false ,8,  0, u64(0xffffffff'ffffffff)},

    {builtin_type::s8_t, true, true, 1, u64(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  u64(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  u64(-(0xffffffff / 2)), (0xffffffff / 2)},
    {builtin_type::s64_t, true, true ,8,  u64(-(0xffffffff'ffffffff / 2)), u64(0xffffffff'ffffffff / 2)},

    {builtin_type::c8_t, true, false, 1, 0, 0xff},

    {builtin_type::byte_t, true, false, 1, 0, 0xff},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    // note: the range limits on this aren't really integral
    {builtin_type::f64_t, false, false ,8,  0, u64(0xffffffff'ffffffff)},

    {builtin_type::null_t, false,false, GPR_SIZE,0,0},

    // internal

    {builtin_type::void_t, false, false, 0, 0, 0},
};

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


String fmt_index(Interloper& itl,u32 index)
{
    if(index == RUNTIME_SIZE)
    {
        return make_string(itl.string_allocator,"[]",2);
    }

    char buf[32];
    const u32 len = sprintf(buf,"[%d]",index);

    return make_string(itl.string_allocator,buf,len);
}

void push_const_name(Interloper& itl, StringBuffer& buffer, const Type* type, const String& string)
{
    if(is_const(type))
    {
        push_string(itl.string_allocator,buffer,string);
    }
}

String type_name(Interloper& itl,const Type *type)
{
    StringBuffer prefix;

    StringBuffer compound;

    String plain;

    b32 done = false;

    while(!done)
    {
        switch(type->kind)
        {
            case type_class::pointer_t:
            {
                PointerType* pointer_type = (PointerType*)type;
                const bool nullable = pointer_type->pointer_kind == pointer_type::nullable;
                push_const_name(itl,compound,type," const");
                push_char(itl.string_allocator,compound,nullable? '?' : '@');

                type = pointer_type->contained_type;
                break;
            }

            case type_class::struct_t: 
            {
                push_const_name(itl,prefix,type,"const ");

                const auto structure =  struct_from_type(itl.struct_table,type);
                plain = structure.name;
                done = true;
                break;                
            }

            case type_class::enum_t: 
            {
                push_const_name(itl,prefix,type,"const ");

                const auto enumeration = enum_from_type(itl.enum_table,type);
                plain = enumeration.name;  
                done = true;
                break;              
            }

            case type_class::array_t:
            {
                push_const_name(itl,compound,type," const");

                ArrayType* array_type = (ArrayType*)type;

                push_string(itl.string_allocator,compound,fmt_index(itl,array_type->size));
                type = array_type->contained_type;
                break;
            }

            case type_class::func_pointer_t:
            {
                FuncPointerType* func_type = (FuncPointerType*)type;
                const FuncSig& sig = func_type->sig;
                StringBuffer func_name;

                push_const_name(itl,prefix,type,"const ");

                push_string(itl.string_allocator,func_name,"func(");

                // print args
                // TODO: this should probably hide hidden args...
                for(u32 a = 0; a < count(sig.args); a++)
                {
                    if(a != 0)
                    {
                        push_char(itl.string_allocator,func_name,',');
                    }

                    const auto& sym = sym_from_slot(itl.symbol_table,sig.args[a]);

                    push_string(itl.string_allocator,func_name,sym.name);

                    push_string(itl.string_allocator,func_name," : ");

                    push_string(itl.string_allocator,func_name,type_name(itl,sym.type));
                }

                push_string(itl.string_allocator,func_name,") ");


                // single return
                if(count(sig.return_type) == 1)
                {
                    push_string(itl.string_allocator,func_name,type_name(itl,sig.return_type[0]));
                }

                // tuple return
                else
                {
                    assert(false);
                }

                // null term the string
                push_char(itl.string_allocator,func_name,'\0');

                plain = make_string(func_name);
                done = true;
                break;
            }

            // builtin
            case type_class::builtin_t:
            {
                push_const_name(itl,prefix,type,"const ");

                plain = builtin_type_name(cast_builtin(type));
                done = true;
                break;
            }

            case type_class::tuple_t:
            {
                assert(false);
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

static constexpr u32 ENUM_SIZE = 4;

u32 type_size(Interloper& itl,const Type *type)
{
    switch(type->kind)
    {
        case type_class::func_pointer_t:
        {
            return GPR_SIZE;
        }

        case type_class::struct_t:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.size;
        }

        case type_class::enum_t:
        {
            const auto& enumeration = enum_from_type(itl.enum_table,type);

            // return size of underyling integeral type
            if(enumeration.underlying_type && is_integer(enumeration.underlying_type))
            {
                return type_size(itl,enumeration.underlying_type);
            }

            return ENUM_SIZE;
        }

        case type_class::array_t:
        {
            if(is_runtime_size(type))
            {
                return GPR_SIZE * 2;
            }

            return GPR_SIZE;            
        }

        case type_class::pointer_t:
        {
            return GPR_SIZE;
        }

        case type_class::tuple_t:
        {
            assert(false);
        }

        case type_class::builtin_t:
        {
            const BuiltinType* underlying = (BuiltinType*)type;
            return builtin_size(underlying->builtin);
        }
    }

    assert(false);
}


u32 array_memory_size(Interloper& itl, const ArrayType* array)
{
    UNUSED(itl);

    if(is_runtime_size(array))
    {
        return GPR_SIZE * 2;
    }
    
    // Fixed array, give back the actual size occupied by the buffer
    return array->sub_size * array->size;
}

u32 type_memory_size(Interloper& itl, const Type* type)
{
    switch(type->kind)
    {
        case type_class::array_t:
        {
            return array_memory_size(itl,(ArrayType*)type);
        }

        default:
        {
            return type_size(itl,type);
        }
    }
}

u32 data_size(Interloper& itl,const Type *type)
{
    switch(type->kind)
    {
        case type_class::struct_t:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.data_size;
        }   

        case type_class::array_t:
        {
            return array_memory_size(itl,(ArrayType*)type);
        }

        default:
        {
            return type_size(itl,type);
        }
    }
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


u32 accumulate_count(u32 count, u32 size)
{
    // if count is zero we are just getting started
    return count == 0? size : count * size;
}

void init_arr_sub_sizes(Interloper&itl,Type* type);

u32 init_arr_sub_sizes_internal(Interloper& itl, Type* type)
{
    /* 
        descend until the bottom is reached mark the size
        return up the sub size and mark it across each level of the type


        do this for any sub indirecitons i.e pointers VLA's
    */

    switch(type->kind)
    {
        case type_class::array_t:
        {
            ArrayType* array_type = (ArrayType*)type;
                
            // VLA
            if(is_runtime_size(type))
            {
                array_type->sub_size = init_arr_sub_sizes_internal(itl,index_arr(type));
                return GPR_SIZE * 2;
            }

            // fixed size
            else
            {
                array_type->sub_size = init_arr_sub_sizes_internal(itl,index_arr(type));
                return array_type->sub_size * array_type->size;
            }
        }

        
        case type_class::pointer_t:
        {
            PointerType* pointer_type = (PointerType*)type; 

            // do for sub type
            if(is_array(pointer_type->contained_type))
            {
                init_arr_sub_sizes(itl,pointer_type->contained_type);
            }

            return GPR_SIZE;
        }

        // plain var mark size and return it!
        default:
        {
            const u32 size = type_size(itl,type);
            return size;
        }
    }
}


void init_arr_sub_sizes(Interloper&itl,Type* type)
{
    init_arr_sub_sizes_internal(itl,type);
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


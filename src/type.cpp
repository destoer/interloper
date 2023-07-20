#include <interloper.h>

const BuiltinTypeInfo builtin_type_info[BUILTIN_TYPE_SIZE] =
{
    {builtin_type::u8_t, true, false, 1, 0, 0xff},
    {builtin_type::u16_t, true, false ,2, 0, 0xffff},
    {builtin_type::u32_t, true, false ,4, 0, 0xffffffff},

    {builtin_type::s8_t, true, true, 1, u32(-(0xff / 2)), (0xff / 2)},
    {builtin_type::s16_t, true, true ,2,  u32(-(0xffff / 2)), (0xffff / 2)},
    {builtin_type::s32_t, true, true ,4,  u32(-(0xffffffff / 2)), (0xffffffff / 2)},

    {builtin_type::c8_t, true, false, 1, 0, 0xff},

    {builtin_type::byte_t, true, false, 1, 0, 0xff},

    {builtin_type::bool_t, false, false ,1,  0, 1},

    {builtin_type::null_t, false,false, GPR_SIZE,0,0},

    // internal

    {builtin_type::void_t, false, false, 0, 0, 0},
};

void type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype);
void parse_def(Interloper& itl, TypeDef& def);

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
b32 is_runtime_size(const ArrayType* type)
{
    const ArrayType* array_type = (ArrayType*)type;

    return array_type->size == RUNTIME_SIZE;
}

b32 is_runtime_size(const Type* type)
{
    return is_runtime_size((ArrayType*)type);
}

b32 is_vla(const Type* type)
{
    if(is_array(type))
    {
        return is_runtime_size(type);
    }

    return false;    
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


b32 is_string(const Type* type)
{
    if(is_array(type))
    {
        const ArrayType* array_type = (ArrayType*)type;

        return array_type->contained_type->type_idx == u32(builtin_type::c8_t);
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
const Type* get_plain_type(const Type* type)
{
    for(;;)
    {
        switch(type->type_idx)
        {
            case ARRAY:
            {
                type = index_arr(type);
                break;
            }

            case POINTER:
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
        decsned until the bottom is reached mark the size
        return up the sub size and mark it across each level of the type


        do this for any sub indirecitons i.e pointers VLA's
    */


    switch(type->type_idx)
    {
        case ARRAY:
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

        
        case POINTER:
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


// total block size of the array
u32 arr_size(const Type* type)
{
    const ArrayType* array_type = (ArrayType*)type;

    if(array_type->size == RUNTIME_SIZE)
    {
        return RUNTIME_SIZE;
    }

    return array_type->size * array_type->sub_size;
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


Type* make_raw(Interloper& itl, u32 type, b32 is_constant = false)
{
    return alloc_type<Type>(itl,type,is_constant);
}

Type* make_builtin(Interloper& itl, builtin_type type, b32 is_const = false)
{
    return alloc_type<Type>(itl,u32(type),is_const);
}


Type* make_pointer(Interloper& itl,Type* contained_type, b32 is_constant = false)
{
    PointerType* pointer_type = (PointerType*)alloc_type<PointerType>(itl,POINTER,is_constant);

    pointer_type->contained_type = contained_type;

    return (Type*)pointer_type;
}

Type* make_struct(Interloper& itl, u32 struct_idx, b32 is_constant = false)
{
    StructType* struct_type = (StructType*)alloc_type<StructType>(itl,STRUCT,is_constant);

    struct_type->struct_idx = struct_idx;

    return (Type*)struct_type;
}

Type* make_enum(Interloper& itl, u32 enum_idx, b32 is_constant = false)
{
    EnumType* enum_type = (EnumType*)alloc_type<EnumType>(itl,ENUM,is_constant);

    enum_type->enum_idx = enum_idx;

    return (Type*)enum_type;
}


Type* make_array(Interloper& itl, Type* contained_type, u32 size, b32 is_constant = false)
{
    ArrayType* array_type = (ArrayType*)alloc_type<ArrayType>(itl,ARRAY,is_constant);

    array_type->size = size;
    array_type->contained_type = contained_type;
    init_arr_sub_sizes(itl,(Type*)array_type);

    return (Type*)array_type;
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
    if(type->is_const)
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
        switch(type->type_idx)
        {
            case POINTER:
            {
                push_const_name(itl,compound,type," const");
                push_char(itl.string_allocator,compound,'@');

                PointerType* pointer_type = (PointerType*)type;
                type = pointer_type->contained_type;
                break;
            }

            case STRUCT: 
            {
                push_const_name(itl,prefix,type,"const ");

                const auto structure =  struct_from_type(itl.struct_table,type);
                plain = structure.name;
                done = true;
                break;                
            }

            case ENUM: 
            {
                push_const_name(itl,prefix,type,"const");

                const auto enumeration = enum_from_type(itl.enum_table,type);
                plain = enumeration.name;  
                done = true;
                break;              
            }

            case ARRAY:
            {
                push_const_name(itl,compound,type," const");

                ArrayType* array_type = (ArrayType*)type;

                push_string(itl.string_allocator,compound,fmt_index(itl,array_type->size));
                type = array_type->contained_type;
                break;
            }

            // builtin
            default:
            {
                push_const_name(itl,prefix,type,"const ");

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

void print_type(Interloper& itl, const Type* type)
{
    printf("type: %s\n",type_name(itl,type).buf);
}

// NOTE: 
// to be used externally when attempting to find a type decl
// dont look it up in the type table directly as the definition might not
// have been parsed yet
TypeDecl* lookup_type(Interloper& itl,const String& name)
{
    TypeDecl* user_type = lookup(itl.type_table,name);

    // currently type does not exist
    // attempt to parse the def
    if(!user_type)
    {
        // look if there is a defintion for this type!
        // if there is not then we have an error
        TypeDef *def_ptr = lookup(itl.type_def,name);

        // no such definiton exists
        // NOTE: this is allowed to not panic the 
        // caller is expected to check the pointer and not just
        // compiler error state
        if(!def_ptr)
        {
            return nullptr;
        }

        // okay attempt to parse the def
        TypeDef& def = *def_ptr;
        parse_def(itl,def);

        // def parsing failed in some fashion just bail out
        // there are no options left
        if(itl.error)
        {
            return nullptr;
        }

        // okay now we have the type
        user_type = lookup(itl.type_table,name);
    }

    return user_type;
}

Type* get_type(Interloper& itl, TypeNode* type_decl,u32 struct_idx_override = INVALID_TYPE, b32 complete_type = false)
{
    Type* type = nullptr;

    // override that makes entire type constant
    // i.e arrays, structs, pointers, base
    const b32 is_constant = type_decl->is_constant;

    if(struct_idx_override != INVALID_TYPE)
    {
        type = make_struct(itl,struct_idx_override,is_constant);
    }

    else if(type_decl->type_idx == USER_TYPE)
    {
        const auto name = type_decl->name;

        TypeDecl* user_type = lookup_type(itl,name);

        if(!user_type)
        {
            panic(itl,itl_error::undeclared,"no such type %s\n",type_decl->name);
            return make_builtin(itl,builtin_type::void_t);
        }

        switch(user_type->kind)
        {
            case type_kind::struct_t:
            {
                type = make_struct(itl,user_type->type_idx,is_constant); 
                break;
            }

            case type_kind::enum_t:
            {
                type = make_enum(itl,user_type->type_idx,is_constant);
                break;
            }

            case type_kind::alias_t:
            {
                TypeAlias alias = itl.alias_table[user_type->type_idx];

                type = alias.type;
                break;
            }

            default: assert(false);
        }       
    }

    else
    {
        type = make_raw(itl,type_decl->type_idx,is_constant);
    }

    if(!is_constant)
    {
        type->is_const = type_decl->is_const;
    }

    // arrays, pointers
    for(s32 c = count(type_decl->compound_type) - 1; c >= 0; c--)
    {
        AstNode* node = type_decl->compound_type[c];

        switch(node->type)
        {
            // pointer to current type
            case ast_type::ptr_indirection:
            {
                type = make_pointer(itl,type,is_constant);
                break;
            }

            case ast_type::arr_var_size:
            {
                type = make_array(itl,type,RUNTIME_SIZE,is_constant);

                break;
            }

            // TODO: we need to revise the decl order on this!
            case ast_type::arr_fixed:
            {
                UnaryNode* unary_node = (UnaryNode*)node;

                const auto [size,int_type] = compile_const_int_expression(itl,unary_node->next);

                type = make_array(itl,type,size,is_constant);
                break;
            }

            // TODO: if we allready have a indireciton that aint flat, i.e a vla or pointer
            // this aint legal!
            case ast_type::arr_deduce_size:
            {
                if(complete_type)
                {
                    panic(itl,itl_error::mismatched_args,"type is constant and cannot be deduced by assign\n");
                    return make_builtin(itl,builtin_type::void_t);
                }

                type = make_array(itl,type,DEDUCE_SIZE,is_constant);

                break;
            }

            default:
            {
                panic(itl,itl_error::invalid_expr,"invalid type specifier: %s\n",AST_NAMES[u32(node->type)]);
                return make_builtin(itl,builtin_type::void_t);
            }
        }
    }


    return type;
}

// get back a type that does not need further deduction i.e no size deduction
Type* get_complete_type(Interloper& itl, TypeNode* type_decl)
{
    return get_type(itl,type_decl,INVALID_TYPE,true);
}

// TODO: this is more restrictive than required atm
b32 def_has_indirection(TypeNode *type_decl)
{
    return count(type_decl->compound_type);
}


Type* value_type(Interloper& itl,const Value& value)
{
    if(value.sign)
    {
        const s32 v = value.v;

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,s32(builtin_min(builtin_type::s8_t)),s32(builtin_max(builtin_type::s8_t))))
        {
            return  make_builtin(itl,builtin_type::s8_t);
        }

        else if(in_range(v,s32(builtin_min(builtin_type::s16_t)),s32(builtin_max(builtin_type::s16_t))))
        {
            return make_builtin(itl,builtin_type::s16_t);
        }

        //else if(v,s32(builtin_min(builtin_type::s32_t)),s32(builtin_max(builtin_type::s32_t)))
        else
        {
            return make_builtin(itl,builtin_type::s32_t);
        }
    }

    else
    {
        const u32 v = value.v;

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,builtin_min(builtin_type::u8_t),builtin_max(builtin_type::u8_t)))
        {
            return  make_builtin(itl,builtin_type::u8_t);
        }

        else if(in_range(v,builtin_min(builtin_type::u16_t),builtin_max(builtin_type::u16_t)))
        {
            return make_builtin(itl,builtin_type::u16_t);
        }

        //else if(in_range(v,builtin_min(builtin_type::u32_t),builtin_max(builtin_type::u32_t))
        else
        {
            return make_builtin(itl,builtin_type::u32_t);
        }        
    }    
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
            panic(itl,itl_error::undefined_type_oper,"arithmetic operation undefined for %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
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
                panic(itl,itl_error::int_type_error,"logical comparision on different signs %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
        }

        // both bool
        else if(builtin_r == builtin_type::bool_t && builtin_l == builtin_type::bool_t)
        {
            
        }

        // something else
        else
        {
            panic(itl,itl_error::undefined_type_oper,"logical operation undefined for %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
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
            panic(itl,itl_error::enum_type_error,"comparision on enums is only defined for '==' and '!='");
            return;
        }

        if(ltype->type_idx != rtype->type_idx)
        {
            panic(itl,itl_error::enum_type_error,"expected enum of the same type for comparsions %s : %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }
    }

    // no matching operator
    else 
    {
        panic(itl,itl_error::undefined_type_oper,"logical operation on user defined type: %s : %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }   
}


enum class assign_type
{
    assign,
    arg,
    initializer,
};


void check_const_internal(Interloper&itl, const Type* ltype, const Type* rtype, assign_type type, b32 was_pointer)
{
    if(ltype->is_const)
    {
        // can only assign for arg or initalizer everything
        // unless this is a pointer, if so it is legal as long as the ltype of the contained type is const
        if(type == assign_type::assign && !was_pointer)
        {
            panic(itl,itl_error::const_type_error,"cannot assign rtype to const ltype: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }
    }

    if(rtype->is_const)
    {
        // ltype must be const too
        if(!ltype->is_const)
        {
            panic(itl,itl_error::const_type_error,"cannot assign const rtype to ltype: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;            
        }
    }

    // neither is const is fine in any context
}

// NOTE: this is expected to be called after main sets of type checking
// so this function assumes that every type is the of the same kind at every level
void check_const(Interloper&itl, const Type* ltype, const Type* rtype, assign_type type)
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

    // TODO: how does this play with heavily nested types?
    // was a pointer in the type "above"
    b32 was_pointer = false;

    // check const specifiers at every level
    while(!done)
    {
        switch(ltype->type_idx)
        {
            case ARRAY:
            {
                check_const_internal(itl,ltype,rtype,type,was_pointer);

                // check sub types
                ltype = index_arr(ltype);
                rtype = index_arr(rtype);

                was_pointer = false;
                break;
            }

            case POINTER:
            {
                check_const_internal(itl,ltype,rtype,type,was_pointer);

                // check sub types
                ltype = deref_pointer(ltype);
                rtype = deref_pointer(rtype);

                was_pointer = true;
                break;
            }

            case STRUCT:
            {
                check_const_internal(itl,ltype,rtype,type,was_pointer);
                done = true;
                break;
            }

            case ENUM:
            {
                check_const_internal(itl,ltype,rtype,type,was_pointer);
                done = true;
                break;
            }

            // check end type
            default:
            {
                check_const_internal(itl,ltype,rtype,type,was_pointer);
                done = true;
                break;
            }
        }
    }
}

b32 is_plain_type(const Type* type)
{
    return !is_array(type) && !is_struct(type);
}

b32 plain_type_equal(const Type* ltype, const Type* rtype)
{
    switch(ltype->type_idx)
    {
        case STRUCT:
        {
            StructType* struct_ltype = (StructType*)ltype;
            StructType* struct_rtype = (StructType*)rtype;

            return struct_ltype->struct_idx == struct_rtype->struct_idx;
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


    // any ptr rtype is compatible with byte ptr ltype
    if(ltype->type_idx == POINTER && rtype->type_idx == POINTER)
    {
        const Type* base_type_ltype = deref_pointer(ltype);
        const Type* base_type_rtype = deref_pointer(rtype);

        if(base_type_ltype->type_idx == u32(builtin_type::byte_t))
        {
            return;
        }

        // anything of NULL is implictly converted
        if(base_type_rtype->type_idx == u32(builtin_type::null_t))
        {
            return;
        }
    }


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
        panic(itl,itl_error::pointer_type_error,"expected pointer of type %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }

    // anything else
    else
    {
        // if base types still aernt equal we have a problem!
        if(!plain_type_equal(ltype,rtype))
        {
            panic(itl,itl_error::pointer_type_error,"expected pointer of type %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }
    }
}



void check_assign_plain(Interloper& itl, const Type* ltype, const Type* rtype)
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
                panic(itl,itl_error::int_type_error,"narrowing conversion %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }

            // unsigned cannot assign to signed
            // TODO: do we want to be this pedantic with integer conversions?
            if(!is_signed(ltype) && is_signed(rtype))
            {
                panic(itl,itl_error::int_type_error,"unsigned = signed (%s = %s)\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
        }

        // something else (probably by here we only want the same types to be allowed)
        // i.e when we add a boolean type or pointers etc
        else
        {
            // void is not assignable!
            if(builtin_r == builtin_type::void_t || builtin_l == builtin_type::void_t)
            {
                panic(itl,itl_error::undefined_type_oper,"void assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
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
        StructType* struct_ltype = (StructType*)ltype;
        StructType* struct_rtype = (StructType*)rtype;

        if(struct_ltype->struct_idx != struct_rtype->struct_idx)
        {
            panic(itl,itl_error::struct_error,"struct assign of different types %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }
    }

    else if(is_enum(ltype) && is_enum(rtype))
    {
        EnumType* enum_ltype = (EnumType*)ltype;
        EnumType* enum_rtype = (EnumType*)rtype;

        if(enum_ltype->enum_idx != enum_rtype->enum_idx)
        {
            panic(itl,itl_error::struct_error,"struct assign of different types %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            return;
        }        
    }

    else
    {
        panic(itl,itl_error::undefined_type_oper,"cannot assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
    }
}

void check_assign_internal(Interloper& itl,const Type *ltype, const Type *rtype, assign_type type)
{
    const Type* ltype_copy = ltype;
    const Type* rtype_copy = rtype;

    if(is_plain(rtype) && is_plain(ltype))
    {
        check_assign_plain(itl,ltype,rtype);
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
            // type idx along with the indirection, and contain type
            // must be the same
            if(!is_array(rtype))
            {
                panic(itl,itl_error::array_type_error,"expected array of %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                return;
            }

            // single dimension can trivally convert to a vla
            // for multi dimensional arrays all sizes must match
            if(!is_array(index_arr(ltype)) && !is_array(index_arr(rtype)))
            {
                ArrayType* array_ltype = (ArrayType*)ltype;
                ArrayType* array_rtype = (ArrayType*)rtype;

                if(type != assign_type::arg)
                {
                    if(!is_runtime_size(ltype))
                    {
                        panic(itl,itl_error::array_type_error,"%s = %s, cannot assign to fixed size array\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                        return;
                    }
                } 

                // any rtype size is legal in this context if ltype is a vla
                if(!is_runtime_size(ltype))
                {
                    // must be same size
                    if(array_ltype->size != array_rtype->size)
                    {
                        panic(itl,itl_error::array_type_error,"expected array of size %d got %d\n",array_ltype->size,array_rtype->size);
                        return;
                    }
                }

                ltype = index_arr(ltype);
                rtype = index_arr(rtype);              
            } 


            b32 done = false;

            while(!done)
            {
                if(ltype->type_idx != rtype->type_idx)
                {
                    panic(itl,itl_error::array_type_error,"expected array of underlying type %s got %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                    return;
                }

                switch(ltype->type_idx)
                {
                    case ARRAY:
                    {
                        ArrayType* array_ltype = (ArrayType*)ltype;
                        ArrayType* array_rtype = (ArrayType*)rtype;

                        // for arg passing only
                        // valid
                        // [3] = [3]

                        if(type != assign_type::arg)
                        {
                            if(!is_runtime_size(ltype))
                            {
                                panic(itl,itl_error::array_type_error,"%s = %s, cannot assign to fixed size array\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                                return;
                            }
                        }

                        // must be same size
                        if(array_ltype->size != array_rtype->size)
                        {
                            // provide better error messagee for vlas
                            if(is_runtime_size(ltype) != is_runtime_size(rtype))
                            {
                                panic(itl,itl_error::array_type_error,"%s = %s, cannot assign different array types beyond 1d\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
                                return;                            
                            }

                            panic(itl,itl_error::array_type_error,"expected array of size %d got %d\n",array_ltype->size,array_rtype->size);
                            return;
                        }
                            

                        ltype = index_arr(ltype);
                        rtype = index_arr(rtype);                                       
                        
                        break;
                    }

                    case POINTER:
                    {
                        assert(false);
                    }

                    default:
                    {
                        done = true;
                    }
                }
            }

            // finally type check the base type!
            check_assign_plain(itl,ltype,rtype);
        }

        else
        {
            panic(itl,itl_error::mismatched_args,"cannot assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }
    }


    if(!itl.error)
    {
        // we know this will descend properly so check const!
        check_const(itl,ltype_copy,rtype_copy,type);
    }
}

// check ordinary assign
void check_assign(Interloper& itl,const Type *ltype, const Type *rtype)
{
    check_assign_internal(itl,ltype,rtype,assign_type::assign);
}

void check_assign_arg(Interloper& itl, const Type* ltype, const Type* rtype)
{
    // args behave the same as initalizers
    check_assign_internal(itl,ltype,rtype,assign_type::arg);
}

void check_assign_init(Interloper& itl, const Type* ltype, const Type* rtype)
{
    check_assign_internal(itl,ltype,rtype,assign_type::initializer);
}


void handle_cast(Interloper& itl,Function& func, SymSlot dst_slot,SymSlot src_slot,const Type *old_type, const Type *new_type)
{
    if(itl.error)
    {
        return;
    }

    // handle side effects of the cast
    // builtin type
    if(is_plain_builtin(old_type) && is_plain_builtin(new_type))
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
                        sign_extend_byte(itl,func,dst_slot,src_slot);
                        break;
                    }

                    case builtin_type::s16_t:
                    {
                        sign_extend_half(itl,func,dst_slot,src_slot);
                        break;
                    }

                    default: crash_and_burn("invalid signed integer upcast");
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
                        and_imm(itl,func,dst_slot,src_slot,0xff);
                        break;
                    }

                    case 2:  
                    {
                        and_imm(itl,func,dst_slot,src_slot,0xffff);
                        break;
                    }

                    default: crash_and_burn("invalid signed integer downcast");
                }
            }

            // cast doesnt do anything but move into a tmp so the IR doesnt break
            else
            {
                mov_reg(itl,func,dst_slot,src_slot);
            }

        }

        // bool to integer
        else if(builtin_old == builtin_type::bool_t && is_integer(new_type))
        {
            // do nothing 0 and 1 are fine as integers
            // we do want this to require a cast though so conversions have to be explicit
            mov_reg(itl,func,dst_slot,src_slot);
        } 

        // integer to bool
        // if integer is > 0, its true else false
        else if(is_integer(old_type) && builtin_new == builtin_type::bool_t)
        {
            if(is_signed(old_type))
            {
                cmp_signed_gt_imm(itl,func,dst_slot,src_slot,0);
            }

            // unsigned
            else
            {
                cmp_unsigned_gt_imm(itl,func,dst_slot,src_slot,0);
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
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // as is integer to enum
    else if(is_integer(old_type) && is_enum(new_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // cast does nothing just move the reg, its only acknowledgement your doing something screwy
    else if(is_pointer(old_type) && is_pointer(new_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // pointer to int is fine
    else if(is_pointer(old_type) && is_integer(new_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // int to point is fine usually...
    else if(is_integer(old_type) && is_pointer(new_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // probably only pointers are gonna valid for casts here
    else
    {
        unimplemented("handle cast user defined type %s -> %s!\n",type_name(itl,old_type).buf,type_name(itl,new_type).buf);        
    }

}


Type* access_builtin_type_info(Interloper& itl, Function& func, SymSlot dst_slot, builtin_type type, const String& member_name)
{
    const BuiltinTypeInfo& info = builtin_type_info[u32(type)];

    if(member_name == "size")
    {
        mov_imm(itl,func,dst_slot,info.size);
        return make_builtin(itl,builtin_type::u32_t);
    }

    panic(itl,itl_error::undefined_type_oper,"unknown type info for builtin type %s.%s\n",TYPE_NAMES[u32(type)],member_name.buf);
    return make_builtin(itl,builtin_type::void_t);
}

Type* access_type_info(Interloper& itl, Function& func, SymSlot dst_slot, const TypeDecl& type_decl, const String& member_name)
{
    switch(type_decl.kind)
    {
        case type_kind::builtin:
        {
            builtin_type type = builtin_type(type_decl.type_idx);

            return access_builtin_type_info(itl,func,dst_slot,type,member_name);
        }

        case type_kind::struct_t:
        {
            if(member_name == "size")
            {
                const auto& structure = itl.struct_table[type_decl.type_idx];
                const u32 size = structure.size;

                mov_imm(itl,func,dst_slot,size);

                return make_builtin(itl,builtin_type::u32_t);
            }

            return make_builtin(itl,builtin_type::void_t);
        }

        case type_kind::enum_t:
        {
            if(member_name == "len")
            {
                const auto enumeration = itl.enum_table[type_decl.type_idx];

                const u32 enum_len = enumeration.member_map.size;

                mov_imm(itl,func,dst_slot,enum_len);

                return make_builtin(itl,builtin_type::u32_t);
            }

            else
            {
                panic(itl,itl_error::enum_type_error,"unknown type info for enum %s\n",type_decl.name.buf);
                return make_builtin(itl,builtin_type::void_t);
            }
        }

        // TODO: if this is plain type pass back the type info for the underlying type
        // else fail
        case type_kind::alias_t:
        {
            assert(false);
        }
    }

    assert(false);
}


void add_type_decl(Interloper& itl, u32 type_idx, const String& name, type_kind kind)
{
    TypeDecl type_decl;

    type_decl.type_idx = type_idx;
    type_decl.name = name;
    type_decl.kind = kind;

    add(itl.type_table,type_decl.name,type_decl);    
}


void add_type_def(Interloper& itl, def_kind kind,AstNode* root, const String& name, const String& filename)
{
    TypeDef def;

    def.name = name;
    def.filename = filename;
    def.root = root;
    def.kind = kind;
    def.state = def_state::not_checked;

    add(itl.type_def,def.name,def);
}

b32 type_exists(Interloper& itl, const String& name)
{
    return contains(itl.type_table,name);
}


void parse_struct_def(Interloper& itl, TypeDef& def);
void parse_alias_def(Interloper& itl, TypeDef& def);

void parse_def(Interloper& itl, TypeDef& def)
{
    if(def.state == def_state::not_checked)
    {
        // mark as checking to lock this against recursion!
        def.state = def_state::checking;

        switch(def.kind)
        {
            case def_kind::struct_t:
            {
                parse_struct_def(itl,def);
                break;
            }

            case def_kind::alias_t:
            {
                parse_alias_def(itl,def);
                break;
            }

            // NOTE: for now there is no need to parse enums late so this just stays the same
            case def_kind::enum_t: 
            {
                assert(false);
                break;
            }
        }

        def.state = def_state::checked;
    }

    else
    {
        // TODO: add huertsics to scan for where!
        panic(itl,itl_error::black_hole,"type %s is recursively defined\n",def.name.buf);
        return;
    }
}


void destroy_func(Function& func)
{
    destroy_arr(func.args);
    destroy_arr(func.return_type);

    for(u32 r = 0; r < count(func.registers); r++)
    {
        destroy_reg(func.registers[r]);
    }

    destroy_arr(func.registers);
    destroy_emitter(func.emitter);
}
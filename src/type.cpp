#include <interloper.h>

DefInfo* lookup_definition(NameSpace* root, const String& name);
void print_namespace_tree(NameSpace* root, u32 depth);

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

    // note: the range limits on this aernt really integeral
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

void type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype);
void parse_def(Interloper& itl, TypeDef& def);

b32 is_builtin(const Type* type)
{
    return type->type_idx  < BUILTIN_TYPE_SIZE;
}

b32 is_integer(builtin_type type)
{
    return builtin_type_info[u32(type)].is_integer;
}

b32 is_integer(const Type* type)
{
    return is_builtin(type) && is_integer(builtin_type(type->type_idx));
}

b32 is_float(const Type* type)
{
    return builtin_type(type->type_idx) == builtin_type::f64_t;
}

b32 is_signed(const Type *type)
{
    return is_builtin(type) && builtin_type_info[type->type_idx].is_signed;
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
    return type->type_idx == ARRAY;
}

b32 is_struct(const Type* type)
{
    return type->type_idx == STRUCT;
}

b32 is_func_pointer(const Type* type)
{
    return type->type_idx == FUNC_POINTER;
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

b32 is_void(const Type* type)
{
    return type->type_idx == u32(builtin_type::void_t);
}

b32 is_trivial_copy(const Type *type)
{
    return is_builtin(type) || is_pointer(type) || is_enum(type) || is_func_pointer(type);
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

b32 is_string(const ArrayType* type)
{
    return type->contained_type->type_idx == u32(builtin_type::c8_t);
}

b32 is_string(const Type* type)
{
    if(is_array(type))
    {
        return is_string((ArrayType*)type);
    }

    return false;
}


b32 is_const_string(const ArrayType* type)
{
    return type->contained_type->type_idx == u32(builtin_type::c8_t) && type->contained_type->is_const; 
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

b32 is_fixed_array(const ArrayType* type)
{
    return !is_runtime_size(type);
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
    return builtin_type(type->type_idx);
}

static constexpr u32 ENUM_SIZE = 4;

u32 type_size(Interloper& itl,const Type *type)
{
    switch(type->type_idx)
    {
        case FUNC_POINTER:
        {
            return GPR_SIZE;
        }

        case STRUCT:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.size;
        }

        case ENUM:
        {
            const auto& enumeration = enum_from_type(itl.enum_table,type);

            if(enumeration.kind != enum_type::int_t)
            {
                return ENUM_SIZE;
            }

            // return size of underyling integeral type
            else
            {
                return builtin_size(builtin_type(enumeration.underlying_type_idx));
            }
        }

        case ARRAY:
        {
            if(is_runtime_size(type))
            {
                return GPR_SIZE * 2;
            }

            return GPR_SIZE;            
        }

        case POINTER:
        {
            return GPR_SIZE;
        }

        default:
        {
            return builtin_size(builtin_type(type->type_idx));
        }
    }
}


u32 data_size(Interloper& itl,const Type *type)
{
    switch(type->type_idx)
    {
        case STRUCT:
        {
            const auto& structure = struct_from_type(itl.struct_table,type);
            return structure.data_size;
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
        descend until the bottom is reached mark the size
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
                push_const_name(itl,prefix,type,"const ");

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

            case FUNC_POINTER:
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

Type* copy_type(Interloper& itl, const Type* type);

Type* copy_type_internal(Interloper& itl, const Type* type)
{
    switch(type->type_idx)
    {
        case ARRAY:
        {
            ArrayType* array_type = (ArrayType*)type;
            Type* contained_type = copy_type_internal(itl,array_type->contained_type);

            return make_array(itl,contained_type,array_type->size,type->is_const);
        }

        case POINTER:
        {
            PointerType* pointer_type = (PointerType*)type;

            Type* contained_type = copy_type_internal(itl,pointer_type->contained_type);
            
            return make_pointer(itl,contained_type,type->is_const);
        }

        case STRUCT:
        {
            StructType* struct_type = (StructType*)type;

            return make_struct(itl,struct_type->struct_idx,type->is_const);
        }

        case ENUM:
        {
            EnumType* enum_type = (EnumType*)type;

            return make_enum(itl,enum_type->enum_idx,type->is_const);        
        }

        case FUNC_POINTER:
        {
            FuncPointerType* func_pointer_type = (FuncPointerType*)type;

            FuncPointerType* copy = (FuncPointerType*)alloc_type<FuncPointerType>(itl,FUNC_POINTER,true);

            const auto& sig = func_pointer_type->sig;

            for(u32 a = 0; a < count(sig.args); a++)
            {
                push_var(copy->sig.args,sig.args[a]);
            }

            for(u32 r = 0; r < count(sig.return_type); r++)
            {
                push_var(copy->sig.return_type,copy_type(itl,sig.return_type[r]));
            }

            copy->sig.va_args = sig.va_args;
            copy->sig.hidden_args = sig.hidden_args;
            copy->sig.call_stack_size = sig.call_stack_size;

            push_var(itl.func_pointer,&copy->sig);

            return (Type*)copy;
        }

        default:
        {
            return make_raw(itl,type->type_idx,type->is_const);
        }
    }    
}

Type* copy_type(Interloper& itl, const Type* type)
{
    return copy_type_internal(itl,type);
}


// NOTE: 
// to be used externally when attempting to find a type decl
// dont look it up in the type table directly as the definition might not
// have been parsed yet
TypeDecl* lookup_type_internal(Interloper& itl,NameSpace* name_space,const String& name)
{
    TypeDecl* user_type = name_space == nullptr? lookup_incomplete_decl(itl,name) : lookup_incomplete_decl_scoped(name_space,name);

    if(!user_type)
    {
        return nullptr;
    }

    // currently type does not exist
    // attempt to parse the def
    if(user_type->state != type_def_state::checked)
    {
        // no such definiton exists
        // NOTE: this is allowed to not panic the 
        // caller is expected to check the pointer and not just
        // compiler error state
        if(!(user_type->flags & TYPE_DECL_DEF_FLAG))
        {
            return nullptr;
        }

        // okay attempt to parse the def
        TypeDef& type_def = *((TypeDef*)user_type);
        parse_def(itl,type_def);

        // def parsing failed in some fashion just bail out
        // there are no options left
        if(itl.error)
        {
            return nullptr;
        }
    }

    return user_type;
}

TypeDecl* lookup_type(Interloper& itl,const String& name)
{
    return lookup_type_internal(itl,nullptr,name);
}

TypeDecl* lookup_type_scoped(Interloper& itl,NameSpace* name_space,const String& name)
{
    return lookup_type_internal(itl,name_space,name);
}

Type* make_base_type(Interloper& itl, u32 type_idx, type_kind kind, b32 is_constant)
{
    switch(kind)
    {
        case type_kind::struct_t:
        {
            return make_struct(itl,type_idx,is_constant); 
        }

        case type_kind::enum_t:
        {
            return make_enum(itl,type_idx,is_constant);
        }

        case type_kind::builtin:
        {
            return make_builtin(itl,builtin_type(type_idx),is_constant);
        }

        case type_kind::alias_t:
        {
            return copy_type(itl,itl.alias_table[type_idx]);
        }
    }

    assert(false);
}

// TODO: this is more restrictive than required atm
b32 def_has_indirection(TypeNode *type_decl)
{
    return count(type_decl->compound_type);
}


Type* get_type(Interloper& itl, TypeNode* type_decl,u32 struct_idx_override = INVALID_TYPE, b32 complete_type = false)
{
    Type* type = nullptr;

    // override that makes entire type constant
    // i.e arrays, structs, pointers, base
    const b32 is_constant = type_decl->is_constant;
    b32 is_alias = false;

    // struct has checked that just a name without a full type is allready valid
    // so we wont bother doing this again!
    // NOTE: we check this below as well for other situations such as function pointers
    if(struct_idx_override != INVALID_TYPE)
    {
        type = make_struct(itl,struct_idx_override,is_constant);
    }

    else if(type_decl->type_idx == USER_TYPE)
    {
        // NOTE: here we are doing the heavy lifting on defs by our self
        // to handle out of order decl so we directly query the type table
        // rather than using lookup_type
        const auto name = type_decl->name;
        TypeDecl* user_type = type_decl->name_space? lookup_incomplete_decl_scoped(type_decl->name_space,name) : lookup_incomplete_decl(itl,name);

        // check we have a type definiton
        // no such definiton exists, nothing we can do
        if(!user_type)
        {
            panic(itl,itl_error::undeclared,"type %s is not defined\n",type_decl->name.buf);
            return make_builtin(itl,builtin_type::void_t);
        }

        is_alias = user_type->kind == type_kind::alias_t;   

        // user type does not exist yet
        if(user_type->state != type_def_state::checked)
        {
            // By this point only types that have definitions can not be finalised
            assert(user_type->flags & TYPE_DECL_DEF_FLAG);

            // if this is not currently being checked 
            // parse it
            if(user_type->state == type_def_state::not_checked)
            {
                TypeDef& type_def = *((TypeDef*)user_type);
                parse_def(itl,type_def);

                // def checking went wrong!
                if(itl.error)
                {
                    return make_builtin(itl,builtin_type::void_t);
                }

                // okay now we have a complete type build it!
                type = make_base_type(itl,user_type->type_idx,user_type->kind,is_constant);
            }

            // type is being currently checked?
            // we might have a potential black hole
            else
            {
                // indirection, this is fine we dont need details of the type yet
                if(def_has_indirection(type_decl))
                {
                    type = make_base_type(itl,user_type->type_idx,user_type->kind,is_constant);
                }

                // this is no indirection and we have attempted to parse a type twice
                // this means recursion is happening somewhere
                else
                {
                    // TODO: add huertsics to scan for where!
                    panic(itl,itl_error::black_hole,"type %s is recursively defined\n",name.buf);
                    return make_builtin(itl,builtin_type::void_t);               
                }
            }
        }

        // user defined type allready exists, just pull the info out
        else
        {   
            type = make_base_type(itl,user_type->type_idx,user_type->kind,is_constant); 
        }

            
    }

    else if(type_decl->type_idx == FUNC_POINTER)
    {
        // allocate and create function pointer type
        FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,FUNC_POINTER,is_constant);
        push_var(itl.func_pointer,&type->sig);
        type->sig = {};

        // parse the function sig
        parse_func_sig(itl,itl.symbol_table.cur_namespace,type->sig,*type_decl->func_type);

        return (Type*)type;
    }

    // plain old type
    else
    {
        type = make_raw(itl,type_decl->type_idx,is_constant);
    }

    if(!is_constant)
    {
        // need const on bottom type
        if(is_alias)
        {
            Type* plain_type = get_plain_type(type);
            plain_type->is_const = type_decl->is_const;
        }

        else
        {
            type->is_const = type_decl->is_const;
        }
    }

    b32 indirection = false;

    // arrays, pointers
    // NOTE: parse backwards so the plain type
    // is held at the bottom by any containers
    for(s32 c = count(type_decl->compound_type) - 1; c >= 0; c--)
    {
        AstNode* node = type_decl->compound_type[c];

        switch(node->type)
        {
            // pointer to current type
            case ast_type::ptr_indirection:
            {
                type = make_pointer(itl,type,is_constant);
                indirection = true;
                break;
            }

            case ast_type::arr_var_size:
            {
                type = make_array(itl,type,RUNTIME_SIZE,is_constant);
                indirection = true;
                break;
            }

            case ast_type::arr_fixed:
            {
                UnaryNode* unary_node = (UnaryNode*)node;

                const auto [size,int_type] = compile_const_int_expression(itl,unary_node->next);

                type = make_array(itl,type,size,is_constant);
                break;
            }

            case ast_type::arr_deduce_size:
            {
                if(complete_type)
                {
                    panic(itl,itl_error::mismatched_args,"type is constant and cannot be deduced by assign\n");
                    return make_builtin(itl,builtin_type::void_t);
                }

                // i.e we cant have a pointer to an array with a size deduction
                // it has to hold the indirection...
                if(indirection)
                {
                    panic(itl,itl_error::mismatched_args,"cannot have deduction for array size where indirection allready exists\n");
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


Type* value_type(Interloper& itl,const Value& value)
{
    if(value.sign)
    {
        const s64 v = s64(value.v);

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,s64(builtin_min(builtin_type::s8_t)),s64(builtin_max(builtin_type::s8_t))))
        {
            return make_builtin(itl,builtin_type::s8_t);
        }

        else if(in_range(v,s64(builtin_min(builtin_type::s16_t)),s64(builtin_max(builtin_type::s16_t))))
        {
            return make_builtin(itl,builtin_type::s16_t);
        }

        else if(in_range(v,s64(builtin_min(builtin_type::s32_t)),s64(builtin_max(builtin_type::s32_t))))
        {
            return make_builtin(itl,builtin_type::s32_t);
        }

        else
        {
            return make_builtin(itl,builtin_type::s64_t);
        }
    }

    else
    {
        const u64 v = value.v;

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,builtin_min(builtin_type::u8_t),builtin_max(builtin_type::u8_t)))
        {
            return make_builtin(itl,builtin_type::u8_t);
        }

        else if(in_range(v,builtin_min(builtin_type::u16_t),builtin_max(builtin_type::u16_t)))
        {
            return make_builtin(itl,builtin_type::u16_t);
        }

        else if(in_range(v,builtin_min(builtin_type::u32_t),builtin_max(builtin_type::u32_t)))
        {
            return make_builtin(itl,builtin_type::u32_t);
        }

        else
        {
            return make_builtin(itl,builtin_type::u64_t);
        }        
    }    
}


Type* effective_arith_type(Interloper& itl,Type *ltype, Type *rtype, op_type op_kind)
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

        // both floats, just a float
        if(is_float(rtype) && is_float(ltype))
        {
            return make_builtin(itl,builtin_type::f64_t);
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
        if(op_kind != op_type::sub_reg && op_kind != op_type::add_reg)
        {
            panic(itl,itl_error::undefined_type_oper,"Pointer arithmetic is only defined for addition and subtraction\n");
            return make_builtin(itl,builtin_type::void_t);            
        }

        return ltype;
    }

    else if(is_pointer(ltype) && is_pointer(rtype) && op_kind == op_type::sub_reg)
    {
        return make_builtin(itl,GPR_SIZE_TYPE);
    }


    // one or more user defined
    else
    {
        panic(itl,itl_error::undefined_type_oper,"arithmetic operation undefined for %s and %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        return make_builtin(itl,builtin_type::void_t);    
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

        // both float
        else if(is_float(rtype) && is_float(ltype))
        {

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


void check_const_internal(Interloper&itl, const Type* ltype, const Type* rtype, assign_type type, b32 was_reference)
{

    // const ltype is of no concern if while an arg or initializer (in this instance they are the same thing)
    // i.e first time initialization, it is a problem if this is an assign though
    // but we only really care for assigns on the "top level" if its a pointer
    if(ltype->is_const && type == assign_type::assign && !was_reference)
    {
        panic(itl,itl_error::const_type_error,"cannot assign rtype to const ltype: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        return;
    }

    // for an rtype a copy is fine, unless it was a reference in which case
    // the ltype must also be const
    if(rtype->is_const)
    {
        if(!ltype->is_const && was_reference)
        {
            panic(itl,itl_error::const_type_error,"cannot assign const ref rtype to ltype: %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
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
/*
    // value types can be copied if only the rype is const
    if(is_value_type(rtype) && is_value_type(ltype))
    {
        // can be copied fine
        if(rtype->is_const && !ltype->is_const)
        {
            return;
        }
    }
*/
    // was the type above us a reference?
    b32 was_reference = false;

    // check const specifiers at every level
    while(!done)
    {
        switch(ltype->type_idx)
        {
            case ARRAY:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);

                // check sub types
                ltype = index_arr(ltype);
                rtype = index_arr(rtype);

                // array counts as a pointer
                was_reference = true;
                break;
            }

            case POINTER:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);

                // check sub types
                ltype = deref_pointer(ltype);
                rtype = deref_pointer(rtype);

                was_reference = true;
                break;
            }

            case STRUCT:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);
                done = true;
                break;
            }

            case ENUM:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);
                done = true;
                break;
            }

            case FUNC_POINTER:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);
                done = true;
                break;
            }

            // check end type
            default:
            {
                check_const_internal(itl,ltype,rtype,type,was_reference);
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

        case FUNC_POINTER:
        {
            assert(false);
        }

        default:
        {
            return ltype->type_idx == rtype->type_idx;
        }
    }
}

b32 type_equal(const Type* ltype, const Type* rtype)
{
    switch(ltype->type_idx)
    {
        case ARRAY:
        {
            assert(false);
        }

        case POINTER:
        {
            if(ltype->is_const != rtype->is_const)
            {
                return false;
            }

            return type_equal(deref_pointer(ltype),deref_pointer(rtype));
        }

        default:
        {
            return plain_type_equal(ltype,rtype);
        }
    }
}


bool is_byte_ptr(const Type* type)
{
    return is_pointer(type) && deref_pointer(type)->type_idx == u32(builtin_type::byte_t);
}

bool is_byte_array(const Type* type)
{
    return is_array(type) && index_arr(type)->type_idx == u32(builtin_type::byte_t);
}

void type_check_pointer(Interloper& itl,const Type* ltype, const Type* rtype)
{
    const auto base_ltype = ltype;
    const auto base_rtype = rtype;

    // null rtype auto converted 
    if(is_pointer(ltype) && is_pointer(rtype) && deref_pointer(rtype)->type_idx == u32(builtin_type::null_t))
    {
        return;
    }
    
    // any rtype can be assigned to a byte ptr
    if(is_byte_ptr(ltype) && is_pointer(rtype))
    {
        return;
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
                    if(!is_runtime_size(ltype) || !is_runtime_size(rtype))
                    {
                        panic(itl,itl_error::array_type_error,"Pointer to fixed array");
                        return;
                    }

                    ltype = index_arr(ltype);
                    rtype = index_arr(rtype);

                    break;
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
        panic(itl,itl_error::pointer_type_error,"expected pointer of type %s got %s\n",type_name(itl,base_ltype).buf,type_name(itl,base_rtype).buf);
        return;
    }

    // anything else
    else
    {
        // if base types still aernt equal we have a problem!
        if(!plain_type_equal(ltype,rtype))
        {
            panic(itl,itl_error::pointer_type_error,"expected pointer of type %s got %s\n",type_name(itl,base_ltype).buf,type_name(itl,base_rtype).buf);
            return;
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
                panic(itl,itl_error::undefined_type_oper,"invalid assign %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
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

    else if(is_func_pointer(ltype) && is_func_pointer(rtype))
    {
        FuncPointerType* func_ltype = (FuncPointerType*)ltype;
        FuncPointerType* func_rtype = (FuncPointerType*)rtype;

        if(count(func_ltype->sig.args) != count(func_rtype->sig.args))
        {
            panic(itl,itl_error::mismatched_args,"func pointers have mistached arg sizes %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }

        if(count(func_ltype->sig.return_type) != count(func_rtype->sig.return_type))
        {
            panic(itl,itl_error::mismatched_args,"func pointers have mistached return type sizes %s = %s\n",type_name(itl,ltype).buf,type_name(itl,rtype).buf);
        }

        // check every type in function pointer is equal
        // implict conversions are not valid here!

        // check args
        for(u32 a = 0; a < count(func_ltype->sig.args); a++)
        {
            auto& lsym = sym_from_slot(itl.symbol_table,func_ltype->sig.args[a]);
            auto& rsym = sym_from_slot(itl.symbol_table,func_rtype->sig.args[a]);

            if(!type_equal(lsym.type,rsym.type))
            {
                panic(itl,itl_error::mismatched_args,"func pointer arg type %d does not match:\n%s = %s\n\n",
                    a,type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
        }

        // check ret type
        for(u32 r = 0; r < count(func_ltype->sig.return_type); r++)
        {
            if(!type_equal(func_ltype->sig.return_type[r],func_rtype->sig.return_type[r]))
            {
                panic(itl,itl_error::mismatched_args,"func pointer return type %d does not match:\n%s = %s\n\n",
                    r,type_name(itl,ltype).buf,type_name(itl,rtype).buf);
            }
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


void clip_arith_type(Interloper &itl, Function& func,SymSlot dst_slot, SymSlot src_slot, u32 size)
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

                    case builtin_type::s32_t:
                    {
                        sign_extend_word(itl,func,dst_slot,src_slot);
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
                clip_arith_type(itl,func,dst_slot,src_slot,type_size(itl,new_type));
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

        else if(is_float(old_type) && is_integer(new_type))
        {
            cvt_fi(itl,func,dst_slot,src_slot);
        }

        else if(is_integer(old_type) && is_float(new_type))
        {
            cvt_if(itl,func,dst_slot,src_slot);
        }

        else
        {
            panic(itl,itl_error::illegal_cast,"cannot cast %s -> %s\n",type_name(itl,old_type).buf,type_name(itl,new_type).buf);
        }
    }

    // these cast do no conversions just move the reg 
    // they are only acknowledgement's your doing something screwy

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

    // pointer to pointer or integer
    else if(is_pointer(old_type) && (is_pointer(new_type) || is_integer(new_type)))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }


    // integer to pointer
    else if(is_integer(old_type) && (is_pointer(new_type)))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // func pointer cast to pointer or integer is fine
    else if(is_func_pointer(old_type) && (is_pointer(new_type) || is_integer(new_type)))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    // dont know
    else
    {
        panic(itl,itl_error::illegal_cast,"cannot cast %s -> %s\n",type_name(itl,old_type).buf,type_name(itl,new_type).buf);      
    }

}

std::pair<Type*,u64> access_builtin_type_info(Interloper& itl, builtin_type type, const String& member_name)
{
    const BuiltinTypeInfo& info = builtin_type_info[u32(type)];

    if(member_name == "size")
    {
        return std::pair{make_builtin(itl,builtin_type::u32_t),info.size};
    }

    else if(member_name == "max")
    {
        return std::pair{make_builtin(itl,builtin_type::u32_t),info.max};
    }

    else if(member_name == "min")
    {
        return std::pair{make_builtin(itl,builtin_type::u32_t),info.min};
    }

    panic(itl,itl_error::undefined_type_oper,"unknown type info for builtin type %s.%s\n",TYPE_NAMES[u32(type)],member_name.buf);
    return std::pair{make_builtin(itl,builtin_type::u32_t),0};
}



Type* access_builtin_type_info(Interloper& itl, Function& func, SymSlot dst_slot, builtin_type type, const String& member_name)
{
    auto [rtype,ans] = access_builtin_type_info(itl,type,member_name);

    mov_imm(itl,func,dst_slot,ans);

    return rtype;
}


std::pair<Type*,u32> access_type_info(Interloper& itl,const TypeDecl& type_decl, const String& member_name)
{
    switch(type_decl.kind)
    {
        case type_kind::builtin:
        {
            builtin_type type = builtin_type(type_decl.type_idx);

            return access_builtin_type_info(itl,type,member_name);
        }

        case type_kind::struct_t:
        {
            if(member_name == "size")
            {
                const auto& structure = itl.struct_table[type_decl.type_idx];
                const u32 size = structure.size;

                return std::pair{make_builtin(itl,builtin_type::u32_t),size};
            }

            else
            {
                panic(itl,itl_error::enum_type_error,"unknown type info for struct %s\n",type_decl.name.buf);
                return std::pair{make_builtin(itl,builtin_type::u32_t),0};
            }
        }

        case type_kind::enum_t:
        {
            if(member_name == "len")
            {
                const auto enumeration = itl.enum_table[type_decl.type_idx];

                const u32 enum_len = enumeration.member_map.size;

                return std::pair{make_builtin(itl,builtin_type::u32_t),enum_len};
            }

            else
            {
                panic(itl,itl_error::enum_type_error,"unknown type info for enum %s\n",type_decl.name.buf);
                return std::pair{make_builtin(itl,builtin_type::u32_t),0};
            }
        }

        case type_kind::alias_t:
        {
            panic(itl,itl_error::generic_type_error,"cannot access type properties on alias %s\n",type_decl.name.buf);
            return std::pair{make_builtin(itl,builtin_type::u32_t),0};
        }
    }

    assert(false);
}

Type* access_type_info(Interloper& itl, Function& func, SymSlot dst_slot, const TypeDecl& type_decl, const String& member_name)
{
    auto [type,ans] = access_type_info(itl,type_decl,member_name);

    mov_imm(itl,func,dst_slot,ans);

    return type;
}

void add_type_to_scope(NameSpace* name_space, TypeDecl* decl)
{
    DefInfo info;
    info.type = definition_type::type;
    info.type_decl = decl;

    add(name_space->table,decl->name,info);
}

template<typename T>
T* alloc_type_decl(Interloper& itl)
{
    T* out = (T*)allocate(itl.type_allocator,sizeof(T));
    *out = {};

    return out;
}

void add_internal_type_decl(Interloper& itl, u32 type_idx, const String& name, type_kind kind)
{
    TypeDecl* type_decl = alloc_type_decl<TypeDecl>(itl);

    type_decl->type_idx = type_idx;
    type_decl->name = name;
    type_decl->kind = kind;
    type_decl->name_space = itl.global_namespace;
    type_decl->state = type_def_state::checked;
    
    add_type_to_scope(itl.global_namespace,type_decl);    
}


void add_type_definition(Interloper& itl, type_def_kind kind, AstNode* root, const String& name, const String& filename, NameSpace* name_space)
{
    TypeDef* definition = alloc_type_decl<TypeDef>(itl);

    definition->decl.name = name;
    definition->decl.flags = TYPE_DECL_DEF_FLAG;
    definition->decl.name_space = name_space;

    definition->filename = filename;
    definition->root = root;
    definition->kind = kind;


    add_type_to_scope(name_space,(TypeDecl*)definition);
}

b32 type_exists(Interloper& itl, const String& name)
{
    return lookup_complete_decl(itl,name) != nullptr;
}

void add_internal_alias(Interloper& itl, Type* type,const String& name)
{
    const u32 type_idx = count(itl.alias_table);
    add_internal_type_decl(itl,type_idx,name,type_kind::alias_t); 
    push_var(itl.alias_table,type);   
}

void finalise_type(TypeDecl& decl, u32 type_idx,type_kind kind)
{
    decl.type_idx = type_idx;
    decl.state = type_def_state::checked;
    decl.kind = kind;
}

void parse_alias_def(Interloper& itl, TypeDef& def)
{
    AliasNode* node = (AliasNode*)def.root;

    Type* type = get_complete_type(itl,node->type);

    if(itl.error)
    {
        return;
    }

    if(itl.print_types)
    {
        printf("type alias %s = %s\n",node->name.buf,type_name(itl,type).buf);
    }

    const u32 type_idx = count(itl.alias_table);
    finalise_type(def.decl,type_idx,type_kind::alias_t);
    push_var(itl.alias_table,type); 
}

void declare_compiler_type_aliases(Interloper& itl) 
{
    /// usize
    add_internal_alias(itl,make_builtin(itl,builtin_type::u64_t),"usize");

    // ssize
    add_internal_alias(itl,make_builtin(itl,builtin_type::s64_t),"ssize");
}

void parse_struct_def(Interloper& itl, TypeDef& def);
void parse_alias_def(Interloper& itl, TypeDef& def);
void parse_enum_def(Interloper& itl, TypeDef& def, Set<u64>& set);

void parse_def(Interloper& itl, TypeDef& def)
{
    // this node make be from a different context
    // save the current one
    push_context(itl);

    if(def.decl.state == type_def_state::not_checked)
    {
        // mark as checking to lock this against recursion!
        def.decl.state = type_def_state::checking;

        switch(def.kind)
        {
            case type_def_kind::struct_t:
            {
                parse_struct_def(itl,def);
                break;
            }

            case type_def_kind::alias_t:
            {
                parse_alias_def(itl,def);
                break;
            }

            case type_def_kind::enum_t: 
            {
                auto set = make_set<u64>();

                parse_enum_def(itl,def,set);
                destroy_set(set);
                break;
            }
        }
    }

    else
    {
        // TODO: add huertsics to scan for where!
        panic(itl,itl_error::black_hole,"type %s is recursively defined\n",def.decl.name.buf);
        return;
    }

    pop_context(itl);
}


void destroy_sig(FuncSig& sig)
{
    destroy_arr(sig.args);
    destroy_arr(sig.return_type);
    destroy_arr(sig.args);
}

void destroy_func(Function& func)
{
    destroy_sig(func.sig);

    for(u32 r = 0; r < count(func.registers); r++)
    {
        destroy_reg(func.registers[r]);
    }

    destroy_arr(func.registers);
    destroy_emitter(func.emitter);
}
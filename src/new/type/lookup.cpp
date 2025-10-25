
void print_type(Interloper& itl, const Type* type)
{
    printf("type: %s\n",type_name(itl,type).buf);
}

Type* copy_type(Interloper& itl, const Type* type);

Type* copy_type_internal(Interloper& itl, const Type* type)
{
    switch(type->kind)
    {
        case type_class::array_t:
        {
            ArrayType* array_type = (ArrayType*)type;
            Type* contained_type = copy_type_internal(itl,array_type->contained_type);

            return make_array(itl,contained_type,array_type->size,type->flags);
        }

        case type_class::pointer_t:
        {
            PointerType* pointer_type = (PointerType*)type;

            Type* contained_type = copy_type_internal(itl,pointer_type->contained_type);
            
            return make_pointer(itl,contained_type,pointer_type->pointer_kind,type->flags);
        }

        case type_class::struct_t:
        {
            StructType* struct_type = (StructType*)type;

            return make_struct(itl,struct_type->struct_idx,type->flags);
        }

        case type_class::enum_t:
        {
            EnumType* enum_type = (EnumType*)type;

            return make_enum(itl,enum_type->enum_idx,type->flags);        
        }

        case type_class::func_pointer_t:
        {
            FuncPointerType* func_pointer_type = (FuncPointerType*)type;

            FuncPointerType* copy = (FuncPointerType*)alloc_type<FuncPointerType>(itl,type_class::func_pointer_t,type->flags);
            copy->sig = {};

            const auto& sig = func_pointer_type->sig;
            copy->sig = sig;

            copy->sig.args = copy_array(sig.args);

            copy->sig.return_type = {};
            for(u32 r = 0; r < count(sig.return_type); r++)
            {
                push_var(copy->sig.return_type,copy_type(itl,sig.return_type[r]));
            }

            copy->sig.pass_as_reg = copy_array(sig.pass_as_reg);
            push_var(itl.func_pointer,&copy->sig);

            return (Type*)copy;
        }

        case type_class::tuple_t: assert(false); break;

        case type_class::builtin_t:
        {
            return make_builtin(itl,cast_builtin(type),type->flags);
        }
    }

    assert(false);    
}

Type* copy_type(Interloper& itl, const Type* type)
{
    return copy_type_internal(itl,type);
}


// NOTE: 
// to be used externally when attempting to find a type decl
// dont look it up in the type table directly as the definition might not
// have been parsed yet
Option<TypeDecl*> lookup_type_internal(Interloper& itl,NameSpace* name_space,const String& name)
{
    TypeDecl* user_type = name_space == nullptr? lookup_incomplete_decl(itl,name) : lookup_incomplete_decl_scoped(name_space,name);

    if(!user_type)
    {
        return option::none;
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
            return option::none;
        }

        // okay attempt to parse the def
        TypeDef& type_def = *((TypeDef*)user_type);

        // def parsing failed in some fashion just bail out
        // there are no options left
        const auto def_err = parse_def(itl,type_def);
        if(def_err)
        {
            return option::none;
        }
    }

    return user_type;
}

Option<TypeDecl*> lookup_type(Interloper& itl,const String& name)
{
    return lookup_type_internal(itl,nullptr,name);
}

Option<TypeDecl*> lookup_type_scoped(Interloper& itl,NameSpace* name_space,const String& name)
{
    return lookup_type_internal(itl,name_space,name);
}

Type* make_base_type(Interloper& itl, u32 type_idx, type_kind kind, u32 flags)
{
    switch(kind)
    {
        case type_kind::struct_t:
        {
            return make_struct(itl,type_idx,flags); 
        }

        case type_kind::enum_t:
        {
            return make_enum(itl,type_idx,flags);
        }

        case type_kind::builtin:
        {
            return make_builtin(itl,builtin_type(type_idx),flags);
        }

        case type_kind::alias_t:
        {
            return copy_type(itl,itl.alias_table[type_idx]);
        }
    }

    assert(false);
}

// TODO: this is more restrictive than required atm
b32 def_has_indirection(const TypeNode *type_decl)
{
    return count(type_decl->compound);
}


TypeResult get_type(Interloper& itl, TypeNode* type_decl,u32 struct_idx_override = INVALID_TYPE, b32 complete_type = false)
{
    if(type_decl->node.expr_type)
    {
        return type_decl->node.expr_type;
    }

    Type* type = nullptr;

    // override that makes entire type constant
    // i.e arrays, structs, pointers, base
    const u32 flags = type_decl->is_constant? TYPE_FLAG_CONST : 0; 
    b32 is_alias = false;

    // struct has checked that just a name without a full type is allready valid
    // so we wont bother doing this again!
    // NOTE: we check this below as well for other situations such as function pointers
    if(struct_idx_override != INVALID_TYPE)
    {
        type = make_struct(itl,struct_idx_override,flags);
    }

    else
    {
        switch(type_decl->kind)
        {
            case type_node_kind::user:
            {
                // NOTE: here we are doing the heavy lifting on defs by our self
                // to handle out of order decl so we directly query the type table
                // rather than using lookup_type
                const auto name = type_decl->name;
                TypeDecl* user_type = type_decl->name_space? lookup_incomplete_decl_scoped(type_decl->name_space,name) : lookup_incomplete_decl(itl,name);

                // check we have a type definition
                // no such definition exists, nothing we can do
                if(!user_type)
                {
                    return compile_error(itl,itl_error::undeclared,"type %S is not defined",type_decl->name);
                }

                is_alias = user_type->kind == type_kind::alias_t;   

                // user type does not exist yet
                if(user_type->state != type_def_state::checked)
                {
                    // By this point only types that have definitions can not be finalized
                    assert(user_type->flags & TYPE_DECL_DEF_FLAG);

                    // if this is not currently being checked 
                    // parse it
                    if(user_type->state == type_def_state::not_checked)
                    {
                        TypeDef& type_def = *((TypeDef*)user_type);

                        const auto type_err = parse_def(itl,type_def);
                        if(type_err)
                        {
                            return *type_err;
                        }

                        // okay now we have a complete type build it!
                        type = make_base_type(itl,user_type->type_idx,user_type->kind,flags);
                    }

                    // type is being currently checked?
                    // we might have a potential black hole
                    else
                    {
                        // indirection, this is fine we dont need details of the type yet
                        if(def_has_indirection(type_decl))
                        {
                            type = make_base_type(itl,user_type->type_idx,user_type->kind,flags);
                        }

                        // this is no indirection and we have attempted to parse a type twice
                        // this means recursion is happening somewhere
                        else
                        {
                            // TODO: add heuristics to scan for where!
                            return compile_error(itl,itl_error::black_hole,"Lookup type: type %S is recursively defined",name);           
                        }
                    }
                }

                // user defined type allready exists, just pull the info out
                else
                {   
                    type = make_base_type(itl,user_type->type_idx,user_type->kind,flags); 
                }

                break;
            }
        

            case type_node_kind::func_pointer:
            {
                // allocate and create function pointer type
                FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,type_class::func_pointer_t,flags);
                push_var(itl.func_pointer,&type->sig);
                type->sig = {};

                // parse the function sig
                const auto func_err = parse_func_sig(itl,itl.symbol_table.ctx->name_space,type->sig,*type_decl->func_type,func_sig_kind::function_pointer);
                if(func_err)
                {
                    return *func_err;
                }

                return (Type*)type;
            }

            case type_node_kind::builtin:
            {
                type = make_builtin(itl,type_decl->builtin,flags);
                break;
            }
        }
    }

    const u32 const_flag = type_decl->is_const? TYPE_FLAG_CONST : 0;

    // need const on bottom type
    if(is_alias)
    {
        Type* plain_type = get_plain_type(type);
        plain_type->flags |= const_flag;
        
    }

    else
    {
        type->flags |= const_flag;
    }

    b32 indirection = false;

    // arrays, pointers
    // NOTE: parse backwards so the plain type
    // is held at the bottom by any containers
    for(s32 c = count(type_decl->compound) - 1; c >= 0; c--)
    {
        const CompoundType compound = type_decl->compound[c];

        switch(compound.type)
        {
            // pointer to current type
            case compound_type::ptr:
            {
                type = make_pointer(itl,type,pointer_type::reference,flags);
                indirection = true;
                break;
            }

            case compound_type::nullable_ptr:
            {
                type = make_pointer(itl,type,pointer_type::nullable,flags);
                indirection = true;
                break;
            }

            case compound_type::arr_var_size:
            {
                type = make_array(itl,type,RUNTIME_SIZE,flags);
                indirection = true;
                break;
            }

            case compound_type::arr_fixed_size:
            {
                assert(false);
                // auto expr_res = compile_const_int_expression(itl,compound.array_size);

                // if(!expr_res)
                // {
                //     return expr_res.error();
                // }

                // const auto const_int = *expr_res;

                // type = make_array(itl,type,const_int.value,flags);
                break;
            }

            case compound_type::arr_deduce_size:
            {
                if(complete_type)
                {
                    return compile_error(itl,itl_error::mismatched_args,"type is constant and cannot be deduced by assign");
                }

                // i.e we cant have a pointer to an array with a size deduction
                // it has to hold the indirection...
                if(indirection)
                {
                    return compile_error(itl,itl_error::mismatched_args,"cannot have deduction for array size where indirection allready exists");
                }

                type = make_array(itl,type,DEDUCE_SIZE,flags);

                break;
            }
        }
    }

    type_decl->node.expr_type = type;
    return type;
}

// get back a type that does not need further deduction i.e no size deduction
TypeResult get_complete_type(Interloper& itl, TypeNode* type_decl)
{
    return get_type(itl,type_decl,INVALID_TYPE,true);
}
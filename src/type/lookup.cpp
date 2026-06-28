
Type* copy_type(Interloper& itl, const Type* type);
TypeResult find_generic_type(Interloper& itl, const String& name);


void print_type(Interloper& itl, const Type* type)
{
    printf("type: %s\n",type_name(itl,type).buf);
}

TypeLookupInfo type_node_to_lookup(const TypeNode* node, type_lookup_kind kind)
{
    TypeLookupInfo info;
    info.name = node->name;
    info.name_space = node->name_space;
    info.generic_args = node->generic_args;
    info.kind = kind;

    return info;
}

TypeLookupInfo type_lookup_from_parts(const String& name, NameSpace* name_space, type_lookup_kind kind)
{
    TypeLookupInfo info;
    info.name = name;
    info.name_space = name_space;
    info.kind = kind;
    
    return info;
}

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


Result<TypeDecl*,itl_error> lookup_type(Interloper& itl,const TypeLookupInfo& info)
{
    const auto res = lookup_incomplete_decl(itl,info);
    if(!res)
    {
        return res;
    }

    TypeDef* def = (TypeDef*)res.value();

    // TODO: Handle overload resolution
    assert(!def->generic_base);

    // currently type does not exist
    // attempt to parse the def
    if(def->decl.state != type_def_state::checked)
    {
        // no such definition exists
        if(!(def->decl.flags & TYPE_DECL_DEF_FLAG))
        {
            return compile_error(itl,itl_error::undeclared,"Type %n%s does not have a definition",info.name_space,info.name);
        }

        // okay attempt to parse the def
        return parse_def(itl,def,info);
    }

    return &def->decl;
}

DefInfo* parser_lookup_definition(Parser& parser, NameSpace* name_space, const String& name)
{
    if(!name_space)
    {
        name_space = parser.ctx.cur_namespace;
    }

    return lookup_definition(name_space,name);
}

FunctionDef* parser_lookup_func(Parser& parser, NameSpace* name_space, const String& name)
{
    DefInfo* info = parser_lookup_definition(parser,name_space,name);
    if(!info)
    {
        return nullptr;
    }

    if(info->type != definition_type::function)
    {
        return nullptr;
    }

    return &parser.func_table->table[info->handle];
}

TypeDecl* parser_lookup_type(Parser& parser, NameSpace* name_space, const String& name)
{
    DefInfo* info = parser_lookup_definition(parser,name_space,name);
    if(!info)
    {
        return nullptr;
    }

    if(info->type != definition_type::type)
    {
        return nullptr;
    }

    return info->type_decl;
}


bool parser_type_kind_exists(Parser& parser, NameSpace* name_space, const String& name, type_kind kind)
{ 
    if(!name_space)
    {
        name_space = parser.ctx.cur_namespace;
    }

    const DefInfo* def = lookup_typed_definition(name_space,name,definition_type::type);
    if(!def)
    {
        return false;
    }

    return def->type_decl->kind == kind;
}

bool parser_type_exists(Parser& parser, NameSpace* name_space, const String& name)
{ 
    if(!name_space)
    {
        name_space = parser.ctx.cur_namespace;
    }

    return lookup_typed_definition(name_space,name,definition_type::type) != nullptr;
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
            case type_node_kind::generic:
            {
                const auto name = type_decl->name;
                is_alias = true;


                const auto res = find_generic_type(itl,name);
                if(!res)
                {
                    return res.error();
                }

                type = *res;
                break;
            }

            case type_node_kind::user:
            {
                // NOTE: here we are doing the heavy lifting on defs by our self
                // to handle out of order decl so we directly query the type table
                // rather than using lookup_type
                const auto user_res =  lookup_incomplete_decl(itl,type_node_to_lookup(type_decl,type_lookup_kind::any_t));
                if(!user_res)
                {
                    return user_res.error();
                }

                TypeDecl* user_type = *user_res;

                is_alias = user_type->kind == type_kind::alias_t;   

                // user type does not exist yet
                if(user_type->state != type_def_state::checked)
                {
                    // if this is not currently being checked parse it
                    if(user_type->state == type_def_state::not_checked)
                    {
                        const auto type_res = parse_def(itl,(TypeDef*)user_type,type_node_to_lookup(type_decl,type_lookup_kind::any_t));
                        if(!type_res)
                        {
                            return type_res.error();
                        }
                    }

                    // type is being currently checked? 
                    // we might have a potential black hole
                    else if(!def_has_indirection(type_decl))
                    {
                        // TODO: add heuristics to scan for where!
                        return compile_error(itl,itl_error::black_hole,"Lookup type: type %S is recursively defined",user_type->name);     
                    }
                }

                // okay now we have a complete type build it!
                type = make_base_type(itl,user_type->type_idx,user_type->kind,flags);
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

                return type_decl->node.expr_type = (Type*)type;
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

    s32 last_deduction = 0;
    s32 lowest_indirection = count(type_decl->compound) + 1;

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
                lowest_indirection = c;
                break;
            }

            case compound_type::nullable_ptr:
            {
                type = make_pointer(itl,type,pointer_type::nullable,flags);
                lowest_indirection = c;
                break;
            }

            case compound_type::arr_var_size:
            {
                type = make_array(itl,type,RUNTIME_SIZE,flags);
                lowest_indirection = c;
                break;
            }

            case compound_type::arr_fixed_size:
            {
                auto const_res = type_check_const_int_expression(itl,compound.array_size);

                if(!const_res)
                {
                    return const_res.error();
                }

                const auto const_int = *const_res;

                type = make_array(itl,type,const_int.value,flags);
                break;
            }

            case compound_type::arr_deduce_size:
            {
                if(c > last_deduction)
                {
                    last_deduction = c;
                }

                if(complete_type)
                {
                    return compile_error(itl,itl_error::mismatched_args,"type is constant and cannot be deduced by assign");
                }

                type = make_array(itl,type,DEDUCE_SIZE,flags);
                break;
            }
        }
    }

    // Any deduction has to proceed the indirection as the deduction is done by an assignment.
    if(last_deduction > lowest_indirection)
    {
        return compile_error(itl,itl_error::mismatched_args,"Cannot have deduction for array size where indirection already exists");
    }

    type_decl->node.expr_type = type;
    return type;
}

// get back a type that does not need further deduction i.e no size deduction
TypeResult get_complete_type(Interloper& itl, TypeNode* type_decl)
{
    return get_type(itl,type_decl,INVALID_TYPE,true);
}
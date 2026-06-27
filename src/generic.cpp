
bool is_known_generic_int(const GenericKnown& known)
{
    return is_integer(known.type_decl->node.expr_type);
}



void print_generic_overload(Interloper& itl, const ConstSpan<Generic>& generic_overload)
{
    for(const auto& generic : generic_overload)
    {   
        if(generic.constraint != constraint_type::type)
        {
            print_itl(itl,"%S = %t",generic.name,generic.type);
        }

        else
        {
            if(is_known_generic_int(generic.known))
            {
                print_itl(itl,"%S = %d",generic.name,generic.known.integer);
            }

            else
            {
                print_itl(itl,"%S = %f",generic.name,generic.known.decimal);
            }
        }
    }

    putchar('\n');
}

// TODO: This should probably be a hash table
Result<Generic,itl_error> find_generic_param(Interloper& itl, const String& name)
{
    for(auto generic : itl.generic_overload.current_overload)
    {
        if(generic.name == name)
        {
            return generic;
        }
    }

    return compile_error(itl,itl_error::undeclared,"Generic %S is not defined",name);
}

TypeResult find_generic_type(Interloper& itl, const String& name)
{
    const auto generic_res = find_generic_param(itl,name);
    if(!generic_res)
    {
        return generic_res.error();
    }

    const auto& generic = *generic_res; 

    return copy_type(itl,generic.type);
}



Result<Array<AstNode*>,parse_error> parse_generic_args(Parser& parser, const GenericOverload& overload, const Token& tok)
{
    const auto err = consume(parser,token_type::logical_lt);
    if(err)
    {
        return *err;
    }

    Array<AstNode*> args; 

    u32 generic_arg = 0;
    b32 done = false;

    while(!consume_match(parser,token_type::logical_gt) && !done)
    {
        if(generic_arg >= count(overload))
        {
            return parser_error(parser,parse_error::itl_error,tok,
                "Generic has too many args got %d expected: %d",count(overload),generic_arg);
        }

        const auto& generic = overload[generic_arg];
        AstNode* arg = nullptr;

        if(generic.constraint != constraint_type::type)
        {
            const auto res = parse_type(parser);

            if(!res)
            {
                destroy_arr(args);
                return res.error();
            }

            arg = (AstNode*)res.value();

            if(!match(parser,token_type::logical_gt))
            {
                const auto err = consume(parser,token_type::comma);
                if(err)
                {
                    return *err;
                }
            }
        }

        else
        {
            const auto res = expr_list(parser,"Generic specifiers",token_type::logical_gt,&done);

            if(!res)
            {
                destroy_arr(args);
                return res.error();
            }

            arg = *res;
        }

        push_var(args,arg);
        generic_arg += 1;
    }

    return args;
}

ParserResult parse_template_call(Parser& parser, NameSpace* name_space, const Token& cur)
{
    const auto& name = cur.literal;

    // Find our function def and parse it so we know what the generics expect.
    FunctionDef* def = parser_lookup_func(parser,name_space,name);
    if(!def)
    {
        return parser_error(parser,parse_error::itl_error,cur,"Function %S does not exist for generic call",name);
    }

    const auto err = parse_func_decl(parser,*def);
    if(err)
    {
        return *err;
    }

    const auto generic_args_res = parse_generic_args(parser,def->root->generic,cur);
    if(!generic_args_res)
    {
        return generic_args_res.error();
    }

    auto func_call_res = func_call(parser,ast_symbol(parser,name_space,name,cur),cur);
    if(!func_call_res)
    {
        return func_call_res;
    }

    FuncCallNode* func_call = (FuncCallNode*)func_call_res.value();
    func_call->generic_args = *generic_args_res;
    add_ast_pointer(parser,&func_call->generic_args);

    return (AstNode*)func_call;
}


GenericScopeGuard switch_generic_context(Interloper& itl, const GenericOverload& overload)
{
    return GenericScopeGuard(itl.generic_overload,overload);
}

TypeResult cut_generic_compound(Interloper& itl, Type* type, TypeNode* decl)
{
    for(const auto& compound : decl->compound)
    {
        switch(compound.type)
        {
            case compound_type::ptr:
            {
                if(!is_pointer(type))
                {
                    return compile_error(itl,itl_error::generic,"Generic type requires pointer got %t",type);
                }

                type = deref_pointer(type);
                break;
            }

            case compound_type::arr_var_size:
            {
                if(!is_array(type))
                {
                    return compile_error(itl,itl_error::generic,"Generic type requires variable sized array got %t",type);
                }

                type = index_arr(type);
                break;
            }

            default: unimplemented("Deduce generic for compound type: %s",COMPOUND_TYPE_NAMES[u32(compound.type)]);
        }
    }

    return type;
}

Option<itl_error> type_check_generic_known(Interloper& itl, Generic* generic, AstNode* arg)
{
    const auto expected_type_res = get_type(itl,generic->known.type_decl);
    if(!expected_type_res)
    {
        return expected_type_res.error();
    }

    Type* expected_type = *expected_type_res;

    if(!is_builtin(expected_type))
    {
        return compile_error(itl,itl_error::generic,"Generic known values only support builtin types");
    }

    const auto res = type_check_expr(itl,arg);
    if(!res)
    {
        return res.error();
    }

    const auto err = check_assign_arg(itl,expected_type,*res);
    if(err)
    {
        return err;
    }

    switch(arg->known_value.type)
    {
        case known_value_type::gpr_t:
        {
            generic->known.integer = arg->known_value.gpr;
            break;
        }

        case known_value_type::fpr_t:
            generic->known.decimal = arg->known_value.fpr;
            break;

        case known_value_type::none_t:
        {
            return compile_error(itl,itl_error::generic,"Builtin generic arg expression is not known at compile time");
        }
    }

    return option::none;
}

Option<itl_error> check_generic_constraints(Interloper& itl, const GenericOverload& generic_overload)
{
    // Check constraints are met.
    for(auto& generic : generic_overload)
    {
        if(!generic.type)
        {
            return compile_error(itl,itl_error::generic,"Generic %S could not be deduced",generic.name);    
        }

        switch(generic.constraint)
        {
            case constraint_type::integer:
            {
                if(!is_integer(generic.type))
                {
                    return compile_error(itl,itl_error::generic,"Generic %S : %t does not meet constraint Integer",generic.name,generic.type);
                }

                break;
            }

            case constraint_type::real:
            {
                if(!is_integer(generic.type) && !is_float(generic.type))
                {
                    return compile_error(itl,itl_error::generic,"Generic %S : %t does not meet constraint Real",generic.name,generic.type);
                }

                break;
            }

            case constraint_type::sized:
            {
                if(type_size(itl,generic.type) == 0)
                {
                    return compile_error(itl,itl_error::generic,"Generic %S : %t is not sized",generic.name,generic.type);
                }

                break;
            }

            case constraint_type::type: break;
        }
    }

    return option::none;
}

Option<itl_error> deduce_generic_types(Interloper& itl, FuncCallNode* func_call, FuncNode& node, HashTable<String,Generic*>& generic_lookup)
{
    // Deduce generic types
    for(u32 a = 0; a < count(node.args); a++)
    {
        DeclNode* decl = node.args[a];

        TypeNode* type_node = decl->type;

        if(type_node->kind != type_node_kind::generic)
        {
            continue;
        }

        AstNode* expr = func_call->args[a];

        const auto generic_opt = lookup(generic_lookup,type_node->name);

        if(!generic_opt)
        {
            return compile_error(itl,itl_error::undeclared,"Generic type %S does not exist",type_node->name);
        }

        auto& generic = *generic_opt;

        auto deduced_type_res = cut_generic_compound(itl,expr->expr_type,decl->type);
        if(!deduced_type_res)
        {
            return deduced_type_res.error();
        }

        Type* deduced_type = *deduced_type_res;


        if(!generic->type)
        {
            generic->type = deduced_type;
        }

        if(type_equal(generic->type,deduced_type))
        {
            continue;
        }

        // Types are not equal attempt to promote them
        if(!(is_integer(generic->type) && is_integer(deduced_type)))
        {
            return compile_error(itl,itl_error::generic,"Mismatched generic types %t and %t",generic->type,deduced_type);
        }
        
        const auto promotion_res = promote_integer_type(itl,generic->type,deduced_type);
        if(!promotion_res)
        {
            return promotion_res.error();
        }

        generic->type = *promotion_res;
    }

    return option::none;
}

Result<Array<Generic>,itl_error> deduce_generic_args(Interloper& itl, FuncNode& node, FuncCallNode* func_call)
{
    auto generic_overload = copy_array(node.generic);

    // TODO: This ideally needs caching.
    auto generic_lookup = make_table<String,Generic*>();

    for(u32 i = 0; i < count(generic_overload); i++)
    {
        const auto& generic = generic_overload[i];

        if(contains(generic_lookup,generic.name))
        {
            destroy_table(generic_lookup);
            destroy_arr(generic_overload);
            return compile_error(itl,itl_error::redeclaration,"Generic %S is declared twice",generic.name);
        }

        add(generic_lookup,generic.name,&generic_overload[i]);
    }

    for(u32 i = 0; i < count(func_call->generic_args); i++)
    {
        AstNode* arg = func_call->generic_args[i];

        if(generic_overload[i].constraint != constraint_type::type)
        {
            const auto type_res = get_type(itl,(TypeNode*)arg);
            if(!type_res)
            {
                destroy_table(generic_lookup);
                destroy_arr(generic_overload);

                return type_res.error();
            }

            generic_overload[i].type = *type_res;
        }

        else
        {
            const auto err = type_check_generic_known(itl,&generic_overload[i],arg);
            if(err)
            {
                destroy_table(generic_lookup);
                destroy_arr(generic_overload);

                return *err;
            }
        }
        
    }

    const auto type_err = deduce_generic_types(itl,func_call,node,generic_lookup);
    destroy_table(generic_lookup);

    if(type_err)
    {
        destroy_arr(generic_overload);
        return *type_err;
    }

    const auto constraint_err = check_generic_constraints(itl,generic_overload);

    if(constraint_err)
    {
        destroy_arr(generic_overload);
        return *constraint_err;
    }

    return generic_overload;
}

bool check_overload(const Function* func, const Array<Generic>& generic_overload)
{
    for(u32 a = 0; a < count(generic_overload); a++)
    {
        if(!type_equal(generic_overload[a].type,func->root->generic[a].type))
        {
            return false;
        }
    }

    return true;
}

Function* find_func_overload(const FuncOverloadTable& overload, const Array<Generic>& generic_overload)
{
    for(Function* func : overload)
    {
        if(check_overload(func,generic_overload))
        {
            return func;
        }
    }

    return nullptr;

}


TypeResult type_check_generic_var(Interloper& itl, AstNode* expr)
{
    GenericVarNode* var = (GenericVarNode*)expr;

    const auto res = find_generic_param(itl,var->name);
    if(!res)
    {
        return res.error();
    }

    const auto& generic = *res;


    if(is_known_generic_int(generic.known))
    {
        var->node.known_value.type = known_value_type::gpr_t;
        var->node.known_value.gpr = generic.known.integer;
    }

    else
    {
        var->node.known_value.type = known_value_type::fpr_t;
        var->node.known_value.fpr = generic.known.decimal;
    }

    return generic.known.type_decl->node.expr_type;
}
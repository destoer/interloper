// TODO: 
// figure out how to put these in with normal symbols slots
//  and mark them for special access that we perform rewriting on inside the 1st stage ir
// because we need something generic


std::pair<u32,Type*> compile_const_int_expression(Interloper& itl, AstNode* node)
{
    switch(node->type)
    {
        case ast_type::value:
        {
            ValueNode* value_node = (ValueNode*)node;
            Value value = value_node->value;

            return std::pair{value.v,value_type(itl,value)};               
        }

        default:
        {
            panic(itl,itl_error::const_type_error,"unrecognised operation for const int initalizer: %s\n",AST_NAMES[u32(node->type)]);
            return std::pair{0,make_builtin(itl,builtin_type::void_t)};
        }
    }
}


void compile_constant_expression(Interloper& itl, Symbol& sym, AstNode* node)
{
    // switch on top level expression
    // check it is correct for the kind of type we expect from this assignment
    if(is_builtin(sym.type))
    {
        const builtin_type type = builtin_type(sym.type->type_idx);
        
        switch(type)
        {
            // integers
            case builtin_type::u8_t:
            case builtin_type::u16_t:
            case builtin_type::u32_t:
            case builtin_type::s8_t:
            case builtin_type::s16_t:
            case builtin_type::s32_t:
            case builtin_type::byte_t:
            case builtin_type::c8_t:
            {
                // compile expression
                auto [v, rtype] = compile_const_int_expression(itl,node);

                if(itl.error)
                {
                    return;
                }

                // type check the integer
                check_assign_init(itl,sym.type,rtype);

                printf("got : %d\n",v);

                assert(false);
                break;
            }


            case builtin_type::bool_t:
            {
                assert(false);
            }

            // these should not be possible...
            case builtin_type::null_t:
            {
                panic(itl,itl_error::undefined_type_oper,"null as dst type in constant expression!?\n");
                return;
            }

            case builtin_type::void_t:
            {
                panic(itl,itl_error::undefined_type_oper,"void as dst type in constant expression!?\n");
                return;
            } 
        }
    }

    else if(is_enum(sym.type))
    {
        assert(false);
    }

    // TODO: need to take a guess on this things size
    // then verify it matches
    else if(is_array(sym.type))
    {
        assert(false);
    }

    else if(is_struct(sym.type))
    {
        assert(false);
    }

    else if(is_pointer(sym.type))
    {
        assert(false);
    }

    // whoops
    else
    {
        assert(false);
    }

    // array intializer

    // struct initializer

}




void compile_constant(Interloper& itl, GlobalDeclNode* node)
{
    // TODO: when we support using const vars in expr
    // we need a locking mechanism to prevent undefined uses

    // setup the correct file for error reporting
    itl.cur_file = node->filename;

    // pull type and name so we can create a symbol
    DeclNode* decl_node = node->decl;

    const auto name = decl_node->name;

    // build the typing info
    Type* type = get_type(itl,decl_node->type);

    // add into table
    auto& sym = add_global(itl,name,type,true);

    // compile the expression
    compile_constant_expression(itl,sym,decl_node->expr);
}

void compile_constants(Interloper& itl)
{
    for(u32 c = 0; c < count(itl.constant_decl); c++)
    {
        compile_constant(itl,itl.constant_decl[c]);

        if(itl.error)
        {
            return;
        }
    }
}
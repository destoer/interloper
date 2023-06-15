// TODO: 
// figure out how to put these in with normal symbols slots
//  and mark them for special access that we perform rewriting on inside the 1st stage ir
// because we need something generic


PoolSlot pool_slot_from_sym(const Symbol& sym)
{
    return pool_slot_from_idx(sym.reg.offset);
}

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

        case ast_type::symbol:
        {
            // TODO: atm this requires correct decl order
            // as we dont have a locking mechanism or a way to lookup exprs
            auto [type,sym_slot] = symbol(itl,node);

            if(itl.error)
            {
                return std::pair{0,make_builtin(itl,builtin_type::void_t)};        
            }

            // not valid if this is not an int
            if(!is_integer(type))
            {
                panic(itl,itl_error::int_type_error,"expected integer for const int expr got %s\n",type_name(itl,type));
                return std::pair{0,make_builtin(itl,builtin_type::void_t)}; 
            }

            // pull sym
            auto& sym = sym_from_slot(itl.symbol_table,sym_slot);

            // get access to const pool so we can inline the value
            const auto pool_slot = pool_slot_from_sym(sym);
            auto& section = pool_section_from_slot(itl.const_pool,pool_slot);

            static_assert(GPR_SIZE == 4);
            const u32 v = read_mem<u32>(itl.const_pool.buf,section.offset);

            return std::pair{v,type};
        }

        case ast_type::plus:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [value_left,type_left] = compile_const_int_expression(itl,bin_node->left);
            const auto [value_right,type_right] = compile_const_int_expression(itl,bin_node->right);

            const u32 ans = value_left + value_right;
            Type* type = effective_arith_type(itl,type_left,type_right);

            return std::pair{ans,type};
        }

        case ast_type::times:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [value_left,type_left] = compile_const_int_expression(itl,bin_node->left);
            const auto [value_right,type_right] = compile_const_int_expression(itl,bin_node->right);

            const u32 ans = value_left * value_right;
            Type* type = effective_arith_type(itl,type_left,type_right);

            return std::pair{ans,type};
        }

        case ast_type::minus:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [value_left,type_left] = compile_const_int_expression(itl,bin_node->left);
            const auto [value_right,type_right] = compile_const_int_expression(itl,bin_node->right);

            const u32 ans = value_left - value_right;
            Type* type = effective_arith_type(itl,type_left,type_right);

            return std::pair{ans,type};
        }

        case ast_type::divide:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [value_left,type_left] = compile_const_int_expression(itl,bin_node->left);
            const auto [value_right,type_right] = compile_const_int_expression(itl,bin_node->right);

            if(value_right == 0)
            {
                panic(itl,itl_error::int_type_error,"attempted to divide by zero in const expr\n");
                return std::pair{0,make_builtin(itl,builtin_type::void_t)}; 
            }

            const u32 ans = value_left / value_right;
            Type* type = effective_arith_type(itl,type_left,type_right);

            return std::pair{ans,type};
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
    itl.cur_expr = node;

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
   
                // push to const pool and save handle as offset for later loading...
                const auto slot = push_const_pool(itl.const_pool,pool_type::var,&v,GPR_SIZE);
                sym.reg.offset = slot.handle;

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

    // force constant
    decl_node->type->is_constant = true;

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
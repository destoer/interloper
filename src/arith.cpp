// NOTE: pass umod or udiv and it will figure out the correct one
template<const op_type type>
Type* compile_arith_op(Interloper& itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    static_assert(
        type == op_type::add_reg || type == op_type::sub_reg || type == op_type::mul_reg ||
        type == op_type::umod_reg || type == op_type::udiv_reg ||
        type == op_type::xor_reg || type == op_type::and_reg || type == op_type::or_reg
    );
    
    itl.arith_depth += 1;

    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left);
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right);

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }

    // pointer arith adds the size of the underlying type
    if(is_pointer(t1) && is_integer(t2) && (type == op_type::add_reg || type == op_type::sub_reg))
    {
        // get size of pointed to type
        Type *contained_type = deref_pointer(t1);

        const RegSlot offset_slot = mul_imm_res(itl,func,v2,type_size(itl,contained_type));
        emit_reg3<type>(itl,func,dst_slot,v1,offset_slot);
    }

    // allow pointer subtraction
    else if(is_pointer(t1) && is_pointer(t2) && type == op_type::sub_reg)
    {
        emit_reg3<op_type::sub_reg>(itl,func,dst_slot,v1,v2);
    }

    // floating point arith
    else if(is_float(t1) && is_float(t2))
    {
        switch(type)
        {
            case op_type::add_reg:
            {
                addf(itl,func,dst_slot,v1,v2);
                break;
            }

            case op_type::sub_reg:
            {
                subf(itl,func,dst_slot,v1,v2);
                break;
            }

            case op_type::mul_reg:
            {
                mulf(itl,func,dst_slot,v1,v2);
                break;
            }

            case op_type::udiv_reg:
            {
                divf(itl,func,dst_slot,v1,v2);
                break;
            }

            default:
            {
                panic(itl,itl_error::invalid_expr,"operation is not defined for floats");
                return make_builtin(itl,builtin_type::void_t);
            }
        }
    }

    // integer arith
    else if(is_integer(t1) && is_integer(t2))
    {
        // figure out correct division type
        if constexpr (type == op_type::udiv_reg)
        {
            if(is_signed(t1))
            {
                emit_reg3<op_type::sdiv_reg>(itl,func,dst_slot,v1,v2);
            }

            else
            {
                emit_reg3<op_type::udiv_reg>(itl,func,dst_slot,v1,v2);
            }
        }

        else if constexpr (type == op_type::umod_reg)
        {
            if(is_signed(t1))
            {
                emit_reg3<op_type::smod_reg>(itl,func,dst_slot,v1,v2);
            }

            else
            {
                emit_reg3<op_type::umod_reg>(itl,func,dst_slot,v1,v2);
            }
        }

        else
        {
            emit_reg3<type>(itl,func,dst_slot,v1,v2);
        }
    }

    else
    {
        panic(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %s and %s\n",type_name(itl,t1).buf,type_name(itl,t2).buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2,type);

    return final_type;        
}



Type* compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, RegSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left);
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right);

    if(!(is_integer(t1) && is_integer(t2)))
    {
        panic(itl,itl_error::int_type_error,"shifts only defined for integers, got %s and %s\n",type_name(itl,t1).buf,type_name(itl,t2).buf);
        return make_builtin(itl,builtin_type::void_t);
    }



    if(right)
    {
        // if signed do a arithmetic shift 
        if(is_signed(t1))
        {
            asr(itl,func,dst_slot,v1,v2);
        }

        else
        {
            lsr(itl,func,dst_slot,v1,v2);
        }
    }

    // left shift
    else
    {
        lsl(itl,func,dst_slot,v1,v2);
    }

    // type being shifted is the resulting type
    return t1;
}


b32 check_static_cmp(Interloper& itl, const Type* value, const Type* oper, u64 v)
{
    // unsigned value against signed value
    // if one side is signed and the other unsigned
    // allow comparision if the unsigned is a static value that
    // the signed side can represent
    if(!is_signed(value) && is_signed(oper))
    {
        // value is within range of operand value
        // change value to a the signed type
        if(v <= builtin_max(cast_builtin(oper)))
        {
            return true;
        }

        else
        {
            panic(itl,itl_error::out_of_bounds,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
        }
    }

    // value is outside the range of the other type
    else if(is_signed(value) == is_signed(oper))
    {
        if(builtin_size(cast_builtin(value)) > builtin_size(cast_builtin(oper)))
        {
            panic(itl,itl_error::out_of_bounds,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
        }
    }

    return false;
}


void emit_short_circuit_branches(Interloper& itl, Function& func, BlockSlot start_block, BlockSlot exit_block, RegSlot dst_slot, enum boolean_logic_op type)
{
    UNUSED(itl);
    auto &blocks = func.emitter.program;

    for(u32 b = start_block.handle; b < count(blocks) - 1; b++)
    {
        auto &block = func.emitter.program[b];
        const BlockSlot next = block_from_idx(b + 1);

        if(block.list.finish)
        {
            if(block.list.finish->value.op == op_type::exit_block)
            {
                remove(block.list,block.list.finish);
                emit_cond_branch(itl,func,block_from_idx(b),exit_block,next,dst_slot,type == boolean_logic_op::and_t? false : true);
            }
        }
    }
}



// TODO: Detect when short ciruciting is unecessary due to a lack of side effects
Type* compile_boolean_logic_op(Interloper& itl,Function &func,AstNode *node, RegSlot dst_slot, boolean_logic_op type, u32 depth)
{
    BinNode* bin_node = (BinNode*)node;
    
    BlockSlot left_block = cur_block(func);

    const ast_type syntax_nested = type == boolean_logic_op::and_t? ast_type::logical_and : ast_type::logical_or;
    const ast_type syntax_switch = type == boolean_logic_op::and_t? ast_type::logical_or : ast_type::logical_and;

    if(bin_node->left->type == syntax_nested)
    {
        compile_boolean_logic_op(itl,func,bin_node->left,dst_slot,type, depth + 1);
    }

    else
    {
        compile_expression(itl,func,bin_node->left,dst_slot);

        // switched from and to or
        // Which means our skip needs to be placed after all of these have compiled
        if(bin_node->left->type == syntax_switch)
        {
            left_block = cur_block(func);
        }
    }

    // First block needs to jump to exit
    if(depth == 0)
    {
        emit_block_internal(func,left_block,op_type::exit_block);
    }

    // Give this a new block we can jump over
    const BlockSlot right_block = new_basic_block(itl,func);
    compile_expression(itl,func,bin_node->right,dst_slot);

    // We are now at the top of the stack create and then rewrite in all the block exits
    if(depth == 0)
    {
        const BlockSlot exit_block = add_fall(itl,func);
        emit_short_circuit_branches(itl,func,left_block,exit_block,dst_slot,type);
    }

    // Any further blocks need an exit jump after compilation
    else
    {
        emit_block_internal(func,right_block,op_type::exit_block);
    }

    return make_builtin(itl,builtin_type::bool_t);
}

// handles <, <=, >, >=, ==, !=
template<const comparison_op type>
Type* compile_comparison_op(Interloper& itl,Function &func,AstNode *node, RegSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    auto [ltype,v1] = compile_oper(itl,func,bin_node->left);
    auto [rtype,v2] = compile_oper(itl,func,bin_node->right);

    // if one side is a value do type checking
    if(is_integer(ltype) && is_integer(rtype))
    {
        if(bin_node->left->type == ast_type::value || bin_node->right->type == ast_type::value)
        {
            if(bin_node->left->type == ast_type::value)
            {
                ValueNode* value_node = (ValueNode*)bin_node->left;
                const u64 v = value_node->value.v;

                const b32 coerce = check_static_cmp(itl,ltype,rtype,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    ltype = rtype;
                }
            }

            // right is a constant
            else
            {
                ValueNode* value_node = (ValueNode*)bin_node->right;
                const u64 v = value_node->value.v;

                
                const b32 coerce = check_static_cmp(itl,rtype,ltype,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    rtype = ltype;
                }
            }
        } 
    }

    if(!check_comparison_operation(itl,ltype,rtype,type))
    {
        return std::nullopt;
    }

    // float 
    if(is_float(ltype))
    {
        static constexpr op_type LOGIC_OPCODE[LOGIC_OP_SIZE] = 
        {
            op_type::cmpflt_reg,op_type::cmpfle_reg,op_type::cmpfgt_reg,op_type::cmpfge_reg,
            op_type::cmpfeq_reg,op_type::cmpfne_reg,        
        };

        constexpr op_type opcode_type = LOGIC_OPCODE[u32(type)];
        emit_reg3<opcode_type>(itl,func,dst_slot,v1,v2);
    }

    // integer operation
    else
    {
        // 0 is unsigned, 1 is signed
        static constexpr op_type LOGIC_OPCODE[2][LOGIC_OP_SIZE] = 
        {
            {op_type::cmpult_reg,op_type::cmpule_reg,op_type::cmpugt_reg,op_type::cmpuge_reg,
            op_type::cmpeq_reg,op_type::cmpne_reg},

            {op_type::cmpslt_reg,op_type::cmpsle_reg,op_type::cmpsgt_reg,
            op_type::cmpsge_reg,op_type::cmpeq_reg,op_type::cmpne_reg},
        };


        // TODO: fixme this should only be done when we know we have a builtin type
        // else we dont care
        const b32 sign = is_signed(ltype);

        if(sign)
        {
            constexpr op_type opcode_type = LOGIC_OPCODE[1][u32(type)];
            emit_reg3<opcode_type>(itl,func,dst_slot,v1,v2);
        }

        else
        {
            constexpr op_type opcode_type = LOGIC_OPCODE[0][u32(type)];
            emit_reg3<opcode_type>(itl,func,dst_slot,v1,v2);
        }
    }

    return make_builtin(itl,builtin_type::bool_t);
}




//  we dont want the 2nd stage IR handling how things need to be copied
// as it does not have the information required easily accessible
void compile_move(Interloper &itl, Function &func, RegSlot dst_slot, RegSlot src_slot, const Type* dst_type, const Type* src_type)
{
    // check the operation is even legal

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underyling type if its a pointer
    if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
    {
        if(is_float(dst_type))
        {
            mov_float(itl,func,dst_slot,src_slot);
        }

        else
        {
            mov_reg(itl,func,dst_slot,src_slot);
        }
    }

    else if(is_array(dst_type) && is_array(src_type))
    {
        if(is_fixed_array(dst_type))
        {
            mov_reg(itl,func,dst_slot,src_slot);
        }

        // runtime
        else
        {
            RegSlot addr_slot;
            
            switch(dst_slot.kind)
            {
                case reg_kind::tmp:
                case reg_kind::sym:
                {
                    addr_slot = addrof_res(itl,func,dst_slot);
                    break;
                }

                case reg_kind::spec:
                {
                    switch(dst_slot.spec)
                    {
                        case spec_reg::rv_struct:
                        {
                            addr_slot = make_sym_reg_slot(func.sig.args[0]);
                            break;
                        }

                        default: assert(false);
                    }

                    break;
                }
            }

            const RegSlot data_slot = load_arr_data(itl,func,src_slot,src_type);
            store_ptr(itl,func,data_slot,addr_slot,0,GPR_SIZE,false);

            const RegSlot len_slot = load_arr_len(itl,func,src_slot,src_type);
            store_ptr(itl,func,len_slot,addr_slot,GPR_SIZE,GPR_SIZE,false);
        } 
    }

    // requires special handling to move
    else if(is_struct(dst_type) && is_struct(src_type))
    {
        switch(dst_slot.kind)
        {
            case reg_kind::sym:
            case reg_kind::tmp:
            {
                const auto src_addr = make_struct_addr(src_slot,0);
                const auto dst_addr = make_struct_addr(dst_slot,0);

                ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
                break;
            }

            case reg_kind::spec:
            {
                switch(dst_slot.spec)
                {
                    // copy out the strucutre using the hidden pointer in the first arg
                    case spec_reg::rv_struct:
                    {
                        const auto src_addr = make_struct_addr(src_slot,0);
                        const auto dst_addr = make_addr(make_sym_reg_slot(func.sig.args[0]),0);

                        ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
                        break;
                    }

                    default: assert(false);
                }
                break;
            }
        }
    }

    else
    {
        assert(false);
    }
}


// TODO: should we make this more flexible?
std::pair<Type*,RegSlot> take_addr(Interloper &itl,Function &func,AstNode *node,RegSlot slot)
{
    NameSpace* name_space = nullptr;

    if(node->type == ast_type::scope)
    {
        ScopeNode* scope_node = (ScopeNode*)node;
        name_space = scan_namespace(itl.global_namespace,scope_node->scope);

        if(!name_space)
        {
            panic(itl,itl_error::undeclared,"Could not find namespace\n");
            return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
        }

        node = scope_node->expr;
    }

    // figure out what the addr is
    switch(node->type)
    {
        case ast_type::symbol:
        {
            LiteralNode* sym_node = (LiteralNode*)node;

            const auto name = sym_node->literal;
            auto sym_ptr = get_sym(itl.symbol_table,name);

            if(!sym_ptr)
            {
                // could be attempting to take a function pointer?
                auto func_def = name_space? lookup_func_def_scope(itl,name_space,name) : lookup_func_def_default(itl,name);

                if(func_def)
                {
                    // this may get called at some point so we need to mark it for compilation...
                    auto func_call_opt = finalise_func(itl,*func_def,(AstNode*)node);

                    if(!func_call_opt)
                    {
                        return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
                    }

                    auto& func_call = *func_call_opt;

                    FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,type_class::func_pointer_t,true);
                    type->sig = func_call.sig;

                    load_func_addr(itl,func,slot,func_call.label_slot);
                    
                    return std::pair{(Type*)type,slot};
                }
                
                // nothing found!
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration in addr\n",name.buf);
                return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
            }

            // get addr on symbol
            auto &sym = *sym_ptr;

            spill_slot(itl,func,sym.reg);

            if(is_fixed_array(sym.type))
            {
                panic(itl,itl_error::array_type_error,"[COMPILE]: cannot take pointer to fixed sized array\n");
                return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
            }

            Type* pointer_type = make_reference(itl,sym.type);

            // actually  get the addr of the ptr
            addrof(itl,func,slot,sym.reg.slot);
            return std::pair{pointer_type,slot};
        }

        case ast_type::index:
        {
            return index_arr(itl,func,node,slot);
        }

        case ast_type::access_struct:
        {
            auto [type,ptr_slot] = compute_member_ptr(itl,func,node);

            // make sure this ptr goes into the dst slot
            mov_reg(itl,func,slot,ptr_slot);

            return std::pair{type,ptr_slot};
        }

        default:
        {
            print(node);
            unimplemented("load_addr expr");
        }
    }
}

std::pair<Type*,RegSlot> take_pointer(Interloper& itl,Function& func, AstNode* deref_node)
{
    const auto [ptr_type,slot] = compile_oper(itl,func,deref_node);

    // make sure we actually have pointer
    if(!is_pointer(ptr_type))
    {
        panic(itl,itl_error::pointer_type_error,"Expected pointer got: %s\n",type_name(itl,ptr_type));
        return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
    }

    return {ptr_type,slot};
}
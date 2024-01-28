// NOTE: pass umod or udiv and it will figure out the correct one
template<const op_type type>
Type* compile_arith_op(Interloper& itl,Function &func,AstNode *node, SymSlot dst_slot)
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
    if(is_pointer(t1) && is_integer(t2))
    {
        // get size of pointed to type
        Type *contained_type = deref_pointer(t1);

        const SymSlot offset_slot = mul_imm_pow2_res(itl,func,v2,type_size(itl,contained_type));
        emit_reg3<type>(itl,func,dst_slot,v1,offset_slot);
    }

    // normal arith
    else
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

    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2,type);

    return final_type;        
}



Type* compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, SymSlot dst_slot)
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

// handles <, <=, >, >=, &&, ||, ==, !=
template<const logic_op type>
Type* compile_logical_op(Interloper& itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    auto [type_left,v1] = compile_oper(itl,func,bin_node->left);
    auto [type_right,v2] = compile_oper(itl,func,bin_node->right);



    // if one side is a value do type checking
    if(is_integer(type_left) && is_integer(type_right))
    {
        if(bin_node->left->type == ast_type::value || bin_node->right->type == ast_type::value)
        {
            if(bin_node->left->type == ast_type::value)
            {
                ValueNode* value_node = (ValueNode*)bin_node->left;
                const u64 v = value_node->value.v;

                const b32 coerce = check_static_cmp(itl,type_left,type_right,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    type_left = type_right;
                }
            }

            // right is a constant
            else
            {
                ValueNode* value_node = (ValueNode*)bin_node->right;
                const u64 v = value_node->value.v;

                
                const b32 coerce = check_static_cmp(itl,type_right,type_left,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    type_right = type_left;
                }
            }
        } 
    }

    // okay now then does a boolean operation make sense for this operator
    // with these types?


    switch(type)
    {
        // only bools are valid
        // || &&

        case logic_op::or_reg: case logic_op::and_reg:
        {
            if(!is_bool(type_left) && is_bool(type_right))
            {
                panic(itl,itl_error::bool_type_error,"operations || and && are only defined on bools\n");
            }
            break;
        }

        // <, <=, >, >=, ==, !=
        // valid if the underlying type is the same
        // and can somehow by interpretted as a integer
        // i.e pointers, ints, bools
        case logic_op::cmplt_reg: case logic_op::cmple_reg: case logic_op::cmpgt_reg:
        case logic_op::cmpge_reg: case logic_op::cmpeq_reg: case logic_op::cmpne_reg:
        {
            check_logical_operation(itl,type_left,type_right,type);
            break;
        }

        // this shouldunt happen
        default: 
        {
            crash_and_burn("%d is not a logical operation\n",s32(type));
        }
    }

    if(!itl.error)
    {
        // 0 is unsigned, 1 is signed
        static constexpr op_type LOGIC_OPCODE[2][LOGIC_OP_SIZE] = 
        {
            {op_type::cmpugt_imm,op_type::cmpult_reg,op_type::cmpule_reg,op_type::cmpugt_reg,op_type::cmpuge_reg,
            op_type::cmpeq_reg,op_type::cmpne_reg,op_type::and_reg,op_type::or_reg},

            {op_type::cmpsgt_imm,op_type::cmpslt_reg,op_type::cmpsle_reg,op_type::cmpsgt_reg,
            op_type::cmpsge_reg,op_type::cmpeq_reg,op_type::cmpne_reg, op_type::and_reg,op_type::or_reg},
        };


        // TODO: fixme this should only be done when we know we have a builtin type
        // else we dont care
        const b32 sign = is_signed(type_left);

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

        return make_builtin(itl,builtin_type::bool_t);
    }

    // operation is not valid for given types..
    else
    {
        return make_builtin(itl,builtin_type::void_t);
    }
}




//  we dont want the 2nd stage IR handling how things need to be copied
// as it does not have the information required easily accessible
void compile_move(Interloper &itl, Function &func, SymSlot dst_slot, SymSlot src_slot, const Type* dst_type, const Type* src_type)
{
    UNUSED(itl);
    // check the operation is even legal

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underyling type if its a pointer
    if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
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
            SymSlot addr_slot;

            if(dst_slot.handle == RV_IR)
            {
                addr_slot = func.sig.args[0];
            }

            else
            {   
                addr_slot = addrof_res(itl,func,dst_slot);
            }

            const SymSlot data_slot = load_arr_data(itl,func,src_slot,src_type);
            store_ptr(itl,func,data_slot,addr_slot,0,GPR_SIZE);

            const SymSlot len_slot = load_arr_len(itl,func,src_slot,src_type);
            store_ptr(itl,func,len_slot,addr_slot,GPR_SIZE,GPR_SIZE);
        } 
    }

    // requires special handling to move
    else if(is_struct(dst_type) && is_struct(src_type))
    {
        // copy out the strucutre using the hidden pointer in the first arg
        if(dst_slot.handle == RV_IR)
        {
            const auto src_addr = make_struct_addr(src_slot,0);
            const auto dst_addr = make_addr(func.sig.args[0],0);

            ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
        } 

        else
        {
            const auto src_addr = make_struct_addr(src_slot,0);
            const auto dst_addr = make_struct_addr(dst_slot,0);

            ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
        }
    }

    else
    {
        assert(false);
    }
}


// TODO: should we make this more flexible?
std::pair<Type*,SymSlot> take_addr(Interloper &itl,Function &func,AstNode *node,SymSlot slot)
{
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
                auto func_def = lookup_func_def(itl,name);

                if(func_def)
                {
                    // this may get called at some point so we need to mark it for compilation...
                    auto func_call_opt = finalise_func(itl,*func_def,(AstNode*)node);

                    if(!func_call_opt)
                    {
                        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
                    }

                    auto& func_call = *func_call_opt;

                    FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,FUNC_POINTER,true);
                    type->sig = func_call.sig;

                    load_func_addr(itl,func,slot,func_call.label_slot);
                    
                    return std::pair{(Type*)type,slot};
                }
                
                // nothing found!
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
            }

            // get addr on symbol
            auto &sym = *sym_ptr;

            spill_slot(itl,func,sym.reg);

            if(is_fixed_array(sym.type))
            {
                panic(itl,itl_error::array_type_error,"[COMPILE]: cannot take pointer to fixed sized array\n");
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
            }

            Type* pointer_type = make_pointer(itl,sym.type);

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

std::pair<Type*,SymSlot> take_pointer(Interloper& itl,Function& func, AstNode* deref_node)
{
    const auto [ptr_type,slot] = compile_oper(itl,func,deref_node);

    // make sure we actually have pointer
    if(!is_pointer(ptr_type))
    {
        panic(itl,itl_error::pointer_type_error,"Expected pointer got: %s\n",type_name(itl,ptr_type));
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }

    return {ptr_type,slot};
}
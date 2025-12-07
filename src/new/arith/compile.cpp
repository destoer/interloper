bool emit_known_rvalue(Interloper& itl, Function& func, arith_bin_op arith,RegSlot dst_slot, const TypedReg& left, Type* rtype, u64 value)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(arith)];
    const auto sign = is_signed(left.type) || is_signed(rtype);

    switch(arith)
    {
        case arith_bin_op::div_t:
        {
            if(sign)
            {
                return false;
            }

            udiv_imm(itl,func,dst_slot,left.slot,value);
            return true;
        }

        case arith_bin_op::mod_t:
        {
            if(sign)
            {
                return false;         
            }

            umod_imm(itl,func,dst_slot,left.slot,value);
            return true;      
        }

        case arith_bin_op::mul_t:
        {
            mul_imm(itl,func,dst_slot,left.slot,value);
            return true;
        }

        default:
        {
            const op_type type = arith_info.imm_form;
            if(type == op_type::none)
            {
                return false;
            }

            emit_imm3_unchecked(itl,func,type,dst_slot,left.slot,value);
            return true; 
        }
    }

    return false;
}


void emit_integer_ir(Interloper& itl, Function& func, arith_bin_op arith, RegSlot dst_slot, TypedReg left, TypedReg right)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(arith)];

    const bool sign = is_signed(left.type);

    const op_type type = sign? arith_info.reg_signed_form : arith_info.reg_unsigned_form;
    emit_reg3_unchecked(itl,func,type,dst_slot,left.slot,right.slot);
}

void emit_integer_arith(Interloper& itl, Function& func,ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    if(node->right->known_value)
    {
        const auto value = *node->right->known_value;
        const auto left = compile_oper(itl,func,node->left);

        if(emit_known_rvalue(itl,func,node->oper,dst_slot,left,node->right->expr_type,value))
        {
            return;
        } 

        const auto right = compile_oper(itl,func,node->right);
        emit_integer_ir(itl,func,node->oper,dst_slot,left,right);
    }

    // If this is commutative we can just switch the operands
    else if(node->left->known_value && arith_info.commutative)
    {
        const auto value = *node->left->known_value;
        const auto right = compile_oper(itl,func,node->right);

        if(emit_known_rvalue(itl,func,node->oper,dst_slot,right,node->left->expr_type,value))
        {
            return;
        }
        
        const auto left = compile_oper(itl,func,node->left);
        emit_integer_ir(itl,func,node->oper,dst_slot,left,right);
    }

    else
    {
        const auto left = compile_oper(itl,func,node->left);
        const auto right = compile_oper(itl,func,node->right);

        emit_integer_ir(itl,func,node->oper,dst_slot,left,right);
    }
}

void emit_float_arith(Interloper& itl, Function& func, ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    const auto left = compile_oper(itl,func,node->left);
    const auto right = compile_oper(itl,func,node->right);

    const op_type type = arith_info.float_form;
    emit_reg3_unchecked(itl,func,type,dst_slot,left.slot,right.slot);
}


void emit_pointer_arith(Interloper& itl, Function& func, ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    // get size of pointed to type
    Type *contained_type = deref_pointer(node->left->expr_type);
    const u32 size = type_size(itl,contained_type);

    const auto left = compile_oper(itl,func,node->left);

    if(node->right->known_value)
    {
        const auto value = *node->right->known_value * size;
        emit_imm3_unchecked(itl,func,arith_info.imm_form,dst_slot,left.slot,value);
    }

    else
    {
        const auto right = compile_oper(itl,func,node->right);

        if(node->oper == arith_bin_op::sub_t)
        {
            const RegSlot offset_slot = mul_imm_res(itl,func,right.slot,size);
            emit_reg3<op_type::sub_reg>(itl,func,dst_slot,left.slot,offset_slot);
        }

        else
        {
            const AddrSlot addr = generate_indexed_pointer(itl,func,left.slot,right.slot,size,0);
            collapse_struct_addr(itl,func,dst_slot,addr);
        }
    }
}

void compile_arith_bin(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    ArithBinNode* node = (ArithBinNode*)expr;

    const auto type = node->node.expr_type;

    // TODO: Consider switching on an enum saved during type checking.

    // pointer arith adds the size of the underlying type
    if(is_pointer(type))
    {
        emit_pointer_arith(itl,func,node,dst_slot);
    }

    // allow pointer subtraction
    else if(is_integer(type) || is_bool(type))
    {
        emit_integer_arith(itl,func,node,dst_slot);
    }

    // floating point arith
    else if(is_float(type))
    {
        emit_float_arith(itl,func,node,dst_slot);
    }

    else
    {
        (void)compile_panic(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %t",type);
    }
}

void compile_shift(Interloper& itl,Function& func, AstNode* expr, RegSlot dst_slot)
{
    ShiftNode* shift = (ShiftNode*)expr;

    const auto left = compile_oper(itl,func,shift->left);
    const auto right = compile_oper(itl,func,shift->right);

    switch(shift->oper)
    {
        case shift_op::right:
        {
            // if signed do a arithmetic shift 
            if(is_signed(shift->node.expr_type))
            {
                asr(itl,func,dst_slot,left.slot,right.slot);
            }

            else
            {
                lsr(itl,func,dst_slot,left.slot,right.slot);
            }

            break;
        }

        case shift_op::left:
        {
            lsl(itl,func,dst_slot,left.slot,right.slot);
            break;
        }
    }
}

void compile_arith_unary(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    ArithUnaryNode* unary = (ArithUnaryNode*)expr;

    switch(unary->oper)
    {
        case arith_unary_op::add_t:
        {
            compile_expression(itl,func,unary->expr,dst_slot);
            break;
        }
        
        case arith_unary_op::sub_t:
        {
            const auto reg = compile_oper(itl,func,unary->expr);

            if(is_float(reg.type))
            {
                const RegSlot slot = movf_imm_res(itl,func,0.0);
                subf(itl,func,dst_slot,slot,reg.slot);
            }


            else
            {
                const RegSlot slot = mov_imm_res(itl,func,0);
                sub(itl,func,dst_slot,slot,reg.slot);
            }

            break;
        }

        case arith_unary_op::bitwise_not_t:
        {
            const auto reg = compile_oper(itl,func,unary->expr);

            not_reg(itl,func,dst_slot,reg.slot);
            break;
        }

        case arith_unary_op::logical_not_t:
        {
            const auto reg = compile_oper(itl,func,unary->expr);

            // integer or pointer, eq to zero
            if(is_integer(reg.type) || is_pointer(reg.type))
            {
                cmp_eq_imm(itl,func,dst_slot,reg.slot,0);
            }

            // Zero length array
            else if(is_array(reg.type))
            {
                const auto len = load_arr_len(itl,func,reg);
                cmp_eq_imm(itl,func,dst_slot,len,0);
            }

            else
            {
                // xor can invert our boolean which is either 1 or 0
                xor_imm(itl,func,dst_slot,reg.slot,1);
            }

            break;
        }
    }
}

void emit_integer_compare(Interloper& itl, Function& func, comparison_op type, bool sign, RegSlot dst, RegSlot left, RegSlot right)
{
    // 0 is unsigned, 1 is signed
    static constexpr op_type COMPARISON_OPCODE[2][COMPARISON_OP_SIZE] = 
    {
        {op_type::cmpult_reg,op_type::cmpule_reg,op_type::cmpugt_reg,op_type::cmpuge_reg,
        op_type::cmpeq_reg,op_type::cmpne_reg},

        {op_type::cmpslt_reg,op_type::cmpsle_reg,op_type::cmpsgt_reg,
        op_type::cmpsge_reg,op_type::cmpeq_reg,op_type::cmpne_reg},
    };


    const op_type opcode_type = COMPARISON_OPCODE[u32(sign)][u32(type)];
    emit_reg3_unchecked(itl,func,opcode_type,dst,left,right);
}

// handles <, <=, >, >=, ==, !=
void compile_comparison(Interloper& itl,Function &func,AstNode *expr, RegSlot dst_slot)
{
    CmpNode* cmp = (CmpNode*)expr;
    const auto type = cmp->oper;

    const auto left = compile_oper(itl,func,cmp->left);
    const auto right = compile_oper(itl,func,cmp->right);


    // float 
    if(is_float(left.type))
    {
        static constexpr op_type COMPARISON_OPCODE[COMPARISON_OP_SIZE] = 
        {
            op_type::cmpflt_reg,op_type::cmpfle_reg,op_type::cmpfgt_reg,op_type::cmpfge_reg,
            op_type::cmpfeq_reg,op_type::cmpfne_reg,        
        };

        const op_type opcode_type = COMPARISON_OPCODE[u32(type)];
        emit_reg3_unchecked(itl,func,opcode_type,dst_slot,left.slot,right.slot);
    }

    // integer operation
    else
    {
        emit_integer_compare(itl,func,type,is_signed(left.type),dst_slot,left.slot,right.slot);
    }
}

void emit_short_circuit_branches(Interloper& itl, Function& func, BlockSlot start_block, BlockSlot exit_block, RegSlot dst_slot, enum boolean_logic_op type)
{
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

void compile_boolean_logic_op(Interloper& itl,Function &func,BooleanLogicNode* logic, RegSlot dst_slot,u32 depth)
{
    BlockSlot left_block = cur_block(func);

    if(logic->left->type == ast_type::boolean_logic)
    {
        BooleanLogicNode* logic_left = (BooleanLogicNode*)logic->left;

        if(logic_left->oper == logic->oper)
        {
            compile_boolean_logic_op(itl,func,logic_left,dst_slot,depth + 1);
        }

        // switched from and to or
        // Which means our skip needs to be placed after all of these have compiled
        else
        {
            compile_expression(itl,func,logic->left,dst_slot);
            left_block = cur_block(func);
        }
    }

    else
    {
        compile_expression(itl,func,logic->left,dst_slot);
    }

    // First block needs to jump to exit
    if(depth == 0)
    {
        const Opcode exit_block = make_implicit_instr(op_type::exit_block);
        emit_block_internal(func,left_block,exit_block);
    }

    // Give this a new block we can jump over
    const BlockSlot right_block = new_basic_block(itl,func);
    compile_expression(itl,func,logic->right,dst_slot);

    // We are now at the top of the stack create and then rewrite in all the block exits
    if(depth == 0)
    {
        const BlockSlot exit_block = add_fall(itl,func);
        emit_short_circuit_branches(itl,func,left_block,exit_block,dst_slot,logic->oper);
    }

    // Any further blocks need an exit jump after compilation
    else
    {
        const Opcode exit_block = make_implicit_instr(op_type::exit_block);
        emit_block_internal(func,right_block,exit_block);
    }
}

void compile_boolean_logic(Interloper& itl,Function &func,AstNode *expr, RegSlot dst_slot)
{
    BooleanLogicNode* logic = (BooleanLogicNode*)expr;
    compile_boolean_logic_op(itl,func,logic,dst_slot,0);
}
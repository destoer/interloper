#include <interloper.h>


Type compile_expression(Interloper &itl,Function &func,AstNode *node);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode &line);
void compile_decl(Interloper &itl,Function &func, const AstNode &line);
void compile_block(Interloper &itl,Function &func,AstNode *node);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);


// scan the top level of the parse tree for functions
// and grab the entire signature
// we wont worry about the scope on functions for now as we wont have namespaces for a while
void parse_function_declarations(Interloper& itl)
{
    for(const auto n : itl.root->nodes)
    {
        const auto &node = *n;
        // unless its a function declaration we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }


        
        const auto return_type = node.nodes[0]->variable_type;
        const auto name = node.literal;
        
        if(itl.function_table.count(name))
        {
            panic(itl,"function %s has been declared twice!\n",name.c_str());
            return;
        }


        std::vector<Symbol> args;


        const auto decl = node.nodes[2];

        // rip every arg
        for(const auto a : decl->nodes)
        {
            const auto name = a->literal;
            const auto type = a->nodes[0]->variable_type;

            args.push_back(Symbol(name,type,true));
        }


        const Function function(name,return_type,args,itl.symbol_table.label_lookup.size());


        itl.function_table[name] = function;

        // add as a label as it this will be need to referenced by call instrs
        // in the ir to get the name back
        add_label(itl.symbol_table,name);
    }

    // ensure the entry function is defined
    if(!itl.function_table.count("main"))
    {
        panic(itl,"main is not defined!\n");
    }
}



std::pair<Type,u32> compile_oper(Interloper& itl,Function &func,AstNode *node)
{
    const auto t1 = compile_expression(itl,func,node);
    const auto v1 = reg(func.emitter.reg_count);

    return std::pair<Type,u32>{t1,v1};
}

// TODO: compile_arith_op, compile_logical_op, compile_shift may need changing
// if we add operator overloading

Type compile_arith_op(Interloper& itl,Function &func,AstNode *node, op_type type)
{
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0]);
    func.emitter.reg_count++;

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1]);


    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2);


    // one of these is just a temp for the result calc
    // so after this we no longer need it
    emit(func.emitter,type,v1,v1,v2);
    func.emitter.reg_count--;

    return final_type;        
}

Type compile_shift(Interloper& itl,Function &func,AstNode *node,bool right)
{
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0]);
    func.emitter.reg_count++;

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1]);

    if(!(is_integer(t1) && is_integer(t2)))
    {
        panic(itl,"shifts only defined for integers, got %s and %s\n",type_name(itl,t1).c_str(),type_name(itl,t2).c_str());
        return Type(builtin_type::void_t);
    }

    if(right)
    {
        // if signed do a arithmetic shift 
        if(is_signed(t1))
        {
            emit(func.emitter,op_type::asr_reg,v1,v1,v2);
        }

        else
        {
            emit(func.emitter,op_type::lsr_reg,v1,v1,v2);
        }
    }

    // left shift
    else
    {
        emit(func.emitter,op_type::lsl_reg,v1,v1,v2);
    }

    // no longer need the tmp
    func.emitter.reg_count--;

    // type being shifted is the resulting type
    return t1;
}

// handles <, <=, >, >=, &&, ||, ==, !=
Type compile_logical_op(Interloper& itl,Function &func,AstNode *node, logic_op type)
{
    auto [t1,v1] = compile_oper(itl,func,node->nodes[0]);
    func.emitter.reg_count++;

    auto [t2,v2] = compile_oper(itl,func,node->nodes[1]);


    // if one side is a value do type checking
    if(is_integer(t1) && is_integer(t2))
    {
        if(node->nodes[0]->type == ast_type::value || node->nodes[1]->type == ast_type::value)
        {
            const bool is_op1 = node->nodes[0]->type == ast_type::value;

            u32 v = is_op1? node->nodes[0]->value.v : node->nodes[1]->value.v;
            Type &oper = is_op1? t2 : t1;
            Type &value = is_op1? t1 : t2;


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
                    value = oper;
                }

                else
                {
                    panic(itl,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
                }
            }

            // value is outside the range of the other type
            else if(is_signed(value) == is_signed(oper))
            {
                if(builtin_size(cast_builtin(value)) > builtin_size(cast_builtin(oper)))
                {
                    panic(itl,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
                }
            }
        } 
    }

    // two bools is fine
    else if(is_bool(t1) && is_bool(t2))
    {

    }

    else
    {
        unimplemented("comparison on non integeral type!");
    }

    // okay now then does a boolean operation make sense for this operator
    // with these types?


    switch(type)
    {
        // only bools are valid
        // || &&

        case logic_op::or_reg: case logic_op::and_reg:
        {
            if(!is_bool(t1) && is_bool(t2))
            {
                panic(itl,"operations || and && are only defined on bools\n");
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
            check_logical_operation(itl,t1,t2);
            break;
        }

        // this shouldunt happen
        default: 
        {
            printf("%d is not a logical operation\n",static_cast<int>(type));
            exit(1);
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
        const auto sign = is_signed(static_cast<builtin_type>(t1.type_idx));

        // if we have gotten this far the sign of both are the same
        const auto op = LOGIC_OPCODE[sign][static_cast<int>(type)];


        // one of these is just a temp for the result calc
        // so afer this we no longer need it
        emit(func.emitter,op,v1,v1,v2);
        func.emitter.reg_count--;

        return Type(builtin_type::bool_t);
    }

    // operation is not valid for given types..
    else
    {
        return Type(builtin_type::void_t);
    }
}



Type compile_function_call(Interloper &itl,Function &func,AstNode *node)
{
    // check function is declared
    if(!itl.function_table.count(node->literal))
    {
        panic(itl,"[COMPILE]: function %s is not declared\n",node->literal.c_str());
        return Type(builtin_type::void_t);
    }
    const auto &func_call = itl.function_table[node->literal];

    // check we have the right number of params
    if(func_call.args.size() != node->nodes.size())
    {
        panic(itl,"[COMPILE]: function call expected %d args got %d\n",func_call.args.size(),node->nodes.size());
        print(node);
        return Type(builtin_type::void_t);
    }


    // TODO: this needs to be redone properly when we try implement a register allocator
    // TODO: allow saved regs can be used while we are pushing args

    // callee save regs in use
    // return register zero is reserved dont push it
    for(u32 i = 1; i < func.emitter.reg_count; i++)
    {
        emit(func.emitter,op_type::save_reg, reg(i));
    }


    // push args in reverse order and type check them
    for(s32 i = func_call.args.size() - 1; i >= 0; i--)
    {
        // TODO: handle being passed args that wont fit inside a single hardware reg
        if(type_size(itl,func_call.args[i].type) > sizeof(u32))
        {
            unimplemented("function arg: non register size: %d\n",type_size(itl,func_call.args[i].type));
        }

        
        // builtin type
        const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[i]);


        // type check the arg
        check_assign(itl,func_call.args[i].type,arg_type);

        // finally push the arg
        emit(func.emitter,op_type::push_arg,reg);
    }



    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    emit(func.emitter,op_type::call,func_call.slot);


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    emit(func.emitter,op_type::clean_args,func_call.args.size());

  

    // restore callee saved values
    // do in reverse order to the save so it can be implemented as push and pop
    // has to be done after the arg cleanup
    for(int i = func.emitter.reg_count - 1; i >= 1; i--)
    {
        emit(func.emitter,op_type::restore_reg, reg(i));
    }




    // if function returns a value save the return register
    if(func.return_type.type_idx != static_cast<int>(builtin_type::void_t))
    {
        emit(func.emitter,op_type::mov_reg,reg(func.emitter.reg_count),reg(RV));
    }

    // result of expr is the return type
    return func_call.return_type;    
}

Type compile_expression(Interloper &itl,Function &func,AstNode *node)
{
    if(!node)
    {
        assert(false);
        return Type(builtin_type::void_t);
    }

   
    switch(node->type)
    {
        case ast_type::cast:
        {
            const auto old_type = compile_expression(itl,func,node->nodes[1]);
            const auto new_type = node->nodes[0]->variable_type;

            handle_cast(itl,func.emitter,old_type,new_type);

            return new_type;
        }


        // multiple assigment
        case ast_type::equal:
        {
            const auto [rtype,reg] = compile_oper(itl,func,node->nodes[1]);

            const auto name = node->nodes[0]->literal;

            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                print(node);
                return Type(builtin_type::void_t);
            }

            
            const auto &sym = sym_opt.value();

            check_assign(itl,sym.type,rtype);

            emit(func.emitter,op_type::mov_reg,slot_idx(sym),reg);

            return sym.type;        
        }


        case ast_type::value:
        { 
            // figure out what the min storage is required to fit this value
            if(node->value.sign)
            {
                const auto value = static_cast<s32>(node->value.v);
                emit(func.emitter,op_type::mov_imm,reg(func.emitter.reg_count),value);

                // what is the smallest storage type that this will fit inside?
                if(in_range(value,static_cast<s32>(builtin_min(builtin_type::s8_t)),static_cast<s32>(builtin_max(builtin_type::s8_t))))
                {
                    return  Type(builtin_type::s8_t);
                }

                else if(in_range(value,static_cast<s32>(builtin_min(builtin_type::s16_t)),static_cast<s32>(builtin_max(builtin_type::s16_t))))
                {
                    return Type(builtin_type::s16_t);
                }

                //else if(value,static_cast<s32>(builtin_min(builtin_type::s32_t)),static_cast<s32>(builtin_max(builtin_type::s32_t)))
                else
                {
                    return Type(builtin_type::s32_t);
                }
            }

            else
            {
                const u32 value = node->value.v;
                emit(func.emitter,op_type::mov_imm,reg(func.emitter.reg_count),value);

                // what is the smallest storage type that this will fit inside?
                if(in_range(value,builtin_min(builtin_type::u8_t),builtin_max(builtin_type::u8_t)))
                {
                    return  Type(builtin_type::u8_t);
                }

                else if(in_range(value,builtin_min(builtin_type::u16_t),builtin_max(builtin_type::u16_t)))
                {
                    return Type(builtin_type::u16_t);
                }

                //else if(in_range(value,builtin_min(builtin_type::u32_t),builtin_max(builtin_type::u32_t))
                else
                {
                    return Type(builtin_type::u32_t);
                }
            }
        }

        case ast_type::false_t:
        {
            emit(func.emitter,op_type::mov_imm,reg(func.emitter.reg_count),0);
            return Type(builtin_type::bool_t);
        }

        case ast_type::true_t:
        {
            emit(func.emitter,op_type::mov_imm,reg(func.emitter.reg_count),1);
            return Type(builtin_type::bool_t);
        }

        case ast_type::symbol:
        {
            const auto name = node->literal;

            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                print(node);
                return Type(builtin_type::void_t);
            }

            const auto &sym = sym_opt.value();

            emit(func.emitter,op_type::mov_reg,reg(func.emitter.reg_count),slot_idx(sym));

            return sym.type;
        }

    
        case ast_type::minus:
        {
            // unary minus
            if(!node->nodes[1])
            {
                // negate by doing 0 - v
                const auto [t,dst] = compile_oper(itl,func,node->nodes[0]);

                func.emitter.reg_count++;

                // todo: make sure our optimiser sees through this
                emit(func.emitter,op_type::mov_imm,reg(func.emitter.reg_count),0);
                emit(func.emitter,op_type::sub_reg,dst,reg(func.emitter.reg_count),dst);
                
                func.emitter.reg_count--;

                return t;
            }

            else
            {
                return compile_arith_op(itl,func,node,op_type::sub_reg);
            }
        }

        case ast_type::plus:
        {
        
            // unary plus
            if(!node->nodes[1])
            {
                return compile_expression(itl,func,node->nodes[0]); 
            }

            else
        
            {
                return compile_arith_op(itl,func,node,op_type::add_reg);
            }
        }
    
        case ast_type::times:
        {
            return compile_arith_op(itl,func,node,op_type::mul_reg);        
        }


        case ast_type::divide:
        {
            return compile_arith_op(itl,func,node,op_type::div_reg);       
        }

        case ast_type::mod:
        {
            return compile_arith_op(itl,func,node,op_type::mod_reg);       
        }

        case ast_type::shift_l:
        {
            return compile_shift(itl,func,node,false);
        }

        case ast_type::shift_r:
        {
            return compile_shift(itl,func,node,true);
        }        
        
        case ast_type::bitwise_and:
        {
            return compile_arith_op(itl,func,node,op_type::and_reg);
        }

        case ast_type::bitwise_or:
        {
            return compile_arith_op(itl,func,node,op_type::or_reg);
        }

        case ast_type::bitwise_xor:
        {
            return compile_arith_op(itl,func,node,op_type::xor_reg);
        }

        case ast_type::bitwise_not:
        {
            const auto [t,reg] = compile_oper(itl,func,node->nodes[0]);

            // TODO: do we need to check this is integer?


            emit(func.emitter,op_type::not_reg,reg);
            return t;
        }    

        case ast_type::logical_not:
        {
            const auto [t,reg] = compile_oper(itl,func,node->nodes[0]);

            if(!is_bool(t))
            {
                panic(itl,"compile: logical_not expected bool got: %s\n",type_name(itl,t).c_str());
                return Type(builtin_type::void_t);
            }

            // xor can invert our boolean which is either 1 or 0
            emit(func.emitter,op_type::xor_imm,reg,reg,1);
            return t;
        }

        // we want to pass in the base operation but we need to do the actual type checking
        // to know what we are comparing later
        // how should we do it?
        case ast_type::logical_lt:
        {
            return compile_logical_op(itl,func,node,logic_op::cmplt_reg);
        }

        case ast_type::logical_le:
        {
            return compile_logical_op(itl,func,node,logic_op::cmple_reg);
        }

        case ast_type::logical_gt:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpgt_reg);
        }

        case ast_type::logical_ge:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpge_reg);
        }

        case ast_type::logical_eq:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpeq_reg);
        }

        case ast_type::logical_ne:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpne_reg);
        }

        case ast_type::logical_and:
        {
            return compile_logical_op(itl,func,node,logic_op::and_reg);
        }

        case ast_type::logical_or:
        {
            return compile_logical_op(itl,func,node,logic_op::or_reg);
        }

        case ast_type::function_call:
        {
            return compile_function_call(itl,func,node);
        }

        default:
        {
            panic(itl,"[COMPILE]: invalid expression\n");
            print(node);
            return Type(builtin_type::void_t);
        }
    }
}

std::string label_name(u32 slot)
{
    return "L" + std::to_string(slot);
}

u32 new_basic_block(Interloper &itl,Function &func)
{
    const u32 slot = itl.symbol_table.label_lookup.size();

    const u32 basic_block = func.emitter.program.size();

    new_block(func.emitter,slot);
    add_label(itl.symbol_table,label_name(slot));

    // offset is the block offset until full resolution
    itl.symbol_table.label_lookup[slot].offset = basic_block;

    return slot;   
}

// asume one cond
void compile_for_block(Interloper &itl,Function &func,AstNode *node)
{
    // scope for any var decls in the stmt
    new_scope(itl.symbol_table);

    const u32 intial_block = func.emitter.program.size() - 1;

    // single statment
    const bool single_statement = node->nodes.size() == 2;

    const u32 cond_expr_idx = single_statement? 0 : 1;
    const u32 block_expr_idx = single_statement? 1 : 3;

    // compile the first stmt (ussualy an assign)
    if(!single_statement)
    {
        assert(node->nodes.size() == 4);

        const auto type = node->nodes[0]->type;

        // handle this being a declaration
        switch(type)
        {
            case ast_type::auto_decl:
            {
                compile_auto_decl(itl,func,*node->nodes[0]);
                break;
            }

            case ast_type::declaration:
            {
                compile_decl(itl,func,*node->nodes[0]);
                break;
            }


            default:
            {
                compile_expression(itl,func,node->nodes[0]);
                break;
            }
        }

        if(itl.error)
        {
            return;
        }
    }

    const auto [t,stmt_cond_reg] = compile_oper(itl,func,node->nodes[cond_expr_idx]);

    if(!is_bool(t))
    {
        panic(itl,"expected bool got %s in for condition\n",type_name(itl,t).c_str());
        return;
    }    


    // compile the body
    const u32 cur = new_basic_block(itl,func);
    
    compile_block(itl,func,node->nodes[block_expr_idx]);    

    // compile loop end stmt
    if(!single_statement)
    {
        compile_expression(itl,func,node->nodes[2]);
    }

    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,node->nodes[cond_expr_idx]);
    emit(func.emitter,op_type::bc,cur,loop_cond_reg);


    const u32 exit_block = new_basic_block(itl,func);

    // emit branch over the loop body in initial block
    // if cond is not met
    func.emitter.program[intial_block].push_back(Opcode(op_type::bnc,exit_block,stmt_cond_reg,0));

    destroy_scope(itl.symbol_table);
}


void compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    const u32 start_block = func.emitter.program.size();

    for(u32 n = 0; n < node->nodes.size(); n++)
    {
        const auto &if_stmt = *node->nodes[n];

        if(if_stmt.type != ast_type::else_t)
        {

            if(n != 0)
            {
                // create new block for compare
                new_basic_block(itl,func);
            }

            // compile the compare expr for conditon
            const auto [t,r] = compile_oper(itl,func,if_stmt.nodes[0]);

            if(!is_bool(t))
            {
                panic(itl,"expected bool got %s in if condition\n",type_name(itl,t).c_str());
                return;
            }

            const u32 cur_block = func.emitter.program.size() - 1;

            // compile the body block
            new_basic_block(itl,func);
            compile_block(itl,func,if_stmt.nodes[1]);

            // add branch over the block we just compiled
            const u32 slot = itl.symbol_table.label_lookup.size();

            func.emitter.program[cur_block].push_back(Opcode(op_type::bnc,slot,r,0));


            // not the last statment (branch is not require)
            if(n != node->nodes.size() - 1)
            {
                // emit a directive so we know to insert a branch
                // to the exit block here once we know how many blocks
                // there are
                emit(func.emitter,op_type::exit_block);
            }
        }

        // else stmt has no expr so its in the first node
        // by definition this is the last statement
        else 
        {
            // create block for body
            new_basic_block(itl,func);

            compile_block(itl,func,if_stmt.nodes[0]);
        }
    }

    // create the exit block, for new code
    const u32 exit_block = new_basic_block(itl,func);



    // for every body block bar the last we just added
    // add a unconditonal branch to the "exit block"
    // the last block is directly before the next and does not need one

    for(u32 b = start_block; b < func.emitter.program.size() - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.back().op == op_type::exit_block)
        {
            block.back() = Opcode(op_type::b,exit_block,0,0);
        }
    }
}



// TODO: fold sucessive adds for stack reclaims
// i.e 

/*
    call 0x10
    add sp, sp, 8
    add sp, sp, 4
    ret

    ->

    call 0x10
    add sp, sp, 12
    ret
*/



void compile_decl(Interloper &itl,Function &func, const AstNode &line)
{
    // get entry into symbol table
    const auto name = line.literal;
    const auto ltype = line.nodes[0]->variable_type;

    if(get_sym(itl.symbol_table,name))
    {
        panic(itl,"redeclared symbol: %s\n",name.c_str());
        return;
    }

    const auto size = type_size(itl,ltype);

    // add allocation information
    const auto slot = add_var(func,name,ltype,size);

    // add new symbol table entry
    add_symbol(itl.symbol_table,name,ltype,slot);


    const auto &sym = get_sym(itl.symbol_table,name).value();


    emit(func.emitter,op_type::alloc_slot,slot_idx(sym));

    // handle right side expression (if present)
    if(line.nodes.size() == 2)
    {
        const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1]);
        check_assign(itl,ltype,rtype);

        emit(func.emitter,op_type::mov_reg,slot_idx(sym),reg);
    }    
}

void compile_auto_decl(Interloper &itl,Function &func, const AstNode &line)
{
    const auto name = line.literal;

    if(get_sym(itl.symbol_table,name))
    {
        panic(itl,"redeclared symbol: %s\n",name.c_str());
        return;
    }

    
    const auto [type,reg] = compile_oper(itl,func,line.nodes[0]);

    // add the symbol

    const auto size = type_size(itl,type);

    // add allocation information
    const auto slot = add_var(func,name,type,size);
    
    // add new symbol table entry
    add_symbol(itl.symbol_table,name,type,slot);

    const auto &sym = get_sym(itl.symbol_table,name).value();


    emit(func.emitter,op_type::alloc_slot,slot_idx(sym));

    emit(func.emitter,op_type::mov_reg,slot_idx(sym),reg);
}

void compile_block(Interloper &itl,Function &func,AstNode *node)
{
   new_scope(itl.symbol_table);

    for(auto l : node->nodes)
    {
        if(itl.error)
        {
            return;
        }

        const auto &line = *l;

        // first register reserved for return value
        func.emitter.reg_count = 1;

        switch(line.type)
        {
            // variable declaration
            case ast_type::declaration:
            {
                compile_decl(itl,func,line);
                break;
            }


            case ast_type::auto_decl:
            {
                compile_auto_decl(itl,func,line);
                break;
            }



            // assignment
            case ast_type::equal:
            {
                const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1]);

                const auto name = line.nodes[0]->literal;


                const auto sym_opt = get_sym(itl.symbol_table,name);
                if(!sym_opt)
                {
                    panic(itl,"[COMPILE]: symbol '%s' assigned before declaration\n",name.c_str());
                    print(l);
                    return;
                }

                const auto &sym = sym_opt.value();

                check_assign(itl,sym.type,rtype);

                emit(func.emitter,op_type::mov_reg,slot_idx(sym),reg);
                break;
            }

            case ast_type::ret:
            {
                // has an arg
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(itl,func,line.nodes[0]);

                    if(itl.error)
                    {
                        return;
                    }

                    check_assign(itl,func.return_type,rtype);

                    // TODO: we are gonna require this when we have a register allocator
                    emit(func.emitter,op_type::ret_mov,reg(RV),v1);
                }
                
                itl.has_return = true;
                emit(func.emitter,op_type::ret);
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,l);
                break;
            }

            case ast_type::block:
            {
                compile_block(itl,func,l);
                break;
            }


            case ast_type::for_block:
            {
                compile_for_block(itl,func,l);
                break;
            }

            case ast_type::if_block:
            {
                compile_if_block(itl,func,l);
                break;
            }

            default:
            {
                panic(itl,"[COMPILE] unexpected token\n");
                print(l);
            }
        }
    }


    // std::max the sizes this is what we need to allocate
    for(int i = 0; i < 3; i++)
    {
        func.size_count[i] = std::max(func.size_count[i],func.size_count_cur[i]);
    }

    // scope is about to be destroyed reclaim the stack for every var that is no longer used
    for(const auto &[key, sym] : itl.symbol_table.table[itl.symbol_table.table.size()-1])
    {
        if(!sym.is_arg)
        {
            emit(func.emitter,op_type::free_slot,slot_idx(sym)); 

            // free the stack alloc for each var thats about to go out of scope
            const auto &var_alloc = func.slot_lookup[sym.slot];
            func.size_count_cur[var_alloc.size >> 1]--;
        }
    }

    destroy_scope(itl.symbol_table);
}

void compile_functions(Interloper &itl)
{
    // global scope
    new_scope(itl.symbol_table);

    for(const auto n: itl.root->nodes)
    {
        const auto &node = *n;

        // unless its a function we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }

        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        new_scope(itl.symbol_table);

        auto &func = itl.function_table[node.literal];

        for(auto &sym: func.args)
        {
            const auto size = type_size(itl,sym.type);

            // this has to be parsed in forward order or all the offsets
            // will be messed up
            const auto slot = add_arg(func,sym.name,sym.type,size);

            add_symbol(itl.symbol_table,sym,slot);
        }


        itl.has_return = false;

        new_basic_block(itl,func);


        // parse out each line of the function
        auto block = node.nodes[1];
        compile_block(itl,func,block);

        destroy_scope(itl.symbol_table);

        if(!itl.has_return)
        {
            // is a void function this is fine
            // we just need to emit the ret at the end 
            if(func.return_type.type_idx == static_cast<int>(builtin_type::void_t))
            {
                emit(func.emitter,op_type::ret);
            }

            else
            {
                panic(itl,"[COMPILE]: non void function without a return\n");
            }
        }


        if(itl.error)
        {
            return;
        }
    }
}

void dump_ir_sym(Interloper &itl)
{
    for(auto &[key,func] : itl.function_table)
    {
        UNUSED(key);

        dump_ir(func,itl.symbol_table.label_lookup);
    }    
}


// TODO: impl source line information on the parse tree

// plan:
// reg alloc -> pointers -> structs -> arrays -> strings -> imports
// -> early stl -> function_pointers -> labels ->  compile time execution ->
// unions -> inline asm

void compile(Interloper &itl,const std::vector<std::string> &lines)
{
    // make sure everything is clean
    itl.program.clear();
    clear(itl.symbol_table);
    itl.function_table.clear();

    // clear the tree if present
    if(itl.root)
    {
        delete_tree(itl.root); itl.root = nullptr;
    }

    itl.error = false;

    // tokenize input file
    {
        if(tokenize(itl.lexer,lines))
        {
            itl.error = true;
            return;
        }

        //print_tokens(tokens);


        // build ast
        const b32 parser_error = parse(&itl.root,itl.lexer.tokens,lines);

    
        if(!itl.root || parser_error)
        {
            itl.error = true;
            return;
        }
    }


    print(itl.root);


    // okay now we need to start doing semantic analysis
    // first handle any imports, macros etc (skip for now)
    // handle any type declartions (skip for now)
    // handle function declartions


    parse_function_declarations(itl);

    if(itl.error)
    {
        return;
    }

    putchar('\n');

    //  print function definitions
/*
    for(const auto &[key, f] : function_table)
    {
        printf("function: %s\n",f.name.c_str());
        printf("returns: %s\n",type_name(f.return_type).c_str());

        puts("args: ");
        // args:
        for(const auto &s: f.args)
        {
            printf("%s: %s\n",s.name.c_str(),type_name(s.type).c_str());
        }
        puts("\n\n");
    }
*/

    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 

    compile_functions(itl);


    // okay we dont need the parse tree anymore
    // free it
    delete_tree(itl.root); itl.root = nullptr;

    if(itl.error)
    {
        return;
    }


    //optimise_ir(itl);

    dump_ir_sym(itl);

    // perform register allocation
    for(auto &[key, func]: itl.function_table)
    {
        UNUSED(key);
        allocate_registers(func);
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm(itl);


}

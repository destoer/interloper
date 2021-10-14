#include <interloper.h>


Interloper::Interloper()
{

}

// scan the top level of the parse tree for functions
// and grab the entire signature
// we wont worry about the scope on functions for now as we wont have namespaces for a while
void Interloper::parse_function_declarations()
{
    for(const auto n : root->nodes)
    {
        const auto &node = *n;
        // unless its a function declaration we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }


        
        const auto return_type = node.nodes[0]->variable_type;
        const auto name = node.literal;
        
        if(function_table.count(name))
        {
            panic("function %s has been declared twice!\n",name.c_str());
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


        const Function function(name,return_type,args,symbol_table.label_lookup.size());


        function_table[name] = function;

        // add as a label as it this will be need to referenced by call instrs
        // in the ir to get the name back
        symbol_table.add_label(name);
    }

    // ensure the entry function is defined
    if(!function_table.count("main"))
    {
        panic("main is not defined!\n");
    }
}



std::pair<Type,u32> Interloper::compile_oper(Function &func,AstNode *node)
{
    const auto t1 = compile_expression(func,node);
    const auto v1 = reg(func.emitter.reg_count);

    return std::pair<Type,u32>{t1,v1};
}

// TODO: compile_arith_op, compile_logical_op, compile_shift may need changing
// if we add operator overloading

Type Interloper::compile_arith_op(Function &func,AstNode *node, op_type type)
{
    const auto [t1,v1] = compile_oper(func,node->nodes[0]);
    func.emitter.reg_count++;

    const auto [t2,v2] = compile_oper(func,node->nodes[1]);


    // produce effective type
    const auto final_type = effective_arith_type(t1,t2);


    // one of these is just a temp for the result calc
    // so after this we no longer need it
    func.emitter.emit(type,v1,v1,v2);
    func.emitter.reg_count--;

    return final_type;        
}

Type Interloper::compile_shift(Function &func,AstNode *node,bool right)
{
    const auto [t1,v1] = compile_oper(func,node->nodes[0]);
    func.emitter.reg_count++;

    const auto [t2,v2] = compile_oper(func,node->nodes[1]);

    if(!(is_integer(t1) && is_integer(t2)))
    {
        panic("shifts only defined for integers, got %s and %s\n",type_name(t1).c_str(),type_name(t2).c_str());
        return Type(builtin_type::void_t);
    }

    if(right)
    {
        // if signed do a arithmetic shift 
        if(is_signed(t1))
        {
            func.emitter.emit(op_type::asr_reg,v1,v1,v2);
        }

        else
        {
            func.emitter.emit(op_type::lsr_reg,v1,v1,v2);
        }
    }

    // left shift
    else
    {
        func.emitter.emit(op_type::lsl_reg,v1,v1,v2);
    }

    // no longer need the tmp
    func.emitter.reg_count--;

    // type being shifted is the resulting type
    return t1;
}

// handles <, <=, >, >=, &&, ||, ==, !=
Type Interloper::compile_logical_op(Function &func,AstNode *node, logic_op type)
{
    auto [t1,v1] = compile_oper(func,node->nodes[0]);
    func.emitter.reg_count++;

    auto [t2,v2] = compile_oper(func,node->nodes[1]);


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
                if(v <= type_max(oper))
                {
                    value = oper;
                }

                else
                {
                    panic("value: %x exceeds type %s\n",v,type_name(oper).c_str());
                }
            }

            // value is outside the range of the other type
            else if(is_signed(value) == is_signed(oper))
            {
                if(type_size(value) > type_size(oper))
                {
                    panic("value: %x exceeds type %s\n",v,type_name(oper).c_str());
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
            if(!is_bool(t1) && is_bool(t2))
            {
                panic("operations || and && are only defined on bools\n");
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
            check_logical_operation(t1,t2);
            break;
        }

        // this shouldunt happen
        default: 
        {
            printf("%d is not a logical operation\n",static_cast<int>(type));
            exit(1);
        }
    }

    if(!error)
    {
        // 0 is unsigned, 1 is signed
        static constexpr op_type LOGIC_OPCODE[2][LOGIC_OP_SIZE] = 
        {
            {op_type::cmpugt_imm,op_type::cmpult_reg,op_type::cmpule_reg,op_type::cmpugt_reg,op_type::cmpuge_reg,
            op_type::cmpeq_reg,op_type::cmpne_reg,op_type::and_reg,op_type::or_reg},

            {op_type::cmpsgt_imm,op_type::cmpslt_reg,op_type::cmpsle_reg,op_type::cmpsgt_reg,
            op_type::cmpsge_reg,op_type::cmpeq_reg,op_type::cmpne_reg, op_type::and_reg,op_type::or_reg},
        };

        const auto sign = is_signed(static_cast<builtin_type>(t1.type_idx));

        // if we have gotten this far the sign of both are the same
        const auto op = LOGIC_OPCODE[sign][static_cast<int>(type)];


        // one of these is just a temp for the result calc
        // so afer this we no longer need it
        func.emitter.emit(op,v1,v1,v2);
        func.emitter.reg_count--;

        return Type(builtin_type::bool_t);
    }

    // operation is not valid for given types..
    else
    {
        return Type(builtin_type::void_t);
    }
}



Type Interloper::compile_function_call(Function &func,AstNode *node)
{
    // check function is declared
    if(!function_table.count(node->literal))
    {
        panic("[COMPILE]: function %s is not declared\n",node->literal.c_str());
        return Type(builtin_type::void_t);
    }
    const auto &func_call = function_table[node->literal];

    // check we have the right number of params
    if(func_call.args.size() != node->nodes.size())
    {
        panic("[COMPILE]: function call expected %d args got %d\n",func_call.args.size(),node->nodes.size());
        print(node);
        return Type(builtin_type::void_t);
    }


    // TODO: this needs to be redone properly when we try implement a register allocator
    // TODO: allow saved regs can be used while we are pushing args

    // callee save regs in use
    // return register zero is reserved dont push it
    for(u32 i = 1; i < func.emitter.reg_count; i++)
    {
        func.emitter.emit(op_type::save_reg, reg(i));
    }


    // push args in reverse order and type check them
    for(s32 i = func_call.args.size() - 1; i >= 0; i--)
    {
        // TODO: handle being passed 
        if(type_size(func_call.args[i].type) > sizeof(u32))
        {
            printf("function arg: non register size: %d\n",type_size(func_call.args[i].type));
            exit(1);
        }

        
        // builtin type
        const auto [arg_type,reg] = compile_oper(func,node->nodes[i]);


        // type check the arg
        check_assign(func_call.args[i].type,arg_type);

        // finally push the arg
        func.emitter.emit(op_type::push_arg,reg);
    }



    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    func.emitter.emit(op_type::call,func_call.slot);


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    func.emitter.emit(op_type::clean_args,func_call.args.size());

  

    // restore callee saved values
    // do in reverse order to the save so it can be implemented as push and pop
    // has to be done after the arg cleanup
    for(int i = func.emitter.reg_count - 1; i >= 1; i--)
    {
        func.emitter.emit(op_type::restore_reg, reg(i));
    }




    // if function returns a value save the return register
    if(func.return_type.type_idx != static_cast<int>(builtin_type::void_t))
    {
        func.emitter.emit(op_type::mov_reg,reg(func.emitter.reg_count),reg(RV));
    }

    // result of expr is the return type
    return func_call.return_type;    
}

Type Interloper::compile_expression(Function &func,AstNode *node)
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
            const auto old_type = compile_expression(func,node->nodes[1]);
            const auto new_type = node->nodes[0]->variable_type;

            handle_cast(func.emitter,old_type,new_type);

            return new_type;
        }


        // multiple assigment
        case ast_type::equal:
        {
            const auto [rtype,reg] = compile_oper(func,node->nodes[1]);

            const auto name = node->nodes[0]->literal;

            const auto sym_opt = symbol_table.get_sym(name);
            if(!sym_opt)
            {
                panic("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                print(node);
                return Type(builtin_type::void_t);
            }

            
            const auto &sym = sym_opt.value();

            check_assign(sym.type,rtype);

            func.emitter.emit(op_type::mov_reg,sym.slot_idx(sym.slot),reg);

            return sym.type;        
        }


        case ast_type::value:
        { 
            // figure out what the min storage is required to fit this value
            if(node->value.sign)
            {
                const auto value = static_cast<s32>(node->value.v);
                func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),value);

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
                func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),value);

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
            func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),0);
            return Type(builtin_type::bool_t);
        }

        case ast_type::true_t:
        {
            func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),1);
            return Type(builtin_type::bool_t);
        }

        case ast_type::symbol:
        {
            const auto name = node->literal;

            const auto sym_opt = symbol_table.get_sym(name);
            if(!sym_opt)
            {
                panic("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                print(node);
                return Type(builtin_type::void_t);
            }

            const auto &sym = sym_opt.value();

            func.emitter.emit(op_type::mov_reg,reg(func.emitter.reg_count),sym.slot_idx(sym.slot));

            return sym.type;
        }

    
        case ast_type::minus:
        {
            // unary minus
            if(!node->nodes[1])
            {
                // negate by doing 0 - v
                const auto [t,dst] = compile_oper(func,node->nodes[0]);

                func.emitter.reg_count++;

                // todo: make sure our optimiser sees through this
                func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),0);
                func.emitter.emit(op_type::sub_reg,dst,reg(func.emitter.reg_count),dst);
                
                func.emitter.reg_count--;

                return t;
            }

            else
            {
                return compile_arith_op(func,node,op_type::sub_reg);
            }
        }

        case ast_type::plus:
        {
        
            // unary plus
            if(!node->nodes[1])
            {
                return compile_expression(func,node->nodes[0]); 
            }

            else
        
            {
                return compile_arith_op(func,node,op_type::add_reg);
            }
        }
    
        case ast_type::times:
        {
            return compile_arith_op(func,node,op_type::mul_reg);        
        }


        case ast_type::divide:
        {
            return compile_arith_op(func,node,op_type::div_reg);       
        }

        case ast_type::mod:
        {
            return compile_arith_op(func,node,op_type::mod_reg);       
        }

        case ast_type::shift_l:
        {
            return compile_shift(func,node,false);
        }

        case ast_type::shift_r:
        {
            return compile_shift(func,node,true);
        }        
        
        case ast_type::bitwise_and:
        {
            return compile_arith_op(func,node,op_type::and_reg);
        }

        case ast_type::bitwise_or:
        {
            return compile_arith_op(func,node,op_type::or_reg);
        }

        case ast_type::bitwise_xor:
        {
            return compile_arith_op(func,node,op_type::xor_reg);
        }

        case ast_type::bitwise_not:
        {
            const auto [t,reg] = compile_oper(func,node->nodes[0]);

            // TODO: do we need to check this is integer?


            func.emitter.emit(op_type::not_reg,reg);
            return t;
        }    

        case ast_type::logical_not:
        {
            const auto [t,reg] = compile_oper(func,node->nodes[0]);

            if(!is_bool(t))
            {
                panic("compile: logical_not expected bool got: %s\n",type_name(t).c_str());
                return Type(builtin_type::void_t);
            }

            // xor can invert our boolean which is either 1 or 0
            func.emitter.emit(op_type::xor_imm,reg,reg,1);
            return t;
        }

        // we want to pass in the base operation but we need to do the actual type checking
        // to know what we are comparing later
        // how should we do it?
        case ast_type::logical_lt:
        {
            return compile_logical_op(func,node,logic_op::cmplt_reg);
        }

        case ast_type::logical_le:
        {
            return compile_logical_op(func,node,logic_op::cmple_reg);
        }

        case ast_type::logical_gt:
        {
            return compile_logical_op(func,node,logic_op::cmpgt_reg);
        }

        case ast_type::logical_ge:
        {
            return compile_logical_op(func,node,logic_op::cmpge_reg);
        }

        case ast_type::logical_eq:
        {
            return compile_logical_op(func,node,logic_op::cmpeq_reg);
        }

        case ast_type::logical_ne:
        {
            return compile_logical_op(func,node,logic_op::cmpne_reg);
        }

        case ast_type::logical_and:
        {
            return compile_logical_op(func,node,logic_op::and_reg);
        }

        case ast_type::logical_or:
        {
            return compile_logical_op(func,node,logic_op::or_reg);
        }

        case ast_type::function_call:
        {
            return compile_function_call(func,node);
        }

        default:
        {
            panic("[COMPILE]: invalid expression\n");
            print(node);
            return Type(builtin_type::void_t);
        }
    }
}

std::string label_name(u32 slot)
{
    return "L" + std::to_string(slot);
}

u32 Interloper::new_basic_block(Function &func)
{
    const u32 slot = symbol_table.label_lookup.size();

    const u32 basic_block = func.emitter.program.size();

    func.emitter.new_block(slot);
    symbol_table.add_label(label_name(slot));

    // offset is the block offset until full resolution
    symbol_table.label_lookup[slot].offset = basic_block;

    return slot;   
}

// asume one cond
void Interloper::compile_for_block(Function &func,AstNode *node)
{
    // scope for any var decls in the stmt
    symbol_table.new_scope();

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
                compile_auto_decl(func,*node->nodes[0]);
                break;
            }

            case ast_type::declaration:
            {
                compile_decl(func,*node->nodes[0]);
                break;
            }


            default:
            {
                compile_expression(func,node->nodes[0]);
                break;
            }
        }

        if(error)
        {
            return;
        }
    }

    const auto [t,stmt_cond_reg] = compile_oper(func,node->nodes[cond_expr_idx]);

    if(!is_bool(t))
    {
        panic("expected bool got %s in for condition\n",type_name(t).c_str());
        return;
    }    


    // compile the body
    const u32 cur = new_basic_block(func);
    
    compile_block(func,node->nodes[block_expr_idx]);    

    // compile loop end stmt
    if(!single_statement)
    {
        compile_expression(func,node->nodes[2]);
    }

    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(func,node->nodes[cond_expr_idx]);
    func.emitter.emit(op_type::bc,cur,loop_cond_reg);


    const u32 exit_block = new_basic_block(func);

    // emit branch over the loop body in initial block
    // if cond is not met
    func.emitter.program[intial_block].push_back(Opcode(op_type::bnc,exit_block,stmt_cond_reg,0));

    symbol_table.destroy_scope();
}


void Interloper::compile_if_block(Function &func,AstNode *node)
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
                new_basic_block(func);
            }

            // compile the compare expr for conditon
            const auto [t,r] = compile_oper(func,if_stmt.nodes[0]);

            if(!is_bool(t))
            {
                panic("expected bool got %s in if condition\n",type_name(t).c_str());
                return;
            }

            const u32 cur_block = func.emitter.program.size() - 1;

            // compile the body block
            new_basic_block(func);
            compile_block(func,if_stmt.nodes[1]);

            // add branch over the block we just compiled
            const u32 slot = symbol_table.label_lookup.size();

            func.emitter.program[cur_block].push_back(Opcode(op_type::bnc,slot,r,0));


            // not the last statment (branch is not require)
            if(n != node->nodes.size() - 1)
            {
                // emit a directive so we know to insert a branch
                // to the exit block here once we know how many blocks
                // there are
                func.emitter.emit(op_type::exit_block);
            }
        }

        // else stmt has no expr so its in the first node
        // by definition this is the last statement
        else 
        {
            // create block for body
            new_basic_block(func);

            compile_block(func,if_stmt.nodes[0]);
        }
    }

    // create the exit block, for new code
    const u32 exit_block = new_basic_block(func);



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



void Interloper::compile_decl(Function &func, const AstNode &line)
{
    // get entry into symbol table
    const auto name = line.literal;
    const auto ltype = line.nodes[0]->variable_type;

    if(symbol_table.get_sym(name))
    {
        panic("redeclared symbol: %s\n",name.c_str());
        return;
    }

    const auto size = type_size(ltype);

    // add allocation information
    const auto slot = func.add_var(name,ltype,size);

    // add new symbol table entry
    symbol_table.add_symbol(name,ltype,slot);


    const auto &sym = symbol_table.get_sym(name).value();


    func.emitter.emit(op_type::alloc_slot,sym.slot_idx(slot));

    // handle right side expression (if present)
    if(line.nodes.size() == 2)
    {
        const auto [rtype,reg] = compile_oper(func,line.nodes[1]);
        check_assign(ltype,rtype);

        func.emitter.emit(op_type::mov_reg,sym.slot_idx(slot),reg);
    }    
}

void Interloper::compile_auto_decl(Function &func, const AstNode &line)
{
    const auto name = line.literal;

    if(symbol_table.get_sym(name))
    {
        panic("redeclared symbol: %s\n",name.c_str());
        return;
    }

    
    const auto [type,reg] = compile_oper(func,line.nodes[0]);

    // add the symbol

    const auto size = type_size(type);

    // add allocation information
    const auto slot = func.add_var(name,type,size);
    
    // add new symbol table entry
    symbol_table.add_symbol(name,type,slot);

    const auto &sym = symbol_table.get_sym(name).value();

    func.emitter.emit(op_type::alloc_slot,sym.slot_idx(slot));

    func.emitter.emit(op_type::mov_reg,sym.slot_idx(slot),reg);
}

void Interloper::compile_block(Function &func,AstNode *node)
{
    symbol_table.new_scope();

    for(auto l : node->nodes)
    {
        if(error)
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
                compile_decl(func,line);
                break;
            }


            case ast_type::auto_decl:
            {
                compile_auto_decl(func,line);
                break;
            }



            // assignment
            case ast_type::equal:
            {
                const auto [rtype,reg] = compile_oper(func,line.nodes[1]);

                const auto name = line.nodes[0]->literal;


                const auto sym_opt = symbol_table.get_sym(name);
                if(!sym_opt)
                {
                    panic("[COMPILE]: symbol '%s' assigned before declaration\n",name.c_str());
                    print(l);
                    return;
                }

                const auto &sym = sym_opt.value();

                check_assign(sym.type,rtype);

                func.emitter.emit(op_type::mov_reg,sym.slot_idx(sym.slot),reg);
                break;
            }

            case ast_type::ret:
            {
                // has an arg
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(func,line.nodes[0]);

                    if(error)
                    {
                        return;
                    }

                    check_assign(func.return_type,rtype);

                    // TODO: we are gonna require this when we have a register allocator
                    func.emitter.emit(op_type::mov_reg,reg(RV),v1);
                }
                
                has_return = true;
                func.emitter.emit(op_type::ret);
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(func,l);
                break;
            }

            case ast_type::block:
            {
                compile_block(func,l);
                break;
            }


            case ast_type::for_block:
            {
                compile_for_block(func,l);
                break;
            }

            case ast_type::if_block:
            {
                compile_if_block(func,l);
                break;
            }

            default:
            {
                panic("[COMPILE] unexpected token\n");
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
    for(const auto &[key, sym] : symbol_table.table[symbol_table.table.size()-1])
    {
        if(!sym.is_arg)
        {
            // okay what the fuck where is this getting deleted?
            func.emitter.emit(op_type::free_slot,sym.slot_idx(sym.slot)); 

            // free the stack alloc for each var thats about to go out of scope
            const auto &var_alloc = func.slot_lookup[sym.slot];
            func.size_count_cur[var_alloc.size >> 1]--;
        }
    }

    symbol_table.destroy_scope();
}

void Interloper::compile_functions()
{
    for(const auto n: root->nodes)
    {
        const auto &node = *n;

        // unless its a function we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }

        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        symbol_table.new_scope();

        auto &func = function_table[node.literal];

        for(auto &sym: func.args)
        {
            const auto size = type_size(sym.type);

            // this has to be parsed in forward order or all the offsets
            // will be messed up
            const auto slot = func.add_arg(sym.name,sym.type,size);

            symbol_table.add_symbol(sym,slot);
        }


        has_return = false;

        new_basic_block(func);


        // parse out each line of the function
        auto block = node.nodes[1];
        compile_block(func,block);

        symbol_table.destroy_scope();

        if(!has_return)
        {
            // is a void function this is fine
            // we just need to emit the ret at the end 
            if(func.return_type.type_idx == static_cast<int>(builtin_type::void_t))
            {
                func.emitter.emit(op_type::ret);
            }

            else
            {
                panic("[COMPILE]: non void function without a return\n");
            }
        }


        if(error)
        {
            return;
        }
    }
}

void Interloper::dump_ir_sym()
{
    for(auto &[key,func] : function_table)
    {
        UNUSED(key);

        func.dump_ir(symbol_table.label_lookup);
    }    
}


// TODO: impl source line information on the parse tree

// plan:
// reg alloc -> pointers -> structs -> arrays -> strings -> imports
// -> early stl -> function_pointers -> labels ->  compile time execution ->
// unions -> inline asm

void Interloper::compile(const std::vector<std::string> &lines)
{
    // make sure everything is clean
    program.clear();
    symbol_table.clear();
    function_table.clear();

    // clear the tree if present
    if(root)
    {
        delete_tree(root); root = nullptr;
    }

    error = false;

    // tokenize input file
    {
        const auto tokens = lexer.tokenize(&lines);

        if(lexer.error)
        {
            error = true;
            return;
        }

        //print_tokens(tokens);


        // build ast
        parser.init(&lines,&tokens);
        parser.parse(&root);
    }
    
    if(!root || parser.error)
    {
        error = true;
        return;
    }

   print(root);


    // okay now we need to start doing semantic analysis
    // first handle any imports, macros etc (skip for now)
    // handle any type declartions (skip for now)
    // handle function declartions


    parse_function_declarations();

    if(error)
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

    compile_functions();


    // okay we dont need the parse tree anymore
    // free it
    delete_tree(root); root = nullptr;

    if(error)
    {
        return;
    }


    optimise_ir();

    dump_ir_sym();

    // perform register allocation
    for(auto &[key, func]: function_table)
    {
        UNUSED(key);
        allocate_registers(func);
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm();


}

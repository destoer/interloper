#include <interloper.h>


Type compile_expression(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
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

        std::vector<u32> args;

        const auto decl = node.nodes[2];

        // rip every arg
        for(const auto a : decl->nodes)
        {
            const auto name = a->literal;
            const auto type = a->nodes[0]->variable_type;

            const auto size = type_size(itl,type);

            // add the var to slot lookup and link to function
            // we will do a add_scope to put it into the scope later
            Symbol sym = Symbol(name,type,size,args.size());
            add_var(itl.symbol_table,sym);

            args.push_back(sym.slot);

            //printf("arg slot %s: %d : %d\n",sym.name.c_str(),sym.slot, args[args.size()-1]);
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



u32 new_slot(Function &func)
{       
    return reg(func.emitter.reg_count++);
}

std::pair<Type,u32> symbol(Interloper &itl, AstNode *node)
{
    const auto name = node->literal;

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(!sym_opt)
    {
        panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
        print(node);
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};
    }

    const auto &sym = sym_opt.value();

    return std::pair<Type,u32>{sym.type,slot_idx(sym)};
}


Type value(Function& func,AstNode *node, u32 dst_slot)
{
    if(node->value.sign)
    {
        const auto value = static_cast<s32>(node->value.v);
        emit(func.emitter,op_type::mov_imm,dst_slot,value);

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
        emit(func.emitter,op_type::mov_imm,dst_slot,value);

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

// this should handle grabbing values and symbols
// if it can see a symbol or a value it wont call compile_expression
std::pair<Type,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot)
{
    panic(node == nullptr,"nullptr in compile_oper");

    // test what the current node is
    if(node->type == ast_type::value)
    {
        const auto t1 = value(func,node,dst_slot);
        return std::pair<Type,u32>{t1,dst_slot};
    }

    else if(node->type == ast_type::symbol)
    {
        return symbol(itl,node);
    }

    // if its a value or symbol return out immdiatly

    // else compile an expr
    else
    {
        const auto t1 = compile_expression(itl,func,node,dst_slot);
        return std::pair<Type,u32>{t1,dst_slot};
    }
}



Type compile_arith_op(Interloper& itl,Function &func,AstNode *node, op_type type, u32 dst_slot)
{
    // how should these two by here work?
    // the dst slot is fundementally for the dst for compile_oper should we just allocate something
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_slot(func));

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_slot(func));


    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2);

    emit(func.emitter,type,dst_slot,v1,v2);

    return final_type;        
}


Type compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, u32 dst_slot)
{
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_slot(func));

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_slot(func));

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
            emit(func.emitter,op_type::asr_reg,dst_slot,v1,v2);
        }

        else
        {
            emit(func.emitter,op_type::lsr_reg,dst_slot,v1,v2);
        }
    }

    // left shift
    else
    {
        emit(func.emitter,op_type::lsl_reg,dst_slot,v1,v2);
    }

    // type being shifted is the resulting type
    return t1;
}


// handles <, <=, >, >=, &&, ||, ==, !=
Type compile_logical_op(Interloper& itl,Function &func,AstNode *node, logic_op type, u32 dst_slot)
{
    auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_slot(func));

    auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_slot(func));


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
            panic("%d is not a logical operation\n",static_cast<int>(type));
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
        emit(func.emitter,op,dst_slot,v1,v2);

        return Type(builtin_type::bool_t);
    }

    // operation is not valid for given types..
    else
    {
        return Type(builtin_type::void_t);
    }
}



Type compile_function_call(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
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

    // for now we are just going with callee saved
    // if we eventually mix caller and callee saved so we can have the called func overwrite values
    // we dont care about then we need to re add the save and restore directives
    // and we will need a push and pop bitset to implement them
    // with care to rewrite where the return register ends up when we restore
    // save all the used regs
    //emit(func.emitter,op_type::save_regs);
    


    // push args in reverse order and type check them
    for(s32 i = func_call.args.size() - 1; i >= 0; i--)
    {
        const auto &arg = itl.symbol_table.slot_lookup[func_call.args[i]];

        // TODO: handle being passed args that wont fit inside a single hardware reg
        if(type_size(itl,arg.type) > sizeof(u32))
        {
            unimplemented("function arg: non register size: %d\n",type_size(itl,arg.type));
        }

        
        // builtin type
        const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[i],new_slot(func));


        // type check the arg
        check_assign(itl,arg.type,arg_type);

        // finally push the arg
        emit(func.emitter,op_type::push_arg,reg);
    }


    const bool returns_value = func.return_type.type_idx != static_cast<int>(builtin_type::void_t);

    // if we have a register in R0 we need to spill it so emit a push instr
    if(returns_value)
    {
        emit(func.emitter,op_type::spill_rv);
    }


    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    emit(func.emitter,op_type::call,func_call.slot);


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    if(func_call.args.size())
    {
        emit(func.emitter,op_type::clean_args,func_call.args.size());
    }
  

    // restore callee saved values
    //emit(func.emitter,op_type::restore_regs);


    // if function returns a value save the return register
    // our register allocator will have to force a spill on R0 if its in use
    // TODO: we want this as an explicit just mark as allocation ito R0

    if(returns_value)
    {
        emit(func.emitter,op_type::mov_reg,dst_slot,RV_IR);
    }

    // result of expr is the return type
    return func_call.return_type;    
}



std::string label_name(u32 slot)
{
    return "L" + std::to_string(slot);
}

u32 new_basic_block(Interloper &itl,Function &func, block_type type)
{
    const u32 slot = itl.symbol_table.label_lookup.size();

    const u32 basic_block = func.emitter.program.size();

    new_block(func.emitter,type,slot);
    add_label(itl.symbol_table,label_name(slot));

    // offset is the block offset until full resolution
    itl.symbol_table.label_lookup[slot].offset = basic_block;

    return slot;   
}



void compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    const u32 start_block = func.emitter.program.size();

    auto &blocks = func.emitter.program;

    block_type old = blocks[blocks.size() - 1].type;

    for(u32 n = 0; n < node->nodes.size(); n++)
    {
        const auto &if_stmt = *node->nodes[n];

        if(if_stmt.type != ast_type::else_t)
        {

            if(n != 0)
            {
                // create new block for compare
                // TODO: should this have hte type of the initial node or no?
                new_basic_block(itl,func,block_type::chain_cmp_t);
            }

            // compile the compare expr for conditon
            const auto [t,r] = compile_oper(itl,func,if_stmt.nodes[0],new_slot(func));

            if(!is_bool(t))
            {
                panic(itl,"expected bool got %s in if condition\n",type_name(itl,t).c_str());
                return;
            }

            // block for comparison branch
            const u32 cur_block = blocks.size() - 1;

            // compile the body block
            new_basic_block(itl,func,if_stmt.type == ast_type::if_t? block_type::if_t : block_type::else_if_t);
            compile_block(itl,func,if_stmt.nodes[1]);

            // add branch over the block we just compiled
            const u32 slot = itl.symbol_table.label_lookup.size();

            blocks[cur_block].buf.push_back(Opcode(op_type::bnc,slot,r,0));


            // not the last statment (branch is not require)
            if(n != node->nodes.size() - 1)
            {
                // emit a directive so we know to insert a branch
                // to the exit block here once we know how many blocks
                // there are
                emit(func.emitter,op_type::exit_block);
            }

            else
            {
                blocks[blocks.size() - 1].last = true;
            }
        }

        // else stmt has no expr so its in the first node
        // and by definition this is the last statement
        else 
        {
            // create block for body
            new_basic_block(itl,func,block_type::else_t);
            compile_block(itl,func,if_stmt.nodes[0]);

            blocks[blocks.size() - 1].last = true;
        }
    }

    // TODO: is this being a body fine or does it need to take whatever the intial block was?
    // create the exit block, for new code
    const u32 exit_block = new_basic_block(itl,func,old);



    // for every body block bar the last we just added
    // add a unconditonal branch to the "exit block"
    // the last block is directly before the next and does not need one

    for(u32 b = start_block; b < blocks.size() - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.buf.back().op == op_type::exit_block)
        {
            block.buf.back() = Opcode(op_type::b,exit_block,0,0);
        }
    }
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

    auto &blocks = func.emitter.program;
    block_type old = blocks[blocks.size() - 1].type;

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
                compile_expression(itl,func,node->nodes[0],new_slot(func));
                break;
            }
        }

        if(itl.error)
        {
            return;
        }
    }

    const auto [t,stmt_cond_reg] = compile_oper(itl,func,node->nodes[cond_expr_idx],new_slot(func));

    if(!is_bool(t))
    {
        panic(itl,"expected bool got %s in for condition\n",type_name(itl,t).c_str());
        return;
    }    


    // compile the body
    const u32 cur = new_basic_block(itl,func,block_type::for_t);
    
    compile_block(itl,func,node->nodes[block_expr_idx]);    

    // compile loop end stmt
    if(!single_statement)
    {
        compile_expression(itl,func,node->nodes[2],new_slot(func));
    }

    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,node->nodes[cond_expr_idx],new_slot(func));
    emit(func.emitter,op_type::bc,cur,loop_cond_reg);

    const u32 exit_block = new_basic_block(itl,func,old);

    // emit branch over the loop body in initial block
    // if cond is not met
    func.emitter.program[intial_block].buf.push_back(Opcode(op_type::bnc,exit_block,stmt_cond_reg,0));

    destroy_scope(itl.symbol_table);
}


// TODO: this needs a cleanup
// TODO: does it make sense to use the same function for both the @ and & operator?
// TODO: this wont handle arrays
std::pair<Type,u32> load_addr(Interloper &itl,Function &func,AstNode *node,u32 slot, bool addrof)
{
    // figure out what the addr is
    switch(node->type)
    {
        case ast_type::symbol:
        {
            const auto name = node->literal;
            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                print(node);
                return std::pair<Type,u32>{Type(builtin_type::void_t),0};
            }

            const auto &sym = sym_opt.value();

            // type is now what it pointed to
            auto type = sym.type;



            if(addrof)
            {
                type.ptr_indirection += 1;
                // actually  get the addr of the ptr
                emit(func.emitter,op_type::addrof,slot,slot_idx(sym));
                return std::pair<Type,u32>{type,slot};
            }

            else
            {
                if(!is_pointer(type))
                {
                    panic(itl,"[COMPILE]: symbol '%s' is not a pointer\n",name.c_str());
                }
                type.ptr_indirection -= 1;
                return std::pair<Type,u32>{type,slot_idx(sym)};
            }
        }

        default:
        {
            print(node);
            unimplemented("load_addr expr");
        }
    }
}

void do_ptr_load(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        if(is_signed_integer(type))
        {
            // word is register size (we dont need to extend it)
            static const op_type instr[3] = {op_type::lsb, op_type::lsh, op_type::lw};
            emit(func.emitter,instr[size >> 1],dst_slot,addr_slot);       
        }

        // "plain data"
        // just move by size
        else
        {
            static const op_type instr[3] = {op_type::lb, op_type::lh, op_type::lw};
            emit(func.emitter,instr[size >> 1],dst_slot,addr_slot);
        }
    }   

    else
    {
       unimplemented("struct deref");
    }
}

void do_ptr_write(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        static const op_type instr[3] = {op_type::sb, op_type::sh, op_type::sw};
        emit(func.emitter,instr[size >> 1],dst_slot,addr_slot);
    }   

    else
    {
        unimplemented("struct deref");
    }
}

/*
Value const_expr_value(Interloper &itl, AstNode *node)
{

}
*/

Type compile_expression(Interloper &itl,Function &func,AstNode *node,u32 dst_slot)
{
    UNUSED(dst_slot); UNUSED(func);

    if(!node)
    {
        panic("nullptr in compile_expression");
        return Type(builtin_type::void_t);
    }

   
    switch(node->type)
    {

        case ast_type::addrof:
        {
            // want this to also get an addr but we want the actual ptr_count to go up...
            const auto [type,slot] = load_addr(itl,func,node->nodes[0],dst_slot,true);
            return type;
        }

        case ast_type::deref:
        {
            const auto [type,slot]  = load_addr(itl,func,node->nodes[0],new_slot(func),false);
            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;
        }

        case ast_type::cast:
        {
            const auto [old_type,slot] = compile_oper(itl,func,node->nodes[1],new_slot(func));
            const auto new_type = node->nodes[0]->variable_type;

            handle_cast(itl,func.emitter,dst_slot,slot,old_type,new_type);
            return new_type;
        }


        case ast_type::plus:
        {
            // unary plus
            if(!node->nodes[1])
            {
                return compile_expression(itl,func,node->nodes[0],dst_slot); 
            }

            else
            {
                return compile_arith_op(itl,func,node,op_type::add_reg,dst_slot);
            }
        }



        // multiple assigment
        case ast_type::equal:
        {
            const auto [rtype,slot] = compile_oper(itl,func,node->nodes[1],dst_slot);

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


            emit(func.emitter,op_type::mov_reg,slot_idx(sym),slot);

            // TODO: make sure that the silly code this gens
            // is cleaned up by the optimiser
            if(dst_slot != slot)
            {
                emit(func.emitter,op_type::mov_reg,dst_slot,slot);
            }

            return sym.type;        
        }


        case ast_type::divide:
        {
            return compile_arith_op(itl,func,node,op_type::div_reg,dst_slot);
        }

        case ast_type::mod:
        {
            return compile_arith_op(itl,func,node,op_type::mod_reg,dst_slot);       
        }

        case ast_type::times:
        {
            return compile_arith_op(itl,func,node,op_type::mul_reg,dst_slot);
        }

        case ast_type::minus:
        {            
            // unary minus
            if(!node->nodes[1])
            {
                // negate by doing 0 - v
                const auto [t,dst] = compile_oper(itl,func,node->nodes[0],dst_slot);

                const auto slot = new_slot(func);

                // TODO: make sure our optimiser sees through this
                emit(func.emitter,op_type::mov_imm,slot,0);
                emit(func.emitter,op_type::sub_reg,dst,slot,dst);
                
                return t;
            }

            else
            {
                return compile_arith_op(itl,func,node,op_type::sub_reg,dst_slot);
            }
        }

        case ast_type::bitwise_and:
        {
            return compile_arith_op(itl,func,node,op_type::and_reg,dst_slot);
        }

        case ast_type::bitwise_or:
        {
            return compile_arith_op(itl,func,node,op_type::or_reg,dst_slot);
        }

        case ast_type::bitwise_xor:
        {
            return compile_arith_op(itl,func,node,op_type::xor_reg,dst_slot);
        }


        case ast_type::bitwise_not:
        {
            const auto [t,reg] = compile_oper(itl,func,node->nodes[0],dst_slot);

            // TODO: do we need to check this is integer?


            emit(func.emitter,op_type::not_reg,dst_slot,reg);
            return t;
        }            


        case ast_type::shift_l:
        {
            return compile_shift(itl,func,node,false,dst_slot);
        }

        case ast_type::shift_r:
        {
            return compile_shift(itl,func,node,true,dst_slot);
        }        


        case ast_type::false_t:
        {
            emit(func.emitter,op_type::mov_imm,dst_slot,0);
            return Type(builtin_type::bool_t);
        }

        case ast_type::true_t:
        {
            emit(func.emitter,op_type::mov_imm,dst_slot,1);
            return Type(builtin_type::bool_t);
        }


        case ast_type::logical_not:
        {
            const auto [t,reg] = compile_oper(itl,func,node->nodes[0],dst_slot);

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
            return compile_logical_op(itl,func,node,logic_op::cmplt_reg,dst_slot);
        }

        case ast_type::logical_le:
        {
            return compile_logical_op(itl,func,node,logic_op::cmple_reg,dst_slot);
        }

        case ast_type::logical_gt:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpgt_reg,dst_slot);
        }

        case ast_type::logical_ge:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpge_reg,dst_slot);
        }

        case ast_type::logical_eq:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpeq_reg,dst_slot);
        }

        case ast_type::logical_ne:
        {
            return compile_logical_op(itl,func,node,logic_op::cmpne_reg,dst_slot);
        }

        case ast_type::logical_and:
        {
            return compile_logical_op(itl,func,node,logic_op::and_reg,dst_slot);
        }

        case ast_type::logical_or:
        {
            return compile_logical_op(itl,func,node,logic_op::or_reg,dst_slot);
        }


        case ast_type::function_call:
        {
            return compile_function_call(itl,func,node,dst_slot);
        }


        default:
        {
            panic(itl,"[COMPILE]: invalid expression\n");
            print(node);
            return Type(builtin_type::void_t);
        }
    }
}



void compile_decl(Interloper &itl,Function &func, const AstNode &line)
{
    // get entry into symbol table
    const auto name = line.literal;
    const auto ltype = line.nodes[0]->variable_type;

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(sym_opt)
    {
        print(&line);
        print_sym(sym_opt.value());
        panic(itl,"redeclared symbol: %s\n",name.c_str());
        return;
    }

    const auto size = type_size(itl,ltype);

    // add new symbol table entry
    add_symbol(itl.symbol_table,name,ltype,size);


    const auto &sym = get_sym(itl.symbol_table,name).value();


    emit(func.emitter,op_type::alloc_slot,slot_idx(sym));

    // handle right side expression (if present)
    if(line.nodes.size() == 2)
    {
        const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1],slot_idx(sym));

        // oper is a single symbol and the move hasn't happened we need to explictly move it
        if(slot_idx(sym) != reg)
        {
            emit(func.emitter,op_type::mov_reg,slot_idx(sym),reg);
        }

        check_assign(itl,ltype,rtype);
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

    
    const auto [type,reg] = compile_oper(itl,func,line.nodes[0],new_slot(func));

    // add the symbol

    const auto size = type_size(itl,type);

    // add new symbol table entry
    add_symbol(itl.symbol_table,name,type,size);

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
                const auto [rtype,slot] = compile_oper(itl,func,line.nodes[1],new_slot(func));


                if(line.nodes[0]->type != ast_type::symbol)
                {
                    switch(line.nodes[0]->type)
                    {
                        case ast_type::deref:
                        {
                            const auto [type,addr_slot] = load_addr(itl,func,line.nodes[0]->nodes[0],new_slot(func),false);
                            check_assign(itl,type,rtype);
                            do_ptr_write(itl,func,slot,addr_slot,type);
                            break;                        
                        }    

                        default:
                        {
                            unimplemented("non plain assign");
                            break;
                        }
                    }
                }
                
                else
                {
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

                    emit(func.emitter,op_type::mov_reg,slot_idx(sym),slot);
                }
                break;
            }


            case ast_type::ret:
            {
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(itl,func,line.nodes[0],RV_IR);

                    // what we are returning is just a var 
                    // it needs to be moved into ret
                    // TODO: make sure the optimiser sees through this
                    if(v1 != RV_IR)
                    {
                        emit(func.emitter,op_type::mov_reg,RV_IR,v1);
                    }


                    if(itl.error)
                    {
                        return;
                    }

                    check_assign(itl,func.return_type,rtype);
                }

                emit(func.emitter,op_type::ret);
                

                itl.has_return = true;
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,l,new_slot(func));
                break;
            }            


            case ast_type::block:
            {
                compile_block(itl,func,l);
                break;
            }


            case ast_type::if_block:
            {
                compile_if_block(itl,func,l);
                break;
            }

            case ast_type::for_block:
            {
                compile_for_block(itl,func,l);
                break;
            }

            default:
            {
                panic(itl,"[COMPILE] unexpected token\n");
                print(l);
            }
        }
    }


    // scope is about to be destroyed reclaim the stack for every var that is no longer used
    for(const auto &[key, slot] : itl.symbol_table.table[itl.symbol_table.table.size()-1])
    { 
        const auto &sym = itl.symbol_table.slot_lookup[slot];

        // free the stack alloc for each var thats about to go out of scope
        if(sym.arg_num == NON_ARG)
        {
            emit(func.emitter,op_type::free_slot,slot_idx(sym)); 
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

        func.emitter.reg_count = 0;

        // put each arg into scope
        for(auto &a : func.args)
        {
            add_scope(itl.symbol_table,itl.symbol_table.slot_lookup[a]);
        }



        itl.has_return = false;

        new_basic_block(itl,func,block_type::body_t);


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

    destroy_scope(itl.symbol_table); 
}

void dump_ir_sym(Interloper &itl)
{
    for(auto &[key,func] : itl.function_table)
    {
        UNUSED(key);

        dump_ir(func,itl.symbol_table.slot_lookup,itl.symbol_table.label_lookup);
    }    
}


// TODO: impl source line information on the parse tree
// impl raii for varaible scopes to prevent dumb bugs
// remove reliance on stl containers for compilier structs


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
        allocate_registers(func,itl.symbol_table.slot_lookup);
        putchar('\n');
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm(itl);


}

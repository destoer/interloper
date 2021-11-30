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
            Symbol sym = Symbol(name,type,size,args.size()-1);
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
    assert(node != nullptr);

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
    if(returns_value)
    {
        emit(func.emitter,op_type::mov_reg,dst_slot,RV_IR);
    }

    // result of expr is the return type
    return func_call.return_type;    
}



Type compile_expression(Interloper &itl,Function &func,AstNode *node,u32 dst_slot)
{
    UNUSED(dst_slot); UNUSED(func);

    if(!node)
    {
        assert(false);
        return Type(builtin_type::void_t);
    }

   
    // OKAY start slowing reimpl the min ammount to get these working!
    switch(node->type)
    {
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

    // TODO: get rid of the need to use this for calculating stack requirements
    add_var(func,size);

    // add new symbol table entry
    add_symbol(itl.symbol_table,name,ltype,size);


    const auto &sym = get_sym(itl.symbol_table,name).value();


    emit(func.emitter,op_type::alloc_slot,slot_idx(sym));

    // handle right side expression (if present)
    if(line.nodes.size() == 2)
    {
        const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1],slot_idx(sym));
        check_assign(itl,ltype,rtype);
    }    
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


            // assignment
            case ast_type::equal:
            {
                const auto [rtype,slot] = compile_oper(itl,func,line.nodes[1],new_slot(func));

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
                break;
            }


            // stub to no return just to see asm
            case ast_type::ret:
            {
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(itl,func,line.nodes[0],new_slot(func));

                    if(itl.error)
                    {
                        return;
                    }

                    check_assign(itl,func.return_type,rtype);

                    emit(func.emitter,op_type::ret_var,v1);
                }

                else
                {
                    emit(func.emitter,op_type::ret);
                }

                itl.has_return = true;
                break;
            }


            case ast_type::block:
            {
                compile_block(itl,func,l);
                break;
            }

            default:
            {
                panic(itl,"[COMPILE] unexpected token\n");
                print(l);
            }
        }
    }



    // TODO: clean up this nonsense when we improve how stack allocation is done

    // std::max the sizes this is what we need to allocate
    for(int i = 0; i < 3; i++)
    {
        func.size_count[i] = std::max(func.size_count[i],func.size_count_cur[i]);
    }

    // scope is about to be destroyed reclaim the stack for every var that is no longer used
    for(const auto &[key, slot] : itl.symbol_table.table[itl.symbol_table.table.size()-1])
    { 
        const auto &sym = itl.symbol_table.slot_lookup[slot];

        // free the stack alloc for each var thats about to go out of scope
        if(sym.arg_num == NON_ARG)
        {
            emit(func.emitter,op_type::free_slot,slot_idx(sym)); 
            func.size_count_cur[sym.size >> 1]--;
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

        dump_ir(func,itl.symbol_table.slot_lookup,itl.symbol_table.label_lookup);
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
        allocate_registers(func,itl.symbol_table.slot_lookup);
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm(itl);


}

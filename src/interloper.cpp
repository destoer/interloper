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


        // TODO: check for redefinition
        const auto return_type = node.nodes[0]->variable_type;
        const auto name = node.literal;
        

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


// TODO:
//    how do we handle symbols we need i.e function names? or branches
//    the offsets for them will differ from our ir ones and they will be moved
//    during optimisation passes

Type Interloper::compile_arith_op(Function &func,AstNode *node, op_type type)
{
    const auto t1 = compile_expression(func,node->nodes[0]);
    const auto v1 = reg(func.emitter.reg_count);
    func.emitter.reg_count++;

    
    const auto t2 = compile_expression(func,node->nodes[1]);
    const auto v2 = reg(func.emitter.reg_count);

    // produce effective type
    const auto final_type = effective_arith_type(t1,t2);


    // one of these is just a temp for the result calc
    // so afer this we no longer need it
    func.emitter.emit(type,v1,v1,v2);
    func.emitter.reg_count--;

    return final_type;        
}

// handles <, <=, >, >=, &&, ||, ==, !=
Type Interloper::compile_logical_op(Function &func,AstNode *node, op_type type)
{
    const auto t1 = compile_expression(func,node->nodes[0]);
    const auto v1 = reg(func.emitter.reg_count);
    func.emitter.reg_count++; 

    const auto t2 = compile_expression(func,node->nodes[1]);
    const auto v2 = reg(func.emitter.reg_count);  


    // okay now then does a boolean operation make sense for this operator
    // with these types?


    switch(type)
    {
        // only bools are valid
        // || &&

        case op_type::or_reg: case op_type::and_reg:
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
        case op_type::cmplt_reg: case op_type::cmple_reg: case op_type::cmpgt_reg:
        case op_type::cmpge_reg: case op_type::cmpeq_reg: case op_type::cmpne_reg:
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
        // one of these is just a temp for the result calc
        // so afer this we no longer need it
        func.emitter.emit(type,v1,v1,v2);
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
        parser.print(node);
        return Type(builtin_type::void_t);
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
        const auto arg_type = compile_expression(func,node->nodes[i]);

        // type check the arg
        check_assign(func_call.args[i].type,arg_type);

        // finally push the arg
        func.emitter.emit(op_type::push_arg,reg(func.emitter.reg_count));
    }




    // TODO: this needs to be redone properly when we try implement a register allocator


    // START HERE: save callee regs with directives

/*
    // callee save regs in use
    // return register zero is reserved dont push it
    for(u32 i = 1; i < func.emitter.reg_count; i++)
    {
        func.emitter.emit(op_type::push, reg(i));
    }
*/

    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    func.emitter.emit(op_type::call,func_call.slot);

/*
    // restore callee saved values
    for(int i = func.emitter.reg_count - 1; i >= 1; i--)
    {
        func.emitter.emit(op_type::pop, reg(i));
    }
*/


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    func.emitter.emit(op_type::clean_args,func_call.args.size());

  
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
            const auto rtype = compile_expression(func,node->nodes[1]);

            const auto name = node->nodes[0]->literal;

            const auto sym_opt = symbol_table.get_sym(name);
            if(!sym_opt)
            {
                panic("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                parser.print(node);
                return Type(builtin_type::void_t);
            }

            
            const auto &sym = sym_opt.value();

            check_assign(sym.type,rtype);

            func.emitter.emit(op_type::mov_reg,sym.slot_idx(sym.slot),reg(func.emitter.reg_count));

            return sym.type;        
        }


        case ast_type::value:
        {
            func.emitter.emit(op_type::mov_imm,reg(func.emitter.reg_count),node->value);

            // TODO: do we care about tracking if an input value is -ve

            // what is the smallest storage type that this will fit inside?
            if(in_range(node->value,min(builtin_type::u8_t),max(builtin_type::u8_t)))
            {
                return Type(builtin_type::u8_t);
            }

            else if(in_range(node->value,min(builtin_type::u16_t),max(builtin_type::u16_t)))
            {
                return Type(builtin_type::u16_t);
            }

            //else if(in_range(node->value,min(builtin_type::u32_t),max(builtin_type::u32_t))
            else
            {
                return Type(builtin_type::u32_t);
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
                parser.print(node);
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
                const auto t = compile_expression(func,node->nodes[0]);

                // we are done with the reg used to load zero after this
                const auto dst = reg(func.emitter.reg_count);

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
            const auto t = compile_expression(func,node->nodes[0]);

            // TODO: do we need to check this is integer?


            func.emitter.emit(op_type::not_reg,reg(func.emitter.reg_count));
            return t;
        }    

        case ast_type::logical_not:
        {
            const auto t = compile_expression(func,node->nodes[0]);

            if(!is_bool(t))
            {
                panic("compile: logical_not expected bool got: %s\n",type_name(t).c_str());
                return Type(builtin_type::void_t);
            }

            // xor can invert our boolean which is either 1 or 0
            func.emitter.emit(op_type::xor_imm,reg(func.emitter.reg_count),reg(func.emitter.reg_count),1);
            return t;
        }

        case ast_type::logical_lt:
        {
            return compile_logical_op(func,node,op_type::cmplt_reg);
        }

        case ast_type::logical_le:
        {
            return compile_logical_op(func,node,op_type::cmple_reg);
        }

        case ast_type::logical_gt:
        {
            return compile_logical_op(func,node,op_type::cmpgt_reg);
        }

        case ast_type::logical_ge:
        {
            return compile_logical_op(func,node,op_type::cmpge_reg);
        }

        case ast_type::logical_eq:
        {
            return compile_logical_op(func,node,op_type::cmpeq_reg);
        }

        case ast_type::logical_ne:
        {
            return compile_logical_op(func,node,op_type::cmpne_reg);
        }

        case ast_type::logical_and:
        {
            return compile_logical_op(func,node,op_type::and_reg);
        }

        case ast_type::logical_or:
        {
            return compile_logical_op(func,node,op_type::or_reg);
        }

        case ast_type::function_call:
        {
            return compile_function_call(func,node);
        }

        default:
        {
            panic("[COMPILE]: invalid expression\n");
            parser.print(node);
            return Type(builtin_type::void_t);
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


// TODO: implement logical operations

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
                // get entry into symbol table
                const auto name = line.literal;
                const auto ltype = line.nodes[0]->variable_type;

                //const auto slot = symbol_table.sym_count;


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

                // TODO: we cant have a arg when a var is just declared
                // this is probably not needed
                const auto &sym = symbol_table.get_sym(name).value();




                // handle right side expression (if present)
                if(line.nodes.size() == 2)
                {
                    const auto rtype = compile_expression(func,line.nodes[1]);
                    check_assign(ltype,rtype);

                    func.emitter.emit(op_type::mov_reg,sym.slot_idx(slot),reg(func.emitter.reg_count));
                }
                break;
            }


            // assignment
            case ast_type::equal:
            {
                const auto rtype = compile_expression(func,line.nodes[1]);

                const auto name = line.nodes[0]->literal;


                const auto sym_opt = symbol_table.get_sym(name);
                if(!sym_opt)
                {
                    panic("[COMPILE]: symbol '%s' assigned before declaration\n",name.c_str());
                    parser.print(l);
                    return;
                }

                const auto &sym = sym_opt.value();

                check_assign(sym.type,rtype);

                func.emitter.emit(op_type::mov_reg,sym.slot_idx(sym.slot),reg(func.emitter.reg_count));
                break;
            }

            case ast_type::ret:
            {
                // has an arg
                if(line.nodes.size() == 1)
                {
                    const auto rtype = compile_expression(func,line.nodes[0]);
                    if(error)
                    {
                        return;
                    }

                    check_assign(func.return_type,rtype);

                    // TODO: we are gonna require this when we have a register allocator
                    func.emitter.emit(op_type::mov_reg,reg(RV),reg(func.emitter.reg_count));
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

            default:
            {
                panic("[COMPILE] unexpected token\n");
                parser.print(l);
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
            func.emitter.emit(op_type::free_slot_stack,sym.slot);

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
                panic("[COMPILE]: non void function without a return");
            }
        }


        if(error)
        {
            return;
        }

        func.dump_ir(symbol_table.label_lookup);    
    }
}


// TODO: start writing tests for more invalid sequences
// and improve error reporting in later stages of compilation

// plan:
// implement bitwise operators -> boolean + logical -> if statements
// for loops -> pointers -> structs -> arrays -> strings -> imports
// -> early stl -> function_pointers -> labels ->  compile time execution ->
// unions -> inline asm

void Interloper::compile(const std::vector<std::string> &lines)
{
    // make sure everything is clean
    program.clear();
    symbol_table.clear();
    function_table.clear();

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

//    parser.print(root);


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



    //dump_ir_sym();

    //optimise_ir();

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

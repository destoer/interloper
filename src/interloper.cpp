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

        // for now assume void and avoid dealing with a bunch of things
        args.push_back(Symbol("",builtin_type::void_t));
        
        // rip every arg
        


        const Function function(name,return_type,args);


        function_table[name] = function;
    }
}


// TODO:
//    how do we handle symbols we need i.e function names? or branches
//    the offsets for them will differ from our ir ones and they will be moved
//    during optimisation passes

Type Interloper::compile_arith_op(AstNode *node, op_type type)
{
    const auto t1 = compile_expression(node->nodes[0]);
    const auto v1 = reg(emitter.reg_count);
    emitter.reg_count++;

    
    const auto t2 = compile_expression(node->nodes[1]);
    const auto v2 = reg(emitter.reg_count);

    // produce effective type
    const auto final_type = effective_arith_type(t1,t2);


    // one of these is just a temp for the result calc
    // so afer this we no longer need it
    emitter.emit(type,v1,v1,v2);
    emitter.reg_count--;

    return final_type;        
}


Type Interloper::compile_expression(AstNode *node)
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
            const auto old_type = compile_expression(node->nodes[1]);
            const auto new_type = node->nodes[0]->variable_type;

            handle_cast(old_type,new_type);

            return new_type;
        }


        // multiple assigment
        case ast_type::equal:
        {
            const auto rtype = compile_expression(node->nodes[1]);

            const auto name = node->nodes[0]->literal;

            
            if(!symbol_table.exists(name))
            {
                panic("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                parser.print(node);
                return Type(builtin_type::void_t);
            }

            
            const auto &sym = symbol_table[name];

            check_assign(sym.type,rtype);

            emitter.emit(op_type::mov_reg,symbol(sym.slot),reg(emitter.reg_count));

            return sym.type;        
        }


        case ast_type::value:
        {
            emitter.emit(op_type::mov_imm,reg(emitter.reg_count),node->value);

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

        case ast_type::symbol:
        {
            const auto name = node->literal;

            if(!symbol_table.exists(name))
            {
                panic("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                parser.print(node);
                return Type(builtin_type::void_t);
            }

            const auto &sym = symbol_table[name];

            emitter.emit(op_type::mov_reg,reg(emitter.reg_count),symbol(sym.slot));

            return sym.type;
        }

    
        case ast_type::minus:
        {
            // unary minus
            if(!node->nodes[1])
            {
                // negate by doing 0 - v
                const auto t = compile_expression(node->nodes[0]);

                // we are done with the reg used to load zero after this
                const auto dst = reg(emitter.reg_count);

                emitter.reg_count++;

                // todo: make sure our optimiser sees through this
                emitter.emit(op_type::mov_imm,reg(emitter.reg_count),0);
                emitter.emit(op_type::sub_reg,dst,reg(emitter.reg_count),dst);
                
                emitter.reg_count--;

                return t;
            }

            else
            {
                return compile_arith_op(node,op_type::sub_reg);
            }
        }

        case ast_type::plus:
        {
        
            // unary plus
            if(!node->nodes[1])
            {
                return compile_expression(node->nodes[0]); 
            }

            else
        
            {
                return compile_arith_op(node,op_type::add_reg);
            }
        }
    
        case ast_type::times:
        {
            return compile_arith_op(node,op_type::mul_reg);        
        }


        case ast_type::divide:
        {
            return compile_arith_op(node,op_type::div_reg);       
        }

        default:
        {
            panic("[COMPILE]: invalid expression\n");
            parser.print(node);
            return Type(builtin_type::void_t);
        }
    }
}


// in our IR how do we seperate a tempoary from an actual value we need to actually keep

void Interloper::compile_block(AstNode *node)
{
    for(const auto l : node->nodes)
    {
        if(error)
        {
            return;
        }

        const auto &line = *l;

        emitter.reg_count = 0;

        switch(line.type)
        {
            // variable declaration
            case ast_type::declaration:
            {
                // get entry into symbol table
                const auto name = line.literal;
                const auto ltype = line.nodes[0]->variable_type;

                const auto slot = symbol_table.sym_count;

                if(symbol_table.exists(name))
                {
                    panic("redeclared symbol: %s\n",name.c_str());
                    return;
                }

                const auto size = type_size(ltype);

                // add new symbol table entry
                symbol_table.add_symbol(name,ltype,size);


                // handle right side expression (if present)
                if(line.nodes.size() == 2)
                {
                    const auto rtype = compile_expression(line.nodes[1]);
                    check_assign(ltype,rtype);

                    emitter.emit(op_type::mov_reg,symbol(slot),reg(emitter.reg_count));
                }
                break;
            }


            // assignment
            case ast_type::equal:
            {
                const auto rtype = compile_expression(line.nodes[1]);

                const auto name = line.nodes[0]->literal;


                if(!symbol_table.exists(name))
                {
                    panic("[COMPILE]: symbol '%s' assigned before declaration\n",name.c_str());
                    parser.print(l);
                }

                const auto &sym = symbol_table[name];

                check_assign(sym.type,rtype);

                emitter.emit(op_type::mov_reg,symbol(sym.slot),reg(emitter.reg_count));
                break;
            }

            case ast_type::ret:
            {
                const auto rtype = compile_expression(line.nodes[0]);
                
                // TODO: dont hard code this function infromation
                check_assign(function_table["main"].return_type,rtype);

                emitter.emit(op_type::mov_reg,reg(RETURN_REGISTER),reg(emitter.reg_count));
                emitter.emit(op_type::ret);
                break;
            }



            default:
            {
                panic("[COMPILE] unexpected token\n");
                parser.print(l);
            }
        }
    }
}

void Interloper::compile_functions()
{
    for(const auto n: root->nodes)
    {
        // TODO: we want to preserver this information but where shoudl we store it
        // when we have more than one function?
        //emitter.sym_count = 0;

        const auto &node = *n;

        // unless its a function we dont care
        if(node.type != ast_type::function)
        {
            continue;
        }

        // TODO:
        // need to allocate intial slots to the function args...


        // parse out each line of the function
    
        // what is the best way to go about doing this?
        // for now we are going to ignore scoping issues
        
        auto block = node.nodes[1];
        compile_block(block);

        if(error)
        {
            return;
        }    
    }
}


// TODO: start writing tests for more invalid sequences
// and improve error reporting in later stages of compilation

// plan:
// implement bitwise operators -> boolean + logical -> if statements
// for loops -> function calls -> pointers -> arrays -> structs

void Interloper::compile(const std::vector<std::string> &lines)
{
    // make sure everything is clean
    program.clear();
    symbol_table.clear();
    function_table.clear();
    emitter.program.clear();

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

    //parser.print(root);


    // okay now we need to start doing semantic analysis
    // first handle any imports, macros etc (skip for now)
    // handle any type declartions (skip for now)
    // handle function declartions


    parse_function_declarations();

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
    allocate_registers();

    // emit the actual target asm
    // for now we will just perform some adjustment on the register operands
    emit_asm();

    // okay now we need to actually resolve all the addresses into a meaningful place
    // resolve_labels();
}

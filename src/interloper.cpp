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

std::string Interloper::type_name(const Type &type)
{
    if(type.type_idx < BUILTIN_TYPE_SIZE)
    {
        return builtin_type_name(static_cast<builtin_type>(type.type_idx));
    }

    // TODO: make this return properly for user defined types
    else
    {
        return "";
    }
}

// TODO: for now we are going to ignore typechecking
// but we will have to check each part of this expr coerces to the right type at some point

// how do we want to emit this because we are having something thats like 
// a) ones going to functions and vars will they need to be compiled differently

// b) how do we emit the correct name

// c) how do we handle the type? (we can ignore this concern for now but it needs to be solved at some point)

// d) how do we handle symbols we need i.e function names? or branches
//    the offsets for them will differ from our ir ones and they will be moved
//    during optimisation passes

void Interloper::compile_arith_op(AstNode *node, op_type type)
{
    compile_expression(node->nodes[0]);
    compile_expression(node->nodes[1]);

    const auto v1 = reg(emitter.reg_count-2);
    const auto v2 = reg(emitter.reg_count-1);

    // we are now done with one of the registers as it was only used a tmp
    // for calculating the result
    const auto dst = reg(v1);
    emitter.reg_count -= 1; 

    emitter.emit(type,dst,v1,v2);        
}

void Interloper::compile_expression(AstNode *node)
{
    if(!node)
    {
        return;
    }

   
    switch(node->type)
    {
        // multiple assigment
        case ast_type::equal:
        {
            compile_expression(node->nodes[1]);

            const auto name = node->nodes[0]->literal;

            // TODO: this needs to be type checked

            
            const bool exists = symbol_table.count(name);

            if(!exists)
            {
                printf("[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
                parser.print(node);
                exit(1);
            }

            
            auto &sym = symbol_table[name];
            emitter.emit(op_type::mov_reg,symbol(sym.slot),reg(emitter.reg_count - 1));

            break;            
        }


        case ast_type::value:
        {
            emitter.emit(op_type::mov_imm,reg(emitter.reg_count++),node->value);
            break;
        }

        case ast_type::symbol:
        {
            // check symbol is declared or else error
            const bool exists = symbol_table.count(node->literal);

            if(!exists)
            {
                printf("[COMPILE]: symbol '%s' used before declaration\n",node->literal.c_str());
                parser.print(node);
                exit(1);
            }

            
            auto &sym = symbol_table[node->literal];


            emitter.emit(op_type::mov_reg,reg(emitter.reg_count++),symbol(sym.slot));
            break;
        }

    
        case ast_type::minus:
        {
            compile_arith_op(node,op_type::sub_reg);
            break;
        }

        case ast_type::plus:
        {
            compile_arith_op(node,op_type::add_reg);
            break;
        }
    
        case ast_type::times:
        {
            compile_arith_op(node,op_type::mul_reg);
            break;          
        }


        case ast_type::divide:
        {
            compile_arith_op(node,op_type::div_reg);
            break;           
        }

        default:
        {
            printf("[COMPILE]: invalid expression\n");
            parser.print(node);
            exit(1);
        }
    }
}


// in our IR how do we seperate a tempoary from an actual value we need to actually keep

void Interloper::compile_block(AstNode *node)
{
    for(const auto l : node->nodes)
    {
        const auto &line = *l;

        emitter.reg_count = 0;

        switch(line.type)
        {
            // variable declaration
            case ast_type::declaration:
            {
                // get entry into symbol table
                const auto name = line.literal;
                const auto type = line.nodes[0]->variable_type;

                const auto slot = emitter.sym_count;

                // add new symbol table entry
                symbol_table[name] = Symbol(name,type,emitter.sym_count++);

                // allocate new symbol slot in the IR
                slot_lookup.push_back(name);

                // handle right side expression (if present)
                if(line.nodes.size() == 2)
                {
                    compile_expression(line.nodes[1]);
                    emitter.emit(op_type::mov_reg,symbol(slot),reg(emitter.reg_count - 1));
                }
                break;
            }


            // assignment
            case ast_type::equal:
            {
                compile_expression(line.nodes[1]);

                const bool exists = symbol_table.count(line.nodes[0]->literal);

                if(!exists)
                {
                    printf("[COMPILE]: symbol '%s' assigned before declaration\n",line.nodes[0]->literal.c_str());
                    parser.print(l);
                    exit(1);
                }

                auto &sym = symbol_table[line.nodes[0]->literal];


                emitter.emit(op_type::mov_reg,symbol(sym.slot),reg(emitter.reg_count - 1));
                break;
            }

            case ast_type::ret:
            {
                compile_expression(line.nodes[0]);
                
                emitter.emit(op_type::mov_reg,reg(RETURN_REGISTER),reg(emitter.reg_count - 1));
                emitter.emit(op_type::ret);
                break;
            }



            default:
            {
                printf("[COMPILE] unexpected token\n");
                parser.print(l);
                exit(1);
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
    }
}

void Interloper::compile(const std::vector<std::string> &lines)
{
    // tokenize input file
    const auto tokens = lexer.tokenize(&lines);

    if(lexer.error)
    {
        exit(1);
    }

    //print_tokens(tokens);


    // build ast

    parser.init(&lines,&tokens);
    parser.parse(&root);
    
    
    if(!root || parser.error)
    {
        exit(1);
    }

    parser.print(root);


    // okay now we need to start doing semantic analysis
    // first handle any imports, macros etc (skip for now)
    // handle any type declartions (skip for now)
    // go through every function type check and emit ir



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

    //dump_ir();

    // optimise_ir();

    // perform register allocation
    allocate_registers();

    dump_ir();

    // emit the actual target asm
    // for now we will just perform some adjustment on the register operands
    emit_asm();

    // okay now we need to actually resolve all the addresses into a meaningful place
    // resolve_labels();

    Interpretter interpretter;
    interpretter.run(reinterpret_cast<uint8_t*>(program.data()),program.size() * sizeof(Opcode));
}

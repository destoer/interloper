#include <interloper.h>

#include "lexer.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "optimize.cpp"
#include "ir.cpp"


Type compile_expression(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode &line);
std::pair<Type, u32> read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
std::pair<Type, u32> index_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
void compile_decl(Interloper &itl,Function &func, const AstNode &line);
void compile_block(Interloper &itl,Function &func,AstNode *node);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);
std::pair<Type,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot);



void dump_ir_sym(Interloper &itl)
{
    for(auto &[key,func] : itl.function_table)
    {
        UNUSED(key);

        dump_ir(func,itl.symbol_table.slot_lookup,itl.symbol_table.label_lookup);
    }    
}

u32 eval_const_expr(const AstNode *node)
{
    assert(node);

    switch(node->type)
    {
        case ast_type::value: return node->value.v;

        case ast_type::times: return eval_const_expr(node->nodes[0]) * eval_const_expr(node->nodes[1]);

        case ast_type::plus: return eval_const_expr(node->nodes[0]) + eval_const_expr(node->nodes[1]);

        case ast_type::minus: return eval_const_expr(node->nodes[0]) - eval_const_expr(node->nodes[1]);

        case ast_type::divide:
        {
            const u32 v1 = eval_const_expr(node->nodes[0]);
            const u32 v2 = eval_const_expr(node->nodes[1]);

            if(v2 == 0)
            {
                panic("division by zero in eval const expr");
            }

            return v1 / v2;
        }

        default: print(node); unimplemented("eval const expr node"); break;
    }
}




Type get_type(Interloper &itl, AstNode *type_decl)
{
    Type type;

    type.type_idx = type_decl->type_idx;

    // not a plain plain type
    if(type_decl->nodes.size())
    {
        AstNode *arr_decl = nullptr;
        AstNode *ptr_decl = nullptr;

        for(auto n : type_decl->nodes)
        {
            switch(n->type)
            {
                case ast_type::ptr_indirection:
                {
                    ptr_decl = n;
                    break;
                }

                case ast_type::arr_dimensions:
                {
                    if(ptr_decl)
                    {
                        type.contains_ptr = true;
                    }
                    arr_decl = n;
                    break;
                }

                default: assert(false);
            }
        }

        // parse out pointer indirection
        if(ptr_decl)
        {
            type.ptr_indirection = ptr_decl->type_idx;
        }

        // parse out array dimensions
        if(arr_decl)
        {
            type.degree = arr_decl->nodes.size();
            
            for(u32 i = 0; i < type.degree; i++)
            {
                if(i >= MAX_ARR_SIZE)
                {
                    panic(itl,"array dimensions execeeded %s\n",type_decl->literal.c_str());
                    return type;
                }

                auto n = arr_decl->nodes[i];

                // variable size
                if(n->type == ast_type::arr_var_size)
                {
                    type.dimensions[i] = RUNTIME_SIZE;
                }

                // fixed size: const expr
                else
                {
                    type.dimensions[i] = eval_const_expr(n);
                }
            }
        }


        

    }

    return type;
}


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


        
        const auto return_type = get_type(itl,node.nodes[0]);
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
            const auto type = get_type(itl,a->nodes[0]);

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



std::pair<Type,u32> symbol(Interloper &itl, AstNode *node)
{
    const auto name = node->literal;

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(!sym_opt)
    {
        panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.c_str());
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


void do_ptr_load(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func.emitter,load_ptr(dst_slot,addr_slot,0,size,is_signed(type)));
    }   

    else if(is_array(type))
    {
        // read and copy data
        const u32 buf_slot = new_slot(func);
        emit(func.emitter,load_ptr(buf_slot,addr_slot,0,GPR_SIZE,false));
        emit(func.emitter,op_type::store_arr_data,buf_slot,dst_slot);

        // read and copy len
        const u32 len_slot = new_slot(func);
        emit(func.emitter,load_ptr(len_slot,addr_slot,GPR_SIZE,GPR_SIZE,false));
        emit(func.emitter,op_type::store_arr_len,len_slot,dst_slot);
    }

    else
    {
        unimplemented("struct read");
    }  
}


void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func.emitter,store_ptr(dst_slot,addr_slot,size,0));  
    }

    else
    {
        unimplemented("struct write");
    } 
}

// TODO: we want this but have a bool that diffentiates between
// reads/writes
std::pair<Type,u32> load_struct(Interloper &itl,Function &func, AstNode *node, u32 dst_slot)
{
    // TODO: how should we emit this for pointers?

    // load_struct <dst> <struct>,<member_idx>

    // TODO: assumes no depth
    const u32 struct_slot = new_slot(func);
    auto [type, slot] = compile_oper(itl,func,node->nodes[0],struct_slot);

    // TODO: for now we assume that this is a single member
    const auto member = node->literal;

    if(is_pointer(type))
    {
        type.ptr_indirection -= 1;


        if(is_array(type))
        {
            if(member == "len")
            {
                // TODO: how should this work for multi dimensional arrays?
                emit(func.emitter,load_ptr(dst_slot,slot,GPR_SIZE,GPR_SIZE,false));
                return std::pair<Type,u32>{Type(GPR_SIZE_TYPE),dst_slot};
            }

            else
            {
                unimplemented("array unknown member access %s\n",member.c_str());
            }           
        }

        else
        {
            unimplemented("struct access via pointer");
        }
    }

    // is an array hardcode the members
    else if(is_array(type))
    {
        if(member == "len")
        {
            // TODO: how should this work for multi dimensional arrays?
            emit(func.emitter,op_type::load_arr_len,dst_slot,slot);
            return std::pair<Type,u32>{Type(GPR_SIZE_TYPE),dst_slot};
        }

        else
        {
            unimplemented("array unknown member %s\n",member.c_str());
        }
    }

    else
    {
        unimplemented("struct access on user defined type");
    }
}

// this should handle grabbing values and symbols
// if it can see a symbol or a value it wont call compile_expression
std::pair<Type,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot)
{
    panic(node == nullptr,"nullptr in compile_oper");

    // test what the current node is
    switch(node->type)
    {
        case ast_type::value:
        {
            const auto t1 = value(func,node,dst_slot);
            return std::pair<Type,u32>{t1,dst_slot};
        }

        case ast_type::symbol:
        {
            return symbol(itl,node);
        }

        case ast_type::access_member:
        {
            const auto [type,addr_slot] = load_struct(itl,func,node,dst_slot);
            return std::pair<Type,u32>{type,dst_slot};
        }

        case ast_type::array_access:
        {
            return read_arr(itl,func,node,dst_slot);
        }

        // compile an expr
        default:
        {
            const auto t1 = compile_expression(itl,func,node,dst_slot);
            return std::pair<Type,u32>{t1,dst_slot};
        }
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




//  we dont want the 2nd stage IR handling how things need to be copied
// as it does not have the information required easily accessible
void compile_move(Interloper &itl, Function &func, u32 dst_slot, u32 src_slot, const Type& dst_type, const Type& src_type)
{
    UNUSED(itl);
    // check the operation is even legal

    

    // can be moved by a simple data copy
    if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
    {
        emit(func.emitter,op_type::mov_reg,dst_slot,src_slot);
    }

    else if(is_array(dst_type) && is_array(src_type))
    {
        // store the len
        const u32 len_slot = new_slot(func);
        emit(func.emitter,op_type::load_arr_len,len_slot,src_slot,0);
        emit(func.emitter,op_type::store_arr_len,len_slot,dst_slot,0);


        // store the buf
        const u32 buf_slot = new_slot(func);
        emit(func.emitter,op_type::load_arr_data,buf_slot,src_slot,0);
        emit(func.emitter,op_type::store_arr_data,buf_slot,dst_slot,0);
    }

    // requires special handling to move
    else
    {
        unimplemented("move user defined type");

        // arrays

        // structs
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
        return Type(builtin_type::void_t);
    }

    // for now we are just going with callee saved
    // if we eventually mix caller and callee saved so we can have the called func overwrite values
    // we dont care about then we need to re add the save and restore directives
    // and we will need a push and pop bitset to implement them
    // with care to rewrite where the return register ends up when we restore
    // save all the used regs
    //emit(func.emitter,op_type::save_regs);
    
    // how many args are we pushing to the stack?
    u32 arg_clean = 0;

    // push args in reverse order and type check them
    for(s32 i = func_call.args.size() - 1; i >= 0; i--)
    {
        const auto &arg = itl.symbol_table.slot_lookup[func_call.args[i]];

        if(is_array(arg.type))
        {
            
            const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[i],new_slot(func));


            // check what kind of array we are getting by here and just push the struct in reverse order

            // TODO: this needs to be factored off but just hard code it all for now
            if(is_runtime_size(arg_type.dimensions[0]))
            {
                const u32 len_slot = new_slot(func);
                emit(func.emitter,op_type::load_arr_len,len_slot,reg,0);
                emit(func.emitter,op_type::push_arg,len_slot);

                const u32 data_slot = new_slot(func);
                emit(func.emitter,op_type::load_arr_data,data_slot,reg,0);
                emit(func.emitter,op_type::push_arg,data_slot);

                arg_clean += 2;
            }

            // standard array pass
            else
            {
                if(is_runtime_size(arg.type.dimensions[0]))
                {
                    const u32 len_slot = new_slot(func);
                    emit(func.emitter,op_type::load_arr_len,len_slot,reg,0);
                    emit(func.emitter,op_type::push_arg,len_slot);

                    const u32 data_slot = new_slot(func);
                    emit(func.emitter,op_type::load_arr_data,data_slot,reg,0);
                    emit(func.emitter,op_type::push_arg,data_slot);

                    arg_clean += 2;
                }

                else
                {
                    unimplemented("fixed size passed to fixed size");
                }
            }

            check_assign(itl,arg.type,arg_type,true);
        }

        // TODO: handle being passed args that wont fit inside a single hardware reg
        // NOTE: this is just an impl guard we weill probably want specific handlerls for structs etc
        else if(type_size(itl,arg.type) > sizeof(u32))
        {
            unimplemented("function arg: non register size: %d\n",type_size(itl,arg.type));
        }

        // plain builtin in variable
        else
        {
            // builtin type
            const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[i],new_slot(func));


            // type check the arg
            check_assign(itl,arg.type,arg_type,true);

            // finally push the arg
            emit(func.emitter,op_type::push_arg,reg);

            arg_clean++;
        }
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
    if(arg_clean)
    {
        emit(func.emitter,op_type::clean_args,arg_clean);
    }
  

    // restore callee saved values
    //emit(func.emitter,op_type::restore_regs);


    // store the return value back into a reg
    if(returns_value)
    {
        // TODO: is this dst type correct?
        compile_move(itl,func,dst_slot,RV_IR,func.return_type,func.return_type);
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

    new_block(&itl.list_allocator,func.emitter,type,slot);
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

            append(blocks[cur_block].list,Opcode(op_type::bnc,slot,r,0));


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

        if(block.list.end->opcode.op == op_type::exit_block)
        {
            block.list.end->opcode = Opcode(op_type::b,exit_block,0,0);
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
    append(func.emitter.program[intial_block].list,Opcode(op_type::bnc,exit_block,stmt_cond_reg,0));

    destroy_scope(itl.symbol_table);
}


// TODO: this needs a cleanup
// TODO: does it make sense to use the same function for both the @ and & operator?
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
                return std::pair<Type,u32>{Type(builtin_type::void_t),0};
            }

            const auto &sym = sym_opt.value();

            // type is now what it pointed to
            auto type = sym.type;



            if(addrof)
            {
                if(is_array(sym.type))
                {
                    // we only want to allow taking a pointer to a vla
                    // as the real one doesnt have a struct...
                    if(!is_runtime_size(sym.type.dimensions[0]))
                    {
                        panic(itl,"cannot take pointer to fixed size array: %s\n",name.c_str());
                        return std::pair<Type,u32>{Type(builtin_type::void_t),0};
                    }
                }

                type.ptr_indirection += 1;

                // actually  get the addr of the ptr
                emit(func.emitter,op_type::addrof,slot,slot_idx(sym));
                return std::pair<Type,u32>{type,slot};
            }

            // deref
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

        case ast_type::array_access:
        {
            if(addrof)
            {
                return index_arr(itl,func,node,slot);
            }

            else
            {
                auto [type,addr_slot] = index_arr(itl,func,node,slot);

                // actually load the pointer with ptr_load
                type.ptr_indirection -= 1;
                const u32 ptr_slot = new_slot(func);
                do_ptr_load(itl,func,ptr_slot,addr_slot,type);

                // contained type is not actually a pointer
                if(!is_pointer(type))
                {
                    panic(itl,"[COMPILE]: array '%s' does not contain a pointer\n",node->literal.c_str());
                    return std::pair<Type,u32>{Type(builtin_type::void_t),0};
                }

                // okay now just index out the final type
                type.ptr_indirection -= 1;

                return std::pair<Type,u32>{type,ptr_slot};
            }
        }

        default:
        {
            print(node);
            unimplemented("load_addr expr");
        }
    }
}


std::pair<Type, u32> index_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    const auto arr_name = node->literal;

    const auto arr_opt = get_sym(itl.symbol_table,arr_name);

    if(!arr_opt)
    {
        panic(itl,"[COMPILE]: array '%s' used before declaration\n",arr_name.c_str());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};       
    }

    const auto arr = arr_opt.value();

    if(!is_array(arr.type))
    {
        panic(itl,"[COMPILE]: '%s' is not an array got type %s\n",arr_name.c_str(),type_name(itl,arr.type).c_str());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};  
    }

    

    // TODO: for now we assume this is a plain array access
    // this will have to be slightly adjusted for var length arrays
    // on a single dimension

    /*
        // repeat for every index
        // and just add the exprs on each subscript together
        mov t0, <expr_result>
        mul t1, t0, arr_size

        // after full offset is calced
        load_arr t0, [arr,t1] 
    */
    const auto [subscript_type,subscript_slot] = compile_oper(itl,func,node->nodes[0],new_slot(func));
    if(!is_integer(subscript_type))
    {
        panic(itl,"[COMPILE]: expected integeral expr for array subscript got %s\n",type_name(itl,subscript_type));
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};  
    }

    const u32 index_slot = new_slot(func);

    // TODO: handle this for multi dimensional arrays

    // TODO: bound check this at compile time for const exprs, or fixed size arrays

    // perform the access and get the underlying type
    auto accessed_type = contained_arr_type(arr.type);
    const auto size = type_size(itl,accessed_type);

    emit(func.emitter,op_type::mul_imm,index_slot,subscript_slot,size);   

    const auto data_slot = new_slot(func);

    emit(func.emitter,op_type::load_arr_data,data_slot,slot_idx(arr));
    emit(func.emitter,op_type::arr_index,dst_slot,data_slot,index_slot);

    // pointer to this type!
    accessed_type.ptr_indirection += 1;
    

    return std::pair<Type,u32>{accessed_type,dst_slot};
}

std::pair<Type, u32> read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_slot(func));

    // deref of pointer
    type.ptr_indirection -= 1;


    do_ptr_load(itl,func,dst_slot,addr_slot,type);

    return std::pair<Type,u32>{type,dst_slot};
}


// TODO: our type checking for our array assigns has to be done out here to ensure locals are type checked correctly
void write_arr(Interloper &itl,Function &func,AstNode *node,const Type& write_type, u32 slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_slot(func));

    // deref of pointer
    type.ptr_indirection -= 1;

    do_ptr_store(itl,func,slot,addr_slot,type);

    check_assign(itl,type,write_type);
}


Type compile_expression(Interloper &itl,Function &func,AstNode *node,u32 dst_slot)
{
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
            const auto [type,slot] = load_addr(itl,func,node->nodes[0],new_slot(func),false);
            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;
        }

        case ast_type::cast:
        {
            const auto [old_type,slot] = compile_oper(itl,func,node->nodes[1],new_slot(func));
            const auto new_type = get_type(itl,node->nodes[0]);

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
                return Type(builtin_type::void_t);
            }

            const auto &sym = sym_opt.value();

            check_assign(itl,sym.type,rtype);

            compile_move(itl,func,slot_idx(sym),slot,sym.type,rtype);

            // TODO: make sure that the silly code this gens
            // is cleaned up by the optimiser
            if(dst_slot != slot)
            {
                compile_move(itl,func,dst_slot,slot,sym.type,rtype);
            }

            return sym.type;        
        }

        
        
        // TODO: this has to be reworked to support nesting
        // TODO: the way we want to move depends on the way it is used
        // we fundemtnally want to return out the array ->
        // but we need to return out not only the length + pointer

        case ast_type::arr_initializer:
        {
            // for each val

                //  check the type against the assigned one

            // we need to setup an initial array size 
            // so we need to design the IR for this
            // we need to decide how this will be shoved on the stack too

            // TODO: how do we want to emit the intial array setup operation?
            // just emit a bunch of stores by gutting our array deref methods?

            unimplemented("array initializer");
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
            return Type(builtin_type::void_t);
        }
    }
}


void compile_arr_decl(Interloper& itl, Function& func, const AstNode &line, const Symbol& array)
{

    // TODO: this has to be reworked to support nesting
    // TODO: the way we want to move depends on the way it is used
    // we fundemtnally want to return out the array ->
    // but we need to return out not only the length + pointer

    // TODO: we need to check if this array is just a constant
    // and then just shove it in the const data section

    assert(array.type.degree == 1);

    // fixed size
    if(!is_runtime_size(array.type.dimensions[0]))
    {
        const auto [size,count] = arr_size(itl,array.type); 
        emit(func.emitter,op_type::alloc_slot,slot_idx(array),size,count);
    }

    else
    {
        emit(func.emitter,op_type::alloc_vla,slot_idx(array));
    }

    // has an initalizer
    if(line.nodes.size() == 2)
    {
        // TODO: type check this is an array
        // TODO: typecheck the length on this
        // TODO: handle this on a variable length array

       if(line.nodes[1]->type == ast_type::arr_initializer)
       {

            const Type index_type = contained_arr_type(array.type);

            const u32 count = line.nodes[1]->nodes.size();
            const u32 size = type_size(itl,index_type);

            // if this is vla allocate the struct and supply the setup info
            if(is_runtime_size(array.type.dimensions[0]))
            {


                const u32 data_slot = new_slot(func);
                emit(func.emitter,op_type::buf_alloc,data_slot,slot_idx(array));

                // store the index we need here
                emit(func.emitter,op_type::state_dump,size,count);

                // now store the data
                emit(func.emitter,op_type::store_arr_data,data_slot,slot_idx(array));

                //  and the length
                const u32 len_slot = new_slot(func);
                emit(func.emitter,op_type::mov_imm,len_slot,count);
                emit(func.emitter,op_type::store_arr_len,len_slot,slot_idx(array));
            }

            // for each val
            for(u32 i = 0; i < count; i++)
            {
                auto n = line.nodes[1]->nodes[i];

                // check the type against cur type and check it matches
                // perform type promotion if necessary for integers etc
                const auto [rtype,reg] = compile_oper(itl,func,n,new_slot(func));

                check_assign(itl,index_type,rtype);

                if(itl.error)
                {
                    return;
                }

                emit(func.emitter,op_type::init_arr_idx,slot_idx(array),reg,i);
            }

        }

        else
        {
            const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1],slot_idx(array));

            check_assign(itl,array.type,rtype); 

            // oper is a single symbol and the move hasn't happened we need to explictly move it
            if(slot_idx(array) != reg)
            {
                compile_move(itl,func,slot_idx(array),reg,array.type,rtype);
            }
        }
    }    
}


void compile_decl(Interloper &itl,Function &func, const AstNode &line)
{
    // get entry into symbol table
    const auto name = line.literal;
    const auto ltype = get_type(itl,line.nodes[0]);

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
    const auto &sym = add_symbol(itl.symbol_table,name,ltype,size);




    if(is_array(sym.type))
    {
        compile_arr_decl(itl,func,line,sym);
    }

    // handle right side expression (if present)
    // standard decl
    else if(line.nodes.size() == 2)
    {
        emit(func.emitter,op_type::alloc_slot,slot_idx(sym));

        // normal assign
        const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1],slot_idx(sym));

        // oper is a single symbol and the move hasn't happened we need to explictly move it
        if(slot_idx(sym) != reg)
        {
            compile_move(itl,func,slot_idx(sym),reg,sym.type,rtype);
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
    const auto &sym = add_symbol(itl.symbol_table,name,type,size);

    emit(func.emitter,op_type::alloc_slot,slot_idx(sym));
    compile_move(itl,func,slot_idx(sym),reg,sym.type,type);
}


void compile_block(Interloper &itl,Function &func,AstNode *node)
{
    new_scope(itl.symbol_table);

    for(auto l : node->nodes)
    {
        // for error printing
        itl.cur_line = l;

        const auto &line = *l;

        if(itl.error)
        {
            return;
        }

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
                            do_ptr_store(itl,func,slot,addr_slot,type);
                            break;                        
                        }

                        case ast_type::array_access:
                        {
                            write_arr(itl,func,line.nodes[0],rtype,slot);
                            break;
                        }    

                        default:
                        {
                            print(line.nodes[0]);
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
                        break;
                    }

                    const auto &sym = sym_opt.value();

                    check_assign(itl,sym.type,rtype);

                    compile_move(itl,func,slot_idx(sym),slot,sym.type,rtype);
                }
                break;
            }


            case ast_type::ret:
            {
                // returns a value
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(itl,func,line.nodes[0],RV_IR);

                    // TODO: need to devise a scheme for returning out large items
                    if(type_size(itl,func.return_type) > GPR_SIZE)
                    {
                        unimplemented("return large sized var");
                    }

    
                    if(v1 != RV_IR)
                    {
                        compile_move(itl,func,RV_IR,v1,func.return_type,rtype);
                    }


                    if(itl.error)
                    {
                        break;
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


// TODO: impl source line information on the parse tree

// TODO: make prints such as the assembly and parse tree command line flags

// TODO: basic type checking for returning pointers to local's

// remove reliance on stl containers for compiler structs
// and do a big refactoring pass on the compiler when arrays are implemented

// finish up arrays then do the cleanup above
// need jagged arrays, mult dimensional arrays and pointer semantics on arrays working

// array order
/*
    // <--- need to check what kind of array we are using inside the IR
    // and emit the appropiate set of loads
    // i.e .len should use the offset + GPR_SIZE
    array_conv +
    array_var_size #
    <--- type checking tests for these here

    // DO fixed sized first so we have some idea about the upper level encoding
    // without the complexlitly of VLA
    array_multi_fixed_size

    array_multi_vla
    <--- type checking tests for multi dimensional arrays

    pointers to arrays


    refactoring if need be
*/


// impl source line information for parse tree

// plan:
// arrays -> strings -> imports -> tuples -> structs -> 
// -> early stl -> function_pointers -> labels ->  compile time execution ->
// unions -> inline asm -> debugg memory guards -> ...

void destroy_itl(Interloper &itl)
{
    itl.program.clear();
    clear(itl.symbol_table);
    itl.function_table.clear();

    // clear the tree if present
    if(itl.root)
    {
        delete_tree(itl.root); 
        itl.root = nullptr;
        itl.cur_line = nullptr;
    }

    destory_allocator(itl.list_allocator);
}

static constexpr u32 LIST_INITIAL_SIZE = 10 * 1024;

void compile(Interloper &itl,const std::vector<std::string> &lines)
{
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
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
        const b32 parser_error = parse(itl.parser,&itl.root,itl.lexer.tokens,lines);

    
        if(!itl.root || parser_error)
        {
            itl.error = true;
            return;
        }
    }

    if(itl.print_ast)
    {
        print(itl.root);
    }

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
    delete_tree(itl.root);
    itl.root = nullptr;
    itl.cur_line = nullptr;

    if(itl.error)
    {
        return;
    }


    //optimise_ir(itl);

    if(itl.print_ir)
    {
        dump_ir_sym(itl);
    }
    
    // perform register allocation
    for(auto &[key, func]: itl.function_table)
    {
        UNUSED(key);
        allocate_registers(itl,func);
        putchar('\n');
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm(itl);


}

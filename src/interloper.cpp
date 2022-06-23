#include <interloper.h>

#include "lexer.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "optimize.cpp"
#include "ir.cpp"
#include "struct.cpp"


Type compile_expression(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode &line);
std::pair<Type, u32> read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
std::pair<Type, u32> index_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
void compile_decl(Interloper &itl,Function &func, const AstNode &line);
void compile_block(Interloper &itl,Function &func,AstNode *node);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);
std::pair<Type,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot);

std::pair<Type,u32> read_struct(Interloper& itl,Function& func, u32 dst_slot, AstNode *node);
void write_struct(Interloper& itl,Function& func, u32 src_slot, const Type& rtype, AstNode *node);

void traverse_struct_initializer(Interloper& itl, Function& func, AstNode* node, const u32 addr_slot, const Struct& structure, u32 offset = 0);


std::string get_program_name(const std::string &filename)
{
    if(!contains(filename,"."))
    {
        return filename + ".itl";
    }

    return filename;
}


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



void print_func_decl(Interloper& itl,const Function &func)
{
    printf("func: %s\n",func.name.c_str());

    for(auto slot : func.args)
    {
        auto &sym = sym_from_slot(itl.symbol_table.slot_lookup,slot);

        print(itl,sym); 
    }

    printf("return type %s\n",type_name(itl,func.return_type).c_str());  
}

// scan the top level of the parse tree for functions
// and grab the entire signature
// we wont worry about the scope on functions for now as we wont have namespaces for a while
void parse_function_declarations(Interloper& itl)
{
    for(const auto n : itl.func_root->nodes)
    {
        const auto &node = *n;


        const auto return_type = get_type(itl,node.nodes[0]);
        const auto name = node.literal;
        
        if(itl.function_table.count(name))
        {
            panic(itl,"function %s has been declared twice!\n",name.c_str());
            return;
        }

        std::vector<u32> args;

        const auto decl = node.nodes[2];

        // if we are returning out a struct we have a hidden pointer to return it out of in the first arg!
        const b32 sfa = type_size(itl,return_type) > GPR_SIZE;

        u32 arg_offset = 0;

        if(sfa)
        {
            Type ptr_type = return_type;
            ptr_type.ptr_indirection += 1;

            // add the var to slot lookup and link to function
            // we will do a add_scope to put it into the scope later
            Symbol sym = Symbol("_struct_ret_ptr",ptr_type,GPR_SIZE,arg_offset);
            add_var(itl.symbol_table,sym);

            args.push_back(sym.slot);     

            arg_offset += GPR_SIZE;
        }

        // rip every arg
        for(const auto a : decl->nodes)
        {
            const auto name = a->literal;
            const auto type = get_type(itl,a->nodes[0]);

            const auto size = type_size(itl,type);

            // add the var to slot lookup and link to function
            // we will do a add_scope to put it into the scope later
            Symbol sym = Symbol(name,type,size,arg_offset);
            add_var(itl.symbol_table,sym);

            args.push_back(sym.slot);

            // if size is below GPR just make it take that much
            const u32 arg_size = sym.size < GPR_SIZE? 4 : size;

            arg_offset += arg_size;

            //printf("arg slot %s: %d : %d\n",sym.name.c_str(),sym.slot, args[args.size()-1]);
        }


        const Function function(name,return_type,args,itl.symbol_table.label_lookup.size());


        //print_func_decl(itl,function);

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

    return std::pair<Type,u32>{sym.type,sym.slot};
}


Type value(Function& func,AstNode *node, u32 dst_slot)
{
    if(node->value.sign)
    {
        const s32 value = s32(node->value.v);
        emit(func.emitter,op_type::mov_imm,dst_slot,value);

        // what is the smallest storage type that this will fit inside?
        if(in_range(value,s32(builtin_min(builtin_type::s8_t)),s32(builtin_max(builtin_type::s8_t))))
        {
            return  Type(builtin_type::s8_t);
        }

        else if(in_range(value,s32(builtin_min(builtin_type::s16_t)),s32(builtin_max(builtin_type::s16_t))))
        {
            return Type(builtin_type::s16_t);
        }

        //else if(value,s32(builtin_min(builtin_type::s32_t)),s32(builtin_max(builtin_type::s32_t)))
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


void do_ptr_load(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type, u32 offset = 0)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func.emitter,load_ptr(dst_slot,addr_slot,offset,size,is_signed(type)));
    }   

    else if(is_array(type))
    {
        unimplemented("load arr from ptr");
    }

    else
    {
        unimplemented("struct read");
    }  
}


void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type, u32 offset = 0)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func.emitter,store_ptr(dst_slot,addr_slot,size,offset));  
    }

    else
    {
        unimplemented("struct write");
    } 
}


// this should handle grabbing values and symbols
// if it can see a symbol or a value it wont call compile_expression (it is up to caller)
// to decide what to actually do with operands
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

        case ast_type::access_struct:
        {
            return read_struct(itl,func,dst_slot,node);
        }

        case ast_type::char_t:
        {
            emit(func.emitter,op_type::mov_imm,dst_slot,node->literal[0]);
            return std::pair<Type,u32>{Type(builtin_type::u8_t),dst_slot};
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
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_tmp(func));

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_tmp(func));

    // pointer arith adds the size of the underlying type
    if(is_pointer(t1) && is_integer(t2))
    {
        if(type != op_type::sub_reg && type != op_type::add_reg)
        {
            panic(itl,"Pointer arithmetic is only defined on subtraction and additon! %s : %s\n",type_name(itl,t1).c_str(),type_name(itl,t2).c_str());
            return Type(builtin_type::void_t);
        }

        const u32 offset_slot = new_tmp(func);

        // get size of pointed to type
        Type contained = t2;
        contained.ptr_indirection -= 1;

        emit(func.emitter,op_type::mul_imm,offset_slot,v2,type_size(itl,t2));
        emit(func.emitter,type,dst_slot,v1,offset_slot);
    }

    // normal arith
    else
    {
        emit(func.emitter,type,dst_slot,v1,v2);
    }


    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2);

    

    return final_type;        
}


Type compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, u32 dst_slot)
{
    const auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_tmp(func));

    const auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_tmp(func));

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
    auto [t1,v1] = compile_oper(itl,func,node->nodes[0],new_tmp(func));

    auto [t2,v2] = compile_oper(itl,func,node->nodes[1],new_tmp(func));


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
            panic("%d is not a logical operation\n",s32(type));
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
        const auto sign = is_signed(builtin_type(t1.type_idx));

        // if we have gotten this far the sign of both are the same
        const auto op = LOGIC_OPCODE[sign][u32(type)];


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

    
    // convert fixed size pointer..
    if(is_fixed_array_pointer(src_type))
    {
        unimplemented("convert fixed size pointer");
    }

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underyling type if its a pointer
    else if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
    {
        emit(func.emitter,op_type::mov_reg,dst_slot,src_slot);
    }

    else if(is_array(dst_type) && is_array(src_type))
    {
        unimplemented("move array");
    }

    // requires special handling to move
    else
    {
        // copy out the strucutre using the hidden pointer in the first arg
        if(dst_slot == RV_IR)
        {
            const u32 ptr = new_tmp(func);
            emit(func.emitter,op_type::addrof,ptr,src_slot);

            ir_memcpy(itl,func,func.args[0],ptr,type_size(itl,dst_type));
        } 

        else
        {
            const u32 src_ptr = new_tmp(func);
            emit(func.emitter,op_type::addrof,src_ptr,src_slot);

            const u32 dst_ptr = new_tmp(func);
            emit(func.emitter,op_type::addrof,dst_ptr,dst_slot);

            ir_memcpy(itl,func,dst_ptr,src_ptr,type_size(itl,dst_type));
        }
    }
}

#include "intrin.cpp"

Type compile_function_call(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    if(intrin_table.count(node->literal))
    {
        const auto handler = intrin_table[node->literal];
        return handler(itl,func,node,dst_slot);
    }

    // check function is declared
    if(!itl.function_table.count(node->literal))
    {
        panic(itl,"[COMPILE]: function %s is not declared\n",node->literal.c_str());
        return Type(builtin_type::void_t);
    }
    const auto &func_call = itl.function_table[node->literal];

    const bool return_struct = type_size(itl,func_call.return_type) > GPR_SIZE;


    const s32 arg_offset = return_struct? 1 : 0;

    // check we have the right number of params
    if((func_call.args.size() - arg_offset) != node->nodes.size())
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
    for(s32 i = func_call.args.size() - 1; i >= arg_offset; i--)
    {
        const auto &arg =  sym_from_slot(itl.symbol_table.slot_lookup,func_call.args[i]);
  
        const u32 arg_idx = i - arg_offset;

        if(is_array(arg.type))
        {
            assert(arg.type.degree == 1);

            // pass a static string (TODO: we need to add const and make sure that the arg is marked as it)
            // const_pool_addr <slot>, offset  to load the address
            if(node->nodes[i]->type == ast_type::string)
            {
                const auto rtype = type_array(builtin_type::u8_t,node->nodes[arg_idx]->literal.size(),true);
                check_assign(itl,arg.type,rtype,true);
                
                // push the len offset
                const u32 len_slot = new_tmp(func);
                emit(func.emitter,op_type::mov_imm,len_slot,rtype.dimensions[0]);
                emit(func.emitter,op_type::push_arg,len_slot);

                // push the data offset
                const u32 static_offset = alloc_const_pool(itl,node->nodes[arg_idx]->literal.data(),rtype.dimensions[0],1);

                const u32 addr_slot = new_tmp(func);
                emit(func.emitter,op_type::pool_addr,addr_slot,static_offset,CONST_POOL);
                emit(func.emitter,op_type::push_arg,addr_slot);

                arg_clean += 2;
            }

            else
            {
                auto [arg_type,reg] = compile_oper(itl,func,node->nodes[arg_idx],new_tmp(func));

                // fixed sized array
                if(is_fixed_array_pointer(arg_type))
                {
                    const u32 len_slot = new_tmp(func);
                    emit(func.emitter,op_type::mov_imm,len_slot,arg_type.dimensions[0]);
                    emit(func.emitter,op_type::push_arg,len_slot);

                    emit(func.emitter,op_type::push_arg,reg);

                    // no longer care about the ptr
                    arg_type.ptr_indirection -= 1;

                    arg_clean += 2;                    
                }

                // push vla struct in reverse order
                // This conversion is implicit
                // TODO: this needs to handle conversions on multidimensional arrays
                else if(is_runtime_size(arg.type,0))
                {
                    const u32 len_slot = new_tmp(func);
                    emit(func.emitter,op_type::load_arr_len,len_slot,reg,0);
                    emit(func.emitter,op_type::push_arg,len_slot);

                    const u32 data_slot = new_tmp(func);
                    emit(func.emitter,op_type::load_arr_data,data_slot,reg,0);
                    emit(func.emitter,op_type::push_arg,data_slot);

                    arg_clean += 2;                
                }

                else
                {
                    unimplemented("pass fixed size");
                }

                check_assign(itl,arg.type,arg_type,true);
            }
        }


        else if(is_struct(arg.type))
        {
            const auto structure = struct_from_type(itl.struct_table,arg.type);

            const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[arg_idx],new_tmp(func));
            check_assign(itl,arg.type,arg_type,true);


            // TODO: support copies with larger loads
            static_assert(GPR_SIZE == sizeof(u32));

            // alloc the struct size for our copy
            emit(func.emitter,op_type::alloc_stack,structure.size);

            const u32 ptr = new_tmp(func);
            const u32 dst = new_tmp(func);

            // need to save SP as it will get pushed last
            emit(func.emitter,op_type::mov_reg,dst,SP_IR);
            emit(func.emitter,op_type::addrof,ptr,reg);

            ir_memcpy(itl,func,dst,ptr,structure.size);

            // clean up the stack push
            arg_clean += structure.size / GPR_SIZE;
        }

        // plain builtin in variable
        else
        {
            // builtin type
            const auto [arg_type,reg] = compile_oper(itl,func,node->nodes[arg_idx],new_tmp(func));


            // type check the arg
            check_assign(itl,arg.type,arg_type,true);

            // finally push the arg
            emit(func.emitter,op_type::push_arg,reg);

            arg_clean++;
        }
    }

    // push hidden arg for a struct return if we need it

   
    if(return_struct)
    {
        if(dst_slot == NO_SLOT)
        {
            unimplemented("no_slot: binding on large return type");
        }

        else if(is_sym(dst_slot))
        {
            arg_clean += 1;

            const u32 addr = new_tmp(func);
            emit(func.emitter,op_type::addrof,addr,dst_slot);
            
            emit(func.emitter,op_type::push_arg,addr);
        }

        else
        {
            unimplemented("tmp: large binding on return type");
        }
    }




    const bool returns_value = func_call.return_type.type_idx != u32(builtin_type::void_t);


    // if we have a register in R0 we need to save it so its not overwritten
    if(returns_value && !return_struct)
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

    // normal return
    if(!return_struct)
    {
        // store the return value back into a reg (if its actually binded)
        if(returns_value && dst_slot != NO_SLOT)
        {
            // TODO: is this dst type correct?
            compile_move(itl,func,dst_slot,RV_IR,func.return_type,func.return_type);
        }
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
            const auto [t,r] = compile_oper(itl,func,if_stmt.nodes[0],new_tmp(func));

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
                compile_expression(itl,func,node->nodes[0],new_tmp(func));
                break;
            }
        }

        if(itl.error)
        {
            return;
        }
    }

    const auto [t,stmt_cond_reg] = compile_oper(itl,func,node->nodes[cond_expr_idx],new_tmp(func));

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
        compile_expression(itl,func,node->nodes[2],new_tmp(func));
    }

    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,node->nodes[cond_expr_idx],new_tmp(func));
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
                emit(func.emitter,op_type::addrof,slot,sym.slot);
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
                return std::pair<Type,u32>{type,sym.slot};
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
                const u32 ptr_slot = new_tmp(func);
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

        case ast_type::access_struct:
        {
            if(addrof)
            {
                unimplemented("struct member addrof");
            }

            // deref on struct member that is a ptr
            else
            {
                auto [type,ptr_slot] = read_struct(itl,func,new_tmp(func),node);
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

// indexes off a given type + ptr
std::pair<Type,u32> index_arr_internal(Interloper& itl, Function &func,AstNode* node, const std::string& arr_name,
     const Type& type, u32 ptr_slot, u32 dst_slot)
{

    if(!is_array(type))
    {
        panic(itl,"[COMPILE]: '%s' is not an array got type %s\n",arr_name.c_str(),type_name(itl,type).c_str());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};  
    }


    if(node->nodes.size() > type.degree)
    {
        panic(itl,"Out of bounds indexing for array %s (%d:%d)\n",arr_name.c_str(),type.degree,node->nodes.size());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};  
    }

    auto accessed_type = contained_arr_type(type);
   
    // preaccumlate the total size per index (easiest in reverse order)
    // i.e [1][0][0], [0][1][0], [0][0][1]
    // how much does each of these add to the offsetting?

    u32 offsets[MAX_ARR_SIZE];

    offsets[type.degree - 1] = type_size(itl,accessed_type);


    for(s32 i = type.degree - 2; i >= 0; i--)
    {
        const u32 count = type.dimensions[i];

        // TODO: for now we are just going to assume we are indexing a fixed size array
        if(count == RUNTIME_SIZE)
        {
            unimplemented("index VLA");
        }

        else
        {
            offsets[i] = type.dimensions[i + 1] * offsets[i + 1];
        }    
    }

    
    u32 last_slot = ptr_slot;

    for(u32 i = 0; i < node->nodes.size(); i++)
    {

        const auto [subscript_type,subscript_slot] = compile_oper(itl,func,node->nodes[i],new_tmp(func));
        if(!is_integer(subscript_type))
        {
            panic(itl,"[COMPILE]: expected integeral expr for array subscript got %s\n",type_name(itl,subscript_type).c_str());
            return std::pair<Type,u32>{Type(builtin_type::void_t),0};  
        }

        /*
        const u32 count = type.dimensions[i];

        // this just works for single dimension VLA
        
        if(count == RUNTIME_SIZE)
        {
            unimplemented("index VLA");
        }

        else
        */
        {
            const u32 size = offsets[i];

            const u32 mul_slot = new_tmp(func);
            emit(func.emitter,op_type::mul_imm,mul_slot,subscript_slot,size);   

            const bool last_index = i ==  node->nodes.size() - 1;

            const u32 add_slot = last_index? dst_slot : new_tmp(func);
            emit(func.emitter,op_type::add_reg,add_slot,last_slot,mul_slot);

            last_slot = add_slot;
        }
    }


    // return pointer to sub array
    if(node->nodes.size() < type.degree)
    {
        auto sub_arr_type = type;
        
        // make it a sub array
        sub_arr_type.degree -= node->nodes.size();

        // move the dimensions back over itself
        memcpy(&sub_arr_type.dimensions[0],&type.dimensions[node->nodes.size()],sub_arr_type.degree * sizeof(type.dimensions[0]));



        sub_arr_type.ptr_indirection += 1;

        return std::pair<Type,u32>{sub_arr_type,dst_slot};
    }

    // return pointer to held type
    else
    {
        accessed_type.ptr_indirection += 1;
        return std::pair<Type,u32>{accessed_type,dst_slot};
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

    // get the initial data ptr
    const u32 data_slot = new_tmp(func);
    emit(func.emitter,op_type::load_arr_data,data_slot,arr.slot);

    return index_arr_internal(itl,func,node,arr_name,arr.type,data_slot,dst_slot);
}

std::pair<Type, u32> read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_tmp(func));

    // fixed array needs conversion by host
    if(is_fixed_array_pointer(type))
    {
        return std::pair<Type,u32>{type,addr_slot};
    }


    // deref of pointer
    type.ptr_indirection -= 1;

    do_ptr_load(itl,func,dst_slot,addr_slot,type);


    return std::pair<Type,u32>{type,dst_slot};
}


// TODO: our type checking for our array assigns has to be done out here to ensure locals are type checked correctly
void write_arr(Interloper &itl,Function &func,AstNode *node,const Type& write_type, u32 slot)
{
    auto [type,addr_slot] = index_arr(itl,func,node,new_tmp(func));


    // convert fixed size pointer..
    if(is_fixed_array_pointer(write_type))
    {
        unimplemented("convert fixed size pointer");
    }

    else
    {
        // deref of pointer
        type.ptr_indirection -= 1;

        do_ptr_store(itl,func,slot,addr_slot,type);

        check_assign(itl,type,write_type);
    }
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
            const auto [type,slot] = load_addr(itl,func,node->nodes[0],new_tmp(func),false);
            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;
        }

        case ast_type::cast:
        {
            const auto [old_type,slot] = compile_oper(itl,func,node->nodes[1],new_tmp(func));
            const auto new_type = get_type(itl,node->nodes[0]);

            handle_cast(itl,func.emitter,dst_slot,slot,old_type,new_type);
            return new_type;
        }

        case ast_type::sizeof_t:
        {
            // TODO: is it worth while adding this for type names?
            const auto [type,slot] = compile_oper(itl,func,node->nodes[0],new_tmp(func));

            // we only want the type of the expr
            if(is_tmp(slot))
            {
                emit(func.emitter,op_type::free_reg,slot);
            }

            const u32 size = type_size(itl,type);
            emit(func.emitter,op_type::mov_imm,dst_slot,size);

            return Type(builtin_type::u32_t);
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

            compile_move(itl,func,sym.slot,slot,sym.type,rtype);

            // TODO: make sure that the silly code this gens
            // is cleaned up by the optimiser
            if(dst_slot != slot)
            {
                compile_move(itl,func,dst_slot,slot,sym.type,rtype);
            }

            return sym.type;        
        }

        // TODO: do we want to allow other uses of this?
        case ast_type::initializer_list:
        {
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

                const auto slot = new_tmp(func);

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

        case ast_type::null_t:
        {
            emit(func.emitter,op_type::mov_imm,dst_slot,0);

            Type type = Type(builtin_type::null_t);
            type.ptr_indirection = 1;

            return type;
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

void traverse_arr_initializer(Interloper& itl,Function& func,AstNode *node,const u32 addr_slot, Type& type, u32 depth, u32* offset)
{
    const u32 node_len = node->nodes.size();

    // just a straight assign
    if(!node_len)
    {
        if(node->type == ast_type::string)
        {            
            if(type.degree != 1)
            {
                panic(itl,"expected single dimensional array got %d\n",type.degree);
                return;
            }

            // handle auto sizing
            if(type.dimensions[0] == DEDUCE_SIZE)
            {
                type.dimensions[0] = node->literal.size();
            }

            if(type.dimensions[0] == RUNTIME_SIZE)
            {
                unimplemented("string vla");
            }

            if(type.dimensions[0] < node->literal.size())
            {
                panic(itl,"expected array of atleast size %d got %d\n",node->literal.size(),type.dimensions[0]);
            }

            const auto base_type = contained_arr_type(type);
            const auto rtype = Type(builtin_type::u8_t);


            for(u32 i = 0; i < node->literal.size(); i++)
            {
                const u32 slot = new_tmp(func);
                emit(func.emitter,op_type::mov_imm,slot,node->literal[i]);
                check_assign(itl,base_type,rtype,false,true);

                do_ptr_store(itl,func,slot,addr_slot,rtype,*offset);

                *offset = *offset + 1;
            }           
        }

        else
        {
            unimplemented("single intializer");
        }

        return;
    }


    for(u32 i = 0; i < node_len; i++)
    {
        if(itl.error)
        {
            return;
        }

        // decl has too many dimensions
        if(depth >= type.degree)
        {
            panic(itl,"array declaration dimension exceeds type, expected: %d got %d\n",type.degree,depth);
            return;
        }

        // this just gets the first node size
        if(type.dimensions[depth] == DEDUCE_SIZE)
        {
            type.dimensions[depth] = node_len;
        }

        else if(type.dimensions[depth] == RUNTIME_SIZE)
        {
            unimplemented("VLA assign");
        }

        const u32 count = type.dimensions[depth];

        // type check the actual ammount is right
        // TODO: this should allow not specifing the full ammount but for now just keep it simple
        if(count != node_len)
        {
            panic(itl,"array expects %d initializers got %d\n",count,node_len);
        }        

        // descend each sub initializer until we hit one containing values
        // for now we are just gonna print them out, and then we will figure out how to emit the inialzation code
        if(node->nodes[i]->type == ast_type::initializer_list)
        {
            const auto base_type = contained_arr_type(type);

            // TODO: we need a check to handle arrays of structs in here!
            if(depth == type.degree - 1 && is_struct(base_type))
            {
                const u32 size = type_size(itl,base_type);

                const auto structure = struct_from_type(itl.struct_table,base_type);

                traverse_struct_initializer(itl,func,node->nodes[i],addr_slot,structure,*offset);

                *offset = *offset + size;
            }

            else
            {
                traverse_arr_initializer(itl,func,node->nodes[i],addr_slot,type,depth + 1,offset);
            }
        }

        else
        {
            auto [first_type,first_reg] = compile_oper(itl,func,node->nodes[i],new_tmp(func));

            
            // we have values!!
            if(!is_array(first_type))
            {
                // make sure this only happens at the max "depth" 
                if(depth != type.degree - 1)
                {
                    panic(itl,"array should not be initialized with values at depth %d expected %d\n",depth,type.degree - 1);
                    return;
                }

                const auto base_type = contained_arr_type(type);
                const u32 size = type_size(itl,base_type);

                check_assign(itl,base_type,first_type,false,true);

                do_ptr_store(itl,func,first_reg,addr_slot,base_type,*offset);
                *offset = *offset + size;

                for(i = 1; i < node_len; i++)
                {
                    auto [rtype,reg] = compile_oper(itl,func,node->nodes[i],new_tmp(func));
                    check_assign(itl,base_type,rtype,false,true);

                    do_ptr_store(itl,func,reg,addr_slot,base_type,*offset);
                    *offset = *offset + size;
                }
            }
            

            // handle an array (this should fufill the current "depth req in its entirety")
            else
            {
                unimplemented("arr initializer with array");
            }
            
        }   
    }
}

void compile_arr_decl(Interloper& itl, Function& func, const AstNode &line, const Symbol& sym)
{
    // this reference will get invalidated under the current setup
    // so pull the slot
    const u32 slot = sym.slot;

    // This allocation needs to happen before we initialize the array but we dont have all the information yet
    // so we need to finish it up later
    emit(func.emitter,op_type::alloc_slot,slot,0,0);
    ListNode* alloc = get_cur_end(func.emitter);


    // has an initalizer
    // TODO: need to handle dumping this as a constant if the array is constant
    // rather than runtime setup
    if(line.nodes.size() == 2)
    {
        const u32 addr_slot = new_tmp(itl,GPR_SIZE);

        // we have added a new sym this ref has moved
        Symbol &array = sym_from_slot(itl.symbol_table.slot_lookup,slot);


        emit(func.emitter,op_type::addrof,addr_slot,slot);


        u32 idx = 0;
        traverse_arr_initializer(itl,func,line.nodes[1],addr_slot,array.type,0,&idx);
    }

    if(itl.error)
    {
        return;
    }


    const Symbol &array = sym_from_slot(itl.symbol_table.slot_lookup,slot);

    auto [size,count] = arr_size(itl,array.type);

    if(count == RUNTIME_SIZE)
    {
        unimplemented("DECL VLA");
    }

    // this has not been inited by traverse_arr_initializer
    else if(count == DEDUCE_SIZE)
    {
        panic(itl,"auto sized array %s does not have an initializer\n",array.name.c_str());
        return;
    }

    else
    {
        if(size > GPR_SIZE)
        {
            count = gpr_count(size * count);

            // we have the allocation information now complete it
            alloc->opcode = Opcode(op_type::alloc_slot,array.slot,GPR_SIZE,count);
        }

        else
        {
            // we have the allocation information now complete it
            alloc->opcode = Opcode(op_type::alloc_slot,array.slot,size,count);
        }
    }
}


void traverse_struct_initializer(Interloper& itl, Function& func, AstNode* node, const u32 addr_slot, const Struct& structure, u32 offset)
{
    const u32 node_len = node->nodes.size();
    const u32 member_size = structure.members.size();

    if(node_len != member_size)
    {
        panic(itl,"arr initlizier missing initlizer expected %d got %d\n",member_size,node_len);
        return;
    }
    
    for(u32 i = 0; i < structure.members.size(); i++)
    {
        const auto member = structure.members[i];
    
        // either sub struct OR array member initializer
        if(node->nodes[i]->type == ast_type::initializer_list)
        {
            if(is_array(member.type))
            {
               u32 arr_offset = offset + member.offset;
               auto type = member.type;

               traverse_arr_initializer(itl,func,node->nodes[i],addr_slot,type,0, &arr_offset);
            }

            else if(is_struct(member.type))
            {
                const Struct& sub_struct = struct_from_type(itl.struct_table,member.type);
                traverse_struct_initializer(itl,func,node->nodes[i],addr_slot,sub_struct,offset + member.offset);
            }

            else
            {
                panic(itl,"nested struct initalizer for basic type %s : %s\n",member.name.c_str(),type_name(itl,member.type).c_str());
                return;
            }
        }

        // we have a list of plain values we can actually initialize
        else
        {
            // get the operand and type check it
            const auto [rtype,slot] = compile_oper(itl,func,node->nodes[i],new_tmp(func));
            check_assign(itl,member.type,rtype);

            do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset + offset);
        }
    } 
}


void compile_struct_decl(Interloper& itl, Function& func, const AstNode &line, Symbol& sym)
{
    const auto structure = struct_from_type(itl.struct_table,sym.type);

    const u32 count = gpr_count(structure.size);
    emit(func.emitter,op_type::alloc_slot,sym.slot,GPR_SIZE,count);

    if(line.nodes.size() == 2)
    {
        if(line.nodes[1]->type == ast_type::initializer_list)
        {
            // insertion will break our reference
            const u32 sym_slot = sym.slot;

            const u32 addr_slot = new_tmp(itl,GPR_SIZE);

            emit(func.emitter,op_type::addrof,addr_slot,sym_slot);
            traverse_struct_initializer(itl,func,line.nodes[1],addr_slot,structure);
        }

        else
        {
            const auto [rtype,slot] = compile_oper(itl,func,line.nodes[1],sym.slot);

            // oper is a single symbol and the move hasn't happened we need to explictly move it
            if(sym.slot != slot)
            {
                compile_move(itl,func,sym.slot,slot,sym.type,rtype);
            }

            check_assign(itl,sym.type,rtype,false,true);        
        }
    }

    else
    {
        // insertion will break our reference
        const u32 sym_slot = sym.slot;

        const u32 addr_slot = new_tmp(itl,GPR_SIZE);
        emit(func.emitter,op_type::addrof,addr_slot,sym_slot);

        for(const auto &member : structure.members)
        {
            if(member.expr)
            {
                if(member.expr->type == ast_type::initializer_list)
                {
                    unimplemented("initializer list");
                }

                else
                {
                    const auto [rtype,slot] = compile_oper(itl,func,member.expr,new_tmp(func));
                    check_assign(itl,member.type,rtype,false,true); 

                    do_ptr_store(itl,func,slot,addr_slot,member.type,member.offset);
                }
            }

            // TODO: handle nested struct membmer
            // (basically we need to just recurse this method)
            else if(is_struct(member.type))
            {

            }

            // TODO: handle arrays
            else if(is_array(member.type))
            {

            }

            else
            {
                const u32 tmp = new_tmp(func);
                emit(func.emitter,op_type::mov_imm,tmp,default_value(member.type));

                do_ptr_store(itl,func,tmp,addr_slot,member.type,member.offset);
            }
        }
    }

    if(itl.error)
    {
        return;
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
        panic(itl,"redeclared symbol: %s:%s\n",name.c_str(),type_name(itl,sym_opt.value().type).c_str());
        return;
    }

    const auto size = type_size(itl,ltype);

    // add new symbol table entry
    Symbol &sym = add_symbol(itl.symbol_table,name,ltype,size);




    if(is_array(sym.type))
    {
        compile_arr_decl(itl,func,line,sym);
    }

    else if(is_struct(sym.type))
    {
        compile_struct_decl(itl,func,line,sym);
    }

    
    // simple type
    else 
    {
        emit(func.emitter,op_type::alloc_slot,sym.slot);

        // initalizer
        if(line.nodes.size() == 2)
        {
            // normal assign
            const auto [rtype,reg] = compile_oper(itl,func,line.nodes[1],sym.slot);

            // oper is a single symbol and the move hasn't happened we need to explictly move it
            if(sym.slot != reg)
            {
                compile_move(itl,func,sym.slot,reg,sym.type,rtype);
            }

            check_assign(itl,ltype,rtype,false,true);         

            
        }

        // default init
        else
        {
            // NOTE: atm, all standard types use 0 for default, we may need something more flexible
            emit(func.emitter,op_type::mov_imm,sym.slot,default_value(sym.type));
        }
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

    
    const auto [type,reg] = compile_oper(itl,func,line.nodes[0],new_tmp(func));

    // add the symbol

    const auto size = type_size(itl,type);

    // add new symbol table entry
    const auto &sym = add_symbol(itl.symbol_table,name,type,size);

    emit(func.emitter,op_type::alloc_slot,sym.slot);
    compile_move(itl,func,sym.slot,reg,sym.type,type);
}

std::pair<Type,u32> access_array_member(Interloper& itl, Function& func, u32 slot, const Type& type, const std::string& member_name,u32* offset)
{
    const bool is_ptr = is_pointer(type);

    if(member_name == "len")
    {
        if(!is_runtime_size(type,0))
        {
            // TODO: have the optimiser clean up dead code
            emit(func.emitter,op_type::free_reg,slot);
            return std::pair<Type,u32>{type,ACCESS_FIXED_LEN_REG};
        }

        // vla
        else
        {
            if(!is_ptr)
            {
                *offset += GPR_SIZE;
                return std::pair<Type,u32>{Type(builtin_type::u32_t),slot};
            }

            else
            {
                unimplemented("vla len by ptr");
            }
        }
    }

    else if(member_name == "data")
    {
        if(!is_ptr)
        {
            // this should probably be better typed
            return std::pair<Type,u32>{Type(GPR_SIZE_TYPE),slot};
        }

        else
        {
            unimplemented("data by ptr");
        }
    }


    else
    {
        panic(itl,"unknown array member %s\n",member_name.c_str());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};
    }
}

// returns the member + offset
std::pair<Type,u32> access_struct_member(Interloper& itl, Function& func, u32 slot, Type type, const std::string& member_name, u32* offset)
{
    // auto deref pointer
    if(type.ptr_indirection == 1)
    {
        const u32 ptr_slot = slot;

        slot = new_tmp(func);

        do_ptr_load(itl,func,slot,ptr_slot,type,*offset);
        *offset = 0;

        // now we are back to a straight pointer
        type.ptr_indirection -= 1;
    }

    // get offset for struct member
    const auto member_opt = get_member(itl.struct_table,type,member_name);

    if(!member_opt)
    {
        panic(itl,"No such member %s for type %s\n",member_name.c_str(),type_name(itl,type).c_str());
        return std::pair<Type,u32>{Type(builtin_type::void_t),0};
    }

    const auto member = member_opt.value();

    *offset += member.offset;

    return std::pair<Type,u32>{member.type,slot};    
}

// get back a complete pointer
u32 collapse_offset(Function&func, u32 addr_slot, u32 *offset)
{
    if(*offset)
    {
        const u32 final_addr = new_tmp(func);
        emit(func.emitter,op_type::add_imm,final_addr,addr_slot,*offset);

        *offset = 0;

        return final_addr;
    }
    
    else
    {
        return addr_slot;
    }
}

// return type, slot, offset
std::tuple<Type,u32,u32> compute_member_addr(Interloper& itl, Function& func, AstNode* node)
{
    AstNode* expr_node = node->nodes[0];

    // Type is allways the accessed type of the current pointer
    u32 struct_slot = -1;
    Type struct_type;

    // parse out initail expr
    switch(expr_node->type)
    {
        case ast_type::symbol:
        {
            const auto name = expr_node->literal;
            const auto sym_opt = get_sym(itl.symbol_table,name);

            if(!sym_opt)
            {
                panic(itl,"symbol %s used before declaration\n",name.c_str());
                return std::tuple<Type,u32,u32>{Type(builtin_type::void_t),0,0};
            }            

            const auto sym = sym_opt.value();

            // allready a pointer so just return the slot
            // along with the derefed type
            if(is_pointer(sym.type))
            {
                struct_type = sym.type;
                struct_type.ptr_indirection -= 1;

                struct_slot = sym.slot;
            }

            else
            {
                struct_slot = new_tmp(func);
                emit(func.emitter,op_type::addrof,struct_slot,sym.slot);

                struct_type = sym.type;
            }

            break;        
        }

        case ast_type::array_access:
        {
            std::tie(struct_type, struct_slot) = index_arr(itl,func,expr_node,new_tmp(func));

            // we return types in here as the accessed type
            struct_type.ptr_indirection -= 1;

            break;
        }


        default: 
        {
            panic(itl,"Unknown struct access %s\n",AST_NAMES[u32(expr_node->type)]);
            return std::tuple<Type,u32,u32>{Type(builtin_type::void_t),0,0};
        }
    }


    AstNode* members = node->nodes[1];

    u32 member_offset = 0;

    // perform each member access
    for(AstNode* n : members->nodes)
    {
        switch(n->type)
        {
            case ast_type::access_member:
            {

                const auto member_name = n->literal;

                if(is_pointer(struct_type))
                {
                    // pointer to array
                    if(struct_type.degree)
                    {
                       std::tie(struct_type,struct_slot) = access_array_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                    }

                    else
                    {
                        std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                    }
                    
                }

                else if(is_array(struct_type))
                {
                    std::tie(struct_type,struct_slot) = access_array_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                }

                // actual struct member
                else
                {
                    std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,member_name,&member_offset);
                }   
                break;
            }

            case ast_type::array_access:
            {
                
                std::tie(struct_type,struct_slot) = access_struct_member(itl,func,struct_slot,struct_type,n->literal,&member_offset);
                
                struct_slot = collapse_offset(func,struct_slot,&member_offset);

                std::tie(struct_type,struct_slot) = index_arr_internal(itl,func,n,n->literal,struct_type,struct_slot,new_tmp(func));

                // deref of pointer
                struct_type.ptr_indirection -= 1;

                break;
            }

            default: 
            {
                panic(itl,"Unknown member access %s\n",AST_NAMES[u32(n->type)]);
                return std::tuple<Type,u32,u32>{Type(builtin_type::void_t),0,0};
            }
        }
    }

    return std::tuple<Type,u32,u32>{struct_type,struct_slot,member_offset};
}


void write_struct(Interloper& itl,Function& func, u32 src_slot, const Type& rtype, AstNode *node)
{
    const auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);
    check_assign(itl,accessed_type,rtype);
    do_ptr_store(itl,func,src_slot,ptr_slot,accessed_type, offset);
}

std::pair<Type,u32> read_struct(Interloper& itl,Function& func, u32 dst_slot, AstNode *node)
{
    const auto [accessed_type, ptr_slot, offset] = compute_member_addr(itl,func,node);

    // len access on fixed sized array
    if(ptr_slot == ACCESS_FIXED_LEN_REG)
    {
        emit(func.emitter,op_type::mov_imm,dst_slot,accessed_type.dimensions[0]);
        return std::pair<Type,u32>{Type(builtin_type::u32_t),dst_slot};
    }

    do_ptr_load(itl,func,dst_slot,ptr_slot,accessed_type,offset);
    return std::pair<Type,u32>{accessed_type,dst_slot};
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
                const auto [rtype,slot] = compile_oper(itl,func,line.nodes[1],new_tmp(func));


                if(line.nodes[0]->type != ast_type::symbol)
                {
                    switch(line.nodes[0]->type)
                    {
                        case ast_type::deref:
                        {
                            const auto [type,addr_slot] = load_addr(itl,func,line.nodes[0]->nodes[0],new_tmp(func),false);
                            check_assign(itl,type,rtype);
                            do_ptr_store(itl,func,slot,addr_slot,type);
                            break;                        
                        }

                        case ast_type::array_access:
                        {
                            write_arr(itl,func,line.nodes[0],rtype,slot);
                            break;
                        }

                        // write on struct member!
                        case ast_type::access_struct:
                        {
                            write_struct(itl,func,slot,rtype,line.nodes[0]);
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
                        break;
                    }

                    const auto &sym = sym_opt.value();

                    check_assign(itl,sym.type,rtype);

                    compile_move(itl,func,sym.slot,slot,sym.type,rtype);
                }
                break;
            }


            case ast_type::ret:
            {
                // returns a value
                if(line.nodes.size() == 1)
                {
                    const auto [rtype,v1] = compile_oper(itl,func,line.nodes[0],RV_IR);
    
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
                compile_function_call(itl,func,l,NO_SLOT);
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
            }
        }
    }


    // scope is about to be destroyed reclaim the stack for every var that is no longer used
    for(const auto &[key, slot] : itl.symbol_table.table[itl.symbol_table.table.size()-1])
    { 
        const auto &sym = itl.symbol_table.slot_lookup[slot];

        // free the stack alloc for each var thats about to go out of scope
        if(sym.arg_offset == NON_ARG)
        {
            if(is_array(sym.type))
            {
                auto [size,count] = arr_size(itl,sym.type);



                if(size != RUNTIME_SIZE)
                {
                    if(size > GPR_SIZE)
                    {
                        count = gpr_count(size * size);
                        emit(func.emitter,op_type::free_slot,sym.slot,GPR_SIZE,count);
                    }

                    else
                    {
                        emit(func.emitter,op_type::free_slot,sym.slot,size,count);
                    }
                }
            }

            else if(is_struct(sym.type))
            {
                const auto structure = struct_from_type(itl.struct_table,sym.type);

                const u32 count = gpr_count(structure.size);
                emit(func.emitter,op_type::free_slot,sym.slot,GPR_SIZE,count);                
            }

            else
            {
                emit(func.emitter,op_type::free_slot,sym.slot);
            } 
        }
    }

    destroy_scope(itl.symbol_table);
}

void compile_functions(Interloper &itl)
{
    // global scope
    new_scope(itl.symbol_table);

    for(const auto n: itl.func_root->nodes)
    {
        const auto &node = *n;

        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        new_scope(itl.symbol_table);

        auto &func = itl.function_table[node.literal];

        func.emitter.reg_count = 0;

        // put each arg into scope
        for(auto &a : func.args)
        {
            auto &sym = sym_from_slot(itl.symbol_table.slot_lookup,a);
            add_scope(itl.symbol_table,sym);
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
            if(func.return_type.type_idx == u32(builtin_type::void_t))
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



// general refactor
// -> remove duplicate code

// -> source line information on parse tree (impl assert)
// NOTE: we should mark the top level decl with what file its from

// -> move ast to arena allocation (not urgent)
// -> impl own Array, String, and HashMap structs (not urgent)
// -> move tokenizer over to batching (not urgent)
// -> improve const expressions
// -> add global constants (and eventually globals but just leave this for now because we dont wanna handle allocating them)
// -> update comments
// -> impl a a smarter register allocator rather than just blindly spilling things
// -> handle block args inside the reg allocator


// TODO: basic type checking for returning pointers to local's

// feature plan:
// structs -> default_values -> tuples ->  
// -> make imports not include uneeded funcs -> global const's 
// -> switch -> enum -> function_pointers
// -> early stl  -> labels ->  compile time execution ->
// unions -> debugg memory guards -> ...

void destory_ast(Interloper& itl)
{
    // delete function def
    delete_tree(itl.func_root); 
    itl.func_root = nullptr;

    // delete all the struct defintions
    for(auto &[key,def] : itl.struct_def)
    {
        UNUSED(key);

        delete_tree(def.root);
        def.root = nullptr;
    }

    itl.struct_def.clear();

    
    itl.cur_line = nullptr;    
}

void destroy_itl(Interloper &itl)
{
    destroy(itl.program);
    destroy(itl.const_pool);
    destory(itl.struct_table);
    clear(itl.symbol_table);
    itl.function_table.clear();

    destory_ast(itl);

    destory_allocator(itl.list_allocator);
}

static constexpr u32 LIST_INITIAL_SIZE = 10 * 1024;

void compile(Interloper &itl,const std::string& initial_filename)
{
    printf("compiling file: %s\n",initial_filename.c_str());

    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.error = false;


    // parse intial input file
    {
        itl.func_root = ast_plain(ast_type::root);

        // build ast
        const b32 parser_error = parse(itl,initial_filename);

    
        if(parser_error)
        {
            itl.error = true;
            destroy_itl(itl);
            return;
        }
    }

    if(itl.print_ast)
    {
        // print all struct defs
        for(auto &[key,def] : itl.struct_def)
        {
            print(def.root);
        }
       
        // print function defs
        print(itl.func_root);
    }

    // parse out any of the top level decl we need
    parse_struct_declarations(itl);
    
    parse_function_declarations(itl);

    if(itl.error)
    {
        destroy_itl(itl);
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
    destory_ast(itl);

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

        if(itl.print_stack_allocation || itl.print_reg_allocation)
        {
            putchar('\n');
        }
    }

    // emit the actual target asm
    // for now we will just collect the emitter IR
    // and resolve labels
    emit_asm(itl);
}

#include <interloper.h>

#include "lexer.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "optimize.cpp"
#include "ir.cpp"
#include "struct.cpp"


Type* compile_expression(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
void compile_decl(Interloper &itl,Function &func, const AstNode *line);
void compile_block(Interloper &itl,Function &func,BlockNode *node);
u32 compile_basic_block(Interloper &itl,Function &func,BlockNode *node, block_type type);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);
std::pair<Type*,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot);


void dump_ir_sym(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.used_func); f++)
    {
        Function& func = *lookup(itl.function_table,itl.used_func[f]);
        dump_ir(func,itl.symbol_table);
    }
}

// TODO: we want to overhaul this with a more general mechanism for getting values
// by running code at compile time, but just use this for now
u32 eval_int_expr(AstNode *node)
{
    assert(node);

    switch(node->type)
    {
        case ast_type::value: 
        {
            ValueNode* value_node = (ValueNode*)node;
            return value_node->value.v;
        }

        case ast_type::times: 
        {
            BinNode* bin_node = (BinNode*)node;

            return eval_int_expr(bin_node->left) * eval_int_expr(bin_node->right);
        }

        case ast_type::plus:
        {
            BinNode* bin_node = (BinNode*)node;

            return eval_int_expr(bin_node->left) + eval_int_expr(bin_node->right);
        }

        case ast_type::minus: 
        {
            BinNode* bin_node = (BinNode*)node;

            return eval_int_expr(bin_node->left) - eval_int_expr(bin_node->right);
        }

        case ast_type::divide:
        {
            BinNode* bin_node = (BinNode*)node;

            const u32 v1 = eval_int_expr(bin_node->left);
            const u32 v2 = eval_int_expr(bin_node->right);

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
    printf("func: %s\n",func.name.buf);

    for(u32 a = 0; a < count(func.args); a++)
    {
        const u32 slot = func.args[a];
        auto &sym = sym_from_slot(itl.symbol_table,slot);

        print(itl,sym); 
    }

    for(u32 r = 0; r < count(func.return_type); r++)
    {
        printf("return type %s\n",type_name(itl,func.return_type[r]).buf);
    }
}


void finalise_def(Function& func, Array<Type*> rt, Array<u32> a, u32 hidden_args, u32 s)
{
    func.return_type = rt;
    func.args = a;
    func.slot = s;
    func.hidden_args = hidden_args;
}


// scan the top level of the parse tree for functions
// and grab the entire signature
// we wont worry about the scope on functions for now as we wont have namespaces for a while
void parse_function_declarations(Interloper& itl)
{
    

    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto& bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            const FuncNode& node = *func.root;

            itl.cur_file = node.filename;

            // parse out out the return type
            Array<u32> args;
            u32 arg_offset = 0;
            u32 hidden_args = 0;

            Array<Type*> return_type;


            // NOTE: void return's will have a void type
            if(count(node.return_type) == 1)
            {
                push_var(return_type,get_type(itl,node.return_type[0]));

                // we are returning a struct add a hidden pointer as first arg
                if(type_size(itl,return_type[0]) > GPR_SIZE)
                {
                    assert(false);
                }   
            }

            // tuple return
            else if(count(node.return_type) > 1)
            {
                assert(false);
            }

            // handle args
            const auto name = node.name;
            const auto decl = node.args;

            // rip every arg
            for(u32 i = 0; i < count(decl); i++)
            {
                const auto a = decl[i];

                const auto name = a->name;
                const auto type = get_type(itl,a->type);

                const auto size = type_size(itl,type);

                // add the var to slot lookup and link to function
                // we will do a add_scope to put it into the scope later
                Symbol sym = make_sym(itl.symbol_table,name,type,size,arg_offset);
                add_var(itl.symbol_table,sym);

                push_var(args,sym.slot);

                // if size is below GPR just make it take that much
                const u32 arg_size = sym.size < GPR_SIZE? 4 : size;

                arg_offset += arg_size;

                //printf("arg slot %s: %d : %d\n",sym.name.buf,sym.slot, args[args.size()-1]);
            }


            finalise_def(func,return_type,args,hidden_args,count(itl.symbol_table.label_lookup));


            // add as a label as it this will be need to referenced by call instrs
            // in the ir to get the name back
            add_label(itl.symbol_table,name);
        }
    }
}



std::pair<Type*,u32> symbol(Interloper &itl, AstNode *node)
{
    LiteralNode* lit_node = (LiteralNode*)node;

    const auto name = lit_node->literal;

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(!sym_opt)
    {
        panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
        return std::pair<Type*,u32>{make_builtin(itl,builtin_type::void_t),0};
    }

    const auto &sym = sym_opt.value();

    return std::pair<Type*,u32>{sym.type,sym.slot};
}


Type* value(Interloper& itl,Function& func,AstNode *node, u32 dst_slot)
{
    ValueNode* value_node = (ValueNode*)node;
    Value value = value_node->value;

    if(value.sign)
    {
        const s32 v = s32(value.v);
        emit(func,op_type::mov_imm,dst_slot,v);

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,s32(builtin_min(builtin_type::s8_t)),s32(builtin_max(builtin_type::s8_t))))
        {
            return  make_builtin(itl,builtin_type::s8_t);
        }

        else if(in_range(v,s32(builtin_min(builtin_type::s16_t)),s32(builtin_max(builtin_type::s16_t))))
        {
            return make_builtin(itl,builtin_type::s16_t);
        }

        //else if(v,s32(builtin_min(builtin_type::s32_t)),s32(builtin_max(builtin_type::s32_t)))
        else
        {
            return make_builtin(itl,builtin_type::s32_t);
        }
    }

    else
    {
        const u32 v = value.v;
        emit(func,op_type::mov_imm,dst_slot,v);

        // what is the smallest storage type that this will fit inside?
        if(in_range(v,builtin_min(builtin_type::u8_t),builtin_max(builtin_type::u8_t)))
        {
            return  make_builtin(itl,builtin_type::u8_t);
        }

        else if(in_range(v,builtin_min(builtin_type::u16_t),builtin_max(builtin_type::u16_t)))
        {
            return make_builtin(itl,builtin_type::u16_t);
        }

        //else if(in_range(v,builtin_min(builtin_type::u32_t),builtin_max(builtin_type::u32_t))
        else
        {
            return make_builtin(itl,builtin_type::u32_t);
        }
    }    
}


// get back a complete pointer
u32 collapse_offset(Function&func, u32 addr_slot, u32 *offset)
{
    if(*offset)
    {
        const u32 final_addr = emit_res(func,op_type::add_imm,addr_slot,*offset);
        *offset = 0;

        return final_addr;
    }
    
    else
    {
        return addr_slot;
    }
}

void do_ptr_load(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type* type, u32 offset = 0)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func,load_ptr(dst_slot,addr_slot,offset,size,is_signed(type)));
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


void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type* type, u32 offset = 0)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func,store_ptr(dst_slot,addr_slot,size,offset));  
    }

    else
    {
        u32 src_ptr = emit_res(func,op_type::addrof,dst_slot);
        src_ptr = collapse_offset(func,src_ptr,&offset);        

        ir_memcpy(itl,func,addr_slot,src_ptr,type_size(itl,type));        
    } 
}


// this should handle grabbing values and symbols
// if it can see a symbol or a value it wont call compile_expression (it is up to caller)
// to decide what to actually do with operands
std::pair<Type*,u32> compile_oper(Interloper& itl,Function &func,AstNode *node, u32 dst_slot)
{
    panic(node == nullptr,"nullptr in compile_oper");

    // for error printing
    itl.cur_expr = node;

    // test what the current node is
    switch(node->type)
    {
        case ast_type::value:
        {
            const auto t1 = value(itl,func,node,dst_slot);
            return std::pair<Type*,u32>{t1,dst_slot};
        }

        case ast_type::symbol:
        {
            return symbol(itl,node);
        }


        case ast_type::char_t:
        {
            CharNode* char_node = (CharNode*)node;

            emit(func,op_type::mov_imm,dst_slot,char_node->character);
            return std::pair<Type*,u32>{make_builtin(itl,builtin_type::u8_t),dst_slot};
        }


        // compile an expr
        default:
        {
            const auto t1 = compile_expression(itl,func,node,dst_slot);
            return std::pair<Type*,u32>{t1,dst_slot};
        }
    }
}



Type* compile_arith_op(Interloper& itl,Function &func,AstNode *node, op_type type, u32 dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left,new_tmp(func));
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right,new_tmp(func));

    // pointer arith adds the size of the underlying type
    if(is_pointer(t1) && is_integer(t2))
    {
        assert(false);
    }

    // normal arith
    else
    {
        emit(func,type,dst_slot,v1,v2);
    }


    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2);

    

    return final_type;        
}


Type* compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, u32 dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left,new_tmp(func));
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right,new_tmp(func));

    if(!(is_integer(t1) && is_integer(t2)))
    {
        panic(itl,"shifts only defined for integers, got %s and %s\n",type_name(itl,t1).buf,type_name(itl,t2).buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    if(right)
    {
        // if signed do a arithmetic shift 
        if(is_signed(t1))
        {
            emit(func,op_type::asr_reg,dst_slot,v1,v2);
        }

        else
        {
            emit(func,op_type::lsr_reg,dst_slot,v1,v2);
        }
    }

    // left shift
    else
    {
        emit(func,op_type::lsl_reg,dst_slot,v1,v2);
    }

    // type being shifted is the resulting type
    return t1;
}


b32 check_static_cmp(Interloper& itl, const Type* value, const Type* oper, u32 v)
{
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
            return true;
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

    return false;
}

// handles <, <=, >, >=, &&, ||, ==, !=
Type* compile_logical_op(Interloper& itl,Function &func,AstNode *node, logic_op type, u32 dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    auto [type_left,v1] = compile_oper(itl,func,bin_node->left,new_tmp(func));
    auto [type_right,v2] = compile_oper(itl,func,bin_node->right,new_tmp(func));



    // if one side is a value do type checking
    if(is_integer(type_left) && is_integer(type_right))
    {
        if(bin_node->left->type == ast_type::value || bin_node->right->type == ast_type::value)
        {
            if(bin_node->left->type == ast_type::value)
            {
                ValueNode* value_node = (ValueNode*)bin_node->left;
                const u32 v = value_node->value.v;

                const b32 coerce = check_static_cmp(itl,type_left,type_right,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    type_left = type_right;
                }
            }

            // right is a constant
            else
            {
                ValueNode* value_node = (ValueNode*)bin_node->right;
                const u32 v = value_node->value.v;

                
                const b32 coerce = check_static_cmp(itl,type_right,type_left,v);

                // within range coerce value type to variable type
                if(coerce)
                {
                    type_right = type_left;
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
            if(!is_bool(type_left) && is_bool(type_right))
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
            check_logical_operation(itl,type_left,type_right,type);
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
        const b32 sign = is_signed(type_left);

        // if we have gotten this far the sign of both are the same
        const op_type op = LOGIC_OPCODE[sign][u32(type)];


        // one of these is just a temp for the result calc
        // so afer this we no longer need it
        emit(func,op,dst_slot,v1,v2);

        return make_builtin(itl,builtin_type::bool_t);
    }

    // operation is not valid for given types..
    else
    {
        return make_builtin(itl,builtin_type::void_t);
    }
}




//  we dont want the 2nd stage IR handling how things need to be copied
// as it does not have the information required easily accessible
void compile_move(Interloper &itl, Function &func, u32 dst_slot, u32 src_slot, const Type* dst_type, const Type* src_type)
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
        emit(func,op_type::mov_reg,dst_slot,src_slot);
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
            const u32 ptr = emit_res(func,op_type::addrof,src_slot);

            ir_memcpy(itl,func,func.args[0],ptr,type_size(itl,dst_type));
        } 

        else
        {
            const u32 src_ptr = emit_res(func,op_type::addrof,src_slot);
            const u32 dst_ptr = emit_res(func,op_type::addrof,dst_slot);

            ir_memcpy(itl,func,dst_ptr,src_ptr,type_size(itl,dst_type));
        }
    }
}

void mark_used(Interloper& itl, Function& func)
{
    if(!func.used)
    {
        push_var(itl.used_func,func.name);
        func.used = true;
    }
}

#include "intrin.cpp"

// used for both tuples and ordinary function calls
Type* compile_function_call(Interloper &itl,Function &func,AstNode *node, u32 dst_slot)
{
    TupleAssignNode* tuple_node = nullptr;

    // is this used for tuple binding?
    if(node->type == ast_type::tuple_assign)
    {
        tuple_node = (TupleAssignNode*)node;
    }

    // NOTE: if this is a multiple return the function call is a child node
    FuncCallNode* call_node = tuple_node? tuple_node->func_call : (FuncCallNode*)node;

    s32 idx = lookup_internal_hashtable(INTRIN_TABLE,INTRIN_TABLE_SIZE,call_node->name);

    if(idx != INVALID_SLOT)
    {
        const auto handler = INTRIN_TABLE[idx].v;
        return handler(itl,func,node,dst_slot);
    }

    Function* func_call_ptr = lookup(itl.function_table,call_node->name);

    // check function is declared
    if(!func_call_ptr)
    {
        panic(itl,"[COMPILE]: function %s is not declared\n",call_node->name.buf);
        return make_builtin(itl,builtin_type::void_t);
    }

    auto &func_call = *func_call_ptr;
    mark_used(itl,func_call);




    //print_func_decl(itl,func_call);

    const s32 hidden_args = func_call.hidden_args;

    // check we have the right number of params
    if((count(func_call.args) - hidden_args) != count(call_node->args))
    {
        panic(itl,"[COMPILE]: function call expected %d args got %d\n",count(func_call.args),count(call_node->args));
        return make_builtin(itl,builtin_type::void_t);
    }


    // check calls on functions with multiple returns are valid
    if(tuple_node && count(func_call.return_type) == 1)
    {
        panic(itl,"attempted to bind %d return values on function with single return\n",count(tuple_node->symbols));
        return make_builtin(itl,builtin_type::void_t);
    }

    if(count(func_call.return_type) > 1)
    {
        if(!tuple_node)
        {
            panic(itl,"Attempted to call multiple return function nested in a expression\n");
            return make_builtin(itl,builtin_type::void_t);
        }

        if(count(func_call.return_type) != count(tuple_node->symbols))
        {
            panic(itl,"Numbers of smybols binded for multiple return does not match function: %d != %d\n",count(tuple_node->symbols),count(call_node->args));
            return make_builtin(itl,builtin_type::void_t);
        }
    }



    // for now we are just going with callee saved
    // if we eventually mix caller and callee saved so we can have the called func overwrite values
    // we dont care about then we need to re add the save and restore directives
    // and we will need a push and pop bitset to implement them
    // with care to rewrite where the return register ends up when we restore
    // save all the used regs
    //emit(func,op_type::save_regs);
    
    // how many args are we pushing to the stack?
    u32 arg_clean = 0;



    // push args in reverse order and type check them
    for(s32 i = count(func_call.args) - 1; i >= hidden_args; i--)
    {
        const auto &arg =  sym_from_slot(itl.symbol_table,func_call.args[i]);
  
        const u32 arg_idx = i - hidden_args;

        if(is_array(arg.type))
        {
            assert(false);
        }


        else if(is_struct(arg.type))
        {
            assert(false);
        }

        // plain builtin in variable
        else
        {
            // builtin type
            const auto [arg_type,reg] = compile_oper(itl,func,call_node->args[arg_idx],new_tmp(func));


            // type check the arg
            check_assign(itl,arg.type,arg_type,true);

            // finally push the arg
            emit(func,op_type::push_arg,reg);

            arg_clean++;
        }
    }

    // push hidden args 

    if(hidden_args)
    {
        assert(false);
    }



    // NOTE: func struct will hold a void value if it has nothing
    const bool returns_value = func_call.return_type[0]->type_idx != u32(builtin_type::void_t);


    // if we have a register in R0 we need to save it so its not overwritten
    if(returns_value && !hidden_args)
    {
        emit(func,op_type::spill_rv);
    }


    // emit call to label slot
    // the actual address will have to resolved as the last compile step
    // once we know the size of all the code
    emit(func,op_type::call,func_call.slot);


    // clean up args after the function call
    // TODO: how should we encode this when we do arg passing in regs
    if(arg_clean)
    {
        emit(func,op_type::clean_args,arg_clean);
    }
  

    // restore callee saved values
    //emit(func,op_type::restore_regs);


    // normal return
    // store the return value back into a reg (if its actually binded)
    if(returns_value && dst_slot != NO_SLOT && !hidden_args)
    {
        // TODO: is this dst type correct?
        compile_move(itl,func,dst_slot,RV_IR,func.return_type[0],func.return_type[0]);
    }
    

    // give back the function's return type

    if(count(func_call.return_type) == 1)
    {
        // result of expr is the return type
        return func_call.return_type[0];
    }

    // tuple 
    else
    {
        return make_raw(itl,TUPLE);
    }    
}



String label_name(SymbolTable& table,u32 slot)
{
    char name[40];
    const u32 len = sprintf(name,"L%d",slot);

    return make_string(*table.string_allocator,name,len);
}

u32 new_basic_block(Interloper &itl,Function &func, block_type type)
{
    const u32 slot = count(itl.symbol_table.label_lookup);

    const u32 basic_block = count(func.emitter.program);

    new_block(&itl.list_allocator,func,type,slot);
    add_label(itl.symbol_table,label_name(itl.symbol_table,slot));

    // offset is the block offset until full resolution
    itl.symbol_table.label_lookup[slot].offset = basic_block;

    return slot;   
}


void compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    const u32 start_block = cur_block(func);

    auto &blocks = func.emitter.program;

    block_type old = blocks[count(blocks) - 1].type;

    BlockNode* if_block = (BlockNode*)node;

    for(u32 n = 0; n < count(if_block->statements); n++)
    {
        const auto type = if_block->statements[n]->type;

        if(type != ast_type::else_t)
        {
            BinNode* if_stmt = (BinNode*)if_block->statements[n];


            if(n != 0)
            {
                // create new block for compare
                // TODO: should this have hte type of the initial node or no?
                new_basic_block(itl,func,block_type::chain_cmp_t);
            }

            // compile the compare expr for conditon
            const auto [t,r] = compile_oper(itl,func,if_stmt->left,new_tmp(func));

            if(!is_bool(t))
            {
                panic(itl,"expected bool got %s in if condition\n",type_name(itl,t).buf);
                return;
            }

            // block for comparison branch
            // cant emit yet as we dont know how large the block is
            const u32 cmp_block = cur_block(func);

            // compile the body block
            const block_type type = if_stmt->node.type == ast_type::if_t? block_type::if_t : block_type::else_if_t;
            compile_basic_block(itl,func,(BlockNode*)if_stmt->right,type);

            // add branch over the block we just compiled
            const u32 slot = count(itl.symbol_table.label_lookup);

            emit_block(func,cmp_block,op_type::bnc,slot,r);


            // not the last statment (branch is not require)
            if(n != count(if_block->statements) - 1)
            {
                // emit a directive so we know to insert a branch
                // to the exit block here once we know how many blocks
                // there are
                emit(func,op_type::exit_block);
            }

            else
            {
                // TODO: we have to spill all here to make sure reg allocation works correctly
                // fix register allocation so we dont have to spill everyhting across basic blocks
                emit(func,op_type::spill_all);
            }
        }

        // else stmt has no expr so its in the first node
        // and by definition this is the last statement
        else 
        {
            UnaryNode* else_stmt = (UnaryNode*)if_block->statements[n];
            compile_basic_block(itl,func,(BlockNode*)else_stmt->next,block_type::else_t);

            // TODO: we have to spill all here to make sure reg allocation works correctly
            // fix register allocation so we dont have to spill everyhting across basic blocks
            emit(func,op_type::spill_all);
        }
    }

    // TODO: is this being a body fine or does it need to take whatever the intial block was?
    // create the exit block, for new code
    const u32 exit_label = new_basic_block(itl,func,old);



    // for every body block bar the last we just added
    // add a unconditonal branch to the "exit block"
    // the last block is directly before the next and does not need one

    for(u32 b = start_block; b < count(blocks) - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.end->opcode.op == op_type::exit_block)
        {
            block.list.end->opcode = Opcode(op_type::b,exit_label,0,0);
        }
    }
}

void compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    const u32 initial_block = cur_block(func);


    auto &blocks = func.emitter.program;
    block_type old = blocks[count(blocks) - 1].type;

    BinNode* while_node = (BinNode*)node;

    // compile cond
    const auto [t,stmt_cond_reg] = compile_oper(itl,func,while_node->left,new_tmp(func));

    // compile body
    const u32 cur = compile_basic_block(itl,func,(BlockNode*)while_node->right,block_type::while_t); 


    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,while_node->left,new_tmp(func));
    emit(func,op_type::bc,cur,loop_cond_reg);

    const u32 exit_label = new_basic_block(itl,func,old);

    // emit branch over the loop body in initial block
    // if cond is not met
    append(func.emitter.program[initial_block].list,Opcode(op_type::bnc,exit_label,stmt_cond_reg,0));    
}

void compile_for_block(Interloper &itl,Function &func,AstNode *node)
{
    // scope for any var decls in the stmt
    new_scope(itl.symbol_table);

    const u32 initial_block = cur_block(func);


    auto &blocks = func.emitter.program;
    block_type old = blocks[count(blocks) - 1].type;

    ForNode* for_node = (ForNode*)node;

    // compile the first stmt (ussualy an assign)


    const auto type = for_node->initializer->type;

    // handle this being a declaration
    switch(type)
    {
        case ast_type::auto_decl:
        {
            compile_auto_decl(itl,func,for_node->initializer);
            break;
        }

        case ast_type::declaration:
        {
            compile_decl(itl,func,for_node->initializer);
            break;
        }


        default:
        {
            compile_expression(itl,func,for_node->initializer,new_tmp(func));
            break;
        }
    }

    if(itl.error)
    {
        return;
    }
    

    const auto [t,stmt_cond_reg] = compile_oper(itl,func,for_node->cond,new_tmp(func));

    if(!is_bool(t))
    {
        panic(itl,"expected bool got %s in for condition\n",type_name(itl,t).buf);
        return;
    }    


    // compile the body
    const u32 cur = compile_basic_block(itl,func,for_node->block,block_type::for_t);    

    // compile loop end stmt
    compile_expression(itl,func,for_node->post,new_tmp(func));
    

    u32 loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,for_node->cond,new_tmp(func));
    emit(func,op_type::bc,cur,loop_cond_reg);

    const u32 exit_label = new_basic_block(itl,func,old);

    // emit branch over the loop body in initial block
    // if cond is not met
    append(func.emitter.program[initial_block].list,Opcode(op_type::bnc,exit_label,stmt_cond_reg,0));

    destroy_scope(itl.symbol_table);
}

/* TODO:
// compiles a statement and gets back a compile time value + type of the expr
std::pair<Type*, void*> exec_constant(Interloper& itl,AstNode* node)
{

}
*/

void compile_switch_block(Interloper& itl,Function& func, AstNode* node)
{
    SwitchNode* switch_node = (SwitchNode*)node;


    const u32 size = count(switch_node->statements);


    enum class switch_kind
    {
        integer,
        enum_t,
    };

    switch_kind switch_type;

    // used for checking enums match
    u32 type_idx = 0;
    

    if(size == 0)
    {
        panic(itl,"Switch statement has no cases");
        return;
    }

    CaseNode* first_case = (CaseNode*)switch_node->statements[0];
    
    // collapse all the values of the statements now we have parsed everything
    // NOTE: we cant do this as the ast is built, as we want to allow usage of values that are defined "out of order"
    switch(first_case->statement->type)
    {
        // enum
        case ast_type::scope:
        {
            ScopeNode* first_stmt = (ScopeNode*)first_case->statement;

            TypeDecl* type_decl = lookup(itl.type_table,first_stmt->scope);

            if(!(type_decl && type_decl->kind == type_kind::enum_t))
            {
                // case is not an enum
                panic(itl,"case is not an emum %s\n",first_stmt->scope.buf);
                return;
            }

            type_idx = type_decl->type_idx;
            Enum& enumeration = itl.enum_table[type_idx];


            for(u32 i = 0; i < size; i++)
            {
                CaseNode* case_node = switch_node->statements[i];


                // check we have a matching enum
                if(case_node->statement->type != ast_type::scope)
                {
                    panic(itl,"switch: one or more cases are not an enum\n");
                    return;
                }

                ScopeNode* scope_node = (ScopeNode*)case_node->statement;

                if(first_stmt->scope != scope_node->scope)
                {
                    panic(itl,"differing enums %s : %s in switch statement\n",first_stmt->scope.buf,scope_node->scope.buf);
                    return;
                }


                // pull the member value
                LiteralNode *member_node = (LiteralNode*)scope_node->expr;
                EnumMember* enum_member = lookup(enumeration.member_map,member_node->literal);

                if(!enum_member)
                {
                    panic(itl,"enum %s no such member %s\n",scope_node->scope.buf,member_node->literal);
                    return;
                }

                case_node->value = enum_member->value;          
            }            

            switch_type = switch_kind::enum_t;
            break;
        }

        // integer expression
        default:
        {
            for(u32 i = 0; i < size; i++)
            {
                CaseNode* case_node = switch_node->statements[i];
                case_node->value = eval_int_expr(case_node->statement);                
            }

            switch_type = switch_kind::integer;
            break;            
        }
    }


    // sort the statements, so we can pull out the gaps
    // and binary search them if we need to when emiting the statement dispatcher
    heap_sort(switch_node->statements,[](const CaseNode* v1, const CaseNode* v2)
    {
        return v1->value > v2->value;
    });

    // check every statement on a enum has a handler
    if(switch_type == switch_kind::enum_t && !switch_node->default_statement && size != itl.enum_table[type_idx].member_map.size)
    {
        // TODO: print the missing cases
        panic(itl,"switch on enum %s missing cases:\n",itl.enum_table[type_idx].name.buf);
        return;     
    }


    u32 gap = 0;

    // gap check all the statements and figure out 
    // if they are close enough to encode as a binary table
    // or if a binary search should be employed instead
    for(u32 i = 0; i < size - 1; i++)
    {
        const u32 cur_gap = switch_node->statements[i + 1]->value - switch_node->statements[i]->value;

        // these statements have no gap, this means they are duplicated
        if(cur_gap == 0)
        {
            panic(itl,"duplicate case %d\n",switch_node->statements[i]->value);
            return;
        }

        gap += cur_gap;
    }

    auto &blocks = func.emitter.program;
    block_type old = blocks[count(blocks) - 1].type;



    // get the number of extra gaps
    gap -= size - 1;

    // TODO: measure what a good value for this is
    static constexpr u32 JUMP_TABLE_LIMIT = 64;


    // TODO: support doing a hybrid approach, of dividing into binary tree searching
    // on jump tables


    // use a jump table
    if(gap < JUMP_TABLE_LIMIT)
    {
        // get the table limits i.e min max
        const s32 min = switch_node->statements[0]->value;
        const s32 max = switch_node->statements[size - 1]->value;
        const u32 range = (max - min) + 1;


        // compile the actual switch expr
        const auto [rtype,expr_slot] = compile_oper(itl,func,switch_node->expr,new_tmp(func));

        // type check the switch stmt
        switch(switch_type)
        {
            case switch_kind::integer:
            {
                if(!is_integer(rtype))
                {
                    panic(itl,"expected integer for switch statement got %s\n",type_name(itl,rtype).buf);
                    return;
                }
                break;
            }

            // TODO:
            case switch_kind::enum_t:
            {
                assert(false);
                break;
            }
        }

        // save our cur block so we can emit the default block dispatch later
        const u32 range_block = cur_block(func);

        // finally emit the dispatch on the table now we know where to exit if the table bounds get execeeded
        const u32 switch_slot = new_tmp(itl,GPR_SIZE);
        emit(func,op_type::sub_imm,switch_slot,expr_slot,min);


        // out of range, branch to default
        const u32 default_cmp = emit_res(func,op_type::cmpugt_imm,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        new_basic_block(itl,func,old);

        // mulitply to get a jump table index
        const u32 table_index = emit_res(func,op_type::mul_imm,switch_slot,GPR_SIZE);

        
        // reserve space for the table inside the constant pool
        const u32 static_offset = reserve_const_pool(itl,pool_type::label,GPR_SIZE * range);
        const u32 table_addr = emit_res(func,op_type::pool_addr,static_offset);

        // get address in the tabel we want
        const u32 final_offset = emit_res(func,op_type::add_reg,table_addr,table_index);

        // load the address out of the jump table
        const u32 target = new_tmp(func);
        emit(func,load_ptr(target, final_offset,0, GPR_SIZE,false));


        // branch on it
        emit(func,op_type::b_reg,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];
            case_node->label = compile_basic_block(itl,func,case_node->block,block_type::case_t);
            case_node->end_block = cur_block(func);
        }




        // NOTE: as default is allways the last block it does not need to have a jump to the exit
        u32 default_label = 0;


        // if there is no default then our exit label is the end
        if(switch_node->default_statement)
        {
            default_label = compile_basic_block(itl,func,(BlockNode*)switch_node->default_statement->next,block_type::case_t);
        

            // TODO: improve reg alloc to handle branching
            emit(func,op_type::spill_all);
        }

        // create a exit block for every case to jump to when its done
        const u32 exit_label = new_basic_block(itl,func,old);

        // if there is no explicit default the default is just after the switch ends
        if(!switch_node->default_statement)
        {
            default_label = exit_label;
        }


        // we have default posisiton now we can emit the branch for the range checking failing
        emit_block(func,range_block,op_type::bc,default_label,default_cmp);



        // populate the jump table
        u32 case_idx = 0;

        
        for(u32 i = 0; i < range; i++)
        {
            const u32 addr = i * GPR_SIZE;

            // this is a non default case
            CaseNode* case_node = switch_node->statements[case_idx];

            static_assert(GPR_SIZE == sizeof(u32));

            if(case_node->value - min == i)
            {
                //printf("case %d -> %d\n",i,case_node->label);

                write_mem<u32>(itl.const_pool, addr, case_node->label);
                case_idx++;

                // add jump to the exit block
                emit_block(func,case_node->end_block,op_type::b,exit_label);
            }

            else
            {
                //printf("case %d -> default(%d)\n",i,default_label);

                write_mem<u32>(itl.const_pool, addr, default_label);
            }
        }
    }


    // use a binary search
    else
    {
        unimplemented("binary search");
    }
}


// TODO: this needs a cleanup
// TODO: does it make sense to use the same function for both the @ and & operator?
std::pair<Type*,u32> load_addr(Interloper &itl,Function &func,AstNode *node,u32 slot, bool addrof)
{
    // figure out what the addr is
    switch(node->type)
    {
        case ast_type::symbol:
        {
            LiteralNode* sym_node = (LiteralNode*)node;

            const auto name = sym_node->literal;
            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return std::pair<Type*,u32>{make_builtin(itl,builtin_type::void_t),0};
            }

            const auto &sym = sym_opt.value();

            if(addrof)
            {
                if(is_array(sym.type))
                {
                    assert(false);
                }

                Type* pointer_type = make_pointer(itl,sym.type);

                // actually  get the addr of the ptr
                emit(func,op_type::addrof,slot,sym.slot);
                return std::pair<Type*,u32>{pointer_type,slot};
            }

            // deref
            else
            {
                if(!is_pointer(sym.type))
                {
                    panic(itl,"[COMPILE]: symbol '%s' is not a pointer\n",name.buf);
                    return std::pair<Type*,u32>{make_builtin(itl,builtin_type::void_t),0};
                }

                PointerType* pointer_type = (PointerType*)sym.type;

                Type* contained_type = pointer_type->contained_type;

                return std::pair<Type*,u32>{contained_type,sym.slot};
            }
        }

        case ast_type::index:
        {
            assert(false);
        }

        case ast_type::access_struct:
        {
            assert(false);
        }

        default:
        {
            print(node);
            unimplemented("load_addr expr");
        }
    }
}


Type* compile_expression(Interloper &itl,Function &func,AstNode *node,u32 dst_slot)
{
    if(!node)
    {
        panic("nullptr in compile_expression");
        return make_builtin(itl,builtin_type::void_t);
    }

   
    switch(node->type)
    {

        case ast_type::addrof:
        {
            UnaryNode* addrof_node = (UnaryNode*)node;

            // want this to also get an addr but we want the actual ptr_count to go up...
            const auto [type,slot] = load_addr(itl,func,addrof_node->next,dst_slot,true);
            return type;
        }

        case ast_type::deref:
        {
            UnaryNode* deref_node = (UnaryNode*)node;

            const auto [type,slot] = load_addr(itl,func,deref_node->next,new_tmp(func),false);
            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;            
        }

        case ast_type::sizeof_t:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            // TODO: is it worth while adding this for type names?
            const auto [type,slot] = compile_oper(itl,func,unary_node->next,new_tmp(func));

            // we only want the type of the expr
            if(is_tmp(slot))
            {
                emit(func,op_type::free_reg,slot);
            }

            const u32 size = type_size(itl,type);
            emit(func,op_type::mov_imm,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);
        }

        case ast_type::cast:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [old_type,slot] = compile_oper(itl,func,bin_node->right,new_tmp(func));
            const auto new_type = get_type(itl,(TypeNode*)bin_node->left);

            handle_cast(itl,func,dst_slot,slot,old_type,new_type);
            return new_type;
        }


        case ast_type::plus:
        {
            // unary plus
            if(node->fmt == ast_fmt::unary)
            {
                UnaryNode* unary_node = (UnaryNode*)node;
                return compile_expression(itl,func,unary_node->next,dst_slot); 
            }

            else
            {
                return compile_arith_op(itl,func,node,op_type::add_reg,dst_slot);
            }
        }



        // multiple assigment
        case ast_type::equal:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [rtype,slot] = compile_oper(itl,func,bin_node->right,dst_slot);

            LiteralNode* lit_node = (LiteralNode*)bin_node->left;

            const auto name = lit_node->literal;

            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return make_builtin(itl,builtin_type::void_t);
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
            print(node);
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
            if(node->fmt == ast_fmt::unary)
            {
                UnaryNode* unary_node = (UnaryNode*)node;

                // negate by doing 0 - v
                const auto [t,dst] = compile_oper(itl,func,unary_node->next,dst_slot);


                // TODO: make sure our optimiser sees through this
                const u32 slot = emit_res(func,op_type::mov_imm,0);
                emit(func,op_type::sub_reg,dst,slot,dst);
                
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
            UnaryNode* unary_node = (UnaryNode*)node;

            const auto [t,reg] = compile_oper(itl,func,unary_node->next,dst_slot);

            // TODO: do we need to check this is integer?


            emit(func,op_type::not_reg,dst_slot,reg);
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
            emit(func,op_type::mov_imm,dst_slot,0);
            return make_builtin(itl,builtin_type::bool_t);
        }

        case ast_type::true_t:
        {
            emit(func,op_type::mov_imm,dst_slot,1);
            return make_builtin(itl,builtin_type::bool_t);
        }

        case ast_type::null_t:
        {
            emit(func,op_type::mov_imm,dst_slot,0);

            Type* plain = make_builtin(itl,builtin_type::null_t);

            return make_pointer(itl,plain);
        }


        case ast_type::logical_not:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            const auto [t,reg] = compile_oper(itl,func,unary_node->next,dst_slot);

            if(!is_bool(t))
            {
                panic(itl,"compile: logical_not expected bool got: %s\n",type_name(itl,t).buf);
                return make_builtin(itl,builtin_type::void_t);
            }

            // xor can invert our boolean which is either 1 or 0
            emit(func,op_type::xor_imm,dst_slot,reg,1);
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

        case ast_type::scope:
        {
            assert(false);
        }

        default:
        {
            panic(itl,"[COMPILE]: invalid expression '%s'\n",AST_NAMES[u32(node->type)]);
            return make_builtin(itl,builtin_type::void_t);
        }
    }
}




void compile_decl(Interloper &itl,Function &func, const AstNode *line)
{
    // get entry into symbol table
    const DeclNode* decl_node = (DeclNode*)line;

    const auto name = decl_node->name;
    const auto ltype = get_type(itl,decl_node->type);

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(sym_opt)
    {
        panic(itl,"redeclared symbol: %s:%s\n",name.buf,type_name(itl,sym_opt.value().type).buf);
        return;
    }

    const auto size = type_size(itl,ltype);

    // add new symbol table entry
    Symbol &sym = add_symbol(itl.symbol_table,name,ltype,size);




    if(is_array(sym.type))
    {
        assert(false);
    }

    else if(is_struct(sym.type))
    {
        assert(false);
    }

    
    // simple type
    else 
    {

        emit(func,op_type::alloc_slot,sym.slot);

        // initalizer
        if(decl_node->expr)
        {
            // normal assign
            const auto [rtype,reg] = compile_oper(itl,func,decl_node->expr,sym.slot);

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
            emit(func,op_type::mov_imm,sym.slot,default_value(sym.type));
        }
    } 
}


void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line)
{
    AutoDeclNode* auto_decl = (AutoDeclNode*)line;

    const auto name = auto_decl->name;

    if(get_sym(itl.symbol_table,name))
    {
        panic(itl,"redeclared symbol: %s\n",name.buf);
        return;
    }

    
    const auto [type,reg] = compile_oper(itl,func,auto_decl->expr,new_tmp(func));

    // add the symbol

    const auto size = type_size(itl,type);

    // add new symbol table entry
    const auto &sym = add_symbol(itl.symbol_table,name,type,size);

    emit(func,op_type::alloc_slot,sym.slot);
    compile_move(itl,func,sym.slot,reg,sym.type,type);
}


// returns label for cur block
u32 compile_basic_block(Interloper& itl, Function& func, BlockNode* block_node, block_type type)
{
    const u32 label_slot = new_basic_block(itl,func,type);
    compile_block(itl,func,block_node);

    return label_slot;
}

void compile_block(Interloper &itl,Function &func,BlockNode *block_node)
{
    new_scope(itl.symbol_table);

    const u32 size = count(block_node->statements);
    for(u32 s = 0; s < size; s++)
    {
        AstNode* line = block_node->statements[s];

        if(itl.error)
        {
            return;
        }

        itl.cur_expr = line;
    
        switch(line->type)
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
                BinNode* assign_node = (BinNode*)line;
                const auto [rtype,slot] = compile_oper(itl,func,assign_node->right,new_tmp(func));


                if(assign_node->left->type != ast_type::symbol)
                {
                    switch(assign_node->left->type)
                    {
                        case ast_type::deref:
                        {
                            UnaryNode* deref_node = (UnaryNode*)assign_node->left;

                            const auto [type,addr_slot] = load_addr(itl,func,deref_node->next,new_tmp(func),false);
                            check_assign(itl,type,rtype);
                            do_ptr_store(itl,func,slot,addr_slot,type);
                            break;                        
                        }
                    
                        case ast_type::index:
                        {
                            assert(false);
                            break;
                        }
                    
                        // write on struct member!
                        case ast_type::access_struct:
                        {
                            assert(false);
                            break;
                        }
                    
                        default:
                        {
                            print(assign_node->left);
                            unimplemented("non plain assign");
                            break;
                        }
                    }
                }
                
                else
                {
                    LiteralNode* sym_node = (LiteralNode*)assign_node->left;

                    const auto name = sym_node->literal;

                    const auto sym_opt = get_sym(itl.symbol_table,name);
                    if(!sym_opt)
                    {
                        panic(itl,"[COMPILE]: symbol '%s' assigned before declaration\n",name.buf);
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
                if(line->fmt == ast_fmt::record)
                {
                    RecordNode* record_node = (RecordNode*)line;

                    // single return
                    if(count(record_node->nodes) == 1)
                    {
                        const auto [rtype,v1] = compile_oper(itl,func,record_node->nodes[0],RV_IR);
        
                        if(v1 != RV_IR)
                        {
                            compile_move(itl,func,RV_IR,v1,func.return_type[0],rtype);
                        }


                        if(itl.error)
                        {
                            break;
                        }

                        check_assign(itl,func.return_type[0],rtype);
                    }

                    // multiple return
                    else
                    {
                        assert(false);
                    }
                
                }

                emit(func,op_type::ret);
                

                itl.has_return = true;
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,line,NO_SLOT);
                break;
            }            


            case ast_type::block:
            {
                compile_block(itl,func,(BlockNode*)line);
                break;
            }


            case ast_type::if_block:
            {
                compile_if_block(itl,func,line);
                break;
            }

            case ast_type::for_block:
            {
                compile_for_block(itl,func,line);
                break;
            }

            case ast_type::while_block:
            {
                compile_while_block(itl,func,line);
                break;
            }

            case ast_type::switch_t:
            {
                compile_switch_block(itl,func,line);
                break;
            }

            case ast_type::tuple_assign:
            {
                compile_function_call(itl,func,line,NO_SLOT);
                break;
            }

            default:
            {
                panic(itl,"[COMPILE] unexpected statement: %s\n",AST_NAMES[u32(line->type)]);
                break;
            }
        }
    }

    auto &table = itl.symbol_table.table[count(itl.symbol_table.table) - 1];

    for(u32 b = 0; b < count(table.buf); b++)
    {
        auto& bucket = table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            const u32 slot = bucket[i].v;

            const auto& sym = itl.symbol_table.slot_lookup[slot];


            // free the stack alloc for each var thats about to go out of scope
            if(sym.arg_offset == NON_ARG)
            {
                if(is_array(sym.type))
                {
                    assert(false);
                }

                else if(is_struct(sym.type))
                {
                    assert(false);
                }

                else
                {
                    emit(func,op_type::free_slot,sym.slot);
                } 
            }
        }
    }

    destroy_scope(itl.symbol_table);
}

void compile_functions(Interloper &itl)
{
    // global scope
    new_scope(itl.symbol_table);

    // push the start func
    mark_used(itl,*lookup(itl.function_table,String("main")));



    for(u32 idx = 0; idx != count(itl.used_func); idx++)
    {
        Function& func = *lookup(itl.function_table,itl.used_func[idx]);

        const auto &node = *func.root;
        itl.cur_file = node.filename;

        
        // put arguments on the symbol table they are marked as args
        // so we know to access them "above" to stack pointer
        new_scope(itl.symbol_table);


        func.emitter.reg_count = 0;

        // put each arg into scope
        for(u32 a = 0; a < count(func.args); a++)
        {
            const u32 slot = func.args[a];

            auto &sym = sym_from_slot(itl.symbol_table,slot);
            add_scope(itl.symbol_table,sym);
        }



        itl.has_return = false;


        // parse out each line of the function
        compile_basic_block(itl,func,node.block,block_type::body_t);

        destroy_scope(itl.symbol_table);

        if(!itl.has_return)
        {
            // is a void function this is fine
            // we just need to emit the ret at the end 
            if(func.return_type[0]->type_idx == u32(builtin_type::void_t))
            {
                emit(func,op_type::ret);
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



// -> add tagging to error's so we can check we get the correct kind of failure on tests

// -> impl static assert
// -> improve const expressions
// -> add global constants (and eventually globals but just leave this for now because we dont wanna handle allocating them)
// -> update comments
// -> impl a a smarter register allocator rather than just blindly spilling things
// -> handle block args inside the reg allocator

// TODO: basic type checking for returning pointers to local's

// feature plan:
// tuples -> global const's 
// enum -> function_pointers
// -> early stl  -> labels ->  compile time execution ->
// discriminated unions -> marcro -> debugg memory guards -> ...

void destroy_ast(Interloper& itl)
{
    // TODO: should we just allocate the arrays with our own allocator
    // instead of malloc so we can just destruct the entire heap?
    // we need to figure out how to impl one anyways
    for(u32 i = 0; i < count(itl.ast_arrays); i++)
    {
       free(*itl.ast_arrays[i]);
    }

    destroy_arr(itl.ast_arrays);

    destroy_allocator(itl.ast_allocator);
    destroy_allocator(itl.ast_string_allocator);

    destroy_table(itl.struct_def);

    
    itl.cur_expr = nullptr;
    itl.cur_file = ""; 
}

void destroy_itl(Interloper &itl)
{
    destroy_arr(itl.program);
    destroy_arr(itl.const_pool);
    destroy_arr(itl.pool_sections);
    clear(itl.symbol_table);
    
    destroy_ast(itl);


    // delete all functions
    for(u32 b = 0; b < count(itl.function_table.buf); b++)
    {
        auto &bucket = itl.function_table.buf[b];

        for(u32 i = 0; i < count(bucket); i++)
        {
            auto& func = bucket[i].v;
            destroy_func(func);
        }
    }

    destroy_table(itl.function_table);
    destroy_arr(itl.used_func);

    // destroy typing tables
#if 0
    destroy_table(itl.struct_def);
    destroy_struct_table(itl.struct_table);
    destroy_enum_table(itl.enum_table);
#endif
    destroy_table(itl.type_table);

    destroy_allocator(itl.list_allocator);
    destroy_allocator(itl.string_allocator);
    destroy_allocator(itl.type_allocator);
}

static constexpr u32 LIST_INITIAL_SIZE = 16 * 1024;
static constexpr u32 STRING_INITIAL_SIZE = 4 * 1024;
static constexpr u32 TYPE_INITIAL_SIZE = 16 * 1024;

void setup_type_table(Interloper& itl)
{
    itl.type_table = make_table<String,TypeDecl>();

    // add all the builtin types  
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        add_type_decl(itl,i,TYPE_NAMES[i],type_kind::builtin);
    }
}

void compile(Interloper &itl,const String& initial_filename)
{
    printf("compiling file: %s\n",initial_filename.buf);

    itl.error = false;

    itl.ast_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);
    itl.ast_string_allocator = make_allocator(STRING_INITIAL_SIZE);

    itl.string_allocator = make_allocator(STRING_INITIAL_SIZE);
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.type_allocator = make_allocator(TYPE_INITIAL_SIZE);

    itl.symbol_table.string_allocator = &itl.string_allocator;

    itl.function_table = make_table<String,Function>();
    itl.struct_def = make_table<String,StructDef>();

    setup_type_table(itl);

    // parse intial input file
    {
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
        for(u32 b = 0; b < count(itl.struct_def.buf); b++)
        {
            auto &bucket = itl.struct_def.buf[b];

            for(u32 i = 0; i < count(bucket); i++)
            {
                auto& def = bucket[i].v;
                print((AstNode*)def.root);
            }
        }


        // print function defs
        for(u32 b = 0; b < count(itl.function_table.buf); b++)
        {
            auto &bucket = itl.function_table.buf[b];

            for(u32 i = 0; i < count(bucket); i++)
            {
                auto& func = bucket[i].v;

                print((AstNode*)func.root);
            }
        }
    }

    // ensure the entry function is defined
    if(!contains(itl.function_table,String("main")))
    {
        panic(itl,"main is not defined!\n");
        destroy_itl(itl);
        return;
    }

#if 0
    // parse out any of the top level decl we need
    parse_struct_declarations(itl);
#endif

    parse_function_declarations(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    putchar('\n');


    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 

    compile_functions(itl);


    // okay we dont need the parse tree anymore
    // free it
    destroy_ast(itl);

    if(itl.error)
    {
        return;
    }


    //optimise_ir(itl);

    if(itl.print_ir)
    {
        dump_ir_sym(itl);
    }
    
    // perform register allocation on used functions
    for(u32 n = 0; n < count(itl.used_func); n++)
    {
        const auto& name = itl.used_func[n];

        Function& func = *lookup(itl.function_table,name);

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

    printf("OK\n\n");
}

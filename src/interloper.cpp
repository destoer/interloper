#include <interloper.h>

Type* compile_expression(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);
std::pair<Type*, SymSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
void compile_decl(Interloper &itl,Function &func,AstNode *line, b32 global = false);
void compile_block(Interloper &itl,Function &func,BlockNode *node);
BlockSlot compile_basic_block(Interloper &itl,Function &func,BlockNode *node);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);
std::pair<Type*,SymSlot> compile_oper(Interloper& itl,Function &func,AstNode *node);

void write_arr(Interloper &itl,Function &func,AstNode *node,Type* write_type, u32 slot);
Type* read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
std::pair<Type*, SymSlot> index_arr(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);
void traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,const SymSlot addr_slot, ArrayType* type, u32* offset);
std::pair<Type*,SymSlot> index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, SymSlot ptr_slot, SymSlot dst_slot);



void compile_move(Interloper &itl, Function &func, SymSlot dst_slot, SymSlot src_slot, const Type* dst_type, const Type* src_type);

std::pair<Type*,SymSlot> load_addr(Interloper &itl,Function &func,AstNode *node,SymSlot slot, bool addrof);

void add_func(Interloper& itl, const String& name, FuncNode* root);

void alloc_slot(Interloper& itl,Function& func, const SymSlot slot, b32 force_alloc);

SymSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
SymSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
SymSlot load_arr_data(Interloper& itl,Function& func,SymSlot slot, const Type* type);
SymSlot load_arr_len(Interloper& itl,Function& func,SymSlot slot, const Type* type);

void load_ptr(Interloper &itl,Function& func,SymSlot dst_slot,SymSlot addr_slot,u32 offset,u32 size, b32 is_signed);
void store_ptr(Interloper &itl,Function& func,SymSlot src_slot,SymSlot addr_slot,u32 offset,u32 size);

std::pair<Type*,SymSlot> symbol(Interloper &itl, AstNode *node);

#include "lexer.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "ir.cpp"
#include "elf.cpp"
#include "optimize.cpp"
#include "memory.cpp"
#include "struct.cpp"
#include "enum.cpp"
#include "rtti.cpp"
#include "func.cpp"
#include "array.cpp"
#include "constant.cpp"


void dump_sym_ir(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        dump_ir_sym(itl,func,itl.symbol_table);
    }
}


void dump_reg_ir(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        dump_ir_reg(itl,func,itl.symbol_table);
    }
}


std::pair<Type*,SymSlot> symbol(Interloper &itl, AstNode *node)
{
    LiteralNode* lit_node = (LiteralNode*)node;

    const auto name = lit_node->literal;

    const auto sym_ptr = get_sym(itl.symbol_table,name);
    if(!sym_ptr)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }

    const auto &sym = *sym_ptr;

    return std::pair{sym.type,sym.reg.slot};
}


Type* value(Interloper& itl,Function& func,AstNode *node, SymSlot dst_slot)
{
    ValueNode* value_node = (ValueNode*)node;
    Value value = value_node->value;

    mov_imm(itl,func,dst_slot,value.v);
    return value_type(itl,value);    
}


// for compiling operands i.e we dont care where it goes as long as we get something!
// i.e inside operators, function args, the call is responsible for making sure it goes in the right place
// NOTE: this returns out fixed array pointers and may require conversion by caller!
std::pair<Type*,SymSlot> compile_oper(Interloper& itl,Function &func,AstNode *node)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_oper");
    }

    // for error printing
    itl.cur_expr = node;


    switch(node->type)
    {
        // just want symbol name out without copying it
        case ast_type::symbol:
        {
            return symbol(itl,node);
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}

// NOTE: pass umod or udiv and it will figure out the correct one
template<const op_type type>
Type* compile_arith_op(Interloper& itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    static_assert(
        type == op_type::add_reg || type == op_type::sub_reg || type == op_type::mul_reg ||
        type == op_type::umod_reg || type == op_type::udiv_reg ||
        type == op_type::xor_reg || type == op_type::and_reg || type == op_type::or_reg
    );
    
    itl.arith_depth += 1;

    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left);
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right);

    if(itl.error)
    {
        return make_builtin(itl,builtin_type::void_t);
    }

    // pointer arith adds the size of the underlying type
    if(is_pointer(t1) && is_integer(t2))
    {
        // get size of pointed to type
        Type *contained_type = deref_pointer(t1);

        const SymSlot offset_slot = mul_imm_pow2_res(itl,func,v2,type_size(itl,contained_type));
        emit_reg3<type>(itl,func,dst_slot,v1,offset_slot);
    }

    // normal arith
    else
    {
        // figure out correct division type
        if constexpr (type == op_type::udiv_reg)
        {
            if(is_signed(t1))
            {
                emit_reg3<op_type::sdiv_reg>(itl,func,dst_slot,v1,v2);
            }

            else
            {
                emit_reg3<op_type::udiv_reg>(itl,func,dst_slot,v1,v2);
            }
        }

        else if constexpr (type == op_type::umod_reg)
        {
            if(is_signed(t1))
            {
                emit_reg3<op_type::smod_reg>(itl,func,dst_slot,v1,v2);
            }

            else
            {
                emit_reg3<op_type::umod_reg>(itl,func,dst_slot,v1,v2);
            }
        }

        else
        {
            emit_reg3<type>(itl,func,dst_slot,v1,v2);
        }
    }

    // produce effective type
    const auto final_type = effective_arith_type(itl,t1,t2,type);

    return final_type;        
}



Type* compile_shift(Interloper& itl,Function &func,AstNode *node,bool right, SymSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left);
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right);

    if(!(is_integer(t1) && is_integer(t2)))
    {
        panic(itl,itl_error::int_type_error,"shifts only defined for integers, got %s and %s\n",type_name(itl,t1).buf,type_name(itl,t2).buf);
        return make_builtin(itl,builtin_type::void_t);
    }



    if(right)
    {
        // if signed do a arithmetic shift 
        if(is_signed(t1))
        {
            asr(itl,func,dst_slot,v1,v2);
        }

        else
        {
            lsr(itl,func,dst_slot,v1,v2);
        }
    }

    // left shift
    else
    {
        lsl(itl,func,dst_slot,v1,v2);
    }

    // type being shifted is the resulting type
    return t1;
}


b32 check_static_cmp(Interloper& itl, const Type* value, const Type* oper, u64 v)
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
            panic(itl,itl_error::out_of_bounds,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
        }
    }

    // value is outside the range of the other type
    else if(is_signed(value) == is_signed(oper))
    {
        if(builtin_size(cast_builtin(value)) > builtin_size(cast_builtin(oper)))
        {
            panic(itl,itl_error::out_of_bounds,"value: %x exceeds type %s\n",v,builtin_type_name(cast_builtin(oper)));
        }
    }

    return false;
}

// handles <, <=, >, >=, &&, ||, ==, !=
template<const logic_op type>
Type* compile_logical_op(Interloper& itl,Function &func,AstNode *node, SymSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    auto [type_left,v1] = compile_oper(itl,func,bin_node->left);
    auto [type_right,v2] = compile_oper(itl,func,bin_node->right);



    // if one side is a value do type checking
    if(is_integer(type_left) && is_integer(type_right))
    {
        if(bin_node->left->type == ast_type::value || bin_node->right->type == ast_type::value)
        {
            if(bin_node->left->type == ast_type::value)
            {
                ValueNode* value_node = (ValueNode*)bin_node->left;
                const u64 v = value_node->value.v;

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
                const u64 v = value_node->value.v;

                
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
                panic(itl,itl_error::bool_type_error,"operations || and && are only defined on bools\n");
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
            crash_and_burn("%d is not a logical operation\n",s32(type));
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

        if(sign)
        {
            constexpr op_type opcode_type = LOGIC_OPCODE[1][u32(type)];
            emit_reg3<opcode_type>(itl,func,dst_slot,v1,v2);
        }

        else
        {
            constexpr op_type opcode_type = LOGIC_OPCODE[0][u32(type)];
            emit_reg3<opcode_type>(itl,func,dst_slot,v1,v2);
        }

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
void compile_move(Interloper &itl, Function &func, SymSlot dst_slot, SymSlot src_slot, const Type* dst_type, const Type* src_type)
{
    UNUSED(itl);
    // check the operation is even legal

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underyling type if its a pointer
    if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
    {
        mov_reg(itl,func,dst_slot,src_slot);
    }

    else if(is_array(dst_type) && is_array(src_type))
    {
        if(is_fixed_array(dst_type))
        {
            mov_reg(itl,func,dst_slot,src_slot);
        }

        // runtime
        else
        {
            SymSlot addr_slot;

            if(dst_slot.handle == RV_IR)
            {
                addr_slot = func.sig.args[0];
            }

            else
            {   
                addr_slot = addrof_res(itl,func,dst_slot);
            }

            const SymSlot data_slot = load_arr_data(itl,func,src_slot,src_type);
            store_ptr(itl,func,data_slot,addr_slot,0,GPR_SIZE);

            const SymSlot len_slot = load_arr_len(itl,func,src_slot,src_type);
            store_ptr(itl,func,len_slot,addr_slot,GPR_SIZE,GPR_SIZE);
        } 
    }

    // requires special handling to move
    else if(is_struct(dst_type) && is_struct(src_type))
    {
        // copy out the strucutre using the hidden pointer in the first arg
        if(dst_slot.handle == RV_IR)
        {
            const auto src_addr = make_struct_addr(src_slot,0);
            const auto dst_addr = make_addr(func.sig.args[0],0);

            ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
        } 

        else
        {
            const auto src_addr = make_struct_addr(src_slot,0);
            const auto dst_addr = make_struct_addr(dst_slot,0);

            ir_memcpy(itl,func,dst_addr,src_addr,type_size(itl,dst_type));
        }
    }

    else
    {
        assert(false);
    }
}


void compile_if_block(Interloper &itl,Function &func,AstNode *node)
{
    BlockNode* if_block = (BlockNode*)node;

    BlockSlot start_block = cur_block(func);
    BlockSlot exit_block;

    for(u32 n = 0; n < count(if_block->statements); n++)
    {
        BinNode* if_stmt = (BinNode*)if_block->statements[n];

        // compile the compare expr if conditon
        auto [t,r] = compile_oper(itl,func,if_stmt->left);

        // integer or pointer is fine check they aernt zero as a shorthand
        if(is_integer(t) || is_pointer(t))
        {
            r = cmp_ne_imm_res(itl,func,r,0);
        }
        
        else if(!is_bool(t))
        {
            panic(itl,itl_error::bool_type_error,"expected bool got %s in if condition\n",type_name(itl,t).buf);
            return;
        }

        // block for comparison branch
        // cant emit yet as we dont know how may blocks the if statement we are jumping over is yet
        const BlockSlot cmp_block = cur_block(func);

        // compile the body block
        const BlockSlot body_slot = compile_basic_block(itl,func,(BlockNode*)if_stmt->right);

        // not the last statment 
        if(n != count(if_block->statements) - 1)
        {
            // indicate we need to jump the exit block
            emit_exit_block(itl,func);

            if(if_block->statements[n+1]->type == ast_type::else_t)
            {
                // else stmt has no expr so its in the first node
                // and by definition this is the last statement with no cond so we have to explictly splice a jump to it
                UnaryNode* else_stmt = (UnaryNode*)if_block->statements[n+1];
                const BlockSlot else_slot = compile_basic_block(itl,func,(BlockNode*)else_stmt->next);

                // add branch over body we compiled to else statement
                emit_cond_branch(func,cmp_block,else_slot,body_slot,r,false);

                exit_block = add_fall(itl,func);
                break;
            }

            else
            {
                // create new block for compare for the next node
                // TODO: should this have hte type of the initial node or no?
                const BlockSlot chain_slot = new_basic_block(itl,func);

                // add branch over the body we compiled earlier
                emit_cond_branch(func,cmp_block,chain_slot,body_slot,r,false);
            }
        }

        // Final block, this exit is done via a fallthrough
        else
        {
            exit_block = add_fall(itl,func);

            // if cond not met just branch into exit block
            emit_cond_branch(func,cmp_block,exit_block,body_slot,r,false);          
        }
    }

    if(itl.error)
    {
        return;
    }

    // now we have to exit block add in every exit
    auto &blocks = func.emitter.program;

    for(u32 b = start_block.handle; b < count(blocks) - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.end)
        {
            if(block.list.end->opcode.op == op_type::exit_block)
            {
                remove(block.list,block.list.end);
                emit_branch(func,block_from_idx(b),exit_block);
            }
        }
    }
}

void compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    const BlockSlot initial_block = cur_block(func);

    BinNode* while_node = (BinNode*)node;

    // compile cond
    auto [cond_type,entry_cond] = compile_oper(itl,func,while_node->left);

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        entry_cond = cmp_ne_imm_res(itl,func,entry_cond,0);
    }

    // check cond is actually a bool
    else if(!is_bool(cond_type))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return;
    }    

    // compile body
    const BlockSlot while_block = compile_basic_block(itl,func,(BlockNode*)while_node->right); 

    const BlockSlot end_block = cur_block(func);

    SymSlot exit_cond;
    std::tie(std::ignore,exit_cond) = compile_oper(itl,func,while_node->left);

    // integer or pointer, check not zero
    if(is_integer(cond_type) || is_pointer(cond_type))
    {
        exit_cond = cmp_ne_imm_res(itl,func,exit_cond,0);
    }

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(func,end_block,while_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,while_block,entry_cond,false);   
}

void compile_for_range_idx(Interloper& itl, Function& func, ForRangeNode* for_node, b32 inc, op_type cmp_type)
{
    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    BinNode* cmp_node = (BinNode*)for_node->cond;



    // grab end
    // NOTE: we need to regrab this later incase it is not a const
    const auto [entry_end_type,entry_end] = compile_oper(itl,func,cmp_node->right);

    // make index the same sign as the end stmt
    const auto& sym = add_symbol(itl,for_node->name_one,entry_end_type); 

    const SymSlot index = sym.reg.slot;

    // grab initalizer
    const auto entry_init_type = compile_expression(itl,func,cmp_node->left,index);

    if(!is_integer(entry_init_type) || !is_integer(entry_end_type))
    {
        panic(itl,itl_error::bool_type_error,"expected integer's in range conditon got %s,%s\n",
            type_name(itl,entry_init_type).buf,type_name(itl,entry_init_type).buf);
        return;
    }    

    // compile in cmp for entry
    SymSlot entry_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,initial_block,cmp_type,entry_cond,index,entry_end);

    // compile the main loop body
    const BlockSlot for_block = compile_basic_block(itl,func,for_node->block); 

    // compile post inc / dec
    if(inc)
    {
        add_imm(itl,func,index,index,1);
    }

    else
    {
        sub_imm(itl,func,index,index,1);
    }

    // compile body check
    const BlockSlot end_block = cur_block(func);

    // regrab end
    const auto [exit_end_type,exit_end] = compile_oper(itl,func,cmp_node->right);

    SymSlot exit_cond = new_tmp(func,GPR_SIZE);
    emit_block_internal_slot(func,end_block,cmp_type,exit_cond,index,exit_end);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);    
}

void compile_for_range_arr(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    const auto [type, arr_slot] = compile_expression_tmp(itl,func,for_node->cond);

    if(!is_array(type))
    {
        panic(itl,itl_error::array_type_error,"Expected array for range stmt got %s\n",type_name(itl,type).buf);
        return;
    }

    ArrayType* arr_type = (ArrayType*)type;

    const u32 index_size = arr_type->sub_size;

    // attempting to index statically zero array
    // we dont care
    if(is_fixed_array(arr_type) && arr_type->size == 0)
    {
        return;
    }

    // save initial block so we can dump a branch later
    const BlockSlot initial_block = cur_block(func);

    const b32 track_idx = for_node->name_two.buf != nullptr;
    SymSlot index = SYM_ERROR;

    SymSlot data = SYM_ERROR;

    // create var to hold data
    if(!for_node->take_pointer)
    {
        const auto& sym = add_symbol(itl,for_node->name_one,index_arr(arr_type));
        data = sym.reg.slot;
    }

    else
    {
        const auto& sym = add_symbol(itl,for_node->name_one,make_pointer(itl,index_arr(arr_type)));
        data = sym.reg.slot;
    }
    

    if(track_idx)
    {
        // check our loop index does not exist
        if(symbol_exists(itl.symbol_table,for_node->name_two))
        {
            panic(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
            return;
        }        

        // make a fake constant symbol
        auto& sym = add_symbol(itl,for_node->name_two,make_builtin(itl,builtin_type::u32_t,true));
        sym.reg.flags &= ~CONST;

        // init the index
        index = sym.reg.slot;
        mov_imm(itl,func,index,0);
    }

    // setup the loop grab data and len
    const auto arr_len = load_arr_len(itl,func,arr_slot,type);
    const auto arr_bytes = mul_imm_pow2_res(itl,func,arr_len,index_size);

    const auto arr_data = load_arr_data(itl,func,arr_slot,type);

    // compute array end
    const auto arr_end = add_res(itl,func,arr_data,arr_bytes);

    SymSlot entry_cond = {SYMBOL_NO_SLOT};

    // if this is a fixed size array we dont need to check it
    // on entry apart from the zero check handled above ^
    // because we know it has members
    if(is_runtime_size(arr_type))
    {
        // check array is not empty
        entry_cond = cmp_ne_res(itl,func,arr_data,arr_end);
    }
    
    // compile the main loop body
    const BlockSlot for_block = new_basic_block(itl,func);

    // if we want the data actually load it for us
    if(!for_node->take_pointer)
    {
        // insert the array load inside the for block
        // before we compile the actual stmts
        do_ptr_load(itl,func,data,arr_data,arr_type->contained_type);
    }

    // move the pointer in the data
    else
    {
        mov_reg(itl,func,data,arr_data);
    }

    compile_block(itl,func,for_node->block);

    // compile body check
    const BlockSlot end_block = cur_block(func);

    // goto next array index
    add_imm(itl,func,arr_data,arr_data,index_size);

    if(track_idx)
    {
        add_imm(itl,func,index,index,1);
    }

    const auto exit_cond = cmp_ne_res(itl,func,arr_data,arr_end);

    // compile in branches
    const BlockSlot exit_block = new_basic_block(itl,func);

    // emit loop branch
    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    if(is_runtime_size(arr_type))
    {
        // emit branch over the loop body if array is empty
        emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);  
    }

    // fixed size array only need to add a fall
    // as we know its not empty by this point
    else
    {
        add_block_exit(func,initial_block,for_block);
    }
}


void compile_for_range(Interloper& itl, Function& func, ForRangeNode* for_node)
{
    // scope for any var decls in the stmt
    new_scope(itl.symbol_table);



    // check our loop index does not exist
    if(symbol_exists(itl.symbol_table,for_node->name_one))
    {
        panic(itl,itl_error::redeclaration,"redeclared index in for range loop: %s\n",for_node->name_one.buf);
        return;
    }

    // determine what kind of loop term we have
    switch(for_node->cond->type)
    {
        // <= or < is inc
        case ast_type::logical_lt:
        {
            compile_for_range_idx(itl,func,for_node,true,op_type::cmpslt_reg);
            break;
        }

        case ast_type::logical_le:
        {
            compile_for_range_idx(itl,func,for_node,true,op_type::cmpsle_reg);
            break;
        }

        // >= or > is dec
        case ast_type::logical_gt:
        {
            compile_for_range_idx(itl,func,for_node,false,op_type::cmpsgt_reg);
            break;
        }

        case ast_type::logical_ge:
        {
            compile_for_range_idx(itl,func,for_node,false,op_type::cmpsge_reg);
            break;
        }

        // array index!
        default:
        {
            compile_for_range_arr(itl,func,for_node);
            break;
        }
    }

    destroy_scope(itl.symbol_table);
}

void compile_for_iter(Interloper& itl, Function& func, ForIterNode* for_node)
{
    // scope for any var decls in the stmt
    new_scope(itl.symbol_table);

    const BlockSlot initial_block = cur_block(func);
  

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
            compile_expression_tmp(itl,func,for_node->initializer);
            break;
        }
    }

    if(itl.error)
    {
        return;
    }
    
    // compile cond for entry and check it is a bool
    const auto [cond_type,entry_cond] = compile_oper(itl,func,for_node->cond);

    if(!is_bool(cond_type))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,cond_type).buf);
        return;
    }    


    // compile the body
    const BlockSlot for_block = compile_basic_block(itl,func,for_node->block);    

    // compile loop end stmt
    compile_expression_tmp(itl,func,for_node->post);
    
    if(itl.error)
    {
        return;
    }

    const BlockSlot end_block = cur_block(func);


    SymSlot exit_cond;
    std::tie(std::ignore,exit_cond) = compile_oper(itl,func,for_node->cond);

    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(func,end_block,for_block,exit_block,exit_cond,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,for_block,entry_cond,false);

    destroy_scope(itl.symbol_table);    
}

SymSlot mul_imm_pow2_res(Interloper& itl, Function& func, SymSlot src,s32 imm)
{
    u32 shift = log2(imm);

    return lsl_imm_res(itl,func,src,shift);
}

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
        panic(itl,itl_error::missing_args,"Switch statement has no cases");
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

            TypeDecl* type_decl = lookup_type(itl,first_stmt->scope);

            if(!(type_decl && type_decl->kind == type_kind::enum_t))
            {
                // case is not an enum
                panic(itl,itl_error::enum_type_error,"case is not an emum %s\n",first_stmt->scope.buf);
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
                    panic(itl,itl_error::enum_type_error,"switch: one or more cases are not an enum\n");
                    return;
                }

                ScopeNode* scope_node = (ScopeNode*)case_node->statement;

                if(first_stmt->scope != scope_node->scope)
                {
                    panic(itl,itl_error::enum_type_error,"differing enums %s : %s in switch statement\n",first_stmt->scope.buf,scope_node->scope.buf);
                    return;
                }


                // pull the member value
                LiteralNode *member_node = (LiteralNode*)scope_node->expr;
                EnumMember* enum_member = lookup(enumeration.member_map,member_node->literal);

                if(!enum_member)
                {
                    panic(itl,itl_error::enum_type_error,"enum %s no such member %s\n",scope_node->scope.buf,member_node->literal);
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

                const auto [case_value,case_type] = compile_const_int_expression(itl,case_node->statement); 
                case_node->value = case_value;             
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
        panic(itl,itl_error::undeclared,"switch on enum %s missing cases:\n",itl.enum_table[type_idx].name.buf);
        return;     
    }


    u64 gap = 0;

    // gap check all the statements and figure out 
    // if they are close enough to encode as a binary table
    // or if a binary search should be employed instead
    for(u32 i = 0; i < size - 1; i++)
    {
        const u64 cur_gap = switch_node->statements[i + 1]->value - switch_node->statements[i]->value;

        // these statements have no gap, this means they are duplicated
        if(cur_gap == 0)
        {
            panic(itl,itl_error::redeclaration,"duplicate case %d\n",switch_node->statements[i]->value);
            return;
        }

        gap += cur_gap;
    }


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
        const s64 min = switch_node->statements[0]->value;
        const s64 max = switch_node->statements[size - 1]->value;
        const u64 range = (max - min) + 1;


        // compile the actual switch expr
        const auto [rtype,expr_slot] = compile_oper(itl,func,switch_node->expr);

        // type check the switch stmt
        switch(switch_type)
        {
            case switch_kind::integer:
            {
                if(!is_integer(rtype))
                {
                    panic(itl,itl_error::int_type_error,"expected integer for switch statement got %s\n",type_name(itl,rtype).buf);
                    return;
                }
                break;
            }

            case switch_kind::enum_t:
            {
                if(is_enum(rtype))
                {
                    EnumType* enum_type = (EnumType*)rtype;
                    if(enum_type->enum_idx != type_idx)
                    {
                        panic(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",itl.enum_table[type_idx].name.buf,type_name(itl,rtype));
                        return;                        
                    }
                }

                else
                {
                    panic(itl,itl_error::enum_type_error,"expected enum of type %s got %s\n",itl.enum_table[type_idx].name.buf,type_name(itl,rtype));
                    return;                    
                }
                break;
            }
        }

        // save our cur block so we can emit the default block dispatch later
        const BlockSlot range_block = cur_block(func);

        // finally emit the dispatch on the table now we know where to exit if the table bounds get execeeded
        const SymSlot switch_slot = new_tmp(func,GPR_SIZE);
        sub_imm(itl,func,switch_slot,expr_slot,min);

        const SymSlot default_cmp = new_tmp(func,GPR_SIZE);
        cmp_unsigned_gt_imm(itl,func,default_cmp,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        const BlockSlot dispatch_block = new_basic_block(itl,func);

        // mulitply to get a jump table index
        const SymSlot table_index = mul_imm_pow2_res(itl,func,switch_slot,GPR_SIZE);

        
        // reserve space for the table inside the constant pool
        const PoolSlot pool_slot = reserve_const_pool_section(itl.const_pool,pool_type::jump_table,GPR_SIZE * range);
        const SymSlot table_addr = pool_addr_res(itl,func,pool_slot,0);

        // get address in the tabel we want
        const SymSlot final_offset = add_res(itl,func,table_addr,table_index);

        // load the address out of the jump table
        const SymSlot target = new_tmp_ptr(func);
        load_ptr(itl,func,target, final_offset,0, GPR_SIZE,false);

        // branch on it
        branch_reg(itl,func,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];

            const BlockSlot case_slot = compile_basic_block(itl,func,case_node->block);

            // add link from dispatch to case
            add_block_exit(func,dispatch_block,case_slot);

            const Block& case_block = block_from_slot(func,case_slot);
            case_node->label = case_block.label_slot;

            case_node->end_block = cur_block(func);
        }




        // NOTE: as default is allways the last block it does not need to have a jump to the exit
        BlockSlot default_block;


        // if there is no default then our exit label is the end
        if(switch_node->default_statement)
        {
            default_block = compile_basic_block(itl,func,(BlockNode*)switch_node->default_statement->next);
            add_block_exit(func,dispatch_block,default_block);
        }

        BlockSlot default_block_end = cur_block(func);

        // create a exit block for every case to jump to when its done
        const BlockSlot exit_block = new_basic_block(itl,func);

        // if there is no explicit default the default is just after the switch ends
        if(!switch_node->default_statement)
        {
            default_block = exit_block;
        }

        else
        {
            // default falls through to exit
            add_block_exit(func,default_block_end,exit_block);
        }


        // we have default posisiton now we can emit the branch for the range checking failing
        emit_cond_branch(func,range_block,default_block,dispatch_block,default_cmp,true);



        // populate the jump table
        u32 case_idx = 0;

        
        const LabelSlot default_label = block_from_slot(func,default_block).label_slot; 

        // NOTE: we do a second pass because we did not know where the default block is stored when
        // we compiled all the switch statements
        for(u32 i = 0; i < range; i++)
        {
            const u32 addr = i * GPR_SIZE;

            // this is a non default case
            CaseNode* case_node = switch_node->statements[case_idx];

            // current jump table entry matches case
            if(case_node->value - min == i)
            {
                //printf("case %ld -> %d L%d\n",case_node->value,addr,case_node->label.handle);

                write_const_pool_label(itl.const_pool,pool_slot, addr, case_node->label);
                case_idx++;

                // add jump to the exit block
                emit_branch(func,case_node->end_block,exit_block);
            }

            // as statements as sorted this means there is no match emit default
            else
            {
                //printf("case %d -> %d default(L%d)\n",i,addr,default_label.handle);

                write_const_pool_label(itl.const_pool,pool_slot, addr, default_label);
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
std::pair<Type*,SymSlot> load_addr(Interloper &itl,Function &func,AstNode *node,SymSlot slot, b32 take_addr)
{
    // figure out what the addr is
    switch(node->type)
    {
        case ast_type::symbol:
        {
            LiteralNode* sym_node = (LiteralNode*)node;

            const auto name = sym_node->literal;
            auto sym_ptr = get_sym(itl.symbol_table,name);

            if(!sym_ptr)
            {
                // could be attempting to take a function pointer?
                if(take_addr)
                {
                    auto func_def = lookup_func_def(itl,name);

                    if(func_def)
                    {
                        // this may get called at some point so we need to mark it for compilation...
                        auto func_call_opt = finalise_func(itl,*func_def,(AstNode*)node);

                        if(!func_call_opt)
                        {
                            return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
                        }

                        auto& func_call = *func_call_opt;

                        FuncPointerType* type = (FuncPointerType*)alloc_type<FuncPointerType>(itl,FUNC_POINTER,true);
                        type->sig = func_call.sig;

                        load_func_addr(itl,func,slot,func_call.label_slot);
                        
                        return std::pair{(Type*)type,slot};
                    }
                }

                // nothing found!
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
            }

            auto &sym = *sym_ptr;

            if(take_addr)
            {
                spill_slot(itl,func,sym.reg);

                if(is_array(sym.type))
                {
                    assert(false);
                }

                Type* pointer_type = make_pointer(itl,sym.type);

                // actually  get the addr of the ptr
                addrof(itl,func,slot,sym.reg.slot);
                return std::pair{pointer_type,slot};
            }

            // deref
            else
            {
                if(!is_pointer(sym.type))
                {
                    panic(itl,itl_error::pointer_type_error,"[COMPILE]: symbol '%s' is not a pointer\n",name.buf);
                    return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
                }

                PointerType* pointer_type = (PointerType*)sym.type;

                Type* contained_type = pointer_type->contained_type;

                return std::pair{contained_type,sym.reg.slot};
            }
        }

        case ast_type::index:
        {
            if(take_addr)
            {
                return index_arr(itl,func,node,slot);
            }

            else
            {
                IndexNode* index_node = (IndexNode*)node;


                auto [type,addr_slot] = index_arr(itl,func,node,slot);

                // actually load the pointer with ptr_load
                type = deref_pointer(type);

                const SymSlot ptr_slot = new_tmp_ptr(func);
                do_ptr_load(itl,func,ptr_slot,addr_slot,type);

                // contained type is not actually a pointer
                if(!is_pointer(type))
                {
                    panic(itl,itl_error::pointer_type_error,"[COMPILE]: array '%s' does not contain a pointer\n",index_node->name.buf);
                    return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
                }

                // okay now just index out the final type
                type = deref_pointer(type);

                return std::pair{type,ptr_slot};
            }
        }

        case ast_type::access_struct:
        {
            if(take_addr)
            {
                auto [type,ptr_slot] = compute_member_ptr(itl,func,node);

                // make sure this ptr goes into the dst slot
                mov_reg(itl,func,slot,ptr_slot);

                return std::pair{type,ptr_slot};
            }

            // deref on struct member that is a ptr
            else
            {
                auto type = read_struct(itl,func,slot,node);
                type = deref_pointer(type);

                return std::pair{type,slot};
            }
        }

        default:
        {
            print(node);
            unimplemented("load_addr expr");
        }
    }
}

Type* compile_expression(Interloper &itl,Function &func,AstNode *node,SymSlot dst_slot)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_expression");
        return make_builtin(itl,builtin_type::void_t);
    }

    itl.cur_expr = node;
   
    switch(node->type)
    {

        case ast_type::value:
        {
            const auto t1 = value(itl,func,node,dst_slot);
            return t1;
        }

        case ast_type::symbol:
        {
            const auto [type, slot] = symbol(itl,node);
            
            if(itl.error)
            {
                return make_builtin(itl,builtin_type::void_t);
            }

            compile_move(itl,func,dst_slot,slot,type,type);
            return type;
        }


        case ast_type::char_t:
        {
            CharNode* char_node = (CharNode*)node;

            mov_imm(itl,func,dst_slot,char_node->character);
            return make_builtin(itl,builtin_type::c8_t);
        }

        case ast_type::access_struct:
        {
            // are we accessing type info on a type name?
            BinNode* member_root = (BinNode*)node;
            AstNode* expr_node = member_root->left;

            if(expr_node->type == ast_type::symbol)
            {
                RecordNode* members = (RecordNode*)member_root->right;

                // potential type info access
                if(count(members->nodes) == 1 && members->nodes[0]->type == ast_type::access_member)
                {
                    LiteralNode* sym_node = (LiteralNode*)expr_node;
                    const auto name = sym_node->literal;

                    TypeDecl* type_decl = lookup_type(itl,name);

                    if(type_decl)
                    {
                        LiteralNode* member_node = (LiteralNode*) members->nodes[0];

                        auto type = access_type_info(itl,func,dst_slot,*type_decl,member_node->literal);

                        return type;
                    }
                }
            }

            return read_struct(itl,func,dst_slot,node);           
        }

        case ast_type::builtin_type_info:
        {
            BuiltinAccessNode* builtin = (BuiltinAccessNode*)node;

            return access_builtin_type_info(itl,func,dst_slot,builtin->type,builtin->field);
        }

        case ast_type::index:
        {
            const auto type = read_arr(itl,func,node,dst_slot);

            return type;
        }

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

            const auto [type,slot] = load_addr(itl,func,deref_node->next,new_tmp_ptr(func),false);
            if(itl.error)
            {
                return make_builtin(itl,builtin_type::void_t);
            }

            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;            
        }

        case ast_type::sizeof_t:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            // TODO: should this work with type names?
            const auto [type,slot] = compile_oper(itl,func,unary_node->next);


            const u32 size = type_size(itl,type);
            mov_imm(itl,func,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);
        }

        case ast_type::sizeof_type_t:
        {
            TypeNode* type_node = (TypeNode*)node;

            // just move in the type size
            const u32 size = type_size(itl,get_type(itl,type_node));
            mov_imm(itl,func,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);
        }

        case ast_type::cast:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [old_type,slot] = compile_oper(itl,func,bin_node->right);
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
                return compile_arith_op<op_type::add_reg>(itl,func,node,dst_slot);
            }
        }



        // multiple assigment
        case ast_type::equal:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto rtype = compile_expression(itl,func,bin_node->right,dst_slot);

            if(bin_node->left->fmt != ast_fmt::literal)
            {
                panic(itl,itl_error::invalid_statement,"[COMPILE]: expected symbol in multiple assign\n");
            }

            LiteralNode* lit_node = (LiteralNode*)bin_node->left;

            const auto name = lit_node->literal;

            const auto sym_ptr = get_sym(itl.symbol_table,name);
            if(!sym_ptr)
            {
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return make_builtin(itl,builtin_type::void_t);
            }

            const auto &sym = *sym_ptr;

            check_assign(itl,sym.type,rtype);

            compile_move(itl,func,sym.reg.slot,dst_slot,sym.type,rtype);

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
            return compile_arith_op<op_type::udiv_reg>(itl,func,node,dst_slot);
        }

        case ast_type::mod:
        {
            return compile_arith_op<op_type::umod_reg>(itl,func,node,dst_slot);       
        }

        case ast_type::times:
        {
            return compile_arith_op<op_type::mul_reg>(itl,func,node,dst_slot);
        }

        case ast_type::minus:
        {            
            // unary minus
            if(node->fmt == ast_fmt::unary)
            {
                UnaryNode* unary_node = (UnaryNode*)node;

                // negate by doing 0 - v
                const auto [t,dst] = compile_oper(itl,func,unary_node->next);


                // TODO: make sure our optimiser sees through this
                const SymSlot slot = mov_imm_res(itl,func,0);
                sub(itl,func,dst,slot,dst);
                
                return t;
            }

            else
            {
                return compile_arith_op<op_type::sub_reg>(itl,func,node,dst_slot);
            }
        }

        case ast_type::bitwise_and:
        {
            return compile_arith_op<op_type::and_reg>(itl,func,node,dst_slot);
        }

        case ast_type::bitwise_or:
        {
            return compile_arith_op<op_type::or_reg>(itl,func,node,dst_slot);
        }

        case ast_type::bitwise_xor:
        {
            return compile_arith_op<op_type::xor_reg>(itl,func,node,dst_slot);
        }


        case ast_type::bitwise_not:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            const auto [t,reg] = compile_oper(itl,func,unary_node->next);

            // TODO: do we need to check this is integer?


            not_reg(itl,func,dst_slot,reg);
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
            mov_imm(itl,func,dst_slot,0);
            return make_builtin(itl,builtin_type::bool_t);
        }

        case ast_type::true_t:
        {
            mov_imm(itl,func,dst_slot,1);
            return make_builtin(itl,builtin_type::bool_t);
        }

        case ast_type::null_t:
        {
            mov_imm(itl,func,dst_slot,0);

            Type* plain = make_builtin(itl,builtin_type::null_t);

            return make_pointer(itl,plain);
        }


        case ast_type::logical_not:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            const auto [t,reg] = compile_oper(itl,func,unary_node->next);

            // integer or pointer, eq to zero
            if(is_integer(t) || is_pointer(t))
            {
                cmp_eq_imm(itl,func,dst_slot,reg,0);
                return make_builtin(itl,builtin_type::bool_t);
            }

            // logical not on bool
            else if(!is_bool(t))
            {
                panic(itl,itl_error::bool_type_error,"compile: logical_not expected bool got: %s\n",type_name(itl,t).buf);
                return make_builtin(itl,builtin_type::void_t);
            }

            // xor can invert our boolean which is either 1 or 0
            xor_imm(itl,func,dst_slot,reg,1);
            return t;
        }

        // we want to pass in the base operation but we need to do the actual type checking
        // to know what we are comparing later
        // how should we do it?
        case ast_type::logical_lt:
        {
            return compile_logical_op<logic_op::cmplt_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_le:
        {
            return compile_logical_op<logic_op::cmple_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_gt:
        {
            return compile_logical_op<logic_op::cmpgt_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_ge:
        {
            return compile_logical_op<logic_op::cmpge_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_eq:
        {
            return compile_logical_op<logic_op::cmpeq_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_ne:
        {
            return compile_logical_op<logic_op::cmpne_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_and:
        {
            return compile_logical_op<logic_op::and_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_or:
        {
            return compile_logical_op<logic_op::or_reg>(itl,func,node,dst_slot);
        }


        case ast_type::function_call:
        {
            return compile_function_call(itl,func,node,dst_slot);
        }

        case ast_type::scope:
        {
            // TODO: this assumes an enum, when we add scoping
            // just check if the last scope happens to be an enum

            ScopeNode* scope_node = (ScopeNode*)node;

            TypeDecl* type_decl = lookup_type(itl,scope_node->scope);

            if(type_decl && type_decl->kind == type_kind::enum_t)
            {
                const String &enum_name = scope_node->scope;

                auto enumeration = itl.enum_table[type_decl->type_idx];

                if(scope_node->expr->type != ast_type::symbol)
                {
                    panic(itl,itl_error::enum_type_error,"expected enum member of enum %s",enum_name.buf);
                    return make_builtin(itl,builtin_type::void_t);
                }

                LiteralNode *member_node = (LiteralNode*)scope_node->expr;


                EnumMember* enum_member = lookup(enumeration.member_map,member_node->literal);

                if(!enum_member)
                {
                    panic(itl,itl_error::enum_type_error,"enum %s no such member %s\n",enum_name.buf,member_node->literal);
                    return make_builtin(itl,builtin_type::void_t);
                }

                // emit mov on the enum value
                mov_imm(itl,func,dst_slot,enum_member->value);

                return make_enum_type(itl,enumeration);
            }

            // TODO: we dont have namespacing
            else 
            {
                // TODO: this wont print a full scope
                panic(itl,itl_error::none,"no such scope %s\n",scope_node->scope.buf);
                return make_builtin(itl,builtin_type::void_t);
            }
        }

        default:
        {
            panic(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'\n",AST_NAMES[u32(node->type)]);
            return make_builtin(itl,builtin_type::void_t);
        }
    }
}

void compile_decl(Interloper &itl,Function &func, AstNode *line, b32 global)
{
    // get entry into symbol table
    const DeclNode* decl_node = (DeclNode*)line;

    const auto name = decl_node->name;
    const auto ltype = get_type(itl,decl_node->type);

    const auto sym_ptr = get_sym(itl.symbol_table,name);

    if(sym_ptr)
    {
        panic(itl,itl_error::redeclaration,"redeclared symbol: %s:%s\n",name.buf,type_name(itl,sym_ptr->type).buf);
        return;
    }

    SymSlot slot = {NO_SLOT};

    // add new symbol table entry
    {
        // dont hold this sym reference as its no doubt going to be invalidated
        // as we are actively compiling expressions
        Symbol &sym = global? add_global(itl,name,ltype,false) : add_symbol(itl,name,ltype);
        slot = sym.reg.slot;
    }

    

    if(is_array(ltype))
    {
        compile_arr_decl(itl,func,decl_node,slot);
    }

    else if(is_struct(ltype))
    {
        compile_struct_decl(itl,func,decl_node,slot);
    }

    // simple type
    else 
    {
        alloc_slot(itl,func,slot,false);
        
        // initalizer
        if(decl_node->expr)
        {
            if(decl_node->expr->type != ast_type::no_init)
            {
                // normal assign
                const auto rtype = compile_expression(itl,func,decl_node->expr,slot);
            
                // our symbol reference might have moved because of compile_expression
                auto &sym = sym_from_slot(itl.symbol_table,slot);

                if(is_unsigned_integer(sym.type))
                {
                    clip_arith_type(itl,func,sym.reg.slot,sym.reg.slot,sym.reg.size);
                }
            
                if(itl.error)
                {
                    return;
                }

                check_assign_init(itl,ltype,rtype);
            }             
        }

        // default init
        else
        {
            mov_imm(itl,func,slot,0);
        }
    } 

    // need to perform sizing AFTER initalizers have been parsed
    // just in case we need to do any size deduction
    if(global)
    {
        auto &sym = sym_from_slot(itl.symbol_table,slot);
        reserve_global_alloc(itl,sym);
    }
}

std::pair<Type*, SymSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    // assume a size then refine it with expr result
    const SymSlot dst_slot = new_tmp(func,GPR_SIZE);

    Type* type = compile_expression(itl,func,node,dst_slot);

    func.registers[dst_slot.handle] = make_reg(itl,reg_kind::tmp,dst_slot.handle,type);

    return std::pair{type,dst_slot};
}





void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line)
{
    AutoDeclNode* auto_decl = (AutoDeclNode*)line;

    const auto name = auto_decl->name;

    if(get_sym(itl.symbol_table,name))
    {
        panic(itl,itl_error::redeclaration,"redeclared symbol: %s\n",name.buf);
        return;
    }

    const auto [type,reg] = compile_oper(itl,func,auto_decl->expr);

    if(itl.error)
    {
        return;
    }

    // attempting to deduce a type from void is nonsense
    if(is_void(type))
    {
        panic(itl,itl_error::undefined_type_oper,"Result of auto decl is void\n");
        return;
    }

    // add the symbol

    // add new symbol table entry
    const auto &sym = add_symbol(itl,name,type);

    alloc_slot(itl,func,sym.reg.slot,!is_plain_type(type));
    compile_move(itl,func,sym.reg.slot,reg,sym.type,type);
}


BlockSlot compile_basic_block(Interloper& itl, Function& func, BlockNode* block_node)
{
    const BlockSlot block_slot = new_basic_block(itl,func);
    compile_block(itl,func,block_node);

    return block_slot;
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

            case ast_type::const_decl:
            {
                compile_constant_decl(itl,(DeclNode*)line,false);
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
                
                if(assign_node->left->type != ast_type::symbol)
                {
                    const auto [rtype,slot] = compile_oper(itl,func,assign_node->right);

                    switch(assign_node->left->type)
                    {
                        case ast_type::deref:
                        {
                            UnaryNode* deref_node = (UnaryNode*)assign_node->left;

                            const auto [type,addr_slot] = load_addr(itl,func,deref_node->next,new_tmp_ptr(func),false);
                            check_assign(itl,type,rtype);
                            do_ptr_store(itl,func,slot,addr_slot,type);
                            break;                        
                        }
                    
                        case ast_type::index:
                        {
                            write_arr(itl,func,assign_node->left,rtype,slot);
                            break;
                        }
                    
                        // write on struct member!
                        case ast_type::access_struct:
                        {
                            write_struct(itl,func,slot,rtype,assign_node->left);
                            break;
                        }
                    
                        default:
                        {
                            panic(itl,itl_error::invalid_expr,"could not assign to expr: %s\n",AST_NAMES[u32(assign_node->left->type)]);
                            break;
                        }
                    }
                }
                
                else
                {
                    LiteralNode* sym_node = (LiteralNode*)assign_node->left;

                    const auto name = sym_node->literal;

                    const auto sym_ptr = get_sym(itl.symbol_table,name);
                    if(!sym_ptr)
                    {
                        panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' assigned before declaration\n",name.buf);
                        break;
                    }

                    // copy these locally incase the symbol moves
                    const auto slot = sym_ptr->reg.slot;
                    const auto size = sym_ptr->reg.size;
                    const auto ltype = sym_ptr->type;

                    const auto rtype = compile_expression(itl,func,assign_node->right,slot);
                    check_assign(itl,ltype,rtype);

                    if(is_unsigned_integer(ltype))
                    {
                        clip_arith_type(itl,func,slot,slot,size);
                    }
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
                        const auto rtype = compile_expression(itl,func,record_node->nodes[0],sym_from_idx(RV_IR));
        
                        if(itl.error)
                        {
                            break;
                        }

                        check_assign_init(itl,func.sig.return_type[0],rtype);
                    }

                    // multiple return
                    else
                    {
                        if(count(record_node->nodes) != count(func.sig.return_type))
                        {
                            panic(itl,itl_error::mismatched_args,"Invalid number of return parameters for function %s : %d != %d\n",
                                func.name.buf,count(record_node->nodes),count(func.sig.return_type));
                            
                            return;
                        }
                        

                        for(u32 r = 0; r < count(func.sig.return_type); r++)
                        {
                            // void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type, u32 offset = 0)
                            // NOTE: Pointers are in the first set of args i.e the hidden ones
                            const auto [rtype, ret_slot] = compile_oper(itl,func,record_node->nodes[r]);

                            // check each param
                            check_assign(itl,func.sig.return_type[r],rtype);

                            do_ptr_store(itl,func,ret_slot,func.sig.args[r],func.sig.return_type[r]);
                        }
                    }
                
                }

                // no return
                else
                {
                    if(func.sig.return_type[0]->type_idx != u32(builtin_type::void_t))
                    {
                        panic(itl,itl_error::missing_args,"Expected return type of %s got nothing\n",type_name(itl,func.sig.return_type[0]).buf);
                        return;
                    }
                }

                ret(itl,func);
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,line,sym_from_idx(NO_SLOT));
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

            case ast_type::for_iter:
            {
                compile_for_iter(itl,func,(ForIterNode*)line);
                break;
            }

            case ast_type::for_range:
            {
                compile_for_range(itl,func,(ForRangeNode*)line);
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
                compile_function_call(itl,func,line,sym_from_idx(NO_SLOT));
                break;
            }

            case ast_type::const_assert:
            {
                UnaryNode* unary_node = (UnaryNode*)line;

                const b32 pass = compile_const_bool_expression(itl,unary_node->next);

                if(!pass)
                {
                    panic(itl,itl_error::const_assert,"Compile time assertion failed\n");
                }
                break;
            }

            default:
            {
                panic(itl,itl_error::invalid_statement,"[COMPILE] unexpected statement: %s\n",AST_NAMES[u32(line->type)]);
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
            auto& sym = sym_from_slot(itl.symbol_table,bucket[i].v);

            // free the stack alloc for each var thats about to go out of scope
            if(sym.arg_offset == NON_ARG)
            {
                free_sym(itl,func,sym);
            }
        }
    }

    destroy_scope(itl.symbol_table);
}


void compile_globals(Interloper& itl)
{
    // create a dummy void func called init_global
    // that we can compile all our global inits into!
    auto& func = create_dummy_func(itl,"init_global");

    for(u32 c = 0; c < count(itl.global_decl); c++)
    {
        itl.cur_file = itl.global_decl[c]->filename;
        compile_decl(itl,func,(AstNode*)itl.global_decl[c]->decl,true);

        if(itl.error)
        {
            return;
        }
    }

    finalise_global_offset(itl);
}

// -> impl static assert
// -> improve const expressions
// -> handle block args inside the reg allocator and get a proper global allocator

// TODO: basic type checking for returning pointers to local's

// feature plan:
// function_pointers
// -> early stl  -> function pointers -> compile time execution ->
// better enums -> unions? -> debug memory guards -> ...

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

    destroy_table(itl.type_def);
    destroy_arr(itl.global_def);

    itl.cur_expr = nullptr;
    itl.cur_file = ""; 
}

void destroy_itl(Interloper &itl)
{
    destroy_asm_emitter(itl.asm_emitter);
    destroy_arr(itl.program);
    destroy_const_pool(itl.const_pool);
    destroy_sym_table(itl.symbol_table);
    destroy_arr(itl.constant_decl);
    destroy_arr(itl.global_decl);
    destroy_arr(itl.global_alloc.array_allocation);
    
    destroy_ast(itl);

    destroy_func_table(itl.func_table);

    // destroy typing tables
    destroy_struct_table(itl.struct_table);
    destroy_enum_table(itl.enum_table);
    destroy_table(itl.type_table);
    destroy_arr(itl.alias_table);
    destroy_arr(itl.tmp_alias_table);

    destroy_allocator(itl.list_allocator);
    destroy_allocator(itl.string_allocator);

    
    for(u32 p = 0; p < count(itl.func_pointer); p++)
    {
        destroy_sig(*itl.func_pointer[p]);
    }

    destroy_arr(itl.func_pointer);

    destroy_allocator(itl.type_allocator);
}

static constexpr u32 LIST_INITIAL_SIZE = 16 * 1024;
static constexpr u32 STRING_INITIAL_SIZE = 4 * 1024;
static constexpr u32 TYPE_INITIAL_SIZE =  4 * 1024;

void setup_type_table(Interloper& itl)
{
    itl.type_table = make_table<String,TypeDecl>();

    // add all the builtin types  
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        add_type_decl(itl,i,TYPE_NAMES[i],type_kind::builtin);
    }
}

void check_startup_defs(Interloper& itl)
{   
    if(itl.rtti_enable)
    {
        cache_rtti_structs(itl);
    }

    check_startup_func(itl,"main");
    check_startup_func(itl,"start");

    check_startup_func(itl,"memcpy");
    check_startup_func(itl,"zero_mem");
}

void compile(Interloper &itl,const String& initial_filename)
{
    printf("compiling file: %s\n",initial_filename.buf);

    itl.error = false;
    itl.error_code = itl_error::none;

    itl.ast_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);
    itl.ast_string_allocator = make_allocator(STRING_INITIAL_SIZE);

    itl.string_allocator = make_allocator(STRING_INITIAL_SIZE);
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.type_allocator = make_allocator(TYPE_INITIAL_SIZE);

    itl.symbol_table.string_allocator = &itl.string_allocator;

    itl.func_table = make_func_table();
    itl.type_def = make_table<String,TypeDef>();

    setup_type_table(itl);

    // add an dummy error value as the first handle
    // see SYM_ERROR
    make_sym(itl,"ITL_ERROR",make_builtin(itl,builtin_type::void_t));

    // parse intial input file
    {
        // build ast
        const b32 parser_error = parse(itl,initial_filename);

        if(parser_error)
        {
            // flag as generic parser error
            if(itl.error_code == itl_error::none)
            {
                itl.error = true;
                itl.error_code = itl_error::parse_error;
            }

            destroy_itl(itl);
            return;
        }
    }

    if(itl.print_ast)
    {
        // print all type defs
        for(u32 b = 0; b < count(itl.type_def.buf); b++)
        {
            auto &bucket = itl.type_def.buf[b];

            for(u32 i = 0; i < count(bucket); i++)
            {
                auto& def = bucket[i].v;
                print(def.root);
            }
        }


        // print function defs
        for(u32 b = 0; b < count(itl.func_table.table.buf); b++)
        {
            auto &bucket = itl.func_table.table.buf[b];

            for(u32 i = 0; i < count(bucket); i++)
            {
                auto& func = bucket[i].v;

                print((AstNode*)func.root);
            }
        }
    }


    check_startup_defs(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    putchar('\n');

    // global scope
    new_scope(itl.symbol_table);

    // compile all our constant values 
    compile_constants(itl);
    compile_globals(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 
    compile_functions(itl);

    destroy_scope(itl.symbol_table);     

    // okay we dont need the parse tree anymore
    // free it
    destroy_ast(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }


    if(itl.optimise)
    {
        optimise_ir(itl);
    }
    
    if(itl.print_ir)
    {
        dump_sym_ir(itl);
    }

    switch(itl.arch)
    {
        case arch_target::x86_64_t:
        {
            rewrite_x86_ir(itl);
            break;
        }
    }

    if(itl.print_ir)
    {
        dump_sym_ir(itl);
    }

    // perform register allocation on used functions
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];

        allocate_registers(itl,func);

        if(itl.print_stack_allocation || itl.print_reg_allocation)
        {
            putchar('\n');
        }
    }

    if(itl.print_ir)
    {
        dump_reg_ir(itl);
    }

    // emit the actual target asm
    emit_asm(itl);

    switch(itl.os)
    {
        case os_target::linux_t:
        {
            emit_elf(itl);
            break;
        }
    }

    printf("OK\n\n");
}

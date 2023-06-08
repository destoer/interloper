#include <interloper.h>

Type* compile_expression(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);
std::pair<Type*, SymSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
void compile_decl(Interloper &itl,Function &func, const AstNode *line, b32 global = false);
void compile_block(Interloper &itl,Function &func,BlockNode *node);
BlockSlot compile_basic_block(Interloper &itl,Function &func,BlockNode *node);
void compile_if_block(Interloper &itl,Function &func,AstNode *node);
std::pair<Type*,SymSlot> compile_oper(Interloper& itl,Function &func,AstNode *node);

void write_arr(Interloper &itl,Function &func,AstNode *node,Type* write_type, u32 slot);
std::pair<Type*, SymSlot> read_arr(Interloper &itl,Function &func,AstNode *node, u32 dst_slot);
std::pair<Type*, SymSlot> index_arr(Interloper &itl,Function &func,AstNode *node, SymSlot dst_slot);
void traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,const SymSlot addr_slot, ArrayType* type, u32* offset);
std::pair<Type*,SymSlot> index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, SymSlot ptr_slot, SymSlot dst_slot);



void compile_move(Interloper &itl, Function &func, SymSlot dst_slot, SymSlot src_slot, const Type* dst_type, const Type* src_type);

std::pair<Type*,SymSlot> load_addr(Interloper &itl,Function &func,AstNode *node,SymSlot slot, bool addrof);
void do_ptr_load(Interloper &itl,Function &func,SymSlot dst_slot,SymSlot addr_slot, const Type* type, u32 offset);
SymSlot collapse_offset(Function&func, SymSlot addr_slot, u32 *offset);

void add_func(Interloper& itl, const String& name, FuncNode* root);

void alloc_slot(Function& func, const Reg& reg, b32 force_alloc);

SymSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
SymSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
SymSlot load_arr_data(Interloper& itl,Function& func,SymSlot slot, const Type* type);
SymSlot load_arr_len(Interloper& itl,Function& func,SymSlot slot, const Type* type);

#include "lexer.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "optimize.cpp"
#include "ir.cpp"
#include "struct.cpp"
#include "rtti.cpp"
#include "func.cpp"
#include "array.cpp"


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
                crash_and_burn("division by zero in eval const expr");
            }

            return v1 / v2;
        }

        default: print(node); unimplemented("eval const expr node"); break;
    }
}



TypeAlias make_alias(const String& name, const String& filename, Type* type)
{
    TypeAlias alias;
    alias.name = name;
    alias.filename = filename;
    alias.type = type;

    return alias;
}


void parse_alias_def(Interloper& itl, TypeDef& def)
{
    AliasNode* node = (AliasNode*)def.root;

    Type* type = get_complete_type(itl,node->type);

    if(itl.error)
    {
        return;
    }

    if(itl.print_types)
    {
        printf("type alias %s = %s\n",node->name.buf,type_name(itl,type).buf);
    }

    const u32 slot = count(itl.alias_table);

    const TypeAlias alias = make_alias(node->name,node->filename,type);

    // add the alias
    push_var(itl.alias_table,alias);

    add_type_decl(itl,slot,node->name,type_kind::alias_t);   
}



std::pair<Type*,SymSlot> symbol(Interloper &itl, AstNode *node)
{
    LiteralNode* lit_node = (LiteralNode*)node;

    const auto name = lit_node->literal;

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(!sym_opt)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
    }

    const auto &sym = sym_opt.value();

    return std::pair{sym.type,sym.reg.slot};
}


Type* value(Interloper& itl,Function& func,AstNode *node, SymSlot dst_slot)
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
SymSlot collapse_offset(Function&func, SymSlot addr_slot, u32 *offset)
{
    if(*offset)
    {
        const SymSlot final_addr = emit_res(func,op_type::add_imm,addr_slot,*offset);
        *offset = 0;

        return final_addr;
    }
    
    else
    {
        return addr_slot;
    }
}

void do_ptr_load(Interloper &itl,Function &func,SymSlot dst_slot,SymSlot addr_slot, const Type* type, u32 offset)
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


void do_ptr_store(Interloper &itl,Function &func,SymSlot src_slot,SymSlot addr_slot, const Type* type, u32 offset)
{
    const u32 size = type_size(itl,type);

    if(size <= sizeof(u32))
    {
        emit(func,store_ptr(src_slot,addr_slot,offset,size));  
    }

    else
    {
        SymSlot src_ptr = addrof_res(itl.symbol_table,func,src_slot);
        src_ptr = collapse_offset(func,src_ptr,&offset);        

        ir_memcpy(itl,func,addr_slot,src_ptr,type_size(itl,type));        
    } 
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

        // may get a back a fixed array as a oper and we want to do a conversion on it
        case ast_type::index:
        {
            return read_arr(itl,func,node,new_tmp_ptr(func));
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}



Type* compile_arith_op(Interloper& itl,Function &func,AstNode *node, op_type type, SymSlot dst_slot)
{
    BinNode* bin_node = (BinNode*)node;

    const auto [t1,v1] = compile_oper(itl,func,bin_node->left);
    const auto [t2,v2] = compile_oper(itl,func,bin_node->right);

    // pointer arith adds the size of the underlying type
    if(is_pointer(t1) && is_integer(t2))
    {
        if(type != op_type::sub_reg && type != op_type::add_reg)
        {
            panic(itl,itl_error::int_type_error,"Pointer arithmetic is only defined on subtraction and additon! %s : %s\n",type_name(itl,t1).buf,type_name(itl,t2).buf);
            return make_builtin(itl,builtin_type::void_t);
        }

        // get size of pointed to type
        Type *contained_type = deref_pointer(t1);

        const SymSlot offset_slot = emit_res(func,op_type::mul_imm,v2,type_size(itl,contained_type));
        emit(func,type,dst_slot,v1,offset_slot);
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
Type* compile_logical_op(Interloper& itl,Function &func,AstNode *node, logic_op type, SymSlot dst_slot)
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
void compile_move(Interloper &itl, Function &func, SymSlot dst_slot, SymSlot src_slot, const Type* dst_type, const Type* src_type)
{
    UNUSED(itl);
    // check the operation is even legal

    // can be moved by a simple data copy 
    // NOTE: we use this here so we dont have to care about the underyling type if its a pointer
    if(is_trivial_copy(dst_type) && is_trivial_copy(src_type))
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
        if(dst_slot.handle == RV_IR)
        {
            const SymSlot ptr = addrof_res(itl.symbol_table,func,src_slot);

            ir_memcpy(itl,func,func.args[0],ptr,type_size(itl,dst_type));
        } 

        else
        {
            const SymSlot src_ptr = addrof_res(itl.symbol_table,func,src_slot);
            const SymSlot dst_ptr = addrof_res(itl.symbol_table,func,dst_slot);

            ir_memcpy(itl,func,dst_ptr,src_ptr,type_size(itl,dst_type));
        }
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

        // compile the compare expr for conditon
        const auto [t,r] = compile_oper(itl,func,if_stmt->left);

        if(!is_bool(t))
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
            emit(func,op_type::exit_block);

            if(if_block->statements[n+1]->type == ast_type::else_t)
            {
                // else stmt has no expr so its in the first node
                // and by definition this is the last statement with no cond so we have to explictly splice a jump to it
                UnaryNode* else_stmt = (UnaryNode*)if_block->statements[n+1];
                const BlockSlot else_slot = compile_basic_block(itl,func,(BlockNode*)else_stmt->next);

                // add branch over body we compiled to else statement
                emit_cond_branch(func,cmp_block,else_slot,body_slot,r,false);

                exit_block = add_fall(itl,func,else_slot);
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
            exit_block = add_fall(itl,func,body_slot);

            // if cond not met just branch into exit block
            emit_cond_branch(func,cmp_block,exit_block,body_slot,r,false);          
        }
    }

    // now we have to exit block add in every exit
    auto &blocks = func.emitter.program;

    for(u32 b = start_block.handle; b < count(blocks) - 2; b++)
    {
        auto &block = func.emitter.program[b];

        if(block.list.end->opcode.op == op_type::exit_block)
        {
            remove(block.list,block.list.end);
            emit_branch(func,block_from_idx(b),exit_block);
        }
    }
}

void compile_while_block(Interloper &itl,Function &func,AstNode *node)
{
    const BlockSlot initial_block = cur_block(func);

    BinNode* while_node = (BinNode*)node;

    // compile cond
    const auto [t,stmt_cond_reg] = compile_oper(itl,func,while_node->left);

    if(!is_bool(t))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,t).buf);
        return;
    }    

    // compile body
    const BlockSlot while_block = compile_basic_block(itl,func,(BlockNode*)while_node->right); 

    const BlockSlot end_block = cur_block(func);

    SymSlot loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,while_node->left);

    const BlockSlot exit_block = new_basic_block(itl,func);

    // keep looping to while block if cond is true
    emit_cond_branch(func,end_block,while_block,exit_block,loop_cond_reg,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,while_block,stmt_cond_reg,false);   
}

void compile_for_block(Interloper &itl,Function &func,AstNode *node)
{
    // scope for any var decls in the stmt
    new_scope(itl.symbol_table);

    const BlockSlot initial_block = cur_block(func);

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
            compile_expression_tmp(itl,func,for_node->initializer);
            break;
        }
    }

    if(itl.error)
    {
        return;
    }
    

    const auto [t,stmt_cond_reg] = compile_oper(itl,func,for_node->cond);

    if(!is_bool(t))
    {
        panic(itl,itl_error::bool_type_error,"expected bool got %s in for condition\n",type_name(itl,t).buf);
        return;
    }    


    // compile the body
    const BlockSlot for_block = compile_basic_block(itl,func,for_node->block);    

    // compile loop end stmt
    compile_expression_tmp(itl,func,for_node->post);
    
    const BlockSlot end_block = cur_block(func);


    SymSlot loop_cond_reg;
    std::tie(std::ignore,loop_cond_reg) = compile_oper(itl,func,for_node->cond);

    const BlockSlot exit_block = new_basic_block(itl,func);

    emit_cond_branch(func,end_block,for_block,exit_block,loop_cond_reg,true);

    // emit branch over the loop body in initial block if cond is not met
    emit_cond_branch(func,initial_block,exit_block,for_block,stmt_cond_reg,false);

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
        panic(itl,itl_error::undeclared,"switch on enum %s missing cases:\n",itl.enum_table[type_idx].name.buf);
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
        const s32 min = switch_node->statements[0]->value;
        const s32 max = switch_node->statements[size - 1]->value;
        const u32 range = (max - min) + 1;


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
        emit(func,op_type::sub_imm,switch_slot,expr_slot,min);


        // out of range, branch to default
        const SymSlot default_cmp = emit_res(func,op_type::cmpugt_imm,switch_slot,max - min);

        // NOTE: branch is emitted later as we dont know where it goes yet

        // emit the switch table dispatch
        const BlockSlot dispatch_block = new_basic_block(itl,func);

        // mulitply to get a jump table index
        const SymSlot table_index = emit_res(func,op_type::mul_imm,switch_slot,GPR_SIZE);

        
        // reserve space for the table inside the constant pool
        const PoolSlot pool_slot = reserve_const_pool_section(itl.const_pool,pool_type::jump_table,GPR_SIZE * range);
        const SymSlot table_addr = pool_addr(func,pool_slot);

        // get address in the tabel we want
        const SymSlot final_offset = emit_res(func,op_type::add_reg,table_addr,table_index);

        // load the address out of the jump table
        const SymSlot target = new_tmp_ptr(func);
        emit(func,load_ptr(target, final_offset,0, GPR_SIZE,false));


        // branch on it
        emit(func,op_type::b_reg,target);

        // finally compile all the blocks, and populate the jump table

        // compile each stmt block
        for(u32 i = 0; i < size; i++)
        {
            CaseNode* case_node = switch_node->statements[i];

            const BlockSlot case_slot = compile_basic_block(itl,func,case_node->block);
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
        }

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
            add_block_exit(func,default_block,exit_block);
        }


        // we have default posisiton now we can emit the branch for the range checking failing
        emit_cond_branch(func,range_block,default_block,dispatch_block,default_cmp,true);



        // populate the jump table
        u32 case_idx = 0;

        
        const LabelSlot default_label = block_from_slot(func,default_block).label_slot; 

        for(u32 i = 0; i < range; i++)
        {
            const u32 addr = i * GPR_SIZE;

            // this is a non default case
            CaseNode* case_node = switch_node->statements[case_idx];

            static_assert(GPR_SIZE == sizeof(u32));

            // current jump table entry matches case
            if(case_node->value - min == i)
            {
                //printf("case %d -> %d\n",i,case_node->label);

                write_const_pool_label(itl.const_pool,pool_slot, addr, case_node->label);
                case_idx++;

                // add jump to the exit block
                emit_branch(func,case_node->end_block,exit_block);
            }

            // as statements as sorted this means there is no match emit default
            else
            {
                //printf("case %d -> default(%d)\n",i,default_label);

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
            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return std::pair{make_builtin(itl,builtin_type::void_t),SYM_ERROR};
            }

            const auto &sym = sym_opt.value();

            if(take_addr)
            {
                if(is_array(sym.type))
                {
                    assert(false);
                }

                Type* pointer_type = make_pointer(itl,sym.type);

                // actually  get the addr of the ptr
                addrof(itl.symbol_table,func,slot,sym.reg.slot);
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
                auto [type,ptr_slot,offset] = compute_member_addr(itl,func,node);
                ptr_slot = collapse_offset(func,ptr_slot,&offset);

                // make sure this ptr goes into the dst slot
                emit(func,op_type::mov_reg,slot,ptr_slot);

                // we actually want this as a pointer
                type = make_pointer(itl,type);

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
            
            compile_move(itl,func,dst_slot,slot,type,type);
            return type;
        }


        case ast_type::char_t:
        {
            CharNode* char_node = (CharNode*)node;

            emit(func,op_type::mov_imm,dst_slot,char_node->character);
            return make_builtin(itl,builtin_type::u8_t);
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
            const auto [type, slot] = read_arr(itl,func,node,dst_slot);

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
            do_ptr_load(itl,func,dst_slot,slot,type);
            return type;            
        }

        case ast_type::sizeof_t:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            // TODO: should this work with type names?
            const auto [type,slot] = compile_oper(itl,func,unary_node->next);


            const u32 size = type_size(itl,type);
            emit(func,op_type::mov_imm,dst_slot,size);

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
                return compile_arith_op(itl,func,node,op_type::add_reg,dst_slot);
            }
        }



        // multiple assigment
        case ast_type::equal:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto rtype = compile_expression(itl,func,bin_node->right,dst_slot);

            LiteralNode* lit_node = (LiteralNode*)bin_node->left;

            const auto name = lit_node->literal;

            const auto sym_opt = get_sym(itl.symbol_table,name);
            if(!sym_opt)
            {
                panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
                return make_builtin(itl,builtin_type::void_t);
            }

            const auto &sym = sym_opt.value();

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
                const auto [t,dst] = compile_oper(itl,func,unary_node->next);


                // TODO: make sure our optimiser sees through this
                const SymSlot slot = mov_imm(func,0);
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

            const auto [t,reg] = compile_oper(itl,func,unary_node->next);

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

            const auto [t,reg] = compile_oper(itl,func,unary_node->next);

            if(!is_bool(t))
            {
                panic(itl,itl_error::bool_type_error,"compile: logical_not expected bool got: %s\n",type_name(itl,t).buf);
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
                emit(func,op_type::mov_imm,dst_slot,enum_member->value);

                return make_enum_type(itl,enumeration);
            }

            // TODO: we dont have namespacing
            else 
            {
                assert(false);
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


void compile_decl(Interloper &itl,Function &func, const AstNode *line, b32 global)
{
    // get entry into symbol table
    const DeclNode* decl_node = (DeclNode*)line;

    const auto name = decl_node->name;
    const auto ltype = get_type(itl,decl_node->type);

    const auto sym_opt = get_sym(itl.symbol_table,name);
    if(sym_opt)
    {
        panic(itl,itl_error::redeclaration,"redeclared symbol: %s:%s\n",name.buf,type_name(itl,sym_opt.value().type).buf);
        return;
    }

    const auto size = type_size(itl,ltype);

    // add new symbol table entry
    Symbol &sym = global? add_global(itl.symbol_table,name,ltype,size) : add_symbol(itl.symbol_table,name,ltype,size);




    if(is_array(sym.type))
    {
        compile_arr_decl(itl,func,decl_node,sym);
    }

    else if(is_struct(sym.type))
    {
        compile_struct_decl(itl,func,decl_node,sym);
    }

    
    // simple type
    else 
    {
        alloc_slot(func,sym.reg,false);

        // initalizer
        if(decl_node->expr)
        {
            // normal assign
            const auto rtype = compile_expression(itl,func,decl_node->expr,sym.reg.slot);
            check_assign(itl,ltype,rtype,false,true);             
        }

        // default init
        else
        {
            // NOTE: atm, all standard types use 0 for default, we may need something more flexible
            emit(func,op_type::mov_imm,sym.reg.slot,default_value(sym.type));
        }
    } 
}

std::pair<Type*, SymSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    // assume a size then refine it with expr result
    const SymSlot dst_slot = new_tmp(func,GPR_SIZE);

    Type* type = compile_expression(itl,func,node,dst_slot);

    const u32 size = type_size(itl,type);

    assign_reg_size(func.registers[dst_slot.handle],size);

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

    // TODO: this should just be compile_expresison
    const auto [type,reg] = compile_oper(itl,func,auto_decl->expr);

    // add the symbol

    const auto size = type_size(itl,type);

    // add new symbol table entry
    const auto &sym = add_symbol(itl.symbol_table,name,type,size);

    alloc_slot(func,sym.reg,is_plain_type(type));
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


            case ast_type::auto_decl:
            {
                compile_auto_decl(itl,func,line);
                break;
            }


            // assignment
            case ast_type::equal:
            {
                BinNode* assign_node = (BinNode*)line;
                const auto [rtype,slot] = compile_oper(itl,func,assign_node->right);


                if(assign_node->left->type != ast_type::symbol)
                {
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
                        panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' assigned before declaration\n",name.buf);
                        break;
                    }

                    const auto &sym = sym_opt.value();

                    check_assign(itl,sym.type,rtype);

                    compile_move(itl,func,sym.reg.slot,slot,sym.type,rtype);
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

                        check_assign(itl,func.return_type[0],rtype);
                    }

                    // multiple return
                    else
                    {
                        if(count(record_node->nodes) != count(func.return_type))
                        {
                            panic(itl,itl_error::mismatched_args,"Invalid number of return parameters for function %s : %d != %d\n",
                                func.name.buf,count(record_node->nodes),count(func.return_type));
                            
                            return;
                        }
                        

                        for(u32 r = 0; r < count(func.return_type); r++)
                        {
                            // void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type, u32 offset = 0)
                            // NOTE: Pointers are in the first set of args i.e the hidden ones
                            const auto [rtype, ret_slot] = compile_oper(itl,func,record_node->nodes[r]);

                            // check each param
                            check_assign(itl,func.return_type[r],rtype);

                            do_ptr_store(itl,func,ret_slot,func.args[r],func.return_type[r]);
                        }
                    }
                
                }

                emit(func,op_type::ret);
                

                itl.has_return = true;
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
                compile_function_call(itl,func,line,sym_from_idx(NO_SLOT));
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



// -> add globals
// -> improve inserting static data 
// -> rtti
// -> va_args
// -> print

// -> impl static assert
// -> improve const expressions
// -> handle block args inside the reg allocator

// TODO: basic type checking for returning pointers to local's

// feature plan:
// function_pointers
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

    destroy_table(itl.type_def);
    destroy_arr(itl.global_def);

    itl.cur_expr = nullptr;
    itl.cur_file = ""; 
}

void destroy_itl(Interloper &itl)
{
    destroy_arr(itl.program);
    destroy_const_pool(itl.const_pool);
    destroy_sym_table(itl.symbol_table);
    
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
    destroy_struct_table(itl.struct_table);
    destroy_enum_table(itl.enum_table);
    destroy_table(itl.type_table);
    destroy_arr(itl.alias_table);

    destroy_allocator(itl.list_allocator);
    destroy_allocator(itl.string_allocator);
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

    check_func_exists(itl,"main");
    check_func_exists(itl,"start");
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

    itl.function_table = make_table<String,Function>();
    itl.type_def = make_table<String,TypeDef>();

    setup_type_table(itl);

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


    check_startup_defs(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }


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
        destroy_itl(itl);
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

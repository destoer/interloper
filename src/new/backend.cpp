#include <interloper.h>
#include "backend/op_table.inl"

#include "backend/pool.cpp"
#include "backend/emitter.cpp"
#include "backend/rewrite_arch_ir.cpp"
#include "backend/cfg.cpp"
#include "backend/asm.cpp"
#include "backend/x86_emitter.cpp"
#include "backend/stack_allocator.cpp"
#include "backend/linear_alloc.cpp"
#include "backend/disass.cpp"
#include "backend/ir_x86.cpp"
#include "backend/elf.cpp"
#include "backend/intrin.cpp"
#include "backend/ir.cpp"
#include "backend/memory.cpp"



TypedReg compile_oper(Interloper& itl,Function &func,AstNode *node);
Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot);
AddrSlot generate_indexed_pointer(Interloper& itl, Function& func, RegSlot base, RegSlot index, u32 scale, u32 offset);
void collapse_struct_addr(Interloper& itl, Function& func, RegSlot dst_slot, const AddrSlot struct_slot);

Type* compile_function_call(Interloper& itl, Function& func, FuncCallNode* call_node, RegSlot dst_slot);
void compile_return(Interloper &itl,Function &func, RetNode* ret_node);

Option<itl_error> func_graph_pass(Interloper& itl, Function& func)
{
    // if final block has no return and this is a void func insert one
    if(is_void(func.sig.return_type[0]))
    {    
        auto& end_block = block_from_slot(func,cur_block(func));

        if(!has_func_exit(func,end_block.block_slot))
        {
            ret(itl,func);
        }
    }

    // connect up the cfg
    connect_flow_graph(itl,func); 

    // do liveness analysis
    compute_var_live(itl,func);


    // now check a function exit is reachable from the entry block of the function
    // for a void func this should allways be possible as everything should hit the bottom return
    // that does not have an early return...

    auto& start_block = func.emitter.program[0];

    // check the start block can reach one
    if(!can_reach_exit(start_block))
    {
        auto& label = label_from_slot(itl.symbol_table.label_lookup,start_block.label_slot);

        itl.ctx.expr = (AstNode*)func.root;
        return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s",label.name.buf); 
    }

    for(BlockSlot slot : start_block.links)
    {
        // TODO: have this print the source line of the block
        if(!can_reach_exit(func,slot))
        {
            auto& block = block_from_slot(func,slot);
            auto& label = label_from_slot(itl.symbol_table.label_lookup,block.label_slot);

            itl.ctx.expr = (AstNode*)func.root;   
            dump_ir_sym(itl,func,itl.symbol_table);
            return compile_error(itl,itl_error::missing_return,"[COMPILE]: not all paths return in function at: %s",label.name.buf);
        }
    }

    return option::none;
}

void setup_passing_convention(Interloper& itl, Function& func)
{
    lock_reg_set(itl,func,func.sig.locked_set);

    // Setup calling convention
    for(u32 a = 0; a < count(func.sig.args); a++)
    {
        const SymSlot slot = func.sig.args[a];
        const u32 arg_reg = func.sig.pass_as_reg[a];

        if(arg_reg != NON_ARG)
        {
            const spec_reg arg = spec_reg(SPECIAL_REG_ARG_START + arg_reg);
            mov_unlock(itl,func,make_sym_reg_slot(slot),make_spec_reg_slot(arg));
        }
    }
}

bool emit_known_rvalue(Interloper& itl, Function& func, arith_bin_op arith,RegSlot dst_slot, const TypedReg& left, Type* rtype, u64 value)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(arith)];
    const auto sign = is_signed(left.type) && is_signed(rtype);

    switch(arith)
    {
        case arith_bin_op::div_t:
        {
            if(sign)
            {
                return false;
            }

            udiv_imm(itl,func,dst_slot,left.slot,value);
            return true;
        }

        case arith_bin_op::mod_t:
        {
            if(sign)
            {
                return false;         
            }

            umod_imm(itl,func,dst_slot,left.slot,value);
            return true;      
        }

        case arith_bin_op::mul_t:
        {
            mul_imm(itl,func,dst_slot,left.slot,value);
            return true;
        }

        default:
        {
            const op_type type = arith_info.imm_form;
            if(type == op_type::none)
            {
                return false;
            }

            emit_imm3_unchecked(itl,func,type,dst_slot,left.slot,value);
            return true; 
        }
    }

    return false;
}


void emit_integer_ir(Interloper& itl, Function& func, arith_bin_op arith, RegSlot dst_slot, TypedReg left, TypedReg right)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(arith)];

    const bool sign = is_signed(left.type);

    const op_type type = sign? arith_info.reg_signed_form : arith_info.reg_unsigned_form;
    emit_reg3_unchecked(itl,func,type,dst_slot,left.slot,right.slot);
}

void print_internal(Interloper& itl, const AstNode *root, int depth);;


void emit_integer_arith(Interloper& itl, Function& func,ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    TypedReg left = {};
    TypedReg right = {};

    if(node->right->known_value)
    {
        const auto value = *node->right->known_value;
        left = compile_oper(itl,func,node->left);
        if(emit_known_rvalue(itl,func,node->oper,dst_slot,left,node->right->expr_type,value))
        {
            return;
        } 
    }

    // If this is commutative we can just switch the operands
    else if(node->left->known_value && arith_info.commutative)
    {
        const auto value = *node->left->known_value;
        right = compile_oper(itl,func,node->right);
        if(emit_known_rvalue(itl,func,node->oper,dst_slot,right,node->left->expr_type,value))
        {
            return;
        } 
    }

    emit_integer_ir(itl,func,node->oper,dst_slot,left,right);
}

void emit_float_arith(Interloper& itl, Function& func, ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    const auto left = compile_oper(itl,func,node->left);
    const auto right = compile_oper(itl,func,node->right);

    const op_type type = arith_info.float_form;
    emit_reg3_unchecked(itl,func,type,dst_slot,left.slot,right.slot);
}


void emit_pointer_arith(Interloper& itl, Function& func, ArithBinNode* node, RegSlot dst_slot)
{
    const ArithmeticInfo& arith_info = ARITH_INFO[u32(node->oper)];

    // get size of pointed to type
    Type *contained_type = deref_pointer(node->left->expr_type);
    const u32 size = type_size(itl,contained_type);

    const auto left = compile_oper(itl,func,node->left);

    if(node->right->known_value)
    {
        const auto value = *node->right->known_value * size;
        emit_imm3_unchecked(itl,func,arith_info.imm_form,dst_slot,left.slot,value);
    }

    else
    {
        const auto right = compile_oper(itl,func,node->right);

        if(node->oper == arith_bin_op::sub_t)
        {
            const RegSlot offset_slot = mul_imm_res(itl,func,right.slot,size);
            emit_reg3<op_type::sub_reg>(itl,func,dst_slot,left.slot,offset_slot);
        }

        else
        {
            const AddrSlot addr = generate_indexed_pointer(itl,func,left.slot,right.slot,size,0);
            collapse_struct_addr(itl,func,dst_slot,addr);
        }
    }
}

Type* compile_arith_bin(Interloper& itl, Function& func, ArithBinNode* node, RegSlot dst_slot)
{
    const auto type = node->node.expr_type;

    // TODO: Consider switching on an enum saved during type checking.

    // pointer arith adds the size of the underlying type
    if(is_pointer(type))
    {
        emit_pointer_arith(itl,func,node,dst_slot);
    }

    // allow pointer subtraction
    else if(is_integer(type) || is_bool(type))
    {
        emit_integer_arith(itl,func,node,dst_slot);
    }

    // floating point arith
    else if(is_float(type))
    {
        emit_float_arith(itl,func,node,dst_slot);
    }

    else
    {
        compile_panic(itl,itl_error::int_type_error,"Cannot perform arithmetic operations on %t",type);
    }
    
    return type;
}

TypedReg compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    const RegSlot dst_slot = new_typed_tmp(itl,func,node->expr_type);
    return TypedReg{dst_slot, compile_expression(itl,func,node,dst_slot)};
}


// for compiling operands i.e we dont care where it goes as long as we get something!
// i.e inside operators, function args, the call is responsible for making sure it goes in the right place
// NOTE: this returns out fixed array pointers and may require conversion by caller!
TypedReg compile_oper(Interloper& itl,Function &func,AstNode *node)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_oper");
    }

    // for error printing
    itl.ctx.expr = node;

    // Value is known just return out
    if(node->known_value) 
    {
        const auto value = *node->known_value;
        const RegSlot dst_slot = new_typed_tmp(itl,func,node->expr_type);
        mov_imm(itl,func,dst_slot,value);
        return TypedReg {dst_slot,node->expr_type};
    }


    switch(node->type)
    {
        // just want symbol name out without copying it
        case ast_type::symbol:
        {
            SymbolNode* sym_node  = (SymbolNode*)node;
            const auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);

            return typed_reg(sym);
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}

Type* compile_symbol(Interloper& itl, Function& func, SymbolNode* sym_node, RegSlot dst_slot)
{
    auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);

    const TypedReg src = typed_reg(sym);
    const TypedReg dst = {dst_slot,src.type};

    compile_move(itl,func,dst,src);

    return sym.type;
}

Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_expression");
    }

    log(itl.itl_log,"(%s:%d) Compiling expression %s\n",itl.ctx.filename.buf,node->idx,AST_NAMES[u32(node->type)]);

    itl.ctx.expr = node;
   
    // Value is known just return out
    if(node->known_value) 
    {
        const auto value = *node->known_value;
        mov_imm(itl,func,dst_slot,value);
        return node->expr_type;
    }


    switch(node->type)
    {
        case ast_type::symbol:
        {
            return compile_symbol(itl,func,(SymbolNode*)node,dst_slot);
        }

        case ast_type::arith_bin:
        {
            return compile_arith_bin(itl,func,(ArithBinNode*)node,dst_slot);
        }

        case ast_type::function_call:
        {
            return compile_function_call(itl,func,(FuncCallNode*)node,dst_slot);
        }

        default:
        {
            compile_panic(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'",AST_NAMES[u32(node->type)]);
            break;
        }
    }

    assert(false);
}

void compile_basic_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& sym)
{
    const auto slot = sym.reg.slot;

    alloc_slot(itl,func,slot,false);
    
    if(decl_node->expr->type == ast_type::no_init)
    {
        return;
    }

    // No initializer
    if(!decl_node->expr)
    {
        if(is_float(sym.type))
        {
            movf_imm(itl,func,slot,0.0);
        }

        else
        {
            mov_imm(itl,func,slot,0);
        }
        return;
    }


    // normal assign
    compile_expression(itl,func,decl_node->expr,slot);

    if(is_unsigned_integer(sym.type))
    {
        clip_arith_type(itl,func,slot,slot,sym.reg.size);
    }
}

void compile_decl(Interloper &itl,Function &func, DeclNode* decl_node)
{
    auto& sym = sym_from_slot(itl.symbol_table,decl_node->sym_slot);


    switch(sym.type->kind)
    {
        case type_class::array_t:
        {   
            unimplemented("Array");
            break;
        }

        case type_class::struct_t:
        {
            unimplemented("Struct");
            break;
        }

        default:
        {
            compile_basic_decl(itl,func,decl_node,sym);
            break;
        }
    }
}

void compile_block(Interloper& itl, Function& func,AstBlock& block)
{
    for(AstNode* stmt : block.statement)
    {
        itl.ctx.expr = stmt;

        switch(stmt->type)
        {
            // variable declaration
            case ast_type::decl:
            {
                compile_decl(itl,func,(DeclNode*)stmt);
                break;
            }

            case ast_type::ret:
            {
                compile_return(itl,func,(RetNode*)stmt);
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,(FuncCallNode*)stmt,make_spec_reg_slot(spec_reg::null));
                break;
            }

            default:
            {
                compile_panic(itl,itl_error::invalid_expr,"[COMPILE]: Unknown statement: %s",AST_NAMES[u32(stmt->type)]);
                break;
            }
        }
    }
}

Option<itl_error> compile_function(Interloper& itl, Function& func)
{
    // Dummy function just output a block and setup the graph
    if(!func.root)
    {
        // produce a dummy basic block
        if(count(func.emitter.program) == 0)
        {
            new_basic_block(itl,func);
        }

        return func_graph_pass(itl,func);
    }

    new_basic_block(itl,func);
    setup_passing_convention(itl,func);

    compile_block(itl,func,func.root->block);
    return func_graph_pass(itl,func);
}

Option<itl_error> compile_functions(Interloper& itl)
{
    for(Function* func : itl.func_table.used)
    {
        const auto func_err = compile_function(itl,*func);
        if(func_err)
        {
            return func_err;
        }
    }

    return option::none;
}

void dump_sym_ir(Interloper &itl)
{
    for(u32 f = 0; f < count(itl.func_table.used); f++)
    {
        auto& func = *itl.func_table.used[f];
        dump_ir_sym(itl,func,itl.symbol_table);
    }
}

Option<itl_error> backend(Interloper& itl)
{
    const auto func_err = compile_functions(itl);
    if(func_err)
    {
        return func_err;
    }

    if(itl.print_ir)
    {
        dump_sym_ir(itl);
    }

    return option::none;
}
#include <interloper.h>

// #include "backend/op_table.inl"
#include "backend/pool.cpp"
// #include "backend/emitter.cpp"
#include "backend/reg.cpp"
// #include "backend/rewrite_arch_ir.cpp"
// #include "backend/cfg.cpp"
#include "backend/asm.cpp"
// #include "backend/x86_emitter.cpp"
#include "backend/stack_allocator.cpp"
// #include "backend/linear_alloc.cpp"
// #include "backend/disass.cpp"
// #include "backend/ir_x86.cpp"
// #include "backend/elf.cpp"
#include "backend/intrin.cpp"
// #include "backend/ir.cpp"
#include "backend/memory.cpp"

TypedReg compile_oper(Interloper& itl,Function &func,AstNode *node);
Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot);
AddrSlot generate_indexed_pointer(Interloper& itl, Function& func, RegSlot base, RegSlot index, u32 scale, u32 offset);
void collapse_struct_addr(Interloper& itl, Function& func, RegSlot dst_slot, const AddrSlot struct_slot);

void compile_struct_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& sym);
void write_struct(Interloper& itl, Function& func, TypedReg src, StructAccessNode* struct_access);

void compile_array_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& array);
TypedAddr index_arr(Interloper& itl, Function& func, IndexNode* index);

#include "func/compile.cpp"
#include "arith/compile.cpp"



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
    // for a void func this should always be possible as everything should hit the bottom return
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

TypedReg compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    const auto dst = new_typed_tmp(itl,func,node->expr_type);
    compile_expression(itl,func,node,dst.slot);

    return dst;
}


TypedReg typed_reg_from_sym(Interloper& itl, Function& func, SymbolNode* sym_node)
{
    UNUSED(func);
    if(sym_node->type == sym_node_type::sym_slot)
    {
        const auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);
        return typed_reg(sym);
    }

    unimplemented("Func pointer sym");
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

    switch(node->known_value.type)
    {
        case known_value_type::gpr_t:
        {
            const auto dst = new_typed_tmp(itl,func,node->expr_type);
            mov_imm(itl,func,dst.slot,node->known_value.gpr);
            return dst;
        }

        case known_value_type::fpr_t:
        {
            const RegSlot dst_slot = new_float(func);
            movf_imm(itl,func,dst_slot,node->known_value.fpr);
            return TypedReg {dst_slot,node->expr_type};
        }

        case known_value_type::none_t: break;
    }


    switch(node->type)
    {
        // just want symbol name out without copying it
        case ast_type::symbol:
        {
            return typed_reg_from_sym(itl,func,(SymbolNode*)node);
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}

void compile_symbol(Interloper& itl, Function& func, AstNode* expr, RegSlot dst_slot)
{
    SymbolNode* sym_node = (SymbolNode*)expr;

    const auto src = typed_reg_from_sym(itl,func,sym_node);
    const TypedReg dst = {dst_slot,src.type};

    compile_move(itl,func,dst,src);
}

void compile_expr_unk(Interloper& itl, Function& func, AstNode* node, RegSlot dst_slot)
{
    UNUSED(func); UNUSED(dst_slot);
    (void)compile_panic(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'",AST_INFO[u32(node->type)].name);
}

Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_expression");
    }

    log(itl.itl_log,"(%s:%d) Compiling expression %s\n",itl.ctx.filename.buf,node->idx,AST_INFO[u32(node->type)].name);

    itl.ctx.expr = node;
   
    switch(node->known_value.type)
    {
        case known_value_type::gpr_t:
        {
            mov_imm(itl,func,dst_slot,node->known_value.gpr);
            return node->expr_type;
        }

        case known_value_type::fpr_t:
        {
            movf_imm(itl,func,dst_slot,node->known_value.fpr);
            return node->expr_type;
        }

        case known_value_type::none_t: break;
    }

    const auto& ast_info = AST_INFO[u32(node->type)];
    ast_info.compile_expr(itl,func,node,dst_slot);
    return node->expr_type;
}

void compile_basic_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Symbol& sym)
{
    const auto slot = sym.reg.slot;

    alloc_slot(itl,func,slot,false);
    
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

    // Explicitly uninitialized do not assign anything
    if(decl_node->expr->type == ast_type::no_init)
    {
        return;
    }


    // normal assign
    compile_expression(itl,func,decl_node->expr,slot);

    // Unknown quantity must be clipped
    if(is_unsigned_integer(sym.type) && !known_gpr_node(decl_node->expr))
    {
        clip_arith_type(itl,func,slot,slot,sym.reg.size);
    }
}

void compile_decl(Interloper &itl,Function &func,AstNode* stmt)
{
    DeclNode* decl_node = (DeclNode*)stmt;

    auto& sym = sym_from_slot(itl.symbol_table,decl_node->sym.slot);


    switch(sym.type->kind)
    {
        case type_class::array_t:
        {   
            compile_array_decl(itl,func,decl_node,sym);
            break;
        }

        case type_class::struct_t:
        {
            compile_struct_decl(itl,func,decl_node,sym);
            break;
        }

        default:
        {
            compile_basic_decl(itl,func,decl_node,sym);
            break;
        }
    }
}

void compile_auto_decl(Interloper &itl,Function &func, AstNode* stmt)
{
    AutoDeclNode* auto_decl = (AutoDeclNode*)stmt;

    auto& sym = sym_from_slot(itl.symbol_table,auto_decl->sym.slot);

    // save the alloc node so we can fill the info in later
    alloc_slot(itl,func,sym.reg.slot,!is_plain_type(sym.type));
    compile_expression(itl,func,auto_decl->expr,sym.reg.slot);
}

void compile_assign(Interloper& itl, Function& func, AstNode* stmt)
{
    AssignNode *assign = (AssignNode*)stmt;

    switch(assign->left->type)
    {
        case ast_type::symbol:
        {
            SymbolNode* sym_node = (SymbolNode*)assign->left;
            auto& sym = sym_from_slot(itl.symbol_table,sym_node->sym_slot);


            const RegSlot slot = sym.reg.slot;
            const u32 size = sym.reg.size;
            const Type *ltype = sym.type;

            compile_expression(itl,func,assign->right,slot);

            // If we have assigned an unknown quantity it must be clipped.
            if(is_unsigned_integer(ltype) && !known_gpr_node(assign->right))
            {
                clip_arith_type(itl,func,slot,slot,size);
            }
            break;
        }

        case ast_type::deref:
        {
            DerefNode* deref = (DerefNode*)assign->left;
            const auto src = compile_oper(itl,func,assign->right);
            auto ptr = compile_oper(itl,func,deref->expr);

            // store into the pointer
            ptr.type = deref_pointer(ptr.type); 
            do_ptr_store(itl,func,src,ptr);
            break;
        }

        case ast_type::struct_access:
        {
            const auto src = compile_oper(itl,func,assign->right);
            write_struct(itl,func,src,(StructAccessNode*)assign->left);
            break;
        }

        case ast_type::index:
        {
            const auto dst = index_arr(itl,func,(IndexNode*)assign->left);
            const auto src = compile_oper(itl,func,assign->right);

            do_addr_store(itl,func,src,dst);
            break;
        }

        case ast_type::ignore:
        {
            compile_expression(itl,func,assign->right,new_tmp(func,GPR_SIZE));
            break;
        }

        default:
        {
            (void)compile_panic(itl,itl_error::invalid_expr,"could not assign to expr: %s",AST_INFO[u32(assign->left->type)].name);
            break;
        }
    }
}

void compile_stmt_unk(Interloper& itl, Function& func, AstNode* node)
{
    UNUSED(func);
    (void)compile_panic(itl,itl_error::invalid_expr,"[COMPILE]: Unknown statement: %s",AST_INFO[u32(node->type)].name);   
}

void compile_block(Interloper& itl, Function& func,AstBlock& block)
{
    for(AstNode* stmt : block.statement)
    {
        itl.ctx.expr = stmt;
        const auto& ast_info = AST_INFO[u32(stmt->type)];
        ast_info.compile_stmt(itl,func,stmt);
    }
}

void compile_block_stmt(Interloper& itl, Function& func, AstNode* stmt)
{
    BlockNode* nested_block = (BlockNode*)stmt;
    compile_block(itl,func,nested_block->block);
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
    auto start = std::chrono::high_resolution_clock::now();

    for(Function* func : itl.func_table.used)
    {
        const auto func_err = compile_function(itl,*func);
        if(func_err)
        {
            return func_err;
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    itl.code_gen_time = std::chrono::duration<double, std::milli>(end-start).count();

    return option::none;
}

void dump_sym_ir(Interloper &itl)
{
    for(Function* func : itl.func_table.used)
    {
        dump_ir_sym(itl,*func,itl.symbol_table);
    }
}

void dump_reg_ir(Interloper &itl)
{
    for(Function* func : itl.func_table.used)
    {
        dump_ir_reg(itl,*func,itl.symbol_table);
    }
}

void compile_globals(Interloper& itl)
{
    Function& func = *itl.global_func;

    for(GlobalDeclNode* decl_node : itl.global_decl)
    {
        compile_decl(itl,func,(AstNode*)decl_node->decl);
    }
}

Option<itl_error> backend(Interloper& itl, const String& executable_path)
{
    compile_globals(itl);

    const auto func_err = compile_functions(itl);
    if(func_err)
    {
        return func_err;
    }

    if(itl.print_ir)
    {
        dump_sym_ir(itl);
    }
    
    auto start = std::chrono::high_resolution_clock::now();

    auto end = std::chrono::high_resolution_clock::now();
    itl.backend_time = std::chrono::duration<double, std::milli>(end-start).count();

    printf("OK\n\n");

    return option::none;
}
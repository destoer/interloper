#include <interloper.h>

TypeResult compile_expression(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);
RegResult compile_expression_tmp(Interloper &itl,Function &func,AstNode *node);
Option<itl_error> compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
Option<itl_error> compile_decl(Interloper &itl,Function &func,AstNode *line, b32 global = false);
Option<itl_error> compile_block(Interloper &itl,Function &func,BlockNode *node);
Result<BlockSlot,itl_error> compile_basic_block(Interloper &itl,Function &func,BlockNode *node);

RegResult compile_oper(Interloper& itl,Function &func,AstNode *node);
RegResult compile_oper_const_elide(Interloper& itl,Function& func, AstNode* node);
void unelide_value(Interloper& itl, Function& func, TypedReg& reg);
void unelide_values(Interloper& itl, Function& func, TypedReg& left, TypedReg& right);

TypedAddrResult index_arr(Interloper &itl,Function &func,AstNode *node);
Option<itl_error> traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,AddrSlot* addr_slot, ArrayType* type);
TypedAddrResult index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, RegSlot ptr_slot);

Option<itl_error> compile_move(Interloper &itl, Function &func, const TypedReg& dst, const TypedReg& src);
RegResult take_pointer(Interloper& itl,Function& func, AstNode* deref_node);
void add_func(Interloper& itl, const String& name, NameSpace* name_space, FuncNode* root);

RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_data(Interloper& itl,Function& func,const TypedReg& reg);
RegSlot load_arr_len(Interloper& itl,Function& func,const TypedReg& reg);

RegResult symbol(Interloper &itl, AstNode *node);

Option<itl_error> compile_init_list(Interloper& itl, Function& func, Type* ltype, AddrSlot addr_slot, AstNode* node);

#include "lexer.cpp"
#include "namespace.cpp"
#include "symbol.cpp"
#include "parser.cpp"
#include "ir.cpp"
#include "elf.cpp"
#include "optimize.cpp"
#include "memory.cpp"
#include "struct.cpp"
#include "enum.cpp"
#include "rtti.cpp"
#include "func/decl.cpp"
#include "array.cpp"
#include "constant.cpp"
#include "control_flow.cpp"
#include "arith.cpp"

void push_context(Interloper& itl)
{
    push_var(itl.saved_ctx,itl.ctx);
}

void pop_context(Interloper& itl)
{
    itl.ctx = pop(itl.saved_ctx);
}

void trash_context(Interloper& itl, FileContext& ctx)
{
    itl.ctx = ctx;
}

void trash_context(Interloper& itl, String filename,NameSpace* name_space, AstNode* expr)
{
    itl.ctx.name_space = name_space;
    itl.ctx.filename = filename;
    itl.ctx.expr = expr;
}

// save and overwrite the ctx
FileContextGuard switch_context(Interloper& itl, String filename,NameSpace* name_space, AstNode* expr)
{
    push_context(itl);
    trash_context(itl,filename,name_space,expr);

    return FileContextGuard(itl);
}


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


RegResult symbol_internal(Interloper &itl, AstNode *node, NameSpace* name_space)
{
    LiteralNode* lit_node = (LiteralNode*)node;

    const auto name = lit_node->literal;

    const auto sym_ptr = get_sym_internal(itl.symbol_table,name,name_space);
    if(!sym_ptr)
    {
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration",name.buf);
    }

    const auto &sym = *sym_ptr;

    return typed_reg(sym);
}

RegResult symbol(Interloper &itl, AstNode *node)
{
    return symbol_internal(itl,node,nullptr);
}

RegResult scoped_symbol(Interloper &itl, AstNode *node, NameSpace* name_space)
{
    return symbol_internal(itl,node,name_space);
}

Type* value(Interloper& itl,Function& func,AstNode *node, RegSlot dst_slot)
{
    ValueNode* value_node = (ValueNode*)node;
    Value value = value_node->value;

    mov_imm(itl,func,dst_slot,value.v);
    return value_type(itl,value);    
}


TypedReg value_tmp(Interloper& itl, Function& func, AstNode* node,)
{
    ValueNode* value_node = (ValueNode*)node;
    Value value = value_node->value;

    Type* type = value_type(itl,value);
    RegSlot dst_slot = new_typed_tmp(itl,func,type);


    return make_known_reg(dst_slot,type,value.v,known_type);  
}

void unelide_value(Interloper& itl, Function& func, TypedReg &reg)
{
    if(reg.flags & TYPED_REG_FLAG_ELIDED_VALUE)
    {
        mov_imm(itl,func,reg.slot,reg.known_value);
        reg.flags &= ~TYPED_REG_FLAG_KNOWN_VALUE;
    }
}

void unelide_values(Interloper& itl, Function& func, TypedReg &left, TypedReg &right)
{
    unelide_value(itl,func,left);
    unelide_value(itl,func,right);
}


// for compiling operands i.e we dont care where it goes as long as we get something!
// i.e inside operators, function args, the call is responsible for making sure it goes in the right place
// NOTE: this returns out fixed array pointers and may require conversion by caller!
RegResult compile_oper_internal(Interloper& itl,Function &func,AstNode *node, known_value_type known_type)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_oper");
    }

    // for error printing
    itl.ctx.expr = node;


    switch(node->type)
    {
        // just want symbol name out without copying it
        case ast_type::symbol:
        {
            assert(false);
        }

        case ast_type::value:
        {
            assert(false);
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}

RegResult compile_oper(Interloper& itl,Function &func,AstNode *node)
{
    return compile_oper_internal(itl,func,node,known_value_type::stored);
}

RegResult compile_oper_const_elide(Interloper& itl,Function& func, AstNode* node)
{
    return compile_oper_internal(itl,func,node,known_value_type::elided);
}

Option<itl_error> compile_scoped_stmt(Interloper& itl, Function& func, AstNode* node, NameSpace* name_space)
{
    switch(node->type)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Scope is not valid for stmt: %s",AST_INFO[u32(node->type)].name);
        }
    }

    return option::none;
}

TypeResult compile_scoped_expression(Interloper& itl, Function& func, AstNode* node, RegSlot dst_slot, NameSpace* name_space)
{
    switch(node->type)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Scope is not valid for expression: %s",AST_INFO[u32(node->type)].name);
        }
    }
}




TypeResult compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_expression");
        return make_builtin(itl,builtin_type::void_t);
    }

    log(itl.itl_log,"(%s:%d) Compiling experssion %s\n",itl.ctx.filename.buf,node->idx,AST_INFO[u32(node->type)].name);

    itl.ctx.expr = node;
   
    switch(node->type)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'",AST_INFO[u32(node->type)].name);
        }
    }
}

Option<itl_error> compile_basic_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Type* ltype, SymSlot slot)
{
    const auto reg_slot = make_sym_reg_slot(slot);
    alloc_slot(itl,func,reg_slot,false);
    
    // No initalizer
    if(!decl_node->expr)
    {
        if(is_reference(ltype))
        {
            return compile_error(itl,itl_error::pointer_type_error,"References must have an explicit initializer: %s",type_name(itl,ltype).buf);
        }

        else if(is_float(ltype))
        {
            movf_imm(itl,func,reg_slot,0.0);
        }

        else
        {
            mov_imm(itl,func,reg_slot,0);
        }

        return option::none;
    }

    if(decl_node->expr->type == ast_type::no_init)
    {
        return option::none;
    }

    // normal assign
    const auto rtype_res = compile_expression(itl,func,decl_node->expr,reg_slot);
    if(!rtype_res)
    {
        return rtype_res.error();
    }

    const Type* rtype = *rtype_res;

    // our symbol reference might have moved because of compile_expression
    auto &sym = sym_from_slot(itl.symbol_table,slot);

    if(is_unsigned_integer(sym.type))
    {
        clip_arith_type(itl,func,sym.reg.slot,sym.reg.slot,sym.reg.size);
    }

    return check_assign_init(itl,ltype,rtype);
}

Option<itl_error> compile_decl(Interloper &itl,Function &func, AstNode *line, b32 global)
{
    // get entry into symbol table
    const DeclNode* decl_node = (DeclNode*)line;

    const auto name = decl_node->name;
    const auto ltype_res = get_type(itl,decl_node->type);
    if(!ltype_res)
    {
        return ltype_res.error();
    }

    Type* ltype = *ltype_res;

    const auto sym_ptr = get_sym(itl.symbol_table,name);

    if(sym_ptr)
    {
        return compile_error(itl,itl_error::redeclaration,"redeclared symbol: %s:%s",name.buf,type_name(itl,sym_ptr->type).buf);
    }

    SymSlot slot = {INVALID_HANDLE};

    // add new symbol table entry
    {
        // dont hold this sym reference as its no doubt going to be invalidated
        // as we are actively compiling expressions
        Symbol &sym = global? add_global(itl,name,ltype,false) : add_symbol(itl,name,ltype);
        slot = sym.reg.slot.sym_slot;
    }


    switch(ltype->kind)
    {
        case type_class::array_t:
        {   
            const auto decl_err = compile_arr_decl(itl,func,decl_node,slot);
            if(decl_err)
            {
                return decl_err;
            }
            break;
        }

        case type_class::struct_t:
        {
            const auto struct_err = compile_struct_decl(itl,func,decl_node,slot);
            if(struct_err)
            {
                return struct_err;
            }
            break;
        }

        default:
        {
            const auto decl_err = compile_basic_decl(itl,func,decl_node,ltype,slot);
            if(decl_err)
            {
                return decl_err;
            }
            break;
        }
    }

    // need to perform sizing AFTER initalizers have been parsed
    // just in case we need to do any size deduction
    if(global)
    {
        auto &sym = sym_from_slot(itl.symbol_table,slot);
        reserve_global_alloc(itl,sym);
    }

    return option::none;
}

void update_tmp_typing(Interloper& itl, Function& func, RegSlot dst_slot, const Type* type)
{
    func.registers[dst_slot.tmp_slot.handle] = make_reg(itl,dst_slot,type);
}

RegResult compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    // assume a size then refine it with expr result
    const RegSlot dst_slot = new_tmp(func,GPR_SIZE);

    auto type_res = compile_expression(itl,func,node,dst_slot);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;
    update_tmp_typing(itl,func,dst_slot,type);

    return TypedReg{dst_slot,type};
}





Option<itl_error> compile_auto_decl(Interloper &itl,Function &func, const AstNode *line)
{
    AutoDeclNode* auto_decl = (AutoDeclNode*)line;

    const auto name = auto_decl->name;

    if(get_sym(itl.symbol_table,name))
    {
        return compile_error(itl,itl_error::redeclaration,"redeclared symbol: %s",name.buf);
    }

    // add the symbol
    SymSlot sym_slot = {INVALID_HANDLE};

    // add new symbol table entry
    {
        const auto &sym = add_symbol(itl,name,make_builtin(itl,builtin_type::void_t));
        sym_slot = sym.reg.slot.sym_slot;
    }

    const auto reg_slot = make_sym_reg_slot(sym_slot);

    // save the alloc node so we can fill the info in later
    OpcodeNode* alloc = alloc_slot(itl,func,reg_slot,false);

    // compile the expression so we can get the type!
    auto type_res = compile_expression(itl,func,auto_decl->expr,reg_slot);
    if(!type_res)
    {
        return type_res.error();
    }

    Type* type = *type_res;


    // attempting to deduce a type from void is nonsense
    if(is_void(type))
    {
        return compile_error(itl,itl_error::undefined_type_oper,"Result of auto decl is void");
    }


    // setup the allocation info now we have it
    alloc->value.v[1].imm = !is_plain_type(type);

    // also assign back the correct type and register info
    auto& sym = sym_from_slot(itl.symbol_table,sym_slot);
    sym.reg = make_reg(itl,reg_slot,type);
    sym.type = type;
    return option::none;
}


Result<BlockSlot,itl_error> compile_basic_block(Interloper& itl, Function& func, BlockNode* block_node)
{
    const BlockSlot block_slot = new_basic_block(itl,func);
    const auto block_err = compile_block(itl,func,block_node);
    if(block_err)
    {
        return *block_err;
    }

    return block_slot;
}

Option<itl_error> compile_init_list(Interloper& itl, Function& func, Type* ltype, AddrSlot addr_slot, AstNode* node)
{
    if(is_struct(ltype))
    {
        const auto structure = struct_from_type(itl.struct_table,ltype);

        return traverse_struct_initializer(itl,func,(RecordNode*)node,addr_slot,structure);                        
    }

    else
    {
        return compile_error(itl,itl_error::undefined_type_oper,"initializer list assign not allowed on type: %s",type_name(itl,ltype).buf);
    }
}

Option<itl_error> compile_assign(Interloper& itl, Function& func, AstNode* line)
{
    BinNode* assign_node = (BinNode*)line;
    
    switch(assign_node->left->type)
    {
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"could not assign to expr: %s",AST_INFO[u32(assign_node->left->type)].name);
        }
    }

    assert(false);
    return option::none;
}

Option<itl_error> compile_return(Interloper& itl, Function& func, AstNode* line)
{
    // returns a value
    if(line->fmt == ast_fmt::record)
    {
        RecordNode* record_node = (RecordNode*)line;

        if(count(record_node->nodes) != count(func.sig.return_type))
        {
            return compile_error(itl,itl_error::mismatched_args,"Invalid number of return parameters for function %s : %d != %d",
                func.name.buf,count(record_node->nodes),count(func.sig.return_type));
        }

        // single return
        if(count(record_node->nodes) == 1)
        {
            const RegSlot rv = make_spec_reg_slot(return_reg_from_type(func.sig.return_type[0]));

            switch(rv.spec)
            {
                case spec_reg::rv_struct:
                {
                    const auto rtype_res = compile_expression(itl,func,record_node->nodes[0],rv);
                    if(!rtype_res)
                    {
                        return rtype_res.error();
                    }

                    const auto assign_err = check_assign_init(itl,func.sig.return_type[0],*rtype_res);
                    if(assign_err)
                    {
                        return *assign_err;
                    }

                    break;
                }

                case spec_reg::rv_fpr:
                {
                    // Compile this into a tmp and then move it out so its easy to lock.
                    const auto tmp = new_float(func);
                    const auto rtype_res = compile_expression(itl,func,record_node->nodes[0],tmp);
                    if(!rtype_res)
                    {
                        return rtype_res.error();
                    }

                    const auto assign_err = check_assign_init(itl,func.sig.return_type[0],*rtype_res);
                    if(assign_err)
                    {
                        return assign_err;
                    }  
                    mov_float(itl,func,rv,tmp);
                    break;
                }

                case spec_reg::rv_gpr:
                {
                    // Compile this into a tmp and then move it out so its easy to lock.
                    const auto tmp = new_tmp(func,GPR_SIZE);
                    const auto rtype_res = compile_expression(itl,func,record_node->nodes[0],tmp);
                    if(!rtype_res)
                    {
                        return rtype_res.error();
                    }

                    const auto assign_err = check_assign_init(itl,func.sig.return_type[0],*rtype_res);
                    if(assign_err)
                    {
                        return *assign_err;
                    }  
                    mov_reg(itl,func,rv,tmp);
                    break;
                }

                default: assert(false);
            }
        }

        // multiple return
        else
        {
            for(u32 r = 0; r < count(func.sig.return_type); r++)
            {
                // void do_ptr_store(Interloper &itl,Function &func,u32 dst_slot,u32 addr_slot, const Type& type, u32 offset = 0)
                // NOTE: Pointers are in the first set of args i.e the hidden ones
                const auto tuple_res = compile_oper(itl,func,record_node->nodes[r]);
                if(!tuple_res)
                {
                    return tuple_res.error();
                }

                const auto src = *tuple_res;

                // check each param
                const auto assign_err = check_assign(itl,func.sig.return_type[r],src.type);
                if(assign_err)
                {
                    return *assign_err;
                }

                const TypedReg ptr = {make_sym_reg_slot(func.sig.args[r]),func.sig.return_type[r]};
                const auto store_err = do_ptr_store(itl,func,src.slot,ptr);
                if(store_err)
                {
                    return *store_err;
                }
            }
        }
    
    }

    // no return
    else
    {
        if(!is_void(func.sig.return_type[0]))
        {
            return compile_error(itl,itl_error::missing_args,"Expected return type of %s got nothing",type_name(itl,func.sig.return_type[0]).buf);
        }
    }

    ret(itl,func);
    return option::none;
}

Option<itl_error> compile_block(Interloper &itl,Function &func,BlockNode *block_node)
{
    auto sym_scope_guard = enter_new_anon_scope(itl.symbol_table);

    const u32 size = count(block_node->statements);
    for(u32 s = 0; s < size; s++)
    {
        AstNode* line = block_node->statements[s];

        itl.ctx.expr = line;
    
        switch(line->type)
        {
            default:
            {
                return compile_error(itl,itl_error::invalid_statement,"[COMPILE] unexpected statement: %s",AST_NAMES[u32(line->type)]);
            }
        }
    }

    return option::none;
}


Option<itl_error> compile_globals(Interloper& itl)
{
    // create a dummy void func called init_global
    // that we can compile all our global inits into!
    auto& func = create_dummy_func(itl,"init_global");

    for(u32 c = 0; c < count(itl.global_decl); c++)
    {
        GlobalDeclNode* decl_node = itl.global_decl[c];

        auto context_guard = switch_context(itl,decl_node->filename,decl_node->name_space,(AstNode*)decl_node);
        
        const auto decl_err = compile_decl(itl,func,(AstNode*)decl_node->decl,true);
        if(decl_err)
        {
            return decl_err;
        }
    }

    finalise_global_offset(itl);
    return option::none;
}

// TODO: basic type checking for returning pointers to local's

// feature plan:
// compile time execution -> unions? -> debug memory guards -> ...

Option<itl_error> check_startup_defs(Interloper& itl)
{   
    if(itl.rtti_enable)
    {
        const auto cache_err = cache_rtti_structs(itl);
        if(cache_err)
        {
            return cache_err;
        }
    }

    itl.std_name_space = find_name_space(itl,"std");

    if(!itl.std_name_space)
    {
        return compile_error(itl,itl_error::undeclared,"std namespace is not declared");
    }

    const auto main_err = check_startup_func(itl,"main",itl.global_namespace);
    if(main_err)
    {
        return main_err;
    }

    const auto start_err = check_startup_func(itl,"start",itl.global_namespace);
    if(start_err)
    {
        return start_err;
    }

    const auto memcpy_err = check_startup_func(itl,"memcpy",itl.std_name_space);
    if(memcpy_err)
    {
        return memcpy_err;
    }
    
    const auto zero_err = check_startup_func(itl,"zero_mem",itl.std_name_space);
    if(zero_err)
    {
        return zero_err;
    }

    return option::none;
}

void backend(Interloper& itl, const String& executable_path)
{
    auto start = std::chrono::high_resolution_clock::now();

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
            emit_elf(itl,executable_path);
            break;
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    itl.backend_time = std::chrono::duration<double, std::milli>(end-start).count();

    printf("OK\n\n");
}

Option<itl_error> code_generation(Interloper& itl)
{
    auto start = std::chrono::high_resolution_clock::now();

    const auto start_err = check_startup_defs(itl);
    if(start_err)
    {
        destroy_itl(itl);
        return start_err;
    }

    putchar('\n');

    // compile all our constant values 
    const auto const_err = compile_constants(itl);
    if(const_err)
    {
        destroy_itl(itl);
        return const_err;
    }

    const auto global_err = compile_globals(itl);
    if(global_err)
    {
        destroy_itl(itl);
        return global_err;      
    }

    declare_compiler_constants(itl);


    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 
    const auto func_err = compile_functions(itl);
    if(func_err)
    {
        destroy_itl(itl);
        return func_err;
    }

    // Check all declared symbols are used.
    for(auto& sym : itl.symbol_table.slot_lookup)
    {
        if(sym.references == 0 && sym.reg.segment == reg_segment::local && sym.name[0] != '_')
        {
            trash_context(itl,sym.ctx);
            return compile_error(itl,itl_error::unused_symbol,"Symbol %s is never used",sym.name.buf);
        }
    }

    // okay we dont need the parse tree anymore
    // free it
    destroy_ast(itl);

    auto end = std::chrono::high_resolution_clock::now();

    itl.code_gen_time = std::chrono::duration<double, std::milli>(end-start).count();
    return option::none;
}


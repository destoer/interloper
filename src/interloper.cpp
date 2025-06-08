#include <interloper.h>

TypeResult compile_expression(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);
RegResult compile_expression_tmp(Interloper &itl,Function &func,AstNode *node);
Option<itl_error> compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
Option<itl_error> compile_decl(Interloper &itl,Function &func,AstNode *line, b32 global = false);
Option<itl_error> compile_block(Interloper &itl,Function &func,BlockNode *node);
Result<BlockSlot,itl_error> compile_basic_block(Interloper &itl,Function &func,BlockNode *node);

RegResult compile_oper(Interloper& itl,Function &func,AstNode *node);

RegResult index_arr(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);
Option<itl_error> traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,AddrSlot* addr_slot, ArrayType* type);
RegResult index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, RegSlot ptr_slot, RegSlot dst_slot);

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
#include "func.cpp"
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
        return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
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


// for compiling operands i.e we dont care where it goes as long as we get something!
// i.e inside operators, function args, the call is responsible for making sure it goes in the right place
// NOTE: this returns out fixed array pointers and may require conversion by caller!
RegResult compile_oper(Interloper& itl,Function &func,AstNode *node)
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
            return symbol(itl,node);
        }

        // compile an expr
        default:
        {
            return compile_expression_tmp(itl,func,node);
        }
    }
}

Option<itl_error> compile_scoped_stmt(Interloper& itl, Function& func, AstNode* node, NameSpace* name_space)
{
    switch(node->type)
    {
        case ast_type::function_call:
        {
            const auto call_res = compile_scoped_function_call(itl,name_space,func,node,make_spec_reg_slot(spec_reg::null));
            if(!call_res)
            {
                return call_res.error();
            }
            break;
        }

        case ast_type::tuple_assign:
        {
            const auto call_res = compile_scoped_function_call(itl,name_space,func,node,make_spec_reg_slot(spec_reg::null));
            if(!call_res)
            {
                return call_res.error();
            }
            break;      
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Scope is not valid for stmt: %s\n",AST_NAMES[u32(node->type)]);
        }
    }

    return option::none;
}

TypeResult compile_scoped_expression(Interloper& itl, Function& func, AstNode* node, RegSlot dst_slot, NameSpace* name_space)
{
    switch(node->type)
    {
        case ast_type::function_call:
        {
            return compile_scoped_function_call(itl,name_space,func,node,dst_slot);
        }

        case ast_type::symbol:
        {
            const auto sym_res = scoped_symbol(itl,node,name_space);
            
            if(!sym_res)
            {
                return sym_res.error();
            }

            const auto src = *sym_res;
            const TypedReg dst = {dst_slot,src.type};


            const auto move_err = compile_move(itl,func,dst,src);
            if(!!move_err)
            {
                return *move_err;
            }
            return src.type;
        }

        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"Scope is not valid for expression: %s\n",AST_NAMES[u32(node->type)]);
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

    log(itl.itl_log,"(%s:%d) Compiling experssion %s\n",itl.ctx.filename.buf,node->idx,AST_NAMES[u32(node->type)]);

    itl.ctx.expr = node;
   
    switch(node->type)
    {

        case ast_type::value:
        {
            const auto t1 = value(itl,func,node,dst_slot);
            return t1;
        }

        case ast_type::float_t:
        {
            FloatNode* float_node = (FloatNode*)node;
            movf_imm(itl,func,dst_slot,float_node->value);

            return make_builtin(itl,builtin_type::f64_t);
        }

        case ast_type::symbol:
        {
            const auto sym_res = symbol(itl,node);
            
            if(!sym_res)
            {
                return sym_res.error();
            }

            const auto src = *sym_res;
            const TypedReg dst = {dst_slot,src.type};


            const auto move_err = compile_move(itl,func,dst,src);
            if(!!move_err)
            {
                return *move_err;
            }
            return src.type;
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

                    auto type_decl_opt = lookup_type(itl,name);
                    if(!!type_decl_opt)
                    {
                        TypeDecl* type_decl = *type_decl_opt;
                        LiteralNode* member_node = (LiteralNode*) members->nodes[0];

                        return access_type_info(itl,func,dst_slot,*type_decl,member_node->literal);
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

        case ast_type::slice:
        {
            return slice_array(itl,func,(SliceNode*)node,dst_slot);
        }

        case ast_type::addrof:
        {
            UnaryNode* addrof_node = (UnaryNode*)node;

            const auto res = take_addr(itl,func,addrof_node->next,dst_slot);
            if(!res)
            {
                return res.error();
            }

            auto addr = *res;

            return addr.type;
        }

        case ast_type::deref:
        {
            UnaryNode* deref_node = (UnaryNode*)node;

            const auto res = take_pointer(itl,func,deref_node->next);

            if(!res)
            {
                return res.error();
            }

            auto ptr = *res;

            if(((PointerType*)ptr.type)->pointer_kind == pointer_type::nullable)
            {
                return compile_error(itl,itl_error::pointer_type_error,"Cannot dereference a nullable pointer %s\n",type_name(itl,ptr.type).buf);
            }

            // deref the pointer
            ptr.type = deref_pointer(ptr.type); 
            const auto load_err = do_ptr_load(itl,func,dst_slot,ptr);
            if(!!load_err)
            {
                return *load_err;
            }

            return ptr.type;          
        }

        case ast_type::sizeof_t:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            // TODO: should this work with type names?
            const auto res = compile_oper(itl,func,unary_node->next);
            if(!res)
            {
                return res.error();
            }

            const auto reg = *res;


            const u32 size = type_size(itl,reg.type);
            mov_imm(itl,func,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);
        }

        case ast_type::sizeof_type_t:
        {
            TypeNode* type_node = (TypeNode*)node;
            const auto type_res = get_type(itl,type_node);
            if(!type_res)
            {
                return type_res;
            }

            // just move in the type size
            const u32 size = type_size(itl,*type_res);
            mov_imm(itl,func,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);
        }

        case ast_type::sizeof_data_t:
        {
            TypeNode* type_node = (TypeNode*)node;
            const auto type_res = get_type(itl,type_node);
            if(!type_res)
            {
                return type_res;
            }

            // move in data size
            const u32 size = data_size(itl,*type_res);
            mov_imm(itl,func,dst_slot,size);

            return make_builtin(itl,builtin_type::u32_t);      
        }

        case ast_type::cast:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto expr_res = compile_oper(itl,func,bin_node->right);
            if(!expr_res)
            {
                return expr_res.error();
            }

            const auto new_type_res = get_type(itl,(TypeNode*)bin_node->left);
            if(!new_type_res)
            {
                return new_type_res;
            }

            const auto reg = *expr_res;
            Type* new_type = *new_type_res;

            const auto cast_err = handle_cast(itl,func,dst_slot,reg.slot,reg.type,new_type);
            if(!!cast_err)
            {
                return *cast_err;
            }
            return new_type;
        }

        case ast_type::recast_arr:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto cast_res = compile_oper(itl,func,bin_node->right);
            if(!cast_res)
            {
                return cast_res.error();
            }
            
            const auto new_type_res = get_type(itl,(TypeNode*)bin_node->left);
            if(!new_type_res)
            {
                return new_type_res;
            }

            const auto old = *cast_res;
            Type* new_type = *new_type_res;


            if(!is_flat_array(old.type))
            {
                return compile_error(itl,itl_error::array_type_error,"Expected recast from flat array got: %s\n",type_name(itl,old.type).buf);
            }

            const u32 new_size = type_size(itl,new_type);
            const u32 old_size = type_size(itl,index_arr(old.type));

            const auto new_arr_type = make_array(itl,new_type,RUNTIME_SIZE);

            const auto data_slot = load_arr_data(itl,func,old);
            store_arr_data(itl,func,dst_slot,data_slot);

            const auto len_slot = load_arr_len(itl,func,old);
            const auto byte_slot = mul_imm_res(itl,func,len_slot,old_size);
            const auto converted_len = udiv_imm_res(itl,func,byte_slot,new_size);
            store_arr_len(itl,func,dst_slot,converted_len);
            
            return new_arr_type;            
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

            const auto rtype_res = compile_expression(itl,func,bin_node->right,dst_slot);
            if(!rtype_res)
            {
                return rtype_res.error();
            }

            const auto rtype = *rtype_res;

            if(bin_node->left->fmt != ast_fmt::literal)
            {
                return compile_error(itl,itl_error::invalid_statement,"[COMPILE]: expected symbol in multiple assign\n");
            }

            LiteralNode* lit_node = (LiteralNode*)bin_node->left;

            const auto name = lit_node->literal;

            const auto sym_ptr = get_sym(itl.symbol_table,name);
            if(!sym_ptr)
            {
                return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
            }

            const auto &sym = *sym_ptr;

            const auto assign_err = check_assign(itl,sym.type,rtype);
            if(!!assign_err)
            {
                return *assign_err;
            }

            const TypedReg src = {dst_slot,rtype};
            const auto move_error = compile_move(itl,func,typed_reg(sym),src);
            if(!!move_error)
            {
                return *move_error;
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
                const auto sub_res = compile_oper(itl,func,unary_node->next);
                if(!sub_res)
                {
                    return sub_res.error();
                }

                const auto reg = *sub_res;

                if(is_integer(reg.type))
                {
                    // TODO: make sure our optimiser sees through this
                    const RegSlot slot = mov_imm_res(itl,func,0);
                    sub(itl,func,dst_slot,slot,reg.slot);
                }

                else if(is_float(reg.type))
                {
                    // TODO: make sure our optimiser sees through this
                    const RegSlot slot = movf_imm_res(itl,func,0.0);
                    subf(itl,func,dst_slot,slot,reg.slot);
                }

                else
                {
                    return compile_error(itl,itl_error::undefined_type_oper,"unary minus not valid for type %s\n",type_name(itl,reg.type).buf);
                }
                
                return reg.type;
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

            const auto res = compile_oper(itl,func,unary_node->next);
            if(!res)
            {
                return res.error();
            }

            const auto reg = *res;
            if(!is_integer(reg.type))
            {
                return compile_error(itl,itl_error::int_type_error,"Bitwise not only defind on int got: %s\n",type_name(itl,reg.type).buf);
            }

            not_reg(itl,func,dst_slot,reg.slot);
            return reg.type;
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

            return make_nullable_ptr(itl,plain);
        }


        case ast_type::logical_not:
        {
            UnaryNode* unary_node = (UnaryNode*)node;

            const auto res = compile_oper(itl,func,unary_node->next);
            if(!res)
            {
                return res.error();
            }

            const auto reg = *res;

            // integer or pointer, eq to zero
            if(is_integer(reg.type) || is_pointer(reg.type))
            {
                cmp_eq_imm(itl,func,dst_slot,reg.slot,0);
                return make_builtin(itl,builtin_type::bool_t);
            }

            // logical not on bool
            else if(!is_bool(reg.type))
            {
                return compile_error(itl,itl_error::bool_type_error,"compile: logical_not expected bool got: %s\n",type_name(itl,reg.type).buf);
            }

            // xor can invert our boolean which is either 1 or 0
            xor_imm(itl,func,dst_slot,reg.slot,1);
            return reg.type;
        }

        // we want to pass in the base operation but we need to do the actual type checking
        // to know what we are comparing later
        // how should we do it?
        case ast_type::logical_lt:
        {
            return compile_comparison_op<comparison_op::cmplt_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_le:
        {
            return compile_comparison_op<comparison_op::cmple_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_gt:
        {
            return compile_comparison_op<comparison_op::cmpgt_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_ge:
        {
            return compile_comparison_op<comparison_op::cmpge_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_eq:
        {
            return compile_comparison_op<comparison_op::cmpeq_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_ne:
        {
            return compile_comparison_op<comparison_op::cmpne_reg>(itl,func,node,dst_slot);
        }

        case ast_type::logical_and:
        {
            return compile_boolean_logic_op(itl,func,node,dst_slot,boolean_logic_op::and_t,0);
        }

        case ast_type::logical_or:
        {
            return compile_boolean_logic_op(itl,func,node,dst_slot,boolean_logic_op::or_t, 0);
        }


        case ast_type::function_call:
        {
            return compile_function_call(itl,func,node,dst_slot);
        }

        case ast_type::scope:
        {
            ScopeNode* scope_node = (ScopeNode*)node;

            const auto type_res = compile_enum(itl,func,scope_node,dst_slot);

            if(!type_res)
            {
                return type_res.error();
            }

            Type* type = *type_res;

            // We have an actual enum return it
            if(!is_void(type))
            {
                return type;
            }

            NameSpace* name_space = scan_namespace(itl.global_namespace,scope_node->scope);

            if(!name_space)
            {
                return compile_error(itl,itl_error::undeclared,"Could not find namespace\n");
            }

            return compile_scoped_expression(itl,func,scope_node->expr,dst_slot,name_space);
        }

        case ast_type::string:
        {
            const auto literal_node = (LiteralNode*)node;
            store_const_string(itl,func,literal_node->literal,dst_slot);

            return make_array(itl,make_builtin(itl,builtin_type::c8_t,true),RUNTIME_SIZE);
        }
        
        default:
        {
            return compile_error(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'\n",AST_NAMES[u32(node->type)]);
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
            return compile_error(itl,itl_error::pointer_type_error,"References must have an explicit initializer: %s\n",type_name(itl,ltype).buf);
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
        return compile_error(itl,itl_error::redeclaration,"redeclared symbol: %s:%s\n",name.buf,type_name(itl,sym_ptr->type).buf);
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
            if(!!decl_err)
            {
                return decl_err;
            }
            break;
        }

        case type_class::struct_t:
        {
            const auto struct_err = compile_struct_decl(itl,func,decl_node,slot);
            if(!!struct_err)
            {
                return struct_err;
            }
            break;
        }

        default:
        {
            const auto decl_err = compile_basic_decl(itl,func,decl_node,ltype,slot);
            if(!!decl_err)
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

    func.registers[dst_slot.tmp_slot.handle] = make_reg(itl,dst_slot,type);

    return TypedReg{dst_slot,type};
}





Option<itl_error> compile_auto_decl(Interloper &itl,Function &func, const AstNode *line)
{
    AutoDeclNode* auto_decl = (AutoDeclNode*)line;

    const auto name = auto_decl->name;

    if(get_sym(itl.symbol_table,name))
    {
        return compile_error(itl,itl_error::redeclaration,"redeclared symbol: %s\n",name.buf);
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
        return compile_error(itl,itl_error::undefined_type_oper,"Result of auto decl is void\n");
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
    if(!!block_err)
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
        return compile_error(itl,itl_error::undefined_type_oper,"initializer list assign not allowed on type: %s\n",type_name(itl,ltype).buf);
    }
}

Option<itl_error> compile_assign(Interloper& itl, Function& func, AstNode* line)
{
    BinNode* assign_node = (BinNode*)line;
    
    if(assign_node->left->type != ast_type::symbol)
    {
        switch(assign_node->left->type)
        {
            case ast_type::ignore:
            {
                const auto rtype_res = compile_expression(itl,func,assign_node->right,new_tmp(func,GPR_SIZE));
                if(!rtype_res)
                {
                    return rtype_res.error();
                }

                return option::none;
            }

            case ast_type::deref:
            {
                UnaryNode* deref_node = (UnaryNode*)assign_node->left;
                const auto expr_res = compile_oper(itl,func,assign_node->right);
                if(!expr_res)
                {
                    return expr_res.error();
                }
                const auto ptr_res = take_pointer(itl,func,deref_node->next);

                if(!ptr_res)
                {
                    return ptr_res.error();
                }

                const auto src = *expr_res;
                auto ptr = *ptr_res;


                if(((PointerType*)ptr.type)->pointer_kind == pointer_type::nullable)
                {
                    return compile_error(itl,itl_error::pointer_type_error,"Cannot dereference a nullable pointer %s\n",type_name(itl,ptr.type).buf);
                }

                // store into the pointer
                ptr.type = deref_pointer(ptr.type); 
                const auto store_err = do_ptr_store(itl,func,src.slot,ptr);
                if(!!store_err)
                {
                    return store_err;
                }

                return check_assign(itl,ptr.type,src.type);                      
            }
        
            case ast_type::index:
            {
                const auto src_res = compile_oper(itl,func,assign_node->right);
                if(!src_res)
                {
                    return src_res.error();
                }

                return write_arr(itl,func,assign_node->left,*src_res);
            }
        
            // write on struct member!
            case ast_type::access_struct:
            {
                if(assign_node->right->type == ast_type::initializer_list)
                {
                    auto addr_res = compute_member_addr(itl,func,assign_node->left);
                    if(!addr_res)
                    {
                        return addr_res.error();
                    }

                    auto [ltype, addr_slot] = *addr_res;

                    return compile_init_list(itl,func,ltype,addr_slot,assign_node->right);
                }

                else
                {
                    const auto src_res = compile_oper(itl,func,assign_node->right);
                    if(!src_res)
                    {
                        return src_res.error();
                    }

                    // This Errors we just don't care
                    return write_struct(itl,func,*src_res,assign_node->left);
                }
            }
        
            default:
            {
                return compile_error(itl,itl_error::invalid_expr,"could not assign to expr: %s\n",AST_NAMES[u32(assign_node->left->type)]);
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
            return compile_error(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' assigned before declaration\n",name.buf);
        }


        RegSlot slot = sym_ptr->reg.slot;
        u32 size = sym_ptr->reg.size;
        Type *ltype = sym_ptr->type;

        // handle initializer list
        if(assign_node->right->type == ast_type::initializer_list)
        {
            const auto addr_slot = make_struct_addr(slot,0);
            return compile_init_list(itl,func,ltype,addr_slot,assign_node->right);
        }

        else
        {
            const auto rtype_res = compile_expression(itl,func,assign_node->right,slot);
            if(!rtype_res)
            {
                return rtype_res.error();
            }

            const auto assign_err = check_assign(itl,ltype,*rtype_res);
            if(!!assign_err)
            {
                return *assign_err;
            }

            if(is_unsigned_integer(ltype))
            {
                clip_arith_type(itl,func,slot,slot,size);
            }
            
            return option::none;
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
                    if(!!assign_err)
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
                    if(!!assign_err)
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
                    if(!!assign_err)
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
            if(count(record_node->nodes) != count(func.sig.return_type))
            {
                return compile_error(itl,itl_error::mismatched_args,"Invalid number of return parameters for function %s : %d != %d\n",
                    func.name.buf,count(record_node->nodes),count(func.sig.return_type));
            }
            

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
                if(!!assign_err)
                {
                    return *assign_err;
                }

                const TypedReg ptr = {make_sym_reg_slot(func.sig.args[r]),func.sig.return_type[r]};
                const auto store_err = do_ptr_store(itl,func,src.slot,ptr);
                if(!!store_err)
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
            return compile_error(itl,itl_error::missing_args,"Expected return type of %s got nothing\n",type_name(itl,func.sig.return_type[0]).buf);
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
            // variable declaration
            case ast_type::declaration:
            {
                const auto decl_err = compile_decl(itl,func,line);
                if(!!decl_err)
                {
                    return decl_err;
                }
                break;
            }

            case ast_type::const_decl:
            {
                const auto decl_err = compile_constant_decl(itl,(DeclNode*)line,false);
                if(!!decl_err)
                {
                    return decl_err;
                }
                break;
            }           


            case ast_type::auto_decl:
            {
                const auto decl_err = compile_auto_decl(itl,func,line);
                if(!!decl_err)
                {
                    return decl_err;
                }
                break;
            }


            // assignment
            case ast_type::equal:
            {
                const auto assign_err = compile_assign(itl,func,line);
                if(!!assign_err)
                {
                    return assign_err;
                }
                break;
            }


            case ast_type::ret:
            {
                const auto ret_err = compile_return(itl,func,line);
                if(!!ret_err)
                {
                    return ret_err;
                }
                break;
            }

            case ast_type::scope:
            {
                ScopeNode* scope_node = (ScopeNode*)line;
                NameSpace* name_space = scan_namespace(itl.global_namespace,scope_node->scope);

                if(!name_space)
                {
                    return compile_error(itl,itl_error::undeclared,"Could not find namespace\n");
                }

                const auto stmt_err = compile_scoped_stmt(itl,func,scope_node->expr,name_space);
                if(!!stmt_err)
                {
                    return stmt_err;
                }
                break;
            }

            case ast_type::function_call:
            {
                const auto func_res = compile_function_call(itl,func,line,make_spec_reg_slot(spec_reg::null));
                if(!func_res)
                {
                    return func_res.error();
                }
                break;
            }            


            case ast_type::block:
            {
                const auto block_err = compile_block(itl,func,(BlockNode*)line);
                if(!!block_err)
                {
                    return block_err;
                }
                break;
            }


            case ast_type::if_block:
            {
                const auto if_err = compile_if_block(itl,func,line);
                if(!!if_err)
                {
                    return if_err;
                }
                break;
            }

            case ast_type::for_iter:
            {
                const auto for_err = compile_for_iter(itl,func,(ForIterNode*)line);
                if(!!for_err)
                {
                    return for_err;
                }
                break;
            }

            case ast_type::for_range:
            {
                const auto for_err = compile_for_range(itl,func,(ForRangeNode*)line);
                if(!!for_err)
                {
                    return for_err;
                }
                break;
            }

            case ast_type::while_block:
            {
                const auto while_err = compile_while_block(itl,func,line);
                if(!!while_err)
                {
                    return while_err;
                }
                break;
            }

            case ast_type::switch_t:
            {
                const auto switch_err = compile_switch_block(itl,func,line);
                if(!!switch_err)
                {
                    return switch_err;
                }
                break;
            }

            case ast_type::tuple_assign:
            {
                const auto func_res = compile_function_call(itl,func,line,make_spec_reg_slot(spec_reg::null));
                if(!func_res)
                {
                    return func_res.error();
                }
                break;
            }

            case ast_type::const_assert:
            {
                UnaryNode* unary_node = (UnaryNode*)line;

                const auto pass_res = compile_const_bool_expression(itl,unary_node->next);
                if(!pass_res)
                {
                    return pass_res.error();
                }

                if(!*pass_res)
                {
                    return compile_error(itl,itl_error::const_assert,"Compile time assertion failed\n");
                }
                break;
            }

            case ast_type::struct_return:
            {
                StructReturnNode* struct_return = (StructReturnNode*)line;

                // Check we have a valid struct
                const auto struct_decl_res = lookup_type(itl,struct_return->struct_name);
                if(!struct_decl_res)
                {
                    return compile_error(itl,itl_error::struct_error,"No such struct: %s\n",struct_return->struct_name);
                }

                const auto struct_decl = *struct_decl_res;

                if(struct_decl->kind != type_kind::struct_t)
                {
                    return compile_error(itl,itl_error::struct_error,"No such struct: %s\n",struct_return->struct_name);
                }

                if(count(func.sig.return_type) != 1)
                {
                    return compile_error(itl,itl_error::struct_error,"Expected single return for struct return func expects: %d\n",
                        count(func.sig.return_type));
                }


                // Check the return type matches
                const auto struct_type = make_struct(itl,struct_decl->type_idx);
                const auto assign_err = check_assign(itl,func.sig.return_type[0],struct_type);
                if(!!assign_err)
                {
                    return assign_err;
                }

                // Compile a initializer list into the return type
                const auto &structure = itl.struct_table[struct_decl->type_idx];
                const auto struct_err = traverse_struct_initializer(itl,func,struct_return->record,
                    make_addr(make_sym_reg_slot(func.sig.args[0]),0),structure);
                if(!!struct_err)
                {
                    return struct_err;
                }

                ret(itl,func);
                break;            
            }

            default:
            {
                return compile_error(itl,itl_error::invalid_statement,"[COMPILE] unexpected statement: %s\n",AST_NAMES[u32(line->type)]);
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
        if(!!decl_err)
        {
            return decl_err;
        }
    }

    finalise_global_offset(itl);
    return option::none;
}

// TODO: basic type checking for returning pointers to local's

// feature plan:
// namespace -> compile time execution ->
// -> unions? -> debug memory guards -> ...

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

    destroy_arr(itl.global_def);
    destroy_arr(itl.saved_ctx);

    itl.ctx.expr = nullptr;
    itl.ctx.filename = ""; 
    itl.ctx.name_space = nullptr;
}

void destroy_itl(Interloper &itl)
{
    destroy_asm_emitter(itl.asm_emitter);
    destroy_arr(itl.program);
    destroy_const_pool(itl.const_pool);
    destroy_sym_table(itl.symbol_table);
    destroy_namespace_tree(itl);
    destroy_arr(itl.type_decl);
    destroy_arr(itl.constant_decl);
    destroy_arr(itl.global_decl);
    destroy_arr(itl.global_alloc.array_allocation);
    
    destroy_ast(itl);

    destroy_func_table(itl.func_table);

    // destroy typing tables
    destroy_struct_table(itl.struct_table);
    destroy_enum_table(itl.enum_table);
    destroy_arr(itl.alias_table);

    destroy_arr(itl.name_space_buffer);

    destroy_allocator(itl.list_allocator);
    destroy_allocator(itl.string_allocator);

    
    for(u32 p = 0; p < count(itl.func_pointer); p++)
    {
        destroy_sig(*itl.func_pointer[p]);
    }

    destroy_arr(itl.func_pointer);

    destroy_rtti_cache(itl.rtti_cache);

    destroy_allocator(itl.type_allocator);
}

static constexpr u32 LIST_INITIAL_SIZE = 16 * 1024;
static constexpr u32 STRING_INITIAL_SIZE = 4 * 1024;
static constexpr u32 TYPE_INITIAL_SIZE =  4 * 1024;

void setup_type_table(Interloper& itl)
{
    // add all the builtin types  
    for(u32 i = 0; i < BUILTIN_TYPE_SIZE; i++)
    {
        add_internal_type_decl(itl,i,TYPE_NAMES[i],type_kind::builtin);
    }
}

Option<itl_error> check_startup_defs(Interloper& itl)
{   
    if(itl.rtti_enable)
    {
        const auto cache_err = cache_rtti_structs(itl);
        if(!!cache_err)
        {
            return cache_err;
        }
    }

    itl.std_name_space = find_name_space(itl,"std");

    if(!itl.std_name_space)
    {
        return compile_error(itl,itl_error::undeclared,"std namespace is not declared");
    }

    StructCacheReq register_cache_req = make_struct_cache_req(itl.std_name_space,"X86Register",&itl.register_struct.struct_idx,&itl.register_struct.struct_size);
    add_member_cache_req(register_cache_req,"rsp",&itl.register_struct.rsp_offset);

    auto register_err = cache_structure(itl,register_cache_req);
    if(!!register_err)
    {
        return register_err;
    }

    const auto main_err = check_startup_func(itl,"main",itl.global_namespace);
    if(!!main_err)
    {
        return main_err;
    }

    const auto start_err = check_startup_func(itl,"start",itl.global_namespace);
    if(!!start_err)
    {
        return start_err;
    }

    const auto memcpy_err = check_startup_func(itl,"memcpy",itl.std_name_space);
    if(!!memcpy_err)
    {
        return memcpy_err;
    }
    
    const auto zero_err = check_startup_func(itl,"zero_mem",itl.std_name_space);
    if(!!zero_err)
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
    if(!!start_err)
    {
        destroy_itl(itl);
        return start_err;
    }

    putchar('\n');

    // compile all our constant values 
    const auto const_err = compile_constants(itl);
    if(!!const_err)
    {
        destroy_itl(itl);
        return const_err;
    }

    const auto global_err = compile_globals(itl);
    if(!!global_err)
    {
        destroy_itl(itl);
        return global_err;      
    }

    declare_compiler_constants(itl);


    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 
    const auto func_err = compile_functions(itl);
    if(!!func_err)
    {
        destroy_itl(itl);
        return func_err;
    }

    // okay we dont need the parse tree anymore
    // free it
    destroy_ast(itl);

    auto end = std::chrono::high_resolution_clock::now();

    itl.code_gen_time = std::chrono::duration<double, std::milli>(end-start).count();
    return option::none;
}

Option<parse_error> parsing(Interloper& itl, const String& initial_filename)
{
    // parse intial input file
    auto start = std::chrono::high_resolution_clock::now();

    // build ast
    const auto parse_err = parse(itl,initial_filename);
    if(!!parse_err)
    {
        // flag as generic parser error
        if(itl.error_count == 0)
        {
            itl.error_count = std::max(u32(1),itl.error_count);
            itl.first_error_code = *parse_err == parse_error::lexer_error? itl_error::lexer_error : itl_error::parse_error;
        }

        destroy_itl(itl);
        return parse_err;
    }

    auto end = std::chrono::high_resolution_clock::now();

    itl.parsing_time = std::chrono::duration<double, std::milli>(end-start).count();

    if(itl.print_ast)
    {
        // print type defs
        for(u32 t = 0; t < count(itl.type_decl); t++)
        {
            print(itl.type_decl[t]);
        }

        // print function defs
        for(u32 f = 0; f < count(itl.func_table.table); f++)
        {
            auto& func = itl.func_table.table[f];
            print((AstNode*)func.root);    
        }
    }

    return option::none;
}

Option<itl_error> compile(Interloper &itl,const String& initial_filename, const String& executable_path)
{
    printf("compiling file: %s\n",initial_filename.buf);

    itl.first_error_code = itl_error::unimplemented;
    itl.error_count = 0;

    itl.ast_allocator = make_allocator(AST_ALLOC_DEFAULT_SIZE);
    itl.ast_string_allocator = make_allocator(STRING_INITIAL_SIZE);

    itl.string_allocator = make_allocator(STRING_INITIAL_SIZE);
    itl.list_allocator = make_allocator(LIST_INITIAL_SIZE);
    itl.type_allocator = make_allocator(TYPE_INITIAL_SIZE);
    itl.namespace_allocator = make_allocator(2 * 1024);

    itl.symbol_table.string_allocator = &itl.string_allocator;
    itl.symbol_table.namespace_allocator = &itl.namespace_allocator;
    itl.symbol_table.ctx = &itl.ctx;

    itl.func_table = make_func_table();

    setup_namespace(itl);

    setup_type_table(itl);
    declare_compiler_type_aliases(itl);

    // add an dummy error value as the first handle
    // see SYM_ERROR
    make_sym(itl,"ITL_ERROR",make_builtin(itl,builtin_type::void_t));

    const auto parse_err = parsing(itl,initial_filename);
    if(!!parse_err)
    {
        puts("Parsing error");
        destroy_itl(itl);
        return itl_error::parse_error;
    }

    const auto code_err = code_generation(itl);
    if(!!code_err)
    {
        puts("IR generation error");
        if(itl.error_count > 15)
        {
            printf("Total errors: %d (only 15 reported)\n",itl.error_count);
        }

        destroy_itl(itl);
        return code_err;
    }

    if(itl.optimise)
    {
        optimise_ir(itl);
    }
    
    backend(itl,executable_path);
    return option::none;
}
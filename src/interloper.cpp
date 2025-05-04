#include <interloper.h>

Type* compile_expression(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);
std::pair<Type*, RegSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node);
void compile_auto_decl(Interloper &itl,Function &func, const AstNode *line);
void compile_decl(Interloper &itl,Function &func,AstNode *line, b32 global = false);
void compile_block(Interloper &itl,Function &func,BlockNode *node);
BlockSlot compile_basic_block(Interloper &itl,Function &func,BlockNode *node);

std::pair<Type*,RegSlot> compile_oper(Interloper& itl,Function &func,AstNode *node);

std::pair<Type*, RegSlot> index_arr(Interloper &itl,Function &func,AstNode *node, RegSlot dst_slot);
void traverse_arr_initializer_internal(Interloper& itl,Function& func,RecordNode *list,AddrSlot* addr_slot, ArrayType* type);
std::pair<Type*,RegSlot> index_arr_internal(Interloper& itl, Function &func,IndexNode* index_node, const String& arr_name,
     Type* type, RegSlot ptr_slot, RegSlot dst_slot);

void compile_move(Interloper &itl, Function &func, RegSlot dst_slot, RegSlot src_slot, const Type* dst_type, const Type* src_type);
std::pair<Type*,RegSlot> take_pointer(Interloper& itl,Function& func, AstNode* deref_node);
void add_func(Interloper& itl, const String& name, NameSpace* name_space, FuncNode* root);

RegSlot load_arr_data(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_len(Interloper& itl,Function& func,const Symbol& sym);
RegSlot load_arr_data(Interloper& itl,Function& func,RegSlot slot, const Type* type);
RegSlot load_arr_len(Interloper& itl,Function& func,RegSlot slot, const Type* type);

std::pair<Type*,RegSlot> symbol(Interloper &itl, AstNode *node);

void compile_init_list(Interloper& itl, Function& func, Type* ltype, AddrSlot addr_slot, AstNode* node);

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
void switch_context(Interloper& itl, String filename,NameSpace* name_space, AstNode* expr)
{
    push_context(itl);
    trash_context(itl,filename,name_space,expr);
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


std::pair<Type*,RegSlot> symbol(Interloper &itl, AstNode *node)
{
    LiteralNode* lit_node = (LiteralNode*)node;

    const auto name = lit_node->literal;

    const auto sym_ptr = get_sym(itl.symbol_table,name);
    if(!sym_ptr)
    {
        panic(itl,itl_error::undeclared,"[COMPILE]: symbol '%s' used before declaration\n",name.buf);
        return std::pair{make_builtin(itl,builtin_type::void_t),INVALID_SYM_REG_SLOT};
    }

    const auto &sym = *sym_ptr;

    return std::pair{sym.type,sym.reg.slot};
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
std::pair<Type*,RegSlot> compile_oper(Interloper& itl,Function &func,AstNode *node)
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

void compile_scoped_stmt(Interloper& itl, Function& func, AstNode* node, NameSpace* name_space)
{
    switch(node->type)
    {
        case ast_type::function_call:
        {
            compile_scoped_function_call(itl,name_space,func,node,make_spec_reg_slot(spec_reg::null));
            break;
        }

        case ast_type::tuple_assign:
        {
            compile_scoped_function_call(itl,name_space,func,node,make_spec_reg_slot(spec_reg::null));
            break;      
        }

        default:
        {
            panic(itl,itl_error::invalid_expr,"Scope is not valid for stmt: %s\n",AST_NAMES[u32(node->type)]);
            return;
        }
    }
}

Type* compile_scoped_expression(Interloper& itl, Function& func, AstNode* node, RegSlot dst_slot, NameSpace* name_space)
{
    switch(node->type)
    {
        case ast_type::function_call:
        {
            return compile_scoped_function_call(itl,name_space,func,node,dst_slot);
        }

        default:
        {
            panic(itl,itl_error::invalid_expr,"Scope is not valid for expression: %s\n",AST_NAMES[u32(node->type)]);
            return make_builtin(itl,builtin_type::void_t);
        }
    }
}

Type* compile_expression(Interloper &itl,Function &func,AstNode *node,RegSlot dst_slot)
{
    if(!node)
    {
        crash_and_burn("nullptr in compile_expression");
        return make_builtin(itl,builtin_type::void_t);
    }

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

        case ast_type::slice:
        {
            return slice_array(itl,func,(SliceNode*)node,dst_slot);
        }

        case ast_type::addrof:
        {
            UnaryNode* addrof_node = (UnaryNode*)node;

            const auto [type,slot] = take_addr(itl,func,addrof_node->next,dst_slot);
            return type;
        }

        case ast_type::deref:
        {
            UnaryNode* deref_node = (UnaryNode*)node;

            const auto [ptr_type,slot] = take_pointer(itl,func,deref_node->next);

            if(itl.error)
            {
                return make_builtin(itl,builtin_type::void_t);
            }

            // deref the pointer
            auto type = deref_pointer(ptr_type); 
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

        case ast_type::sizeof_data_t:
        {
            TypeNode* type_node = (TypeNode*)node;

            // move in data size
            const u32 size = data_size(itl,get_type(itl,type_node));
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

        case ast_type::recast_arr:
        {
            BinNode* bin_node = (BinNode*)node;

            const auto [old_arr_type,slot] = compile_oper(itl,func,bin_node->right);
            const auto new_type = get_type(itl,(TypeNode*)bin_node->left);

            if(!is_byte_array(old_arr_type))
            {
                panic(itl,itl_error::array_type_error,"Expected recast from byte array got: %s\n",type_name(itl,old_arr_type).buf);
                return make_builtin(itl,builtin_type::void_t);
            }

            const auto new_arr_type = make_array(itl,new_type,RUNTIME_SIZE);

            const auto data_slot = load_arr_data(itl,func,slot,old_arr_type);
            store_arr_data(itl,func,dst_slot,data_slot);

            const auto len_slot = load_arr_len(itl,func,slot,old_arr_type);
            const auto converted_len = udiv_imm_res(itl,func,len_slot,type_size(itl,new_type));
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
                const auto [t,v1] = compile_oper(itl,func,unary_node->next);

                if(is_integer(t))
                {
                    // TODO: make sure our optimiser sees through this
                    const RegSlot slot = mov_imm_res(itl,func,0);
                    sub(itl,func,dst_slot,slot,v1);
                }

                else if(is_float(t))
                {
                    // TODO: make sure our optimiser sees through this
                    const RegSlot slot = movf_imm_res(itl,func,0.0);
                    subf(itl,func,dst_slot,slot,v1);
                }

                else
                {
                    panic(itl,itl_error::undefined_type_oper,"unary minus not valid for type %s\n",type_name(itl,t).buf);
                    return make_builtin(itl,builtin_type::void_t);
                }
                
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
            // TODO: This is to work around the output var getting locked by reg alloc
            // If its a special reg
            const RegSlot dst_tmp = mov_imm_res(itl,func,0); 
            const auto ltype = compile_boolean_logic_op(itl,func,node,dst_tmp,boolean_logic_op::and_t,0);
            mov_reg(itl,func,dst_slot,dst_tmp);

            return ltype;
        }

        case ast_type::logical_or:
        {
            // TODO: This is to work around the output var getting locked by reg alloc
            // If its a special reg
            const RegSlot dst_tmp = mov_imm_res(itl,func,0); 
            const auto ltype = compile_boolean_logic_op(itl,func,node,dst_tmp,boolean_logic_op::or_t, 0);
            mov_reg(itl,func,dst_slot,dst_tmp);

            return ltype;
        }


        case ast_type::function_call:
        {
            return compile_function_call(itl,func,node,dst_slot);
        }

        case ast_type::scope:
        {
            ScopeNode* scope_node = (ScopeNode*)node;

            const auto type = compile_enum(itl,func,scope_node,dst_slot);

            // Enum was found or an error either way we are done here
            if(itl.error || !is_void(type))
            {
                return type;
            }

            NameSpace* name_space = scan_namespace(itl.global_namespace,scope_node->scope);

            if(!name_space)
            {
                panic(itl,itl_error::undeclared,"Could not find namespace\n");
                return make_builtin(itl,builtin_type::void_t);
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
            panic(itl,itl_error::invalid_expr,"[COMPILE]: invalid expression '%s'\n",AST_NAMES[u32(node->type)]);
            return make_builtin(itl,builtin_type::void_t);
        }
    }
}

void compile_basic_decl(Interloper& itl, Function& func, const DeclNode* decl_node, const Type* ltype, SymSlot slot)
{
    const auto reg_slot = make_sym_reg_slot(slot);
    alloc_slot(itl,func,reg_slot,false);
    
    // No initalizer
    if(!decl_node->expr)
    {
        if(is_float(ltype))
        {
            movf_imm(itl,func,reg_slot,0.0);
        }

        else
        {
            mov_imm(itl,func,reg_slot,0);
        }

        return;
    }

    if(decl_node->expr->type == ast_type::no_init)
    {
        return;
    }

    // normal assign
    const auto rtype = compile_expression(itl,func,decl_node->expr,reg_slot);

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
            compile_arr_decl(itl,func,decl_node,slot);
            break;
        }

        case type_class::struct_t:
        {
            compile_struct_decl(itl,func,decl_node,slot);
            break;
        }

        default:
        {
            compile_basic_decl(itl,func,decl_node,ltype,slot);
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
}

std::pair<Type*, RegSlot> compile_expression_tmp(Interloper &itl,Function &func,AstNode *node)
{
    // assume a size then refine it with expr result
    const RegSlot dst_slot = new_tmp(func,GPR_SIZE);

    Type* type = compile_expression(itl,func,node,dst_slot);

    func.registers[dst_slot.tmp_slot.handle] = make_reg(itl,dst_slot,type);

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
    Type* type = compile_expression(itl,func,auto_decl->expr,reg_slot);


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


    // setup the allocation info now we have it
    alloc->value.v[1].imm = !is_plain_type(type);

    // also assign back the correct type and register info
    auto& sym = sym_from_slot(itl.symbol_table,sym_slot);
    sym.reg = make_reg(itl,reg_slot,type);
    sym.type = type;
}


BlockSlot compile_basic_block(Interloper& itl, Function& func, BlockNode* block_node)
{
    const BlockSlot block_slot = new_basic_block(itl,func);
    compile_block(itl,func,block_node);

    return block_slot;
}

void compile_init_list(Interloper& itl, Function& func, Type* ltype, AddrSlot addr_slot, AstNode* node)
{
    if(is_struct(ltype))
    {
        const auto structure = struct_from_type(itl.struct_table,ltype);

        traverse_struct_initializer(itl,func,(RecordNode*)node,addr_slot,structure);                        
    }

    else
    {
        panic(itl,itl_error::undefined_type_oper,"initializer list assign not allowed on type: %s\n",type_name(itl,ltype).buf);
    }
}

void compile_block(Interloper &itl,Function &func,BlockNode *block_node)
{
    enter_new_anon_scope(itl.symbol_table);

    const u32 size = count(block_node->statements);
    for(u32 s = 0; s < size; s++)
    {
        AstNode* line = block_node->statements[s];

        if(itl.error)
        {
            return;
        }

        itl.ctx.expr = line;
    
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
                    switch(assign_node->left->type)
                    {
                        case ast_type::deref:
                        {
                            const auto [rtype,slot] = compile_oper(itl,func,assign_node->right);

                            UnaryNode* deref_node = (UnaryNode*)assign_node->left;

                            const auto [ptr_type,addr_slot] = take_pointer(itl,func,deref_node->next);

                            if(itl.error)
                            {
                                return;
                            }

                            // store into the pointer
                            auto ltype = deref_pointer(ptr_type); 
                            do_ptr_store(itl,func,slot,addr_slot,ltype);

                            check_assign(itl,ltype,rtype);
                            break;                        
                        }
                    
                        case ast_type::index:
                        {
                            const auto [rtype,slot] = compile_oper(itl,func,assign_node->right);

                            write_arr(itl,func,assign_node->left,rtype,slot);
                            break;
                        }
                    
                        // write on struct member!
                        case ast_type::access_struct:
                        {
                            if(assign_node->right->type == ast_type::initializer_list)
                            {
                                auto [ltype, addr_slot] = compute_member_addr(itl,func,assign_node->left);

                                compile_init_list(itl,func,ltype,addr_slot,assign_node->right);
                            }

                            else
                            {
                                const auto [rtype,slot] = compile_oper(itl,func,assign_node->right);
                                write_struct(itl,func,slot,rtype,assign_node->left);
                            }
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

                    // handle initializer list
                    if(assign_node->right->type == ast_type::initializer_list)
                    {
                        const auto addr_slot = make_struct_addr(slot,0);
                        compile_init_list(itl,func,ltype,addr_slot,assign_node->right);
                    }

                    else
                    {
                        const auto rtype = compile_expression(itl,func,assign_node->right,slot);
                        check_assign(itl,ltype,rtype);

                        if(is_unsigned_integer(ltype))
                        {
                            clip_arith_type(itl,func,slot,slot,size);
                        }
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
                        const RegSlot rv = is_float(func.sig.return_type[0])? make_spec_reg_slot(spec_reg::rv_float) : make_spec_reg_slot(spec_reg::rv);
                        const auto rtype = compile_expression(itl,func,record_node->nodes[0],rv);
        
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

                            do_ptr_store(itl,func,ret_slot,make_sym_reg_slot(func.sig.args[r]),func.sig.return_type[r]);
                        }
                    }
                
                }

                // no return
                else
                {
                    if(!is_void(func.sig.return_type[0]))
                    {
                        panic(itl,itl_error::missing_args,"Expected return type of %s got nothing\n",type_name(itl,func.sig.return_type[0]).buf);
                        return;
                    }
                }

                ret(itl,func);
                break;
            }

            case ast_type::scope:
            {
                ScopeNode* scope_node = (ScopeNode*)line;
                NameSpace* name_space = scan_namespace(itl.global_namespace,scope_node->scope);

                if(!name_space)
                {
                    panic(itl,itl_error::undeclared,"Could not find namespace\n");
                    return;
                }

                compile_scoped_stmt(itl,func,scope_node->expr,name_space);
                break;
            }

            case ast_type::function_call:
            {
                compile_function_call(itl,func,line,make_spec_reg_slot(spec_reg::null));
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
                compile_function_call(itl,func,line,make_spec_reg_slot(spec_reg::null));
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

            case ast_type::struct_return:
            {
                StructReturnNode* struct_return = (StructReturnNode*)line;

                // Check we have a valid struct
                const auto struct_decl = lookup_type(itl,struct_return->struct_name);

                if(!struct_decl || struct_decl->kind != type_kind::struct_t)
                {
                    panic(itl,itl_error::struct_error,"No such struct: %s\n",struct_return->struct_name);
                    break;
                }

                if(count(func.sig.return_type) != 1)
                {
                    panic(itl,itl_error::struct_error,"Expected single return for struct return func expects: %d\n",count(func.sig.return_type));
                    break;
                }


                // Check the return type matches
                const auto struct_type = make_struct(itl,struct_decl->type_idx);
                check_assign(itl,func.sig.return_type[0],struct_type);

                // Compile a initializer list into the return type
                const auto &structure = itl.struct_table[struct_decl->type_idx];
                traverse_struct_initializer(itl,func,struct_return->record,make_addr(make_sym_reg_slot(func.sig.args[0]),0),structure);

                ret(itl,func);
                break;            
            }

            default:
            {
                panic(itl,itl_error::invalid_statement,"[COMPILE] unexpected statement: %s\n",AST_NAMES[u32(line->type)]);
                break;
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
        GlobalDeclNode* decl_node = itl.global_decl[c];

        switch_context(itl,decl_node->filename,decl_node->name_space,(AstNode*)decl_node);
        
        compile_decl(itl,func,(AstNode*)decl_node->decl,true);

        pop_context(itl);

        if(itl.error)
        {
            return;
        }
    }

    finalise_global_offset(itl);
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

void check_startup_defs(Interloper& itl)
{   
    if(itl.rtti_enable)
    {
        cache_rtti_structs(itl);
    }

    check_startup_func(itl,"main",itl.global_namespace);
    check_startup_func(itl,"start",itl.global_namespace);

    itl.std_name_space = find_name_space(itl,"std");

    if(!itl.std_name_space)
    {
        panic(itl,itl_error::undeclared,"std namespace is not declared");
        return;
    }

    check_startup_func(itl,"memcpy",itl.std_name_space);
    check_startup_func(itl,"zero_mem",itl.std_name_space);
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

void code_generation(Interloper& itl)
{
    auto start = std::chrono::high_resolution_clock::now();

    check_startup_defs(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    putchar('\n');

    // compile all our constant values 
    compile_constants(itl);
    compile_globals(itl);

    declare_compiler_constants(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    // go through each function and compile
    // how do we want to handle getting to the entry point / address allocation?
    // do we want a "label" for each function? 
    compile_functions(itl);

    // okay we dont need the parse tree anymore
    // free it
    destroy_ast(itl);

    auto end = std::chrono::high_resolution_clock::now();

    itl.code_gen_time = std::chrono::duration<double, std::milli>(end-start).count();
}

void parsing(Interloper& itl, const String& initial_filename)
{
    // parse intial input file
    auto start = std::chrono::high_resolution_clock::now();

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

}

void compile(Interloper &itl,const String& initial_filename, const String& executable_path)
{
    printf("compiling file: %s\n",initial_filename.buf);

    itl.error = false;
    itl.error_code = itl_error::none;

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

    parsing(itl,initial_filename);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    code_generation(itl);

    if(itl.error)
    {
        destroy_itl(itl);
        return;
    }

    if(itl.optimise)
    {
        optimise_ir(itl);
    }
    
    backend(itl,executable_path);
}
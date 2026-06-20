
AstNode* copy_ast(Interloper& itl, AstNode* node);
TypeNode* copy_ast_type(Interloper& itl,TypeNode* type);
FuncNode* copy_ast_func(Interloper& itl, FuncNode* function);

template<typename T>
T* alloc_node_copy(Interloper& itl, T* src)
{
    T* copy = (T*)allocate(itl.parser_alloc.ast_allocator,sizeof(T));
    *copy = {};
    copy->node = src->node;

    return copy;
} 

void add_copy_data_pointer(Interloper& itl, void* data)
{
    if(data)
    {
        push_raw_var(itl.parser_alloc.ast_arrays,data);
    }
}

template<typename T>
void copy_ast_array(Interloper& itl, Array<T>& copy, const Array<T>& src)
{
    copy = {};
    for(auto arg : src)
    {
        push_var(copy,(T)copy_ast(itl,(AstNode*)arg));
    }

    add_copy_data_pointer(itl,&copy.data);
}


TypeNode* copy_ast_type(Interloper& itl,TypeNode* type)
{
    if(!type)
    {
        return nullptr;
    }

    TypeNode* copy = alloc_node_copy(itl,type);
    *copy = *type;

    copy->func_type = copy_ast_func(itl,type->func_type);
    
    copy->compound = {};
    for(auto& compound : type->compound)
    {
        const CompoundType compound_new = {compound.type,copy_ast(itl,compound.array_size)};
        push_var(copy->compound,compound_new);
    }

    add_copy_data_pointer(itl,&copy->compound.data);
    return copy;
}

DeclNode* copy_ast_decl(Interloper& itl, DeclNode* decl)
{
    if(!decl)
    {
        return nullptr;
    }

    DeclNode* copy = alloc_node_copy(itl,decl);
    *copy = *decl;

    copy->type = copy_ast_type(itl,decl->type);
    copy->expr = copy_ast(itl,decl->expr);

    return copy;
}

AstBlock copy_ast_block(Interloper& itl, const AstBlock& block)
{
    AstBlock out;

    for(auto& stmt: block.statement)
    {
        push_var(out.statement,copy_ast(itl,stmt));
    }

    return out;
}

IfStmt copy_ast_if_stmt(Interloper& itl, const IfStmt& stmt)
{
    IfStmt out;
    out.expr = copy_ast(itl,stmt.expr);

    out.block = (AstBlock*)allocate(itl.parser_alloc.ast_allocator,sizeof(AstBlock));
    *out.block = copy_ast_block(itl,*stmt.block);

    out.type = stmt.type;

    return out;
}

IfNode* copy_ast_if(Interloper& itl, IfNode* if_node)
{
    if(!if_node)
    {
        return nullptr;
    }

    IfNode* copy = alloc_node_copy(itl,if_node);
    *copy = *if_node;

    copy->if_stmt = copy_ast_if_stmt(itl,if_node->if_stmt);
    add_copy_data_pointer(itl,&copy->if_stmt.block->statement.data);    

    copy->else_if_stmt = {};
    for(auto& stmt : if_node->else_if_stmt)
    {
        push_var(copy->else_if_stmt,copy_ast_if_stmt(itl,stmt));
    }

    add_copy_data_pointer(itl,&copy->else_if_stmt.data);

    copy->else_stmt = copy_ast_block(itl,if_node->else_stmt);
    add_copy_data_pointer(itl,&copy->else_stmt.statement.data);


    return copy;
}

FuncNode* copy_ast_func(Interloper& itl, FuncNode* function)
{
    if(!function)
    {
        return nullptr;
    }

    FuncNode* copy = alloc_node_copy(itl,function);

    *copy = *function;

    copy->return_type = {};
    for(auto& return_type : function->return_type)
    {   
        const FuncReturnVar return_copy = {return_type.name,copy_ast_type(itl,return_type.type)};
        push_var(copy->return_type,return_copy);
    }

    copy_ast_array(itl,copy->args,function->args);

    copy->block = copy_ast_block(itl,function->block);

    // NOTE: Generic overload intentionally not copied

    // This is const so we can just the pointer
    // This is only for debugging.
    copy->generic_call = function->generic_call;

    add_copy_data_pointer(itl,&copy->block.statement.data);
    add_copy_data_pointer(itl,&copy->return_type.data);

    return copy;
}


template<typename T>
T* copy_ast_expr_bin_oper_node(Interloper& itl, T* expr)
{
    T* copy = alloc_node_copy(itl,expr);
    copy->oper = expr->oper;
    copy->left = copy_ast(itl,expr->left);
    copy->right = copy_ast(itl,expr->right);

    return copy;
}

RetNode* copy_ast_ret(Interloper& itl, RetNode* ret)
{
    RetNode* copy = alloc_node_copy(itl,ret);

    copy_ast_array(itl,copy->expr,ret->expr);

    return copy;
}

CastNode* copy_ast_cast(Interloper& itl, CastNode* cast)
{
    CastNode* copy = alloc_node_copy(itl,cast);
    copy->type = copy_ast_type(itl,cast->type);
    copy->expr = copy_ast(itl,cast->expr);
    cast->oper = cast->oper;

    return copy;
}

template<typename T>
T* copy_pod_node(Interloper& itl, T* node)
{
    T* copy = alloc_node_copy(itl,node);
    *copy = *node;

    return copy;
}

ArithUnaryNode* copy_arith_unary(Interloper& itl, ArithUnaryNode* unary)
{
    ArithUnaryNode* copy = alloc_node_copy(itl,unary);
    copy->oper = unary->oper;
    copy->expr = copy_ast(itl,unary->expr);

    return copy;
}

AutoDeclNode* copy_ast_auto_decl(Interloper& itl, AutoDeclNode* decl)
{
    AutoDeclNode* copy = alloc_node_copy(itl,decl);
    copy->sym = decl->sym;
    copy->expr = copy_ast(itl,decl->expr);

    return copy;    
}

template<typename T>
T* copy_ast_unary(Interloper& itl, T* unary)
{
    T* copy = alloc_node_copy(itl,unary);
    copy->expr = copy_ast(itl,unary->expr);

    return copy;       
}

template<typename T>
T* copy_ast_expr_bin_node(Interloper& itl, T* bin)
{
    T* copy = alloc_node_copy(itl,bin);
    copy->left = copy_ast(itl,bin->left);
    copy->right = copy_ast(itl,bin->right);

    return copy;    
}

ForRangeNode* copy_ast_for_range(Interloper& itl, ForRangeNode* range)
{
    ForRangeNode* copy = alloc_node_copy(itl,range);
    *copy = *range;

    copy->cond = copy_ast(itl,range->cond);

    copy->block = copy_ast_block(itl,range->block);
    add_copy_data_pointer(itl,&copy->block.statement.data);

    return copy;    
}

// struct FuncCallNode
// {
//     AstNode node;

//     AstNode* expr = nullptr;
//     func_call_type type = func_call_type::call;

//     union
//     {
//         FuncCall call = {};
//         size_t intrinsic_idx;
//     };

//     Array<TypeNode*> generic_args;
//     Array<AstNode*> args;
// };

FuncCallNode* copy_ast_function_call(Interloper& itl, FuncCallNode* call)
{
    FuncCallNode* copy = alloc_node_copy(itl,call);
    *copy = *call;
    
    copy->expr = copy_ast(itl,call->expr);
    copy->call = {};

    copy_ast_array(itl,copy->args,call->args);
    copy_ast_array(itl,copy->generic_args,call->generic_args);

    return copy;
}

TypeOperatorNode* copy_type_operator(Interloper& itl, TypeOperatorNode* type_operator)
{
    TypeOperatorNode* copy = alloc_node_copy(itl,type_operator);
    copy->oper = type_operator->oper;
    copy->type = copy_ast_type(itl,type_operator->type);

    return copy;
}

AstNode* copy_ast_plain(Interloper& itl, AstNode* node)
{
    AstNode* copy = (AstNode*)allocate(itl.parser_alloc.ast_allocator,sizeof(AstNode));
    *copy = *node;

    return copy;
}

StructAccessNode* copy_ast_struct_access(Interloper& itl, StructAccessNode* access)
{
    StructAccessNode* copy = alloc_node_copy(itl,access);
    copy->expr = copy_ast(itl,access->expr);
    copy->flags = access->flags;
    
    copy->members = {};
    for(const auto& member : access->members)
    {
        push_var(copy->members,member);
    }

    add_copy_data_pointer(itl,&copy->members.data);

    return copy;
}


AstNode* copy_ast(Interloper& itl, AstNode* node)
{
    if(!node)
    {
        return nullptr;
    }

    switch(node->type)
    {
        case ast_type::no_init:
        {
            return copy_ast_plain(itl,node);
        }

        case ast_type::function:
        {
            return (AstNode*)copy_ast_func(itl,(FuncNode*)node);
        }

        case ast_type::type_operator:
        {
            return (AstNode*)copy_type_operator(itl,(TypeOperatorNode*)node);
        }

        case ast_type::function_call:
        {
            return (AstNode*)copy_ast_function_call(itl,(FuncCallNode*)node);
        }

        case ast_type::type:
        {
            return (AstNode*)copy_ast_type(itl,(TypeNode*)node);
        }

        case ast_type::decl:
        {
            return (AstNode*)copy_ast_decl(itl,(DeclNode*)node);
        }

        case ast_type::if_t:
        {
            return (AstNode*)copy_ast_if(itl,(IfNode*)node);
        }

        case ast_type::comparison:
        {
            return (AstNode*)copy_ast_expr_bin_oper_node(itl,(CmpNode*)node);
        }

        case ast_type::boolean_logic:
        {
            return (AstNode*)copy_ast_expr_bin_oper_node(itl,(BooleanLogicNode*)node);
        }

        case ast_type::arith_bin:
        {
            return (AstNode*)copy_ast_expr_bin_oper_node(itl,(ArithBinNode*)node);
        }

        case ast_type::shift:
        {
            return (AstNode*)copy_ast_expr_bin_oper_node(itl,(ShiftNode*)node);
        }

        case ast_type::cast:
        {
            return (AstNode*)copy_ast_cast(itl,(CastNode*)node);
        }

        case ast_type::symbol:
        {
            return (AstNode*)copy_pod_node(itl,(SymbolNode*)node);
        }

        case ast_type::generic_var:
        {
            return (AstNode*)copy_pod_node(itl,(GenericVarNode*)node);
        }

        case ast_type::value:
        {
            return (AstNode*)copy_pod_node(itl,(ValueNode*)node);
        }

        case ast_type::arith_unary:
        {
            return (AstNode*)copy_arith_unary(itl,(ArithUnaryNode*)node);
        }

        case ast_type::ret:
        {
            return (AstNode*)copy_ast_ret(itl,(RetNode*)node);
        }

        case ast_type::auto_decl:
        {
            return (AstNode*)copy_ast_auto_decl(itl,(AutoDeclNode*)node);
        }

        case ast_type::deref:
        {
            return (AstNode*)copy_ast_unary(itl,(DerefNode*)node);
        }

        case ast_type::addrof:
        {
            return (AstNode*)copy_ast_unary(itl,(AddrOfNode*)node);
        }

        case ast_type::sizeof_t:
        {
            return (AstNode*)copy_ast_unary(itl,(SizeOfNode*)node);
        }

        case ast_type::const_assert:
        {
            return (AstNode*)copy_ast_unary(itl,(ConstAssertNode*)node);
        }

        case ast_type::struct_access:
        {
            return (AstNode*)copy_ast_struct_access(itl,(StructAccessNode*)node);
        }

        case ast_type::assign:
        {
            return (AstNode*)copy_ast_expr_bin_node(itl,(AssignNode*)node);
        }

        case ast_type::for_range:
        {
            return (AstNode*)copy_ast_for_range(itl,(ForRangeNode*)node);
        }

        default: break;
    }

    unimplemented("Cannot copy ast node of type: %s\n",AST_INFO[u32(node->type)].name);
}
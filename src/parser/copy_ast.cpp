
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

    copy->args= {};
    for(DeclNode* decl : function->args)
    {
        push_var(copy->args,copy_ast_decl(itl,decl));
    }

    copy->block = copy_ast_block(itl,function->block);

    // NOTE: Generic overload intentionally not copied

    // This is const so we can just the pointer
    // This is only for debugging.
    copy->generic_call = function->generic_call;

    add_copy_data_pointer(itl,&copy->block.statement.data);
    add_copy_data_pointer(itl,&copy->return_type.data);
    add_copy_data_pointer(itl,&copy->args.data);

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

    copy->expr = {};
    for(AstNode* expr : ret->expr)
    {
        push_var(copy->expr,copy_ast(itl,expr));
    }

    assert(copy->expr.data != ret->expr.data);
    add_copy_data_pointer(itl,&copy->expr.data);

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

AstNode* copy_ast(Interloper& itl, AstNode* node)
{
    if(!node)
    {
        return nullptr;
    }

    switch(node->type)
    {
        case ast_type::function:
        {
            return (AstNode*)copy_ast_func(itl,(FuncNode*)node);
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

        case ast_type::assign:
        {
            return (AstNode*)copy_ast_expr_bin_node(itl,(AssignNode*)node);
        }

        default: break;
    }

    unimplemented("Cannot copy ast node of type: %s\n",AST_INFO[u32(node->type)].name);
}
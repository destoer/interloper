
AstNode* copy_ast(Interloper& itl, AstNode* node);
TypeNode* copy_ast_type(Interloper& itl,TypeNode* type);
FuncNode* copy_ast_func(Interloper& itl, FuncNode* function);

template<typename T>
T* alloc_node_copy(Interloper& itl, T* src)
{
    T* copy = (T*)allocate(itl.parser_alloc.ast_allocator,sizeof(T));
    copy->node = src->node;

    return copy;
} 

void add_copy_data_pointer(Interloper& itl, void* data)
{
    push_raw_var(itl.parser_alloc.ast_arrays,data);
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

    add_copy_data_pointer(itl,copy->compound.data);
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

AstBlock copy_ast_block(Interloper& itl, const AstBlock block)
{
    AstBlock out;

    for(auto& stmt: block.statement)
    {
        push_var(out.statement,copy_ast(itl,stmt));
    }

    return out;
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

    copy->generic = copy_array(function->generic);

    copy->block = copy_ast_block(itl,function->block);

    add_copy_data_pointer(itl,copy->return_type.data);
    add_copy_data_pointer(itl,copy->args.data);
    add_copy_data_pointer(itl,copy->generic.data);

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

        default: break;
    }

    unimplemented("Cannot copy ast node of type: %s\n",AST_INFO[u32(node->type)].name);
}
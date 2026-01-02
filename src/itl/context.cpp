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

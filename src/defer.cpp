
Option<itl_error> type_check_stmt(Interloper& itl, Function& func, AstNode* stmt);
void compile_stmt(Interloper& itl, Function& func, AstNode* stmt);

void add_defer(Interloper& itl, DeferNode* defer)
{
    defer->prev = itl.cur_defer_node;
    itl.cur_defer_node = defer;
}


Option<itl_error> type_check_defer(Interloper& itl, Function& func, AstNode* stmt)
{
    DeferNode* defer = (DeferNode*)stmt;
    add_defer(itl,defer);

    return type_check_stmt(itl,func,defer->stmt);
}

void compile_defer(Interloper& itl, Function& func, DeferNode* start, DeferNode* end)
{
    // Compiled in reverse order
    while(end != start)
    {
        compile_stmt(itl,func,end->stmt);
        end = end->prev;
    }

}

void destroy_defer(Interloper& itl)
{
    UNUSED(itl);
}


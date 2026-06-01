
Option<itl_error> type_check_stmt(Interloper& itl, Function& func, AstNode* stmt);
void compile_stmt(Interloper& itl, Function& func, AstNode* stmt);

void add_defer(Interloper& itl, DeferNode* defer)
{
    if(!itl.cur_defer_node)
    {
        itl.cur_defer_node = append(itl.defer_list,defer);
        return;
    }

    itl.cur_defer_node = insert_after(itl.defer_list,itl.cur_defer_node,defer);
}


Option<itl_error> type_check_defer(Interloper& itl, Function& func, AstNode* stmt)
{
    DeferNode* defer = (DeferNode*)stmt;
    add_defer(itl,defer);

    return type_check_stmt(itl,func,defer->expr);
}

void compile_defer(Interloper& itl, Function& func, DeferListNode* start, DeferListNode* end)
{
    // Compiled in reverse order
    while(end != start)
    {
        puts("compile defer");
        compile_stmt(itl,func,end->value->expr);
        end = end->prev;
    }

}

void destroy_defer(Interloper& itl)
{
    UNUSED(itl);
}


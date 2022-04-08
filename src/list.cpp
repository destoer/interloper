#include <ir.h>

//TODO: actually implement this
ListNode *alloc_node()
{
    unimplemented("list allocation");
    return nullptr;
}


ListNode *insert(ListNode *cur, const Opcode opcode)
{
    ListNode *node = alloc_node();
    node->opcode = opcode;
    node->prev = cur;
    node->next = cur->next;

    cur->next = node;

    return node;
}

void insert_end(List &list, const Opcode opcode)
{
    list.end = insert(list.end,opcode);
}

ListNode *next(ListNode *node)
{
    return node->next;
}

ListNode *prev(ListNode *node)
{
    return node->prev;
}


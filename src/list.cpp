#include <ir.h>
// TODO: start on this list impl


struct ListNode
{
    Opcode opcode;
    ListNode *next;
    ListNode *prev;
};

//TODO:
ListNode *alloc_node()
{
    return nullptr;
}

void insert(Opcode &opcode, ListNode *cur)
{
    ListNode *node = alloc_node();
    node->opcode = opcode;
    node->prev = cur;
    cur->next = node;
}

ListNode *next(ListNode *node)
{
    return node->next;
}

ListNode *prev(ListNode *node)
{
    return node->prev;
}


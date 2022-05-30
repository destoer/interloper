#include <ir.h>

void print(List &list)
{
    puts("list contents:");

    ListNode *tmp = list.start;
    while(tmp)
    {
        disass_opcode_raw(tmp->opcode);
        tmp = tmp->next;
    }
}

List make_list(ArenaAllocator* allocator)
{
    List list = {};

    // default to a megabyte
    list.allocator = allocator;

    return list;
}

ListNode *alloc_node(List& list)
{
    void* ptr = allocate(*list.allocator,sizeof(ListNode));
    return (ListNode*)(ptr);
}

// returns inserted node
ListNode *insert_at(List &list, ListNode *cur, const Opcode &opcode)
{
    ListNode *node = alloc_node(list);
    *node = {};

    node->opcode = opcode;


    if(!list.start)
    {
        list.start = node;
        list.end = node;
    }

    else
    {
        node->next = cur;
        node->prev = cur->prev;
        cur->prev = node;

        if(cur == list.start)
        {
            list.start = node;    
        }

        else
        {
            node->prev->next = node;
        }
    }

    return node;
}

// returns inserted node
ListNode *insert_after(List &list, ListNode *cur, const Opcode &opcode)
{
    ListNode *node = alloc_node(list);
    *node = {};

    node->opcode = opcode;

    if(!list.start)
    {
        list.start = node;
        list.end = node;        
    }

    else
    {
        node->next = cur->next;
        node->prev = cur;
        cur->next = node;

        if(cur == list.end)
        {
            list.end = node;
        }

        else
        {
            node->next->prev = node;
        }
    }

    return node;
}

void append(List &list, const Opcode opcode)
{
    insert_after(list,list.end,opcode);
}

void insert_front(List &list, const Opcode opcode)
{
    insert_at(list,list.start,opcode);
}

// return node after deleted
ListNode *remove(List &list, ListNode* node)
{
    if(node == list.start)
    {
        list.start = node->next;
        
        if(list.start)
        {
            list.start->prev = nullptr;

            return list.start;
        }

        return nullptr;
    }

    else if(node == list.end)
    {
        list.end = node->prev;

        if(list.end)
        {
            list.end->next = nullptr;
        }

        return nullptr;
    }

    else
    {
        // unlink the "middle" node
        ListNode *before = node->prev;

        before->next = node->next;
        before->next->prev = before;

        return before->next;
    }
}

void cleave_list(List &list, ListNode* node)
{
    node->next = nullptr;

    if(list.start == list.end)
    {
        list.start = node;
    }

    list.end = node;
}

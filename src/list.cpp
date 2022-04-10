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

//TODO: actually implement this with a pool allocator 
ListNode *alloc_node()
{
    //unimplemented("list allocation");
    return new ListNode;
}


ListNode *insert_at(List &list, ListNode *cur, const Opcode &opcode)
{
    ListNode *node = alloc_node();
    node->opcode = opcode;


    if(!list.start)
    {
        list.start = node;
        list.end = node;
    }

    else
    {
        if(cur == list.start)
        {
            list.start = node;    
        }

        node->next = cur;
        node->prev = cur->prev;
        cur->prev = node;
    }

    return node;
}

// TODO: start here
ListNode *insert_after(List &list, ListNode *cur, const Opcode &opcode)
{
    ListNode *node = alloc_node();
    node->opcode = opcode;

    if(!list.start)
    {
        list.start = node;
        list.end = node;        
    }

    else
    {
        if(cur == list.end)
        {
            list.end = node;
        }
        
        node->next = cur->next;
        node->prev = cur;
        cur->next = node;
    }

    print(list);

    return node;
}

void append(List &list, const Opcode opcode)
{
    insert_after(list,list.end,opcode);
}

// TODO:
// void delete_node

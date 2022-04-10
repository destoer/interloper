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


// returns inserted node
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

// returns inserted node
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

    return node;
}

void append(List &list, const Opcode opcode)
{
    insert_after(list,list.end,opcode);
}

// return node after deleted
ListNode *remove(List &list, ListNode* node)
{
    printf("remove: ");
    disass_opcode_raw(node->opcode);

    if(node == list.start)
    {
        list.start = node->next;
        
        if(list.start)
        {
            list.start->prev = nullptr;

            puts("list");
            print(list);

            return list.start->next;
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

        puts("list");
        print(list);

        return nullptr;
    }

    else
    {
        // unlink the "middle" node
        ListNode *before = node->prev;

        before->next = node->next;
        before->next->prev = before;

        puts("list");
        print(list);

        return before->next;
    }
}

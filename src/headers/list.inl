template<typename T>
struct ListNode
{
    T value;
    ListNode *next = nullptr;
    ListNode *prev = nullptr;
};

template<typename T>
struct ListIterator
{
    ListIterator(ListNode<T>* node)
    {
        this->node = node;
    }

    bool operator==(const ListIterator<T>& it) const 
    {
        return node == it.node;
    }

    ListIterator<T>& operator++()
    {
        node = node->next;
        return *this;
    }

    ListNode<T>& operator*()
    {
        return *node;
    }

    const ListNode<T>& operator*() const
    {
        return *node;
    }

    ListNode<T>* node = nullptr;
};

template<typename T>
struct List
{
    // global list Arena allocator
    ArenaAllocator *allocator = nullptr;

    ListNode<T> *start = nullptr;

    ListNode<T> *finish = nullptr;

    ListIterator<T> begin()
    {
        return ListIterator(start);
    }

    ListIterator<T> end()
    {
        return ListIterator<T>(nullptr);
    }

    const ListIterator<T> begin() const
    {
        return ListIterator(start);
    }

    const ListIterator<T> end() const
    {
        return ListIterator<T>(nullptr);
    }
};

enum class insertion_type
{
    after,
    before,
};

template<typename T>
inline List<T> make_list(ArenaAllocator* allocator)
{
    List<T> list = {};

    // default to a megabyte
    list.allocator = allocator;

    return list;
}

template<typename T>
inline ListNode<T> *alloc_node(List<T>& list)
{
    void* ptr = allocate(*list.allocator,sizeof(ListNode<T>));
    return (ListNode<T>*)(ptr);
}

// returns inserted node
template<typename T>
inline ListNode<T> *insert_at(List<T> &list, ListNode<T> *cur, const T &value)
{
    ListNode<T> *node = alloc_node(list);
    *node = {};

    node->value = value;


    if(!list.start)
    {
        list.start = node;
        list.finish = node;
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
template<typename T>
inline ListNode<T> *insert_after(List<T> &list, ListNode<T> *cur, const T& value)
{
    ListNode<T> *node = alloc_node(list);
    *node = {};

    node->value = value;

    if(!list.start)
    {
        list.start = node;
        list.finish = node;        
    }

    else
    {
        node->next = cur->next;
        node->prev = cur;
        cur->next = node;

        if(cur == list.finish)
        {
            list.finish = node;
        }

        else if(node->next)
        {
            node->next->prev = node;
        }
    }

    return node;
}

template<typename T>
inline ListNode<T>* insert_node(List<T>& list, ListNode<T>* cur, const T& value, insertion_type type) 
{
    if(type == insertion_type::after)
    {
        return insert_after(list,cur,value);
    }

    return insert_at(list,cur,value);
}

template<typename T>
inline size_t count_list_dist(const ListNode<T>* start, const ListNode<T>* end)
{
    size_t dist = 0;

    while(start && start != end)
    {
        start = start->next;
        dist++;
    }

    return dist;
}

template<typename T>
inline void append(List<T> &list, const T& value)
{
    insert_after(list,list.finish,value);
}

template<typename T>
inline ListNode<T> *insert_front(List<T> &list, const T& value)
{
    return insert_at(list,list.start,value);
}

// return node after deleted
template<typename T>
inline ListNode<T> *remove(List<T> &list, ListNode<T>* node)
{
    if(!node)
    {
        return nullptr;
    }

    if(node == list.start)
    {
        list.start = node->next;
        
        if(list.start)
        {
            list.start->prev = nullptr;
        }

        return list.start;
    }

    else if(node == list.finish)
    {
        list.finish = node->prev;

        if(list.finish)
        {
            list.finish->next = nullptr;
        }

        return nullptr;
    }

    else
    {
        // unlink the "middle" node
        ListNode<T> *before = node->prev;
        ListNode<T> *next = node->next;

        before->next = next;

        if(next)
        {
            next->prev = before;
        }

        return next;
    }
}
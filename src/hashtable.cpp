
template<typename T>
HashTable<T> make_table()
{
    HashTable<T> table;
    resize(table.buf,HASH_TABLE_DEFAULT_SIZE);

    return table;
}


template<typename T>
void destroy_table(HashTable<T> &table)
{
    for(u32 i = 0; i < count(table.buf); i++)
    {
        Bucket<T>& bucket = table.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            destroy_arr(bucket);
        }
    }    

    destroy_arr(table.buf);
}



u32 hash_slot(u32 size, const String& name)
{
    const u32 hash = hash_string(name);
    const u32 slot = hash & (size - 1);

    return slot;
}


template<typename T>
T* lookup(HashTable<T> &table, const String& name)
{
    const u32 slot = hash_slot(count(table.buf),name);

    Bucket<T>& bucket = table.buf[slot];

    
    for(u32 i = 0; i < count(bucket); i++)
    {
        if(bucket[i].name == name)
        {
            return &bucket[i].v;
        }
    }

    return nullptr;
}


template<typename T>
bool contains(HashTable<T> &table, const String& name)
{
    return lookup(table,name) != nullptr;
}

template<typename T>
void rehash(HashTable<T> &table, u32 table_size)
{
    Array<Bucket<T>> buf_new;
    resize(buf_new,table_size);

    for(u32 i = 0; i < count(table.buf); i++)
    {
        const Bucket<T>& bucket = table.buf[i];

        for(u32 j = 0; j < count(bucket); j++)
        {
            const auto &node = bucket[j];

            const u32 slot = hash_slot(count(buf_new),node.name);
            push_var(buf_new[slot],node);
        }
    }    

    destroy_table(table);

    table.buf = buf_new;
}

template<typename T>
void add(HashTable<T> &table, const String& name, T v)
{
    // TODO: this is very slow
    if(table.size == count(table.buf))
    {
        rehash(table,count(table.buf) * 2);
    }


    const u32 slot = hash_slot(count(table.buf),name);

    Bucket<T>& bucket = table.buf[slot];

    for(u32 i = 0; i < count(bucket); i++)
    {
        // allready exists
        if(bucket[i].name == name)
        {
            bucket[i].v = v;
            return;
        }
    }


    // did not exist in bucket add it
    HashNode<T> node = {name,v};
    push_var(bucket,node);
    table.size++;
}
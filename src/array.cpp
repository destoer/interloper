
// insert at end as a var as a raw set of bytes into the Array
template<typename T, typename Y>
void push_var(Array<T> &arr,Y v)
{
    const u32 size = sizeof(v);

    reserve(arr,size);

    // actually write in the data
    handle_write(&arr.data[count(arr)],v);
    arr.size += size;
}

template<typename T>
u32 count(Array<T> arr)
{
    return arr.size / sizeof(T);
}

template<typename T>
T pop(Array<T> &arr)
{
    const T v = arr.data[count(arr) - 1];
    arr.size -= sizeof(v);

    return v;
}


template<typename T>
void reserve_mem(Array<T> &arr, u32 size)
{
    arr.capacity = size;
    arr.data = (T*)realloc(arr.data,size);       
}

template<typename T>
void resize(Array<T> &arr, u32 len)
{
    const u32 bytes = len * sizeof(T);

    const u32 old_len = count(arr);

    reserve_mem(arr,bytes);
    arr.size = bytes;


    // default initialize the new elements
    for(u32 i = old_len; i < count(arr); i++)
    {
        arr.data[i] = {};
    }
}


// make sure there is enough left for the allocation we are doing
template<typename T>
void reserve(Array<T> &arr, u32 size)
{
    const u32 free_size = arr.capacity - arr.size;

    // we have room to just dump this in
    if(free_size >= size)
    {
        return;
    }

    else
    {   
        const u32 new_capacity = (arr.capacity + size) * 2;
        reserve_mem(arr,new_capacity);
    }
}


// raw mem read and writes over the array
template<typename T, typename Y>
T read_var(const Array<Y> &arr, u32 idx)
{
    return handle_read<T>(&arr.data[idx]);
}

template<typename T, typename Y>
void write_var(Array<Y> &arr, u32 idx, T v)
{
    return handle_write(&arr.data[idx],v);
}

// insert raw memory block into the array
template<typename T>
void push_mem(Array<T>& arr, const void* data, u32 size)
{
    reserve(arr,size);

    memcpy(&arr.data[arr.size],data,size);
    arr.size += size;
}

template<typename T>
void destroy_arr(Array<T> &arr)
{
    if(arr.data)
    {
        free(arr.data);
        arr.data = nullptr;
    }
    arr.size = 0;
    arr.capacity = 0;
}




// build strings on an arena
using StringBuffer = Array<char>;


template<typename T>
void reserve_arena(ArenaAllocator& allocator,Array<T>& arr, u32 size)
{
    const u32 free_size = arr.capacity - arr.size;

    // aquire more memory from the arena
    if(free_size < size)
    {
        Arena& arena = cur_arena(allocator);

        u8* arena_ptr = (u8*)arena.buf;

        const u32 reserve = arena.size - arena.len;

        // we have enough memory left on the end of the arena to just give it more memory
        if((T*)&arena_ptr[arena.len] == &arr[arr.capacity] && size <= reserve)
        {
            reserve_end(arena,size);
            arr.capacity += size;
        }

        // get back a new chunk that is large enough and move it
        else
        {
            // allocate a new buffer large enough
            const u32 new_capacity = (arr.capacity + size) * 2;
            T* new_buf = (T*)allocate(allocator,new_capacity);

            // copy over the old data
            memcpy(new_buf,arr.data,arr.capacity);
            arr.capacity = new_capacity;

            arr.data = new_buf;
        }   
    }    
}

void push_char(ArenaAllocator& allocator, StringBuffer &buffer, char v)
{
    reserve_arena(allocator,buffer,sizeof(v));
    buffer[buffer.size++] = v;
}

void push_string(ArenaAllocator& allocator, StringBuffer& buffer, const String &str)
{
    reserve_arena(allocator,buffer,str.size);
    memcpy(&buffer.data[buffer.size],str.buf,str.size);

    buffer.size += str.size;
}


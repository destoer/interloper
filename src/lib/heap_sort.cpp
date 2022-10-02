u32 left(u32 idx) 
{
	return (2 * idx) + 1;
}

u32 right(u32 idx)
{
	return (2 * idx) + 2;
}

u32 parent(u32 idx)
{
	return (idx - 1) / 2;
}

template<typename T> 
void swap(T& v1, T& v2)
{
    const T tmp = v1;
    v1 = v2;
    v2 = tmp;
}

template<typename T, typename F>
void verify(const Array<T>& heap,u32 i, u32 len, F CMP_FUNC)
{
    for(; i < len; i++)
    {
        const u32 r = right(i);
        const u32 l = left(i);

        if(l < len && CMP_FUNC(heap[l],heap[i]))
        {
            printf("heap violated\n");
            exit(1);
        }

        if(r < len && CMP_FUNC(heap[r],heap[i]))
        {
            printf("heap violated\n");
            exit(1);
        }
    }
}

template<typename T, typename F>
void heapify(Array<T>& heap, u32 idx, u32 len, F CMP_FUNC)
{
    // check children satisfy the heap property
    while(idx <= len)
    {
        u32 target = idx;
        const u32 l = left(idx);
        const u32 r = right(idx);

        // swap idx with targer if its not allready
        if(l < len && CMP_FUNC(heap[l],heap[target]))
        {
            target = l;
        }
        
        if(r < len &&  CMP_FUNC(heap[r],heap[target]))
        {
            target = r;
        }
        
        // not allready in the right place
        if(target != idx)
        {
            swap(heap[target],heap[idx]);
            idx = target;
        }

        // we are allready in the right place we are done!
        else
        {
            break;
        }
    }
}

template<typename T, typename F>
void heap_sort(Array<T>& arr,F CMP_FUNC)
{
    const u32 size = count(arr);

    // setup the heap
    for(s32 i = (size / 2) - 1; i >= 0; i--)
    {
        heapify(arr,i,size,CMP_FUNC);
    }

    //verify(arr,0,size,CMP_FUNC);

    
    for(s32 i = size - 1; i > 0; i--)
    {
        // extract each item to end of array
        // then "hide" it from the heap
        swap(arr[0],arr[i]);

        // heap property is now violated fix it
        heapify(arr,0,i,CMP_FUNC);
    }
}
u32 left(u32 idx) 
{
	return 2 * idx + 1;
}

u32 right(u32 idx)
{
	return 2 * idx + 2;
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
void heapify(Array<T>& heap, u32 idx, u32 len, F GREATER_THAN_FUNC)
{
	// while we are larger than parent swap
	if(idx != 0 && GREATER_THAN_FUNC(heap[idx],heap[parent(idx)]))
	{
        while(idx != 0 && GREATER_THAN_FUNC(heap[idx],heap[parent(idx)]))
        {
            const auto parent_idx = parent(idx);
		    swap(heap[idx],heap[parent_idx]);
		    idx = parent(idx);
        }
	}

    // check that children arent smaller
    else
    {
        while(idx <= len)
        {
            size_t max = idx;
            const auto l = left(idx);
            const auto r = right(idx);

            // swap idx with smallest if its not allready
            if(l < len && GREATER_THAN_FUNC(heap[l],heap[max]))
            {
                max = l;
            }
            
            if(r < len &&  GREATER_THAN_FUNC(heap[r],heap[max]))
            {
                max = r;
            }
            
            // if we aernt the smallest
            if(max != idx)
            {
                swap(heap[max],heap[idx]);
                idx = max;
            }

            // we were the smallest so max heap is fixed
            else
            {
                break;
            }
        }
    }
}

template<typename T, typename F>
void heap_sort(Array<T>& arr,F GREATER_THAN_FUNC)
{
    const u32 size = count(arr);

    // setup the heap
    for(s32 i = (size / 2) - 1; i >= 0; i--)
    {
        heapify(arr,i,size,GREATER_THAN_FUNC);
    }

    
    for(s32 i = size - 1; i > 0; i--)
    {
        // extract each item to end of array
        // then "hide" it from the heap
        swap(arr[0],arr[i]);

        // heap property is now violated fix it
        heapify(arr,0,i,GREATER_THAN_FUNC);
    }
}
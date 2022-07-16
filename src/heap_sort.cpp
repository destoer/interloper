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
void heapify(Array<T>& heap, u32 idx, u32 len, F CMP_FUNC)
{
	// while we are larger than parent swap
	if(idx != 0 && CMP_FUNC(heap[idx],heap[parent(idx)]))
	{
        while(idx != 0 && CMP_FUNC(heap[idx],heap[parent(idx)]))
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
            size_t target = idx;
            const auto l = left(idx);
            const auto r = right(idx);

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

    
    for(s32 i = size - 1; i > 0; i--)
    {
        // extract each item to end of array
        // then "hide" it from the heap
        swap(arr[0],arr[i]);

        // heap property is now violated fix it
        heapify(arr,0,i,CMP_FUNC);
    }
}
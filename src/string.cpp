
bool string_equal(const String& str1, const String& str2)
{
    if(str1.size != str2.size)
    {
        return false;
    }

    for(u32 i = 0; i < str1.size; i++)
    {
        if(str1[i] != str2[i])
        {
            return false;
        }
    }

    return true;    
}

String string_offset(const String& str, u32 offset)
{
    String out;

    out.buf = &str.buf[offset];
    out.size -= offset;

    return out;
}

String string_slice(const String& str, u32 offset, u32 len)
{
    String out;

    out.buf = &str.buf[offset];

    if((str.size - offset) >= len)
    {
        out.size = len;
    }

    else
    {
        out.size = 0;
    }

    return out;
}

// NOTE: expects array to have a null term at the end
String make_string(StringBuffer& buffer)
{
    String string;

    string.buf = buffer.data;
    string.size = count(buffer) - 1;

    return string;
}

String make_string(ArenaAllocator& allocator,const char* str, u32 size)
{
    String string;

    char* ptr  = (char*)allocate(allocator,size + 1);
    memcpy(ptr,str,size);

    // null term the string
    ptr[size] = '\0';

    string.buf = ptr;
    string.size = size;

    return string;
}

String make_string(ArenaAllocator& allocator,const char* str)
{
    return make_string(allocator,str,strlen(str));
}

// make a string from a static
String make_static_string(const char* str, u32 size)
{
    String string;

    string.buf = str;
    string.size = size;

    return string;
}




// TODO: is splatting chars across the hash with a random number good enough?
static constexpr u32 HASH_MAGIC = 0x02579275;

u32 hash_string(const String& str, u32 hash)
{
    for(u32 i = 0; i < str.size; i++)
    {
        hash = (hash * HASH_MAGIC) ^ str[i];
    }

    return hash; 
}


u32 hash_slot(u32 size, const String& name)
{
    const u32 hash = hash_string(name,HASH_MAGIC);
    const u32 slot = hash & (size - 1);

    return slot;
}


String copy_string(ArenaAllocator& allocator, const String& in)
{
    char* ptr  = (char*)allocate(allocator,in.size + 1);
    memcpy(ptr,in.buf,in.size);

    ptr[in.size] = '\0';

    String string;

    string.buf = ptr;
    string.size = in.size;

    return string;
}

String cat_string(ArenaAllocator& allocator, const String &v1, const String& v2)
{
    const u32 size = v1.size + v2.size;

    char* ptr = (char*)allocate(allocator,size + 1);
    memcpy(ptr,v1.buf,v1.size);
    memcpy(&ptr[v1.size],v2.buf,v2.size);

    ptr[size] = '\0';

    String string;
    
    string.buf = ptr;
    string.size = size;

    return string;
}
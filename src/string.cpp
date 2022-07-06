
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

// NOTE: expects array to be null terminated
String make_string(Array<char>& arr)
{
    String string;

    string.buf = arr.data;
    string.size = arr.size - 1;

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

// make a string from a static
String make_static_string(const char* str, u32 size)
{
    String string;

    string.buf = str;
    string.size = size;

    return string;
}


// TODO: write a better hash func
u32 hash_string(const String& str)
{
    u32 hash = 0;

    for(u32 i = 0; i < str.size; i++)
    {
        hash += str[i] * (i + hash);
    }

    return hash;    
}

String copy_string(ArenaAllocator& allocator, const String& in)
{
    char* ptr  = (char*)allocate(allocator,in.size + 1);
    memcpy(ptr,in.buf,in.size + 1);

    String string;

    string.buf = ptr;
    string.size = in.size;
    
    return string;
}

std::string std_string(const String& string)
{
    return std::string(string.buf,string.size);
}
struct String;
bool string_equal(const String& str1, const String& str2);
u32 hash_string(const String& str);


struct String
{
    char& operator[] (u32 idx)
    {
        return this->buf[idx];
    }


    char operator[] (u32 idx) const
    {
        return this->buf[idx];
    }

    bool operator== (const String& other)
    {
        return string_equal(*this,other);
    }
    

    char* buf = nullptr;
    u32 size = 0;
};

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

String make_string(ArenaAllocator& allocator,const char* str, u32 size)
{
    String string;

    string.buf = (char*)allocate(allocator,size);
    string.size = size;

    memcpy(string.buf,str,size);

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
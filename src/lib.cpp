#include <lib.h>
constexpr char path_separator = std::filesystem::path::preferred_separator;


bool contains_ext(const String& str) 
{   
    return strchr(str.buf,'.') != NULL;
}    

template<typename access_type>
access_type handle_read(const void *buf)
{
    access_type v;
    memcpy(&v,buf,sizeof(access_type));
    return v;
}


void print_line(const String& filename,u32 line)
{
    // this is slow, but we are about to terminate anyways
    // when this is used
    FILE *fp = fopen(filename.buf,"r");

    if(!fp)
    {
        printf("could not open file %s for error printing\n",filename.buf);
    }

    char buf[512] = {0};
    for(u32 i = 0; i < line; i++)
    {   
        if(fgets(buf,sizeof(buf) - 2,fp))
        {
            break;
        }
    }

    fclose(fp);

    printf("%s\n",buf);    
}


template<typename access_type>
void handle_write(void *buf, access_type v)
{
    memcpy(buf,&v,sizeof(access_type));
}

template<typename T>
bool in_range(T v, T min, T max)
{
    return v >= min && v <= max;
}


u32 set_bit(u32 v, u32 bit)
{
    return v | (1 << bit);
}

u32 deset_bit(u32 v, u32 bit)
{
    return v & ~(1 << bit);
}

bool is_set(u32 v, u32 bit)
{
    return (v >> bit) & 1;
}

u32 popcount(u32 v)
{
    u32 count = 0;
    for(u32 i = 0; i < sizeof(v) * 8; i++)
    {
        count += is_set(v,i);
    }

    return count;
}


constexpr u32 bit_ceil(u32 v)
{
    u32 ans = 1;
    while(ans < v)
    {
        ans *= 2;
    }

    return ans;
}

#ifndef _MSC_VER
__attribute__((noreturn))
#endif
void unimplemented(const char *fmt, ...)
{
    printf("unimplemented: ");
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    putchar('\n');
    va_end(args);
    exit(1);
}

#ifndef _MSC_VER
__attribute__((noreturn))
#endif
void panic(const char *fmt, ...)
{
    printf("panic: ");
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    putchar('\n');
    exit(1);     
}


void panic(bool cond, const char *fmt, ...)
{
    if(cond)
    {  
        va_list args;
        va_start(args,fmt);
        panic(fmt,args);
        va_end(args);      
    }
} 


#include "alloc.cpp"
#include "array.cpp"
#include "string.cpp"
#include "hashtable.cpp"


// read entire file into a string
Array<char> read_file(const String &filename)
{
    FILE* fp = fopen(filename.buf,"rb");

    Array<char> buf;

    // file is invalid dont bother
    if(!fp)
    {
        return buf;
    }

    // get the file len
    fseek(fp, 0, SEEK_END);
    const u32 len = ftell(fp); 
    fseek(fp, 0, SEEK_SET); 

    // allocate an appriopately sized buffer
    // and read the whole file out
    resize(buf,len + 1);
    fread(buf.data,len,1,fp);

    buf[len] = '\0';

    fclose(fp);
    return buf;
}
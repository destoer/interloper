
constexpr char path_separator = std::filesystem::path::preferred_separator;

// read entire file into a string
std::string read_file(const std::string &filename)
{
    std::ifstream fp{filename};

    if(fp)
    {
        return std::string((std::istreambuf_iterator<char>(fp)),
                    (std::istreambuf_iterator<char>()));
    }

    return "";
}

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


void print_line(const std::string& filename,u32 line)
{
    // this is slow, but we are about to terminate anyways
    // when this is used
    std::fstream fp{filename};

    if(!fp)
    {
        printf("could not open file %s for error printing\n",filename.c_str());
    }

    std::string str;
    for(u32 i = 0; i < line; i++)
    {
        std::getline(fp,str);
    }

    printf("%s\n",str.c_str());    
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


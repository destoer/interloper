#pragma once
#include <vector>
#include <list>
#include <string>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <map>
#include <functional>
#include <algorithm>
#include <optional>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>

// integer typedefs
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;

using s8 = int8_t;
using s16 = int16_t;
using s32 = int32_t;

using b32 = bool;

// read entire file into a string
inline std::string read_file(const std::string &filename)
{
    std::ifstream fp{filename};

    if(fp)
    {
        return std::string((std::istreambuf_iterator<char>(fp)),
                    (std::istreambuf_iterator<char>()));
    }

    return "";
}


inline std::vector<std::string> read_string_lines(const std::string &str)
{
    std::vector<std::string> out;

    std::stringstream line_stream;

    line_stream << str;

    std::string line;
    while(getline(line_stream,line))
    {
        out.push_back(line);
    }

    return out;
}

template<typename access_type>
inline access_type handle_read(const u8 *buf, u32 idx)
{
    access_type v;
    memcpy(&v,&buf[idx],sizeof(access_type));
    return v;
}

template<typename access_type>
inline void handle_write(u8 *buf, u32 idx, access_type v)
{
    memcpy(&buf[idx],&v,sizeof(access_type));
}

template<typename T>
inline bool in_range(T v, T min, T max)
{
    return v >= min && v <= max;
}


// TODO: make sure the number fits in 32 bits
inline u32 convert_imm(const std::string &imm)
{
    if(imm.size() >= 3 && imm.substr(0,2) == "0b")
    {
        return static_cast<u32>(stoul(imm.substr(2),nullptr,2));
    }

    // stoi wont auto detect base for binary strings?
    return static_cast<u32>(stoul(imm,nullptr,0));
}

inline u32 set_bit(u32 v, u32 bit)
{
    return v | (1 << bit);
}

inline u32 deset_bit(u32 v, u32 bit)
{
    return v & ~(1 << bit);
}

inline bool is_set(u32 v, u32 bit)
{
    return (v >> bit) & 1;
}

inline u32 popcount(u32 v)
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
inline void unimplemented(const char *fmt, ...)
{
    printf("unimplemented: ");
    va_list args; 
    va_start(args, fmt);
    vprintf(fmt,args);
    va_end(args);
    exit(1);
}

inline void panic(bool cond, const char *fmt, ...)
{
    if(cond)
    {
        printf("panic: ");
        va_list args; 
        va_start(args, fmt);
        vprintf(fmt,args);
        va_end(args);
        putchar('\n');
        exit(1);        
    }
} 

#ifndef _MSC_VER
__attribute__((noreturn))
#endif
inline void panic(const char *fmt, ...)
{
    va_list args;
    va_start(args,fmt);
    panic(true,fmt,args);

    // no return detection aint good enough...
    exit(1);
}


#define UNUSED(X) ((void)X)
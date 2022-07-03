#pragma once
#include <vector>
#include <list>
#include <string>
#include <unordered_map>
#include <map>
#include <set>
#include <sstream>
#include <fstream>
#include <functional>
#include <algorithm>
#include <filesystem>
#include <optional>
#include <chrono>
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
using u64 = uint64_t;

using s8 = int8_t;
using s16 = int16_t;
using s32 = int32_t;
using s64 = int64_t;

using b32 = bool;
using b8 = bool;

using f32 = float;


#define UNUSED(X) ((void)X)

template<typename T>
struct Array
{
    T& operator [] (u32 i) 
    { 
        return this->data[i];
    }

    T operator [] (u32 i) const
    { 
        return this->data[i];
    }

    T* data = nullptr;

    // in raw bytes
    u32 size = 0;
    u32 capacity = 0;
};

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



void print_line(const std::string& filename,u32 line);
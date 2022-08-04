#pragma once
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

// string
struct String;
bool string_equal(const String& str1, const String& str2);
u32 hash_string(const String& str, u32 hash);
String make_static_string(const char* str, u32 len);


struct String
{
    String() {}

    constexpr String(const char* str) 
    {
        buf = str;

        while(str[size])
        {
            size++;
        }
    }

    char operator[] (u32 idx) const
    {
        return this->buf[idx];
    }

    bool operator== (const String& other) const
    {
        return string_equal(*this,other);
    }

    bool operator!= (const String& other) const 
    {
        return !string_equal(*this,other);
    }
    

    const char* buf = nullptr;

    // NOTE: there is a extra null term on the buf
    // after the size of C string compat
    u32 size = 0;
};

// array
template<typename T>
struct Array
{
    T& operator [] (u32 i) 
    {
        return this->data[i];
    }

    const T& operator [] (u32 i) const
    {
        return this->data[i];
    }

    T* data = nullptr;

    // in raw bytes
    u32 size = 0;
    u32 capacity = 0;
};


// hash table
template<typename Key,typename T>
struct HashNode
{
    Key key = {};
    T v = {};
};



template<typename Key,typename T>
using Bucket = Array<HashNode<Key,T>>;

template<typename Key,typename T>
struct HashTable
{
    u32 size = 0;

    // NOTE: Must be sized at a power of two
    Array<Bucket<Key,T>> buf;
};

static constexpr u32 HASH_TABLE_DEFAULT_SIZE = 256;
static constexpr s32 INVALID_SLOT = -1;

// allocator
struct ArenaAllocator;

struct Arena
{
    // how much have we used?
    u32 len = 0; 

    // how much do we have total?
    u32 size = 0;

    // underyling memory
    void* buf = nullptr;
};


static constexpr u32 ARENA_ALLOC_SIZE = 32;

// for now just have a single pool
// and dont deal with it getting exhausted
struct ArenaAllocator
{
    Arena arena[ARENA_ALLOC_SIZE];
    u32 size = 0;
};

void* allocate(ArenaAllocator& allocator, u32 size);



void print_line(const String& filename,u32 line);
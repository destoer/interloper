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

using s8 = int8_t;
using s16 = int16_t;
using s32 = int32_t;

using b32 = bool;
using b8 = bool;


#define UNUSED(X) ((void)X)

template<typename T>
struct Array
{
    T* data = nullptr;

    // we dont need over 4GB for our purposes so just use a u32 and pack the struct
    u32 size = 0;
    u32 capacity = 0;
};
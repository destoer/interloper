#pragma once
#include <vector>
#include <list>
#include <string>
#include <sstream>
#include <fstream>
#include <unordered_map>
#include <functional>
#include <algorithm>
#include <optional>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>

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
inline access_type handle_read(const uint8_t *buf, uint32_t idx)
{
    access_type v;
    memcpy(&v,&buf[idx],sizeof(access_type));
    return v;
}

template<typename access_type>
inline void handle_write(uint8_t *buf, uint32_t idx, access_type v)
{
    memcpy(&buf[idx],&v,sizeof(access_type));
}


#define UNUSED(X) ((void)X)
#project config

cmake_minimum_required(VERSION 3.10)

project(gen_table)
project(lexer_lut)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB src_files
    "gen_table.cpp"
)

if(WIN32)

else()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Werror -fno-exceptions  -fno-rtti  -fomit-frame-pointer -Wall -Wextra -O0")
endif()

include_directories("../src/headers")
include_directories("../src/")
include_directories("../destoer-lib/src")
add_executable(gen_table ${src_files})
add_executable(lexer_lut "lex_gen.cpp")
#project config

cmake_minimum_required(VERSION 3.10)

project(interloper)

set(CMAKE_UNITY_BUILD ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB src_files
	"src/*.h"
	"src/new/main.cpp"
)

if(WIN32)

else()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Werror -fno-exceptions  -fno-rtti  -fomit-frame-pointer -Wall -Wextra -O0 -fmax-errors=15")
endif()

include_directories("src/headers")
include_directories("src/new")

add_subdirectory("destoer-lib")
add_subdirectory("util")

add_executable(interloper ${src_files})
target_link_libraries(interloper destoer)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
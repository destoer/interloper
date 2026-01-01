#project config

cmake_minimum_required(VERSION 3.10)

project(interloper)

set(CMAKE_UNITY_BUILD ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB src_files
	"src/*.h"
	"src/main.cpp"
)



include_directories("src/headers")
include_directories("src/")

add_subdirectory("destoer-lib")
add_subdirectory("util")

add_executable(interloper ${src_files})
target_link_libraries(interloper destoer)

if(WIN32)

else()
target_compile_options(interloper PRIVATE -g -Werror -fno-exceptions -fno-rtti  -fomit-frame-pointer -Wall -Wextra -O0 -march=native -fmax-errors=15)
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
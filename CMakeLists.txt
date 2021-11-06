#project config

cmake_minimum_required(VERSION 3.10)

project(interloper)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB src_files
	"src/*.h"
	"src/*.cpp"
	"src/test/*.cpp"
)

if(WIN32)

else()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Werror -Wall -Wextra -O3")
endif()

include_directories("src/headers")
include_directories("src/")
add_executable(interloper ${src_files})
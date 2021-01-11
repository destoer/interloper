#project config

cmake_minimum_required(VERSION 3.10)

project(interloper)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

file(GLOB src_files
	"src/*.h"
	"src/*.cpp"
)

include_directories("src/headers")

add_executable(interloper ${src_files})
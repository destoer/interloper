#project config
project(gen_table)
project(lexer_lut)

file(GLOB src_files
    "gen_table.cpp"
)

if(WIN32)

else()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -Werror -fno-exceptions  -fno-rtti  -fomit-frame-pointer -Wall -Wextra -O0")
endif()

include_directories("../src/headers")
include_directories("../src/")
add_executable(gen_table ${src_files})
add_executable(lexer_lut "lex_gen.cpp")
target_link_libraries(gen_table destoer)
target_link_libraries(lexer_lut destoer)
cmake_minimum_required (VERSION 3.28)

enable_testing()

set(CMAKE_C_COMPILER "clang-17")
set(CMAKE_CXX_COMPILER "clang++-17")

project(tap CXX)

# Turning off extensions avoids an issue with the clang 16 compiler
# clang 17 and greater can avoid this setting
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 20)

# Find the Readline package
# find_package(Readline REQUIRED)

add_library(lexer)
target_sources(lexer 
  PUBLIC
    FILE_SET CXX_MODULES FILES 
      src/lexer.cppm
)

add_library(parser)
target_sources(parser 
  PUBLIC
    FILE_SET CXX_MODULES FILES 
      src/lexer.cppm
      src/parser.cppm
)


add_subdirectory(tests)

add_executable(tap src/main.cpp)

# Include the Readline headers
# target_include_directories(tap PRIVATE ${Readline_INCLUDE_DIRS})

target_link_libraries(tap lexer parser readline)

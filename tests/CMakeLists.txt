find_package(GTest REQUIRED)

enable_testing()

include_directories(${GTEST_INCLUDE_DIRS})

# Add your test source files
file(GLOB TEST_SOURCES "*.cpp")

add_executable(tests ${TEST_SOURCES})

# Link your project and GoogleTest
target_link_libraries(tests gtest gtest_main)

add_test(NAME ParserTest COMMAND tests)

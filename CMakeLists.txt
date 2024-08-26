# Top-level CMakeLists.txt

cmake_minimum_required(VERSION 3.10)
project(libcmesa)

# Include the modular CMake files
include(cmake/CompilerSettings.cmake)
include(cmake/FindMESA.cmake)

# Add subdirectories for each module
add_subdirectory(src/chem)
# add_subdirectory(src/eos)
add_subdirectory(src/kap)
add_subdirectory(src/utils)

# Add subdirectory for tests
add_subdirectory(tests/kap)

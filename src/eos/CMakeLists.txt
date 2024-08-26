# Set the module name
set(MODULE_NAME eos)

# List the Fortran and C source files for the module
set(F_SOURCES
    c_eos_interface_lib.f90
    c_eos_interface_def.f90
)

set(C_SOURCES
    c_eos_interface_lib.c
)

# Set the directory where Fortran modules (.mod files) will be placed
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

# Create the include directory if it doesn't exist
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})

# Include directories for the module
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})
include_directories(${CMAKE_SOURCE_DIR}/src/eos)

# Compile Fortran sources into object files
add_library(${MODULE_NAME}_fortran OBJECT ${F_SOURCES})

# Compile C sources into object files
add_library(${MODULE_NAME}_c OBJECT ${C_SOURCES})

# Combine Fortran and C object files into a single static library
add_library(${MODULE_NAME}cmesa STATIC
    $<TARGET_OBJECTS:${MODULE_NAME}_fortran>
    $<TARGET_OBJECTS:${MODULE_NAME}_c>
)

# Set output directories for the static library
set_target_properties(${MODULE_NAME}cmesa PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Install the static library, include files, and .mod files
install(TARGETS ${MODULE_NAME}cmesa
    ARCHIVE DESTINATION ${CMAKE_BINARY_DIR}/lib
)

# Install .mod files and other include files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION ${CMAKE_BINARY_DIR}/include
    FILES_MATCHING PATTERN "*.mod"
)

install(FILES c_eos_interface_external.h c_eos_interface_lib.h
    DESTINATION ${CMAKE_BINARY_DIR}/include)


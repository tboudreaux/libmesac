# cmake/CompilerSettings.cmake
set(CMAKE_C_COMPILER g++)
set(CMAKE_CXX_COMPILER g++)
set(CMAKE_Fortran_COMPILER gfortran)

# Set C standard
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Set Fortran standard
set(CMAKE_Fortran_STANDARD 90)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Common compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fPIE")

enable_language(Fortran)

set_source_files_properties(${CMAKE_SOURCE_DIR}/src/*.c PROPERTIES LANGUAGE CXX)

# cmake/CompilerSettings.cmake
set(CMAKE_C_COMPILER gcc)
set(CMAKE_Fortran_COMPILER gfortran)

set(CMAKE_CXX_COMPILER g++)
SET(CMAKE_CXX_STANDARD 11)
SET(CMAKE_CXX_STANDARD_REQUIRED ON)

# Set C standard
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Set Fortran standard
set(CMAKE_Fortran_STANDARD 90)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Common compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fPIE")

enable_language(Fortran)

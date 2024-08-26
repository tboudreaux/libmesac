# cmake/FindMESA.cmake

# Set MESA directories
if(NOT DEFINED MESA_DIR)
    message(FATAL_ERROR "MESA_DIR is not set. Please set it before building libmesac.")
endif()

if(NOT DEFINED MESASDK_ROOT)
    message(FATAL_ERROR "MESASDK_ROOT is not set. Please set it before building libmesac.")
endif()

# Include and link directories for MESA
include_directories(${MESA_DIR}/include ${MESASDK_ROOT}/include)
link_directories(${MESA_DIR}/lib ${MESASDK_ROOT}/lib)

# Set MESA libraries
# Top-level CMakeLists.txt or a separate FindMESA.cmake

# Run the MESA shell commands and capture their output
execute_process(
    COMMAND mesasdk_lapack95_link
    OUTPUT_VARIABLE LAPACK95_LIB_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND mesasdk_lapack_link
    OUTPUT_VARIABLE LAPACK_LIB_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND mesasdk_blas_link
    OUTPUT_VARIABLE BLAS_LIB_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND mesasdk_hdf5_link
    OUTPUT_VARIABLE HDF5_LIB_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
    COMMAND mesasdk_crmath_link
    OUTPUT_VARIABLE CRMATH_LIB_FLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Additional MESA libraries
set(MESA_LIBS -lkap -lchem -lmath -latm -lconst -lutils -leos -linterp_1d -linterp_2d -lnum -lionization -lmtx -lauto_diff -lcrmath -lhdf5io -lrates -lnet -lmvec -fopenmp -lm -lquadmath)

# Combine the outputs into one variable for SDK libraries
set(SDK_LIBS "${MESA_LIBS} ${BLAS_LIB_FLAGS} ${LAPACK_LIB_FLAGS} ${LAPACK95_LIB_FLAGS} ${HDF5_LIB_FLAGS} ${CRMATH_LIB_FLAGS}")


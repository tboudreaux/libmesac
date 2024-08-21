CC = gcc
CFLAGS = -g
CLIBS = -lmvec -fopenmp -lgfortran -lm -lquadmath 
OUTPUT = rn
SRC = kap_test.c

# List of MESA modules you need to include
MODULES = kap chem math atm const utils eos interp_1d interp_2d num ionization mtx auto_diff crmath hdf5io rates net

BASE_DIR = ${MESA_DIR}

# Include directories for the modules
INCLUDE_DIR = -I$(BASE_DIR)/include -I${MESASDK_ROOT}/include
# Library directories for the modules
LIB_DIR = -L$(BASE_DIR)/lib -L${MESASDK_ROOT}/lib

# MESASDK linker produced flags
HDF5_LIB_FLAGS := $(shell $(MESASDK_ROOT)/bin/mesasdk_hdf5_link)
CRMATH_LIB_FLAGS := $(shell $(MESASDK_ROOT)/bin/mesasdk_crmath_link)
LAPACK_LIB_FLAGS := $(shell $(MESASDK_ROOT)/bin/mesasdk_lapack_link)
LAPACK95_LIB_FLAGS := $(shell $(MESASDK_ROOT)/bin/mesasdk_lapack95_link)
BLAS_LIB_FLAGS := $(shell $(MESASDK_ROOT)/bin/mesasdk_blas_link)

NUMERICAL_LIBS = $(BLAS_LIB_FLAGS) $(LAPACK_LIB_FLAGS) $(LAPACK95_LIB_FLAGS)
IO_LIBS = $(HDF5_LIB_FLAGS)

SDK_LIBS = $(NUMERICAL_LIBS) $(IO_LIBS) $(CRMATH_LIB_FLAGS)

# Libraries to link against (assuming each module has a corresponding library)
LIBS = $(addprefix -l,$(MODULES)) $(SDK_LIBS)

$(OUTPUT): $(SRC)
	$(CC) $(CFLAGS) $(cLDFLAGS) $(SRC) -L../../lib -I../../include -lkapcmesa $(INCLUDE_DIR) $(LIB_DIR) $(LIBS) $(CLIBS) -o $(OUTPUT)

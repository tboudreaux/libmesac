include $(MESA_DIR)/utils/makefile_header

#i Define the Fortran compiler and flags
FC = $(MESASDK_ROOT)/bin/gfortran
FFLAGS = -c -g
LDFLAGS = -fopenmp

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

# Source files for your program
KAPSRC = c_kap_interface_lib.f90
CSRC = c_kap_interface_lib.c
iSRC = c_kap_interface_def.f90

# Output executable name
KAPLIB = libkapf.o
cKAPLIB = libkapc.o
iLIB = libcmesautils.o
OUTPUT = rn

CC = gcc
CFLAGS = -c -g
cLDFLAGS = -fopenmp -lgfortran -lm -lquadmath

BUILDLIBDIR = ./bin
BUILDINCLUDEDIR = ./include

$(KAPLIB): $(KAPSRC)
	$(FC) $(FFLAGS) $(KAPSRC) $(INCLUDE_DIR) $(LIB_DIR) $(LIBS) -I $(BUILDINCLUDEDIR) -I $(BUILDLIBDIR) -linterface_def -o $(BUILDLIBDIR)/$(KAPLIB) $(LDFLAGS)

$(cKAPLIB): $(CSRC)
	$(CC) $(CFLAGS) $(CSRC) -o $(BUILDLIBDIR)/$(cKAPLIB)

$(iLIB): $(iSRC)
	$(FC) $(FFLAGS) $(iSRC) $(INCLUDE_DIR) $(LIB_DIR) $(LIBS) $(LDFLAGS) -o $(BUILDLIBDIR)/$(iLIB) -J $(BUILDINCLUDEDIR)


# Clean rule to remove generated files
clean:
	rm -f $(OUTPUT) *.o *.mod
	rm -r $(BUILDLIBDIR)
	rm -r $(BUILDINCLUDEDIR)
	rm .built

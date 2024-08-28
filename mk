#!/bin/bash
CC=$(which gcc)
CXX=$(which g++)
rm -rf build
mkdir -p build
cd build
cmake .. -DMESA_DIR=$MESA_DIR -DMESASDK_ROOT=$MESASDK_ROOT -DCMAKE_INSTALL_PREFIX=./install -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX
make
make install
cd install/bin
./kap_test --gtest_output=xml:test_results.xml
cd ..

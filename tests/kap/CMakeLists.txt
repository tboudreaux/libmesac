# Define the test executable
set(TEST_NAME kap_test)

# List the source files for the test
set(TEST_SOURCES
    kap_test.cpp
)


# Define the executable for the test
add_executable(${TEST_NAME} ${TEST_SOURCES})

# Specify the include directories for the test
target_include_directories(${TEST_NAME} PRIVATE
    ${GTEST_INCLUDE_DIRS}
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/src/kap
    ${CMAKE_SOURCE_DIR}/src/chem
    ${CMAKE_SOURCE_DIR}/src/utils
)

# Link the test executable against the kap, chem, and utils libraries
target_link_libraries(${TEST_NAME} 
    ${CMAKE_BINARY_DIR}/lib/libkapcmesa.a
    ${CMAKE_BINARY_DIR}/lib/libchemcmesa.a
    ${CMAKE_BINARY_DIR}/lib/libutilscmesa.a
    ${SDK_LIBS}
    gfortran
    GTest::gtest_main
)

# Specify the output directory for the test executable
set_target_properties(${TEST_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/tests/kap
)

# Add a test to be run with CTest
# add_test(NAME ${TEST_NAME}
#          COMMAND ${CMAKE_BINARY_DIR}/tests/kap/${TEST_NAME})

# Install the test executable (optional)
# Define the installation of the test executable
install(TARGETS kap_test
    RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
)
# Install the test data files
install(FILES inlist_sample sample_kap_agb.model
    DESTINATION ${CMAKE_INSTALL_PREFIX}/share/kap_test_data
)

enable_testing()
include(GoogleTest)
gtest_discover_tests(kap_test)


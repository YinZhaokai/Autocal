cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )
enable_language ( Fortran )
project(autocal_validation NONE)
# project (auto_cal_mpi NONE )
# project (auto_cal_loop NONE )

# dependencies
find_package (jsonfortran-${CMAKE_Fortran_COMPILER_ID} 7.1.0 REQUIRED PATHS third_part/json_install/jsonfortran-gnu-7.1.0)
find_package(MPI REQUIRED)

add_definitions("${MPI_FROTRAN_COMPILE_FLAGS}")

include_directories ("${jsonfortran_INCLUDE_DIRS}")
include_directories(SYSTEM ${MPI_INCLUDE_PATH})

add_subdirectory(src)

# set(SOURCE_FILES main.f90)
set(SOURCE_FILES
    ParseToDll.f90
    # autocal_fortran_test.f90
    # autocal_mpi_test.f90
    autocal_validation_test.f90
    fprj_module.f90
    fprj_update_module.f90
    Call_Dll.f90
    SaveComputeData.f90
    ConfigMod.f90
    ParamSampling.f90
    )

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)

add_executable(autocal_validation ${SOURCE_FILES})
target_link_libraries(autocal_validation easydhm jsonfortran-static "${MPI_FORTRAN_LIBRARIES}")
# add_executable(auto_cal_mpi ${SOURCE_FILES})
# target_link_libraries(auto_cal_mpi easydhm jsonfortran-static "${MPI_FORTRAN_LIBRARIES}")
# add_executable(auto_cal_loop ${SOURCE_FILES})
# target_link_libraries(auto_cal_loop easydhm jsonfortran-static "${MPI_FORTRAN_LIBRARIES}")

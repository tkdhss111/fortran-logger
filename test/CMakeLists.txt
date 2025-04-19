set ( EXE unit_test )
set ( SRCS ./unit_test.f90
           ../src/logger_mo.f90
    )

set ( CMAKE_Fortran_FLAGS "-ffree-line-length-none")
set ( CMAKE_Fortran_FLAGS_DEBUG "-Ddebug -O0 ${CMAKE_Fortran_FLAGS}" )

add_executable ( ${EXE} ${SRCS} )
target_include_directories ( ${EXE} PUBLIC "../include" )
add_test ( NAME ${EXE} COMMAND ${EXE} WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

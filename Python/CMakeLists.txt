
find_package(PythonInterp REQUIRED)
find_package(PythonLibs   REQUIRED)
include_directories(${PYTHON_INCLUDE_DIRS} "../")

add_library(CoaSimCore SHARED
	    arg.hh
	    arg.cc
	    intervals.hh
	    intervals.cc
	    nodes.hh
	    nodes.cc
	    trees.hh
	    trees.cc
	    exceptions.hh
	    markers.hh
	    markers.cc
	    simulate.hh
	    simulate.cc
	    main.cc
	   )

target_link_libraries(CoaSimCore ${PYTHON_LIBRARIES})
target_link_libraries(CoaSimCore Core)



message( STATUS "PYTHON: " ${PYTHON_EXECUTABLE} )
execute_process ( COMMAND ${PYTHON_EXECUTABLE} -c "from distutils.sysconfig import get_python_lib; print get_python_lib()" OUTPUT_VARIABLE PYTHON_SITE_PACKAGES OUTPUT_STRIP_TRAILING_WHITESPACE)
message( STATUS "PYTHON_SITE_PACKAGES: " ${PYTHON_SITE_PACKAGES} )

set_target_properties(CoaSimCore PROPERTIES OUTPUT_NAME CoaSimCore PREFIX "" SUFFIX .so)
INSTALL(TARGETS CoaSimCore
	LIBRARY DESTINATION ${PYTHON_SITE_PACKAGES}
	ARCHIVE DESTINATION ${PYTHON_SITE_PACKAGES}
	COMPONENT library
)


set(SETUP_PY_IN "${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in")
set(SETUP_PY    "${CMAKE_CURRENT_BINARY_DIR}/setup.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/modules/CoaSim/__init__.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/modules/CoaSim/IO.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/modules/CoaSim/diseaseModelling.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/modules/CoaSim/popStructure.py")
set(DEPS        "${CMAKE_CURRENT_SOURCE_DIR}/modules/CoaSim/randomMarkers.py")
set(OUTPUT      "${CMAKE_CURRENT_BINARY_DIR}/build/timestamp")

configure_file(${SETUP_PY_IN} ${SETUP_PY})

add_custom_command(OUTPUT ${OUTPUT}
		   COMMAND ${PYTHON} ${SETUP_PY} build
		   COMMAND ${CMAKE_COMMAND} -E touch ${OUTPUT}
		   DEPENDS ${DEPS})
add_custom_target(pyCoaSim ALL DEPENDS CoaSimCore ${OUTPUT})
install(CODE "execute_process(COMMAND ${PYTHON} ${SETUP_PY} install)")

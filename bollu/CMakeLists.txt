cmake_minimum_required(VERSION 3.20)
project(thoth C CXX)

find_package(SDL2 REQUIRED)
find_package(OpenGL REQUIRED)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -fdiagnostics-color=always -fsanitize=address,undefined")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-color=always -O2")


add_executable(gap
main.cpp
)


# target_link_libraries(thoth
#   SDL2::SDL2
#   SDL2::SDL2main
#   ${OPENGL_LIBRARIES})
# target_include_directories(thoth PUBLIC ./backends/ ./)

install(TARGETS gap DESTINATION bin)

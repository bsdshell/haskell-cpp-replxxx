# Specify the minimum version for CMake
# Current Version: 3.23.2
cmake_minimum_required(VERSION 3.1)


# KEY: 
# Project's name
project(cpp)
# Set the output folder where your program will be created
# 
# CMAKE_CXX_STANDARD 14 => C++14
# UR: https://cmake.org/cmake/help/latest/prop_tgt/CXX_STANDARD.html#prop_tgt:CXX_STANDARD 
# 
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR})
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR})

# set(CMAKE_CXX_COMPILER "/usr/bin/g++")
# add_compile_options(-std=c++14)

# The following folder will be included
# include_directories("${PROJECT_SOURCE_DIR}")

# Use /usr/local/inlcude and /usr/local/lib instead.
# set(Boost_INCLUDE_DIR /usr/local/Cellar/boost/1.81.0_1/include)
# set(Boost_LIBRARY_DIR /usr/local/Cellar/boost/1.81.0_1/lib)

set(Boost_INCLUDE_DIR /usr/local/include)
set(Boost_LIBRARY_DIR /usr/local/lib)

find_package(Boost COMPONENTS system filesystem REQUIRED)


include_directories(${Boost_INCLUDE_DIR})

# $cpplib/AronLib.h
include_directories("$ENV{cpplib}")

# $clib/AronCLibNew.h
include_directories("$ENV{clib}")
link_directories(${Boost_LIBRARY_DIR})
# link_directories(/usr/local/Cellar/boost/1.72.0_2/lib)

add_executable(cpp ${PROJECT_SOURCE_DIR}/cpp.cpp)
# https://stackoverflow.com/questions/1065672/how-to-link-against-boost-system-with-cmake
# Use component target since cmake 3.15
target_link_libraries(cpp PUBLIC
    Boost::system 
    Boost::filesystem
)

# KEY: cmake build time, measure time
# https://stackoverflow.com/questions/54377926/how-to-measure-compile-time-per-object-in-cmake
set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CMAKE_COMMAND} -E time") 
set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK "${CMAKE_COMMAND} -E time")


# change executable output to a directory
# set_target_properties(line PROPERTIES RUNTIME_OUTPUT_DIRECTORY $ENV{HOME}/myfile/runbin )

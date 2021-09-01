  cmake_minimum_required(VERSION 3.0)
  project(GEPS)

  set(PROJROOT .)

  # set(CMAKE_BUILD_TYPE Debug)
  set(CMAKE_CXX_FLAGS_DEBUG "-g")
  set(CMAKE_CXX_FLAGS_RELEASE "-O3")

  # set(CMAKE_CXX_STANDARD 11)
  # set(CMAKE_CXX_STANDARD 14)

  # c++ linker
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fopenmp")

  add_executable(main main.c ${SOURCES})

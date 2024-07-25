cmake_minimum_required(VERSION 3.5.0)
project(lispc VERSION 0.1.0 LANGUAGES C)

add_executable(lispc main.c mpc.c)

target_link_libraries(lispc m edit)

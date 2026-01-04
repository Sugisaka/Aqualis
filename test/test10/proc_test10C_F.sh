#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test10C.f90 -ffree-line-length-none  -o test10C.exe
./test10C.exe

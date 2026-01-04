#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test10A.f90 -ffree-line-length-none  -o test10A.exe
./test10A.exe

#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test3_1.f90 -ffree-line-length-none  -o test3_1.exe
./test3_1.exe

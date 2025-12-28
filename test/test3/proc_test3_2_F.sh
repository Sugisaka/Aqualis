#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test3_2.f90 -ffree-line-length-none  -o test3_2.exe
./test3_2.exe

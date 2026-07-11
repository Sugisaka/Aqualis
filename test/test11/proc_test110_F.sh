#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test110.f90 -ffree-line-length-none  -o test110.exe
./test110.exe

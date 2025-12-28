#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test6.f90 -ffree-line-length-none  -o test6.exe
./test6.exe

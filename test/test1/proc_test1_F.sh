#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test1.f90 -ffree-line-length-none  -o test1.exe
./test1.exe

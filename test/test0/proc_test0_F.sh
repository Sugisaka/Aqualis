#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test0.f90 -ffree-line-length-none  -o test0.exe
./test0.exe

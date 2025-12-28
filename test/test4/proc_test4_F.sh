#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test4.f90 -ffree-line-length-none  -o test4.exe
./test4.exe

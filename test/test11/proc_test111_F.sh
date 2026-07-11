#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test111.f90 -ffree-line-length-none  -o test111.exe
./test111.exe

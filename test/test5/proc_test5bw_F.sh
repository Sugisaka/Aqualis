#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test5bw.f90 -ffree-line-length-none  -o test5bw.exe
./test5bw.exe

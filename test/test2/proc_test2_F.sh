#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test2.f90 -ffree-line-length-none  -o test2.exe
./test2.exe

#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test5tr.f90 -ffree-line-length-none  -o test5tr.exe
./test5tr.exe

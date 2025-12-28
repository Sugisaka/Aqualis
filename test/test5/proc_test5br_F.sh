#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test5br.f90 -ffree-line-length-none  -o test5br.exe
./test5br.exe

#!/bin/bash

FC='/usr/bin/gfortran'

$FC  test5tw.f90 -ffree-line-length-none  -o test5tw.exe
./test5tw.exe

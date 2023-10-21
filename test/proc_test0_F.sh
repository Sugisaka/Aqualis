#!/bin/bash

FC='/usr/bin/gfortran'

$FC test0.f90 -o test0.exe
./test0.exe

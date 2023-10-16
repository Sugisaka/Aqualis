#!/bin/bash

FC='/usr/bin/gfortran'

$FC test6bw.f90 -o test6bw.exe
./test6bw.exe

#!/bin/bash

/usr/bin/gfortran -ffree-line-length-none test6_2.f90 -lfftw3 -I/usr/local/include -o test6_2.exe
./test6_2.exe

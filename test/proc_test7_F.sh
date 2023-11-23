#!/bin/bash

FC='/usr/bin/gfortran'

$FC test7.f90 -o test7.exe
./test7.exe

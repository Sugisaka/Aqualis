#!/bin/bash

FC='/usr/bin/gfortran'

$FC test6.f90 -o test6.exe
./test6.exe

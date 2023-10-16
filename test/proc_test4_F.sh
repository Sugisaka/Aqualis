#!/bin/bash

FC='/usr/bin/gfortran'

$FC test4.f90 -o test4.exe
./test4.exe

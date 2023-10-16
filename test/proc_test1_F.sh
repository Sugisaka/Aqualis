#!/bin/bash

FC='/usr/bin/gfortran'

$FC test1.f90 -o test1.exe
./test1.exe

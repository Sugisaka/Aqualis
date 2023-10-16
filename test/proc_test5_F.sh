#!/bin/bash

FC='/usr/bin/gfortran'

$FC test5.f90 -o test5.exe
./test5.exe

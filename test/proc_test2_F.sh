#!/bin/bash

FC='/usr/bin/gfortran'

$FC test2.f90 -o test2.exe
./test2.exe

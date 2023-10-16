#!/bin/bash

FC='/usr/bin/gfortran'

$FC test6tr.f90 -o test6tr.exe
./test6tr.exe

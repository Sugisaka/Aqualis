#!/bin/bash

FC='/usr/bin/gfortran'

$FC test6br.f90 -o test6br.exe
./test6br.exe

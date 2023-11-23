#!/bin/bash

FC='/usr/bin/gfortran'

$FC test7tw.f90 -o test7tw.exe
./test7tw.exe

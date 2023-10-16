#!/bin/bash

FC='/usr/bin/gfortran'

$FC test6tw.f90 -o test6tw.exe
./test6tw.exe

#!/bin/bash

/usr/bin/gfortran -ffree-line-length-none test0.f90 -o test0.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test0.exe

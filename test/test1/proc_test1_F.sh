#!/bin/bash

/usr/bin/gfortran -ffree-line-length-none test1.f90 -o test1.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test1.exe

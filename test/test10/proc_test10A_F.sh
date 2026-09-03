#!/bin/bash

/usr/bin/gfortran -ffree-line-length-none test10A.f90 -o test10A.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test10A.exe

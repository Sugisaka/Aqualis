#!/bin/bash

gcc test6_2.c -lfftw3 -I/usr/include -lm -o test6_2.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test6_2.exe

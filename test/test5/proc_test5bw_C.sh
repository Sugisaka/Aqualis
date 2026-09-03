#!/bin/bash

gcc test5bw.c -lm -o test5bw.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test5bw.exe

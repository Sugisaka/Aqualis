#!/bin/bash

gcc test5tw.c -lm -o test5tw.exe
aqualis_compile_status=$?
if [ "$aqualis_compile_status" -ne 0 ]; then
  printf '%s\n' 'Aqualis: compilation failed.' >&2
  exit "$aqualis_compile_status"
fi

exec ./test5tw.exe

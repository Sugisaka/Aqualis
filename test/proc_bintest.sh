#!/bin/bash

FC='/usr/bin/gfortran'

$FC bintest.f90 -o bintest.exe
./bintest.exe

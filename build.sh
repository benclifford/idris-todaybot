#!/bin/bash -ex

gcc -c ffitest.c
idris -p effects \
      -p config \
      --total \
      ffitest.idr \
      -o i.out 

# \
#      --V2 \
#      --log 255


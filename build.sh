#!/bin/bash -ex

gcc -c ffitest.c
idris -p effects \
      -p config \
      --total \
      --cg-opt="-lcurl" \
      ffitest.idr \
      -o i.out


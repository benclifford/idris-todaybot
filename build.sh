#!/bin/bash -ex

gcc -c ffitest.c
idris --cg-opt="-lcurl" ffitest.idr -o i.out


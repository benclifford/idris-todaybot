#!/bin/bash -ex

gcc -c ffitest.c
idris -p effects \
      -p config \
      -p lightyear \
      --total \
      ffitest.idr \
      Todaybot/TitleParser.idr \
      Todaybot/Morph.idr \
      -o i.out 

# \
#      --V2 \
#      --log 255


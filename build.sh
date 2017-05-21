#!/bin/bash -ex

gcc -c ffitest.c
idris -p effects \
      -p config \
      -p lightyear \
      --total \
      ffitest.idr \
      Todaybot/Ptr.idr \
      Todaybot/TitleParser.idr \
      Todaybot/Morph.idr \
      Todaybot/Date.idr \
      -o i.out 

# \
#      --V2 \
#      --log 255


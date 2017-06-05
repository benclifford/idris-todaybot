#!/bin/bash -ex

gcc -c ffitest.c
gcc -c ptr.c

idris -p effects -o test-null-pointer test-null-pointer.idr 
./test-null-pointer

idris -p effects \
      -p config \
      -p lightyear \
      --total \
      ffitest.idr \
      Todaybot/Ptr.idr \
      Todaybot/TitleParser.idr \
      Todaybot/Morph.idr \
      Todaybot/Date.idr \
      -o todaybot

# \
#      --V2 \
#      --log 255


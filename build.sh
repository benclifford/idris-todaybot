#!/bin/bash -ex

gcc -c ffitest.c
gcc -c ptr.c

idris -p effects -o test-null-pointer test-null-pointer.idr 
./test-null-pointer

# QUESTION/DISCUSSION: if test-todaybot-json-parser.idr
# does not exist, this gives an error on the console but
# does not exit with a unix error code - instead it returns
# exit code 0. This should be filed as a bug - it messes up
# automated build pipelines.
# I could even look at fixing it myself.
idris -p config \
      -p contrib \
      test-todaybot-json-parser.idr \
      -o test-todaybot-json-parser
      # NOT --total because of Config.Json show instance

idris -p effects \
      -p config \
      -p lightyear \
      -p contrib \
      --total \
      Todaybot/Main.idr \
      Todaybot/Ptr.idr \
      Todaybot/TitleParser.idr \
      Todaybot/Morph.idr \
      Todaybot/Date.idr \
      -o todaybot

# \
#      --V2 \
#      --log 255


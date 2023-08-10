#!/bin/bash

# the -- and ./ prevents files from being passed in as options to rm.
# a file would be named "-f /" would run "rm -f /", otherwise... kinda scary

rm -- ./*~
rm -- ./\#*

rm -- ./*.o
rm -- ./*.hi

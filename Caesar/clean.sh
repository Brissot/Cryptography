#!/bin/bash

# the -- and ./ prevents files from being passed in as options to rm.
# a file would be named "-f /" would run "rm -f /", otherwise... kinda scary

# for emacs
rm -- ./*~
rm -- ./\#*

# for ghc
rm -- ./*.o
rm -- ./*.hi

# who needs binaries?
rm TestCaesar
rm TestCaesar2
rm TestVigenere
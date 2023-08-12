#!/bin/bash

echo "Test I - TestCaesar, single steps" > timing.txt
echo "time (./TestCaesar war_n_peace.txt 600 > /dev/null)" >> timing.txt
{ time (./TestCaesar war_n_peace.txt 26 > /dev/null); } 2>> timing.txt
printf "\n\n\n" >> timing.txt

# maybe it takes some time for the os to free resources?
sleep 5
# lmao nop

echo "Test II - TestCaesarII, giant steps" >> timing.txt
echo "time (./TestCaesar2 war_n_peace.txt 600 > /dev/null)" >> timing.txt
{ time (./TestCaesar2 war_n_peace.txt 26 > /dev/null); } 2>> timing.txt
printf "\n\n\n" >> timing.txt
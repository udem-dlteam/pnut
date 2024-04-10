#! /bin/sh

gcc -E -C -P -DSIX_CC_BOOTSTRAP peanut.c > peanut.temp
gcc -o peanut.o -O0 peanut.c
./peanut.o < peanut.temp > peanut.sh
time ksh peanut.sh --no-zero-globals < peanut.temp > peanut-2.sh

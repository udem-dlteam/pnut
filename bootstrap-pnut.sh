#! /bin/sh

gcc -E -C -P -DSIX_CC_BOOTSTRAP pnut.c > pnut.temp
gcc -o pnut.o -O0 pnut.c
./pnut.o < pnut.temp > pnut.sh
time ksh pnut.sh --no-zero-globals < pnut.temp > pnut-2.sh

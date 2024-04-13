#! /bin/sh

gcc -o pnut.exe pnut.c

gcc -E -C -P -DPNUT_CC pnut.c > pnut-after-cpp.c

./pnut.exe < pnut-after-cpp.c > pnut.sh

time ksh pnut.sh --no-zero-globals < pnut-after-cpp.c > pnut-twice-bootstrapped.sh

diff pnut.sh pnut-twice-bootstrapped.sh

wc pnut.c pnut.sh pnut-twice-bootstrapped.sh

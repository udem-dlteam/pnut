echo Arg: $@
gcc -o benchmarks/pnut-sh.exe -Dsh -DSUPPORT_INCLUDE $@ pnut.c
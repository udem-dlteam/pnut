# pnut

A self-compiling C compiler written in C generating POSIX shell scripts.

## How to use

The `pnut` compiler takes a C file path as input, and outputs to stdout the
POSIX shell code.

Here is a demonstration of self compilation:

    $ gcc pnut.c -o pnut.exe
    $ gcc -E -P -DPNUT_CC pnut.c | ./pnut.exe > pnut.sh
    $ gcc -E -P -DPNUT_CC pnut.c | ksh pnut.sh > pnut2.sh
    $ gcc -E -P -DPNUT_CC pnut.c | ksh pnut2.sh > pnut3.sh
    $ wc pnut.c pnut.sh pnut2.sh
        3196   11501   90259 pnut.c
        3633   15782  112316 pnut.sh
        3633   15782  112316 pnut2.sh
        3633   15782  112316 pnut3.sh
       14095   58847  427207 total

Here is an example of using the bootstrapped compiler:

    $ gcc -E -P -DPNUT_CC pi.c | ksh pnut.sh > pi.sh
    $ chmod +x pi.sh
    $ ./pi.sh
    31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185

### Testing

To run the tests: `make test`.

To add new tests, simply add a .c file in the tests directory. On the first
`make test` run, the golden output file will be generated. The golden output
file will then be sued as the expected output on subsequent runs.

## TODO

- #include macros
- macros with arguments
- sizeof
- struct/unions
- string subexpressions
- x86 codegen

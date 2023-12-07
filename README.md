# shell-cc

A C compiler written in POSIX shell and generating POSIX shell scripts

## How to use

The `cc.sh` compiler takes a C file path as input, and outputs the stdout the
compiled shell code. In case of compilation error, it outputs to stderr the
error message.

To compile and run: `./cc.sh test.c > test.sh && ./test.sh`

### Testing

To run the tests: `make test`.

To add new tests, simply add a .c file in the tests directory. On the first
`make test` run, the golden output file will be generated. The golden output
file will then be sued as the expected output on subsequent runs.

## TODO

- #include macros
- macros with arguments
- sizeof
- struct

# shell-cc

A C compiler written in POSIX shell and generating POSIX shell scripts

## How to use

The `cc.sh` compiler takes a C file path as input, and outputs the stdout the
compiled shell code. In case of compilation error, it outputs to stderr the
error message.

To compile and run: `./cc.sh test.c > test.sh && ./test.sh`

## TODO

- #include macros
- macros with arguments
- sizeof
- struct

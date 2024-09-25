# Pnut artifact

This artifact has the following structure:

- `artifact.pdf`: This file
- `pnut-artifact-image.tar.gz`: Docker image containing pnut and its dependencies
- `pnut/`: pnut's source code
- `pnut-website/`: Accompanying website's source code

The docker image serves as a self-contained environment to run pnut and this
document assumes that the reader is using it. However, most instructions
should work on any system with a C compiler and a POSIX compliant shell.

Pnut is also available freely on GitHub <https://github.com/udem-dlteam/pnut>
and the website can be accessed at <https://pnut.sh>. The website allows users
to use pnut to compile C code to shell scripts with a nice interface. It is not
the focus of this artifact and is only provided as a means to present pnut.

## Docker image

The docker image is based on Ubuntu 24.04 (x86-64) and contains the pnut source
files with all the necessary tools to compile and run pnut-sh and
pnut-exe. It also comes with the following shells preinstalled: `bash`, `dash`,
`ksh`, `mksh`, `yash`, `zsh`.

To show compatibility with older bash versions, the image contains a Debian
woody environment from July 2002 with `bash` version 2.05a. The environment
contains its own pnut repository meaning the instructions below apply to
the woody environment. It can be entered by running `./woody.sh` or individual
commands can be run in the woody environment by passing them as arguments to
`./woody.sh` such as `./woody.sh "bash --version"`.

To enter the Docker image, run the following command:

```shell
cat pnut-artifact-image.tar.gz | docker load # Load the image
docker run -it pnut-artifact                 # Start the container
```

## Project structure

Pnut's source code is split between multiple C files, some of which are shared
between pnut-sh and pnut-exe. These files are located at the root of the
repository.

The files for pnut-sh are:

- `pnut.c`: Compiler frontend
- `sh.c`: Shell compiler backend
- `sh-runtime.c`: Shell runtime

And for pnut-exe (i386/x86_64):

- `pnut.c`: Compiler frontend
- `exe.c` Native compiler backend
- `x86.c`: x86 specific declarations
- `elf.c`: ELF file generation (Linux)
- `mach-o.c`: Mach-O file generation (MacOS)

To measure the size of pnut's source, the `./analysis/measure-file-size.sh`
script can be used. For both pnut-sh and pnut-exe, it computes the size of their
source files, with and without comments and blank lines, and the ratio between
the compiled shell script and the original C code.

## How to use pnut

Pnut's command line options are as follows:

- `-D{macro}`: Define a macro
- `-U{macro}`: Undefine a macro that was previously defined
- `-I{path}`: Specify the include path

Arguments that do not start with `-` are treated as files to be compiled.

The compiled program is output to stdout, so it must be redirected to a file to
be executed. Note that the permission of the generated file must be changed with
`chmod +x {file}` to make it executable.

## Building pnut

The Makefile can be used to compile `pnut-sh` and `pnut-exe`, either with GCC
or with pnut itself (in which case, a shell script is generated).

```shell
make pnut-sh      # Compile pnut-sh with GCC
make pnut-exe     # Compile pnut-exe with GCC
make pnut-sh.sh   # Compile pnut-sh with pnut-sh into pnut-sh.sh
make pnut-exe.sh  # Compile pnut-exe with pnut-sh into pnut-exe.sh
```

All files built by the Makefile and other scripts are placed in the `build`
directory. Pnut's native backend supports both Linux and MacOS, on i386 and
x86_64 architectures.

<!-- ARM64 support is available on Linux but not on MacOS.
However, Rosetta emulation can be used to run the x86/x86_64 version on Macs
with ARM processors. -->

The Makefile detects the target platform and architecture and sets the right
compilation options, so it should work out of the box on the systems listed
above.

We encourage you to take a look at the `build/pnut-sh.sh` to see the generated
shell scripts.

```shell
cat build/pnut-sh.sh | less
```

### Bootstrapping pnut

Pnut's main use case is to compile itself. To ensure that the bootstrap process
works, the `./bootstrap-pnut-sh.sh` script can be used. This script compiles
pnut-sh twice: once with pnut-sh compiled with GCC (pnut-sh.sh) and then with
pnut-sh.sh (pnut-sh-twice-bootstrapped.sh). The script then compares the two
generated scripts to ensure that they are identical.

Note that the steps running shell versions of pnut can take a while to complete.

```shell
# Bootstrap pnut-sh.sh with <shell>
# For bash-2.05a, use `export PNUT_OPTIONS='-DRT_FREE_UNSETS_VARS_NOT'`
# before running the script.
# This script takes around 1m30 on bash. Faster on ksh/dash
./bootstrap-pnut-sh.sh --shell bash
```

Or you can invoke pnut directly:

```shell
# Compile pnut-sh to a shell script using GCC
make pnut-sh.sh
# Compile pnut-sh to a shell script with pnut-sh.sh
bash ./build/pnut-sh.sh pnut.c -DRELEASE_PNUT_SH \
  > build/pnut-sh-twice-bootstrapped.sh
sha256sum build/pnut-sh.sh build/pnut-sh-twice-bootstrapped.sh
```

To compile pnut-exe with pnut-sh, the `./bootstrap-pnut-exe.sh` script can be
used. This script compiles pnut-exe to shell twice: once with pnut-sh compiled
with GCC (pnut-exe.sh) and then with pnut-sh.sh (pnut-exe-twice-bootstrapped.sh).

```shell
# Bootstrap pnut-exe with <shell>
# Backends available: x86_64_mac, x86_64_linux, i386_linux
# For bash-2.05a, use `export PNUT_OPTIONS='-DRT_FREE_UNSETS_VARS_NOT'`
# before running the script.
# This script takes a few minutes.
./bootstrap-pnut-exe.sh --backend x86_64_linux --shell bash
```

Or manually:

```shell
make pnut-sh.sh
# First we compile pnut-exe to pnut-exe.sh using pnut-sh.sh
bash ./build/pnut-sh.sh pnut.c -DRELEASE_PNUT_x86_64_linux \
  > build/pnut-exe.sh
chmod +x build/pnut-exe.sh

# We then use pnut-exe.sh to compile pnut-exe again, this time creating a binary
bash ./build/pnut-exe.sh pnut.c -DRELEASE_PNUT_x86_64_linux \
  > build/pnut-exe.exe
chmod +x build/pnut-exe.exe

# Finally, we use pnut-exe to compile pnut-exe to make sure it works
./build/pnut-exe.exe pnut.c -DRELEASE_PNUT_x86_64_linux \
  > build/pnut-exe-twice-bootstrapped.exe

# And we check that the binaries are identical
sha256sum build/pnut-exe.exe build/pnut-exe-twice-bootstrapped.exe
```

<!-- ## Running tests

To run tests, the `./run-tests.sh sh --shell <shell>` command can be used. Tests
live in the `tests` directory, with `test/_sh` containing tests for pnut-sh and
`test/_exe` containing tests for pnut-exe. To run pnut-exe's tests,
`./run-tests.sh <backend>` can be used. -->

## Code samples

The `examples` directory contains examples that can be compiled with pnut-sh.
They come precompiled in the `examples/compiled` directory, but can be
regenerated with `./examples/prepare.sh`. Those examples are much smaller than
`pnut-sh.sh` and we encourage you to inspect the code and run them to get a
feeling of the capabilities of pnut and the shell.

Note that certain examples require specific compilation options to work properly
(mainly for speed). These options are set with a C comment starting with
`// pnut-options:` at the beginning of the file such as in `examples/repl.c`.
The `./examples/prepare.sh` script compiles the examples with the correct
options.

Some examples we find interesting:

- `examples/base64.c`: Encode and decode base64 data (except null bytes).
- `examples/c4.c`: "C in four functions", a simple C interpreter that can run
  itself and a bit more.
- `examples/cat.c`: Output the contents of files passed as arguments or stdin.
- `examples/empty.c`: Does nothing, demonstrate that the runtime is only
  included if needed.
- `examples/fib.c`: Print the first 20 Fibonacci numbers.
- `examples/repl.c`: A R4RS repl compiled with the Ribbit Scheme Compiler.
  Includes a bytecode interpreter for the Ribbit Virtual Machine (RVM) and a
  garbage collector.
- `examples/select-file.c`: TUI that allows the user to select a file to print
  (demonstrate how to shell code can be combined with C code).
- `examples/sha256sum.c`: Compute the SHA-256 hash of files passed as arguments.
- `examples/sum-array.c`: Compute the sum of an array of integers of size 10000.
- `examples/winterpi.c`: Compute the first 2800 digits of pi.

We quite like the `c4.c` and `repl.c` examples so here are a few things to try:

```shell
# Interpret fib.c with c4.sh
$ ksh examples/compiled/c4.sh fib.c
fib(15) = 610
fib(16) = 987
fib(17) = 1597
fib(18) = 2584
fib(19) = 4181
exit(0) cycle = 580148
```

```shell
# Invoke c4 by interpreting c4.c with c4.sh
$ ksh examples/compiled/c4.sh examples/c4.c
usage: c4 [-s] [-d] file ...
exit(-1) cycle = 45
# The interpreter can also be interpreted.
# Here c4.c is interpreted with c4.c interpreted with c4.sh
$ ksh examples/compiled/c4.sh examples/c4.c examples/c4.c
usage: c4 [-s] [-d] file ...
exit(-1) cycle = 45
exit(-1) cycle = 7483170
```

```shell
# Compute the Fibonacci numbers
$ ksh examples/compiled/repl.sh
> (define (fib x)
    (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))
0
> (fib 10)
55
# Sum of the first 11 Fibonacci numbers
> (apply + (map fib '(0 1 2 3 4 5 6 7 8 9 10)))
143
```

```shell
# Write to a file
$ ksh examples/compiled/repl.sh
> (define handle (open-output-file "hello.txt"))
0
> (display "Hello, reviewer!\n" handle)
#f
> ^D (Ctrl-D)
$ cat hello.txt
Hello, reviewer!
```

It's not very fast, but considering that every command is read and compiled
before being executed, it shows that pnut-sh can be used in non-bootstrapping
contexts.

<!-- Other included examples are:

- `examples/all-chars.c`: Print all non-extended ASCII characters.
- `examples/cp.c`: Copy the contents of one file to another.
- `examples/echo.c`: Output the arguments passed to it.
- `examples/hello.c`: Print "Hello, world".
- `examples/print-reverse.c`: Print command-line arguments reversed.
- `examples/reverse.c`: Reverse the order of command-line arguments.
- `examples/wc-stdin.c`: wc that reads from stdin and count lines, words, and characters.
- `examples/wc.c`: Read from stdin _or files_ and count lines, words, and characters.
- `examples/welcome.c`: Ask the user for their name and say hello. -->

### Watching pnut's progress

Compiling large (>100 lines) programs with pnut-sh.sh can take from a few seconds
to a few minutes. Due to the single-pass nature of the compiler, the output is
generated as the input is read, so it can be interesting to watch the compiler's
progress.

```shell
./bootstrap-pnut-sh.sh --shell bash & # Run in the background
tail -f build/pnut-sh-twice-bootstrapped.sh
```

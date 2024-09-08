# Pnut artifacts

## Docker image

The artifact is provided as a Docker image, which contains the pnut source files
and all the necessary tools to compile and run pnut-sh and pnut-exe. The image
comes with the following shells preinstalled: `bash`, `dash`, `ksh`, `mksh`,
`yash`, `zsh`.

To show compatibility with older bash versions, the image contains a Debian
woody environment from July 2002 with `bash` version 2.05a. The environment
contains its own pnut repository meaning the instructions below also apply to
the woody environment. It can be entered by running `./woody.sh` or individual
commands can be run in the woody environment by passing them as arguments to
`./woody.sh` such as `./woody.sh "bash --version"`.

To enter the Docker image, run the following command:

```shell
cat pnut-image.tar.gz | docker load   # Load the image
docker run -it --entrypoint bash pnut # Start the container
```

Pnut is also available freely on GitHub https://github.com/udem-dlteam/pnut.

## Project structure

Pnut's source code is split between multiple C files, some of which are shared
between pnut-sh and pnut-exe. These files are located at the root of the
repository.

The files for pnut-sh are:

- `pnut.c`: Compiler frontend
- `sh.c`: Shell compiler backend
- `sh-runtime.c`: Shell runtime

And for pnut-exe (x86/x86_64):

- `pnut.c`: Compiler frontend
- `exe.c` Native compiler backend
- `x86.c`: x86 specific declarations
- `elf.c`: ELF file generation (Linux)
- `mach-o.c`: Mach-O file generation (MacOS)

To measure the size of Pnut's source, the `./analysis/measure-file-size.sh`
script can be used. For both pnut-sh and pnut-exe, it computes the size of their
source files, with and without comments and blank lines, and the ratio between
the compiled shell script and the original C code.

## Building Pnut

The Makefile can be used to compile `pnut-sh` and `pnut-exe`, either with GCC
or with Pnut itself (in which case, a shell script is generated).

```shell
make pnut-sh      # Compile pnut-sh with GCC
make pnut-exe     # Compile pnut-exe with GCC
make pnut.sh      # Compile pnut-sh with pnut-sh into pnut.sh
make pnut-exe.sh  # Compile pnut-exe with pnut-sh into pnut-exe.sh
```

All files built by the Makefile and other scripts are placed in the `build`
directory. Pnut's native backend supports both Linux and MacOS, on x86 and
x86_64 architectures. ARM64 support is available on Linux but not on MacOS but
Rosetta emulation can be used to run the x86/x86_64 version on Macs with ARM
processors.

The Makefile detects the target platform and architecture and sets the right
compilation options, so it should work out of the box on the systems listed
above.

We encourage you to take a look at the `build/pnut.sh` to see the generated
shell scripts.

```shell
cat build/pnut.sh | less
```

### Bootstrapping Pnut

Pnut's main use case is to compile itself. To ensure that the bootstrap process
works, the `./bootstrap-pnut.sh` script can be used. This script compiles
pnut-sh to shell twice: once with pnut-sh compiled with GCC (pnut.sh) and then
with pnut.sh (pnut-twice-bootstrapped.sh). The script then compares the two
outputs to ensure that they are identical.

```shell
# Bootstrap pnut.sh with <shell>
./bootstrap-pnut.sh --shell ksh # Takes around a minute on ksh
```

To compile pnut-exe with pnut-sh, the `./bootstrap-pnut-exe.sh` script can be
used. This script compiles pnut-exe to shell twice: once with pnut-sh compiled
with GCC (pnut-exe.sh) and then with pnut.sh (pnut-exe-twice-bootstrapped.sh).

```shell
# Bootstrap pnut-exe with <shell>
# Backends available: x86_64_mac, x86_64_linux, i386_linux
./bootstrap-pnut-exe.sh --backend x86_64_linux --shell ksh
```

<!-- ## Running tests

To run tests, the `./run-tests.sh sh --shell <shell>` command can be used. Tests
live in the `tests` directory, with `test/_sh` containing tests for pnut-sh and
`test/_exe` containing tests for pnut-exe. To run pnut-exe's tests,
`./run-tests.sh <backend>` can be used. -->

## Running examples

The `examples` directory contains examples that can be compiled with pnut-sh.
They come precompiled in the `examples/compiled` directory, but can be
regenerated with `./examples/prepare.sh`.

Some interesting examples

- `examples/base64.c`: Encode and decode base64 data.
- `examples/cat.c`: Output the contents of files passed as arguments or stdin.
- `examples/empty.c`: Do nothing, demonstrate that the runtime is only included if needed.
- `examples/fib.c`: Print the first 20 Fibonacci numbers.
- `examples/repl.c`: A R4RS repl compiled with the Ribbit Scheme Compiler. Includes a bytecode interpreter for the Ribbit Virtual Machine (RVM) and a garbage collector.
- `examples/select-file.c`: TUI that allows the user to select a file to print (demonstrate how to shell code can be combined with C code).
- `examples/sha256sum.c`: Compute the SHA-256 hash of files passed as arguments.
- `examples/sum-array.c`: Compute the sum of an array of integers of size 10000.
- `examples/winterpi.c`: Compute the first 2800 digits of pi.

Other included examples are:

- `examples/all-chars.c`: Print all non-extended ASCII characters.
- `examples/cp.c`: Copy the contents of one file to another.
- `examples/echo.c`: Output the arguments passed to it.
- `examples/hello.c`: Print "Hello, world".
- `examples/print-reverse.c`: Print command-line arguments reversed.
- `examples/reverse.c`: Reverse the order of command-line arguments.
- `examples/wc-stdin.c`: wc that reads from stdin and count lines, words, and characters.
- `examples/wc.c`: Read from stdin _or files_ and count lines, words, and characters.
- `examples/welcome.c`: Ask the user for their name and say hello.

<!--
To compile an example, run the following command:

```shell
make pnut-sh
./build/pnut.sh examples/winterpi.c > examples/compiled/winterpi.sh
chmod +x examples/compiled/winterpi.sh
./examples/compiled/winterpi.sh
``` -->

<!-- TODO: Mention repl example -->
<!-- (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) -->

<!-- ## Compiling C4
 -->

## How to use Pnut

Pnut's command line options are as follows:

- `-D{macro}`: Define a macro
- `-U{macro}`: Undefine a macro that was previously defined
- `-I{path}`: Specify the include path

Arguments that do not start with `-` are treated as files to be compiled.

The compiled program is output to stdout, so it must be redirected to a file to
be executed. Note that the permission of the generated file must be changed with
`chmod +x {file}` to make it executable.

### Watching Pnut's progress

Compiling large (>100 lines) programs with pnut.sh can take from a few seconds
to a few minutes. Due to the single-pass nature of the compiler, the output is
generated as the input is read, so it can be interesting to watch the compiler's
progress.

```shell
./bootstrap-pnut.sh --shell ksh & # Run in the background
tail -f build/pnut-twice-bootstrapped.sh
```

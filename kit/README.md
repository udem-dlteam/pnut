# Kit - tools for bootstrapping TCC

This directory contains scripts and tools to bootstrap the Tiny C Compiler
(TCC). The bootstrap can start either from a shell with `pnut-sh.sh`, or from
c4, a small C interpreter. Both paths produce the bit-for-bit identical
`pnut-exe` executable that can then compile TCC.

To bootstrap TCC from `pnut-exe`, the following files are needed:

- `bintools.c`: A collection of small utilities used to prepare the environment,
  including `cat`, `chmod`, `cp`, `mkdir`, `sha256sum`, `simple-patch`, `ungz`
  and `untar`.
- A libc implementation to compile TCC with (pnut-libc or mes-libc).
- Source code for TCC, with a few patches to make it compatible with `pnut-exe`.

## Usage

To ensure that the bootstrap process is reproducible, we provide scripts to
create an isolated environment where the bootstrap can be performed.

1. `kit/bootstrap.sh`: bootstrap `pnut-exe` from `pnut-sh.sh` or c4, then TCC
    from `pnut-exe`.
2. `kit/list-bootstrap-files.sh`: list the files to include in the bootstrap
    environment, given the TCC version and libc to use.
3. `kit/make-jammed.sh` and `utils/jam.sh`: package the bootstrap files in
    the `jammed.sh` shell archive.
4. `kit/setup-rootfs.sh`: prepare the bootstrap environment with `jammed.sh` and
    a preinstalled shell, optionally executing the bootstrap process
    automatically.

```shell
$ sudo rm -rf build/rootfs # Make sure the rootfs directory is empty
$ ./kit/setup-rootfs.sh --dir build/rootfs \
  --bootstrap-shell ksh-i386-sarge \
  --include-utils \
  --extract-archives \
  --execute-bootstrap
[...]
+ ./bintools sha256sum build/tcc-boot0 build/tcc-boot1 build/tcc-boot2 build/tcc-boot3
9b5dd9cc6991016c124bb2c79e9593ed7092fe9780e96d6cc159edcf247c83a7  build/tcc-boot0
fa72fad5ce40797a8f70c5339d9517862986056f0b8c657a3c1612e23809eae9  build/tcc-boot1
03e96a1a63cc9bb3f577a14e50d20476507e3759bdc803f79e31d184bba44185  build/tcc-boot2
03e96a1a63cc9bb3f577a14e50d20476507e3759bdc803f79e31d184bba44185  build/tcc-boot3
```

Running the above command will create an isolated environment in `build/rootfs`
where the bootstrap process is executed automatically. Because shell scripts
aren't particularly fast at compiling C code, the bootstrap process takes a few
minutes to complete. To skip ahead to the TCC bootstrap, the
`--skip-initial-bootstrap` option can be used to have `pnut-exe` compiled by the
host C compiler instead of bootstrapping it from `pnut-sh.sh`.

The SHA256 checksums of the files produced by the bootstrap process are printed
at the end. A fixed point should be reached, showing that the bootstrap process
is done. To verify that the bootstrap is valid, the `kit/test-bootstrap.sh`
script bootstraps TCC twice, once with `pnut-exe` and once with the system gcc
compiler, and compares the checksums of the final binary (`build/tcc-boot2`)
produced by both bootstraps:

```shell
$ ./kit/test-bootstrap.sh
[...] # Bootstrap TCC with pnut-exe in build/rootfs-pnut.
[...] # Bootstrap TCC with gcc in build/rootfs-gcc.
+ ./bintools sha256sum build/tcc-boot0 build/tcc-boot1 build/tcc-boot2 build/tcc-boot3
8ae400b3ce80798212626c7cdb7565be02e553ed6a28bded26abd5c58002a2ff  build/tcc-boot0
d14b6f3894787a246b9584a31909b5ecde71e4ac733141e9eb678e95daa23a3f  build/tcc-boot1
03e96a1a63cc9bb3f577a14e50d20476507e3759bdc803f79e31d184bba44185  build/tcc-boot2
03e96a1a63cc9bb3f577a14e50d20476507e3759bdc803f79e31d184bba44185  build/tcc-boot3
Checksums match, bootstrap is reproducible.
```

The two environments are created with `--skip-initial-bootstrap` since only the
TCC bootstrap is tested; extra options given to `test-bootstrap.sh` are
forwarded to `setup-rootfs.sh`.

### Bootstrapping from c4

`pnut-exe` can also be bootstrapped from c4, a small C interpreter written in
around 600 lines of code, with the help of `cpp.c`, a preprocessor written in
the C subset supported by c4. The size of c4 makes it ideal for bootstrapping,
as it can be easily reviewed and run in minimal environments, and is faster than
bootstrapping from `pnut-sh.sh`. The `--bootstrap-from-c4` `setup-rootfs.sh`
option can be used to bootstrap `pnut-exe` from c4 instead of from `pnut-sh.sh`.

```shell
$ git submodule update --init kit/bootstrap-C4 # Fetch the c4 submodule if not already done
$ sudo rm -rf build/rootfs
$ ./kit/setup-rootfs.sh --dir build/rootfs \
  --bootstrap-shell ksh-i386-sarge \
  --include-utils \
  --extract-archives \
  --bootstrap-from-c4 \
  --execute-bootstrap
```

Note that a shell is still needed to run the bootstrap scripts and extract the
archive, but it no longer compiles anything.

### Options

The following options are available for `setup-rootfs.sh`:

- `--dir <path>`: The directory to create the root filesystem in.
- `--bootstrap-shell <shell>`: The shell to use for bootstrapping (bash-static,
  bash-i386-woody, zsh-i386-sarge, ksh-i386-sarge, dash-i386-lenny).
- `--skip-initial-bootstrap`: Skip the initial bootstrap and use a `pnut-exe`
  executable compiled by the host C compiler (`CC`, default: gcc).
- `--bootstrap-from-c4`: Bootstrap `pnut-exe` from c4 and `cpp.c` instead of
  from `pnut-sh.sh`.
- `--c4 <path>`: Use this prebuilt statically linked c4 executable instead of
  building one from the `kit/bootstrap-C4` submodule.
- `--execute-bootstrap`: Execute the bootstrap process automatically after
  setting up the root filesystem.

And the following options are available for `list-bootstrap-files.sh` and are
passed through from `setup-rootfs.sh`:

- `--include-utils`: Include shell implementations of simple core utilities
  (`ls.sh`, `touch.sh`, `wc.sh`).
- `--extract-archives`: Include the extracted source files of TCC and Mes
  instead of the `tar.gz` archives (removes the need for `tar` and `ungz` in the
  bootstrap environment).
- `--mes-libc`: Use the Mes libc implementation instead of the Pnut libc
  implementation (default: Pnut libc).
- `--tcc-version <version>`: The version of TCC to use (default: 0.9.27).
- `--bootstrap-from-c4`: Include `cpp.c` instead of `pnut-sh.sh` in the
  `jammed.sh` archive.

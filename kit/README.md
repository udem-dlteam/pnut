# Kit - tools for bootstrapping TCC

This directory contains scripts and tools to bootstrap TCC from source using
`pnut-sh.sh`. The bootstrap starts from the following components:

- `pnut-sh.sh`: A C to POSIX shell compiler.
- A POSIX shell: bash, dash, ksh, zsh, etc.

To bootstrap TCC from source, we need the following additional files:

- `pnut-exe.c`: A C to machine code compiler used to compile TCC.
- `bintools.c`: A collection of small utilities used to prepare the environment,
  including `cat`, `chmod`, `cp`, `mkdir`, `sha256sum`, `simple-patch`, `ungz`
  and `untar`.
- A libc implementation to compile TCC with (pnut-libc or mes-libc).
- Source code for TCC, with a few patches to make it compatible with `pnut-exe`.

That's it!

## Usage

To ensure that the bootstrap process is reproducible, we provide scripts to
create an isolated environment where the bootstrap can be performed.

1. `kit/bootstrap.sh`: bootstrap `pnut-exe` from `pnut-sh.sh`, then TCC from
    `pnut-exe`.
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
where the bootstrap process is executed automatically. Because we're
bootstrapping from a shell, the bootstrap process takes a few minutes to
complete. If you only care about the TCC bootstrap, you can skip the shell
bootstrap and use a precompiled `pnut-exe` executable with the
`--skip-shell-bootstrap` option.

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

The two environments are created with `--skip-shell-bootstrap` since only the
TCC bootstrap is tested; extra options given to `test-bootstrap.sh` are
forwarded to `setup-rootfs.sh`.

### Options

The following options are available for `setup-rootfs.sh`:

- `--dir <path>`: The directory to create the root filesystem in.
- `--bootstrap-shell <shell>`: The shell to use for bootstrapping (bash-static,
  bash-i386-woody, zsh-i386-sarge, ksh-i386-sarge, dash-i386-lenny).
- `--skip-shell-bootstrap`: Skip the slow shell bootstrap and use a precompiled
  `pnut-exe` executable.
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

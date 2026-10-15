# Handoff: compiling TCC's aarch64 backend with pnut-exe

## TL;DR

It works, as a **cross** bootstrap. `pnut-exe` (x86-64) compiles TCC 0.9.27 with
`-DTCC_TARGET_ARM64=1`, and the resulting compiler builds a **native AArch64 TCC**
that runs under `qemu-aarch64` and **reaches a fixed point in one step**.

```
pnut-exe (x86-64, bootstrapped from pnut itself)
  └─> tcc-arm64          x86-64 host, AArch64 target        ← pnut compiles arm64-gen.c
        └─> tcc-arm64-boot0   native AArch64, static, pnut libc
              └─> tcc-arm64-boot1   (tcc-boot1.o == tcc-boot0.o → fixed point)
```

Nothing in the chain after `pnut-exe` uses glibc, the cross gcc, or binutils —
the AArch64 `crt1.o`, `libc.a` and `libtcc1.a` are all produced by TCC itself
from `portable_libc` and `kit/libtcc1.c` + `lib/lib-arm64.c`.

pnut still has no AArch64 backend, so this is a cross-compile: the final `tcc`
binary is AArch64 but is built on x86-64. A native-on-arm64 bootstrap needs a
pnut AArch64 backend (or running the last stage on real hardware/qemu).

Seven pnut-side patches were needed; one of them (`assert.h`) is a real bug that
also affects the existing x86 bootstrap.

## Environment added to the machine

Installed with apt (needed only for *validation*, not for the bootstrap):

- `qemu-user`, `qemu-user-binfmt` — run AArch64 binaries
- `binutils-aarch64-linux-gnu` — `objdump`/`nm` to inspect and diff objects
- `gcc-aarch64-linux-gnu`, `libc6-dev-arm64-cross` — used only to build a
  reference TCC and to sanity-check the ELF output

## Reproducing

Scripts are scratch files under `build/arm64/` (gitignored) and should be folded
into `kit/` if this is kept:

| script | what it does |
|---|---|
| `build/arm64/prepare-tcc.sh` | extract `kit/tcc-0.9.27.tar.gz` to `build/arm64/tcc-0.9.27`, copy `kit/config.h`, apply all patches |
| `build/arm64/compile-tcc-arm64.sh` | rebuild `pnut-exe-for-tcc` (safe mode + portable libc) from `build/final-x86_64-pnut/pnut-exe`, then compile TCC with `-DTCC_TARGET_ARM64=1` → `build/arm64/tcc-arm64` |
| `build/arm64/stage1-native-tcc.sh` | build target `crt1.o`/`libc.a`/`libtcc1.a`, then cross-build a native AArch64 `tcc-arm64-boot0` and run `tcc -v` under qemu |
| `build/arm64/stage2-check.sh` | differential: cross tcc vs native tcc on a corpus + the `tcc.c` fixed-point check |
| `build/arm64/diff-test.sh` | differential: pnut-built cross tcc vs a gcc-built one on a corpus |
| `build/arm64/stage1-cross-tcc.sh` | older variant that links stage 1 with the cross gcc (kept for comparison) |
| `build/arm64/try-patch.sh` + `build/arm64/patches/*.py` | rebuild TCC with pnut after a candidate source patch and check that a constant 64-bit shift survives (used to hunt the assert bug) |

Full run (~10 s):

```sh
./build/arm64/prepare-tcc.sh
./build/arm64/compile-tcc-arm64.sh          # -> build/arm64/tcc-arm64 (x86-64, AArch64 target)
./build/arm64/tcc-arm64 -v                  # tcc version 0.9.27 (AArch64 Linux)
./build/arm64/stage1-native-tcc.sh          # -> build/arm64/tcc-arm64-boot0 (AArch64), runs under qemu
./build/arm64/stage2-check.sh               # corpus + fixed-point comparison
```

Artifacts (md5):

```
4a5ee606d68b9c6b30b2052f68ec7e40  build/arm64/tcc-arm64          (x86-64 host)
61c8d837255662f71dcd316342329f30  build/arm64/tcc-arm64-boot0    (AArch64)
0a7ae38ea671277bd52a6bdaff74a94b  build/arm64/tcc-arm64-boot1    (AArch64)
385bebad7287550d0f7a33021cf4a429  build/arm64/stage2/tcc-ncc.o   (compiled by boot0)
385bebad7287550d0f7a33021cf4a429  build/arm64/stage2/tcc-boot1.o (compiled by boot1)  ← identical
```

## Validation results

1. **pnut is faithful.** `tcc-arm64` (built by pnut) and the same TCC built by
   gcc emit byte-identical objects: 12/12 files in the `diff-test.sh` corpus, and
   for `tcc.c` itself (a ~1 MB single TU with `ONE_SOURCE`) `.text`, `.symtab`
   and all symbols match; 10 bytes of `.data` differ (open item #1).
2. **The AArch64 libc works.** `hello3.c` compiled by `tcc-arm64`, linked with
   `crt1.o` + `libc.a` built from `portable_libc`, runs under qemu, prints, reads
   `argc`, and returns the right exit status.
3. **Fixed point.** `boot0` and `boot1` compile `tcc.c` to the same object
   (the property `kit/test-bootstrap.sh` checks for x86).
4. **No segfaults.** See the signal table below.

## Patches (current working tree)

Modified (`git diff`):

- **`portable_libc/include/assert.h` — real bug, affects the x86 bootstrap too.**
  `#define assert(cond) if (!(cond)) abort()` is an unbraced `if`. In
  `if (c) assert(x); else ...` the `else` binds to the `if` *inside* the macro.
  TCC's `arm64_gen_opic()` is exactly that shape:

  ```c
  uint32_t n = 32 << l;
  val = val & (n - 1);
  if (rev) return 0;
  if (!val)
      assert(0);
  else if (op == TOK_SHL)
      o(... lsl ...);
  ```

  With the pnut libc this compiles to "if the shift count is non-zero, do
  nothing", so **every constant 64-bit shift in the target program was silently
  dropped** (found as `assemble_double()` losing `res <<= 32`). Now
  `do { if (!(cond)) abort(); } while (0)` (pnut parses `do/while` fine).
  Grepping TCC for `if (...) assert(...); else` gives exactly one hit,
  `arm64-gen.c:1401`, which is why x86 never noticed. Self-consistency (a fixed
  point) does **not** catch this class of bug — only the gcc-vs-pnut object
  comparison does.
- `portable_libc/include/inttypes.h` — `uintptr_t` for `__aarch64__`
  (`tccelf.c` uses it via `elf.h`).
- `portable_libc/include/stdarg.h` — added an AArch64 branch using TCC's exact
  `va_list` struct (`arm64-gen.c`'s `gen_va_start` hardcodes offsets 0/8/16/24/28),
  plus three macros so one source works on both targets:
  `_VA_LIST_TYPE` (parameter type), `_VA_PASS(ap)` (pass a local `va_list` to a
  `v*printf`), `_VA_ARG(ap, type)`. On x86/x86-64 they degenerate to the old
  by-value behaviour.
- `portable_libc/include/stdio.h`, `portable_libc/src/stdio.c` — `vprintf`,
  `vfprintf`, `vsnprintf` now take `_VA_LIST_TYPE`.
  **Why:** TCC's AArch64 backend has no struct arguments — `arm64_pcs` →
  `arm64_type_size` hits `assert(0)` on `VT_STRUCT` — and its `va_list` is a
  40-byte struct, so `int vprintf(const char *, va_list)` cannot be compiled or
  called. Added the missing `vprintf` declaration too.
- `portable_libc/src/crt1.c` — new `#elif defined(__aarch64__)` section:
  - **TCC's AArch64 backend has no inline asm at all** (`CONFIG_TCC_ASM` is only
    defined for i386/x86-64/arm), and the ELFs TCC emits have a non-executable
    data segment, so the classic `__asm__("... syscall")` approach is impossible.
    The syscall trampoline is therefore raw machine code in a
    `__attribute__((section(".text"))) static const unsigned int[]`, called
    through a function pointer — verified that TCC's AArch64 output puts that in
    the `R E` segment and executes it. Nine instructions: shift `x1..x6 → x0..x5`,
    `x8 ← x0`, `svc #0`, `ret`; `syscall6(n, a..e)` calls it.
  - `_start` is plain C using `__builtin_frame_address(0) + 224`: every function
    TCC generates starts with `stp x29,x30,[sp,#-224]!` (`gfunc_prolog`, the
    varargs save area), so the entry stack pointer — where the kernel put
    `argc`, then `argv` — is always `x29 + 224`.
  - AArch64 has no `chmod`/`access`/`unlink`/`mkdir`/`open` syscalls, only the
    `*at()` forms: implemented with `AT_FDCWD = -100`.
- `portable_libc/src/stdlib.c` — `strtold` for AArch64 written with integer
  arithmetic (binary128: rebias exponent, `sig << 59`). Without it `libc.o`
  references `__extenddftf2`, which `libtcc1.a` for arm64 does not provide
  (`OBJ-arm64 = lib-arm64.o` only; upstream takes soft-float from libgcc).

New TCC patches in `kit/tcc-patches/0.9.27/` (six `.before`/`.after` pairs, all
named `va_list_*`): TCC's own `strcat_vprintf()` and `error1()` take `va_list` by
value, which the AArch64 backend cannot compile. They now use `_VA_LIST_TYPE` /
`_VA_PASS` with a `#ifndef _VA_LIST_TYPE` fallback to plain `va_list`, so gcc and
glibc builds are unaffected. Patch order matters (`va_list_by_pointer` defines
the macros and must be applied first).

Not a patch, but needed for the target libs: AArch64 wants `lib/lib-arm64.c` in
`libtcc1.a` (for `__clear_cache`) and **no** `lib/va_list.c` (varargs are inline
in the backend), which is the inverse of the x86-64 recipe in `kit/bootstrap.sh`.

## Signals / crash behaviour

| compiler | ok.c | bad.c (syntax error) | `__asm__` on arm64 | big.c |
|---|---|---|---|---|
| `tcc-arm64-boot0` (AArch64, qemu) | 0 | 1 | 1 | 0 |
| `tcc-arm64` (x86-64, built by pnut) | 0 | **133 (SIGTRAP, core)** | **133** | 0 |
| `tcc-arm64-by-gcc` (reference) | 0 | 1 | 1 | 0 |

No segfaults anywhere. The SIGTRAP is pre-existing and not arm64-specific:
`tcc_error()` ends in `longjmp(s1->error_jmp_buf, 1)`, but `setjmp.c` only
defines `longjmp` under `ADD_LIBC_STUB`, and `make_tcc_bootstrap()` compiles
`portable_libc/libc.c` **without** that flag. Since pnut-exe is built with
`-DUNDEFINED_LABELS_ARE_RUNTIME_ERRORS`, the missing symbol becomes a runtime
error (`exe.c:2004`) and pnut's fatal handler traps.
**Verified fix:** add `-D ADD_LIBC_STUB` to the pnut→TCC compile → exit 1
instead of SIGTRAP/core dump. It only affects error paths, which is why the x86
bootstrap never noticed. (The native arm64 tcc is unaffected: `go()`-style libc
archives are built with `ADD_LIBC_STUB`, so `longjmp` is the `pnut_abort` stub.)

## Open items

1. **`.data` byte difference (10 bytes) between the pnut-built and gcc-built
   cross TCC**: two anonymous 8-byte slots (`L.618`, `L.619` at `.data+0x2fc0`)
   hold `0x408f400000000000` (IEEE double 1000.0) in the gcc build and
   `0x0000000000000003e8` (integer 1000) in the pnut build. Everything else —
   `.text`, `.symtab`, symbol addresses — is identical. It is a global double
   initialiser emitted as integer bits; harmless for the chain (the arm64 chain
   converges) but it means pnut-built TCC mishandles at least one host `double`
   constant path. Not located in TCC's source yet (there is no `1000.0` literal
   in the tree), so it likely comes from a `double` expression folded by gcc at
   compile time. Worth a look: pnut has no float support, so `CValue`/`SValue`
   (`long double ld` member) is the prime suspect.
2. **`gen_cast` differs between host-built and AArch64-built TCC objects**
   (frame offsets `-112/-128` vs `-120`, some reordering, +84 bytes). It is
   *stable*: `boot1` reproduces `boot0` exactly, so it is a host-dependent
   property (host `long double` size/alignment inside TCC's own frame), not a
   divergence. It does mean the x86 and arm64 chains do not converge to the same
   bytes as each other, only within themselves.
3. **No floating point.** The arm64 stages are built without `-D HAVE_FLOAT`
   (as pnut's TCC always is), so the resulting AArch64 TCC cannot compile
   float/double code. `strtold` is now provided anyway, but float literals,
   `printf("%f")` and the `__truncdfsf2`/`__netf2`/`__fixunstfdi` helpers TCC
   emits for arm64 are not available. Anything that needs float support on
   arm64 still requires libgcc or real soft-float in `libtcc1.a`.
4. **`kit/` is not wired up yet.** `bootstrap.sh` and `list-bootstrap-files.sh`
   still only accept `--arch i386|x86_64`. To integrate: a `--arch arm64` (or
   `--target arm64`) mode that is explicitly *cross-only* (pnut-exe stays x86-64
   or i386), selects `lib/lib-arm64.c` instead of `lib/va_list.c`, uses
   `CONFIG_TCC_ELFINTERP=/lib/ld-linux-aarch64.so.1`, and adds the six
   `va_list_*` patches (they are no-ops on x86, so they can join the common
   `TCC_0_9_27_PATCHES` list). Also worth adding `-D ADD_LIBC_STUB` to
   `make_tcc_bootstrap`.
5. **No inline asm for AArch64 anywhere.** Besides `crt1.c`, this blocks any
   TCC-based build step that needs `asm`, and it is why the raw-machine-code
   trampoline hack exists. If pnut ever grows an AArch64 backend, pnut's own
   runtime can emit `svc #0` directly and this whole problem disappears for
   pnut-built programs (TCC's AArch64 target would still need it for `crt1.o`).
6. `bootstrap.sh`'s `PNUT_SIZE_OPTS` for the arm64 backend: reused the x86-64
   values (`HEAP_SIZE`/`MAX_CODE_SIZE`/`PROGRAM_SIZE` = 2000000). The arm64
   backend compiles fine within those limits.

## Working tree

```
 M portable_libc/include/assert.h        (dangling-else assert — affects x86 too)
 M portable_libc/include/inttypes.h      (uintptr_t for aarch64)
 M portable_libc/include/stdarg.h        (aarch64 va_list + _VA_* macros)
 M portable_libc/include/stdio.h         (v*printf take _VA_LIST_TYPE)
 M portable_libc/src/crt1.c              (aarch64 syscalls + _start)
 M portable_libc/src/stdio.c             (same)
 M portable_libc/src/stdlib.c            (aarch64 strtold)
?? kit/tcc-patches/0.9.27/va_list_by_pointer.{before,after}
?? kit/tcc-patches/0.9.27/va_list_by_pointer_call2.{before,after}
?? kit/tcc-patches/0.9.27/va_list_error1.{before,after}
?? kit/tcc-patches/0.9.27/va_list_error1_noabort.{before,after}
?? kit/tcc-patches/0.9.27/va_list_error1_error.{before,after}
?? kit/tcc-patches/0.9.27/va_list_error1_warning.{before,after}
```

Nothing is committed. `build/` (including `build/arm64/`, the scripts and all
artefacts) is gitignored.

### Regression check before committing

`./kit/test-bootstrap.sh --tcc-version 0.9.27` (and `--arch x86_64`) must still
print matching pnut/gcc checksums — the `assert.h` and `stdarg`/`stdio` changes
touch the x86 path, and the assert fix can change generated code if any x86-side
code had the same dangling-else shape (only `arm64-gen.c` does in TCC, but
`portable_libc` itself is compiled by both pnut and TCC).

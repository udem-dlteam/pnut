all: test
.PHONY: all test clean pnut-exe

.SUFFIXES:

PNUT_SRC=../pnut.c
PNUT_TARGET=-Dtarget_x86_64_linux -DONE_PASS_GENERATOR -DSAFE_MODE -DNO_BUILTIN_LIBC

# pnut-exe built with NO_BUILTIN_LIBC provides only OS primitives: exit, read,
# write, open, close, lseek. The portable_libc provides everything else: malloc,
# printf, fopen, etc. on top of pnut's OS builtins.
pnut-exe: ${PNUT_SRC}
	${CC} -std=c99 ${PNUT_TARGET} ${PNUT_SRC} -o pnut-exe

LIBC_FILES=src/crt1.c libc.c
PNUT_FLAGS=-DUSE_PORTABLE_LIBC -DADD_LIBC_STUB -Iinclude/

test: test_libc_with_gcc test_libc_with_pnut_exe

# Test with system libc (validates tests are correct)
test_libc_with_gcc:
	@echo ======================== gcc \(test-libc.c with system libc\)
	${CC} test-libc.c -o test-libc-gcc
	./test-libc-gcc

# Test with portable_libc via pnut-exe
test_libc_with_pnut_exe: pnut-exe
	@echo ======================== pnut-exe with portable_libc \(test-libc.c\)
	./pnut-exe ${PNUT_FLAGS} ${LIBC_FILES} test-libc.c -o test-libc-pnut
	./test-libc-pnut

clean:
	rm -f pnut-exe test-libc-gcc test-libc-pnut

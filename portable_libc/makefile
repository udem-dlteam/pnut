all: test

.SUFFIXES:

#CC=clang -Wno-incompatible-pointer-types-discards-qualifiers
CC=gcc-13 -Wno-discarded-qualifiers
TCC=tcc -nostdlib
#TCC_VERSION=08a4c52de39b202f02d1ec525c64336d11ad9ccf
#TCC_VERSION=1b57560502d04cbc2e410a960d965765d7f3d635
#TCC=/part/01/feeley/tcc/builds/${TCC_VERSION}/tinycc/tcc

#test: test_with_gcc test_with_gcc_using_portable_libc test_with_tcc_using_portable_libc test_with_pnut_using_portable_libc
test: test_with_gcc test_with_gcc_using_portable_libc test_with_pnut_using_portable_libc

test_with_gcc:
	@echo ======================== gcc
	${CC} test.c
	./a.out abcdef ; echo $$?

test_with_gcc_using_portable_libc:
	@echo ======================== gcc with portable_libc
	cat src/crt1.c src/crti.c portable_libc.c test.c > test-with-libc.c
	${CC} -DUSE_PORTABLE_LIBC -I. -nostdlib -lSystem test-with-libc.c
	./a.out abcdef ; echo $$?

test_with_tcc_using_portable_libc:
	@echo ======================== tcc with portable_libc
	cat src/crt1.c src/crti.c portable_libc.c test.c > test-with-libc.c
	${TCC} -m32 -DUSE_PORTABLE_LIBC -I. -static test-with-libc.c
	./a.out abcdef ; echo $$?

test_with_pnut_using_portable_libc:
	@echo ======================== pnut with portable_libc
	cat src/crt1.c src/crti.c portable_libc.c test.c > test-with-libc.c
	ksh ../build/pnut-sh.sh -DUSE_PORTABLE_LIBC test-with-libc.c > test.sh
	ksh test.sh abcdef ; echo $$?

clean:
	rm -f a.out test.sh test-with-libc.c

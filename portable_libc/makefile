all: test

.SUFFIXES:

#CC=clang -Wno-incompatible-pointer-types-discards-qualifiers
CC=gcc-13 -Wno-discarded-qualifiers

test: test_with_gcc test_with_gcc_using_portable_libc test_with_tcc_using_portable_libc test_with_pnut_using_portable_libc

test_with_gcc:
	@echo ======================== gcc
	${CC} test.c
	./a.out ; echo $$?

test_with_gcc_using_portable_libc:
	@echo ======================== gcc with portable_libc
	${CC} -DUSE_PORTABLE_LIBC test.c portable_libc.c crt1.c crti.c
	./a.out ; echo $$?

test_with_tcc_using_portable_libc:
	@echo ======================== tcc with portable_libc
	tcc -c crt1.c
	tcc -c crti.c
	tcc -DUSE_PORTABLE_LIBC -I. -L. -nostdlib test.c portable_libc.c crt1.o crti.o
	./a.out ; echo $$?

test_with_pnut_using_portable_libc:
	@echo ======================== pnut with portable_libc
	ksh ../bootstrap-results/pnut.sh -DUSE_PORTABLE_LIBC test.c portable_libc.c > test.sh
	ksh test.sh ; echo $$?

clean:
	rm -f a.out test.sh

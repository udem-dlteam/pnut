#define _GNU_SOURCE  /* for memmem */

#ifdef USE_PORTABLE_LIBC
#include "include/stdio.h"
#include "include/string.h"
#include "include/stdlib.h"
#include "include/ctype.h"
#else
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#endif

static int failed = 0;

void check(int cond, const char *name) {
  if (cond) {
    printf("PASS: %s\n", name);
  } else {
    printf("FAIL: %s\n", name);
    failed++;
  }
}

int comp(int n) { return n < 0 ? -1 : n > 0 ? 1 : 0; }

/* ---- string.c ---- */

void test_memset() {
  char buf[8];
  void *r = memset(buf, 'z', 5);
  check(r == buf,                   "memset: returns dest");
  check(buf[0]=='z' && buf[4]=='z', "memset: fills bytes");
  memset(buf, 0, 8);
  check(buf[0]==0 && buf[7]==0,     "memset: zero fill");
  memset(buf, 'a', 0);
  check(buf[0]==0,                  "memset: zero length is no-op");
}

void test_memcpy() {
  char dst[8];
  void *r = memcpy(dst, "hello", 6);
  check(r == dst,                  "memcpy: returns dest");
  check(strcmp(dst, "hello") == 0, "memcpy: copies bytes");
}

void test_memmove() {
  char buf[8] = "abcde";
  void *r = memmove(buf+1, buf, 3);
  check(r == buf+1,                                "memmove: returns dest");
  check(buf[1]=='a' && buf[2]=='b' && buf[3]=='c', "memmove: overlap right");
  strcpy(buf, "abcde");
  memmove(buf, buf+2, 3);
  check(buf[0]=='c' && buf[1]=='d' && buf[2]=='e', "memmove: overlap left");
}

void test_memcmp() {
  check(comp(memcmp("abc", "abc", 3)) ==  0, "memcmp: equal");
  check(comp(memcmp("abc", "abd", 3)) == -1, "memcmp: less");
  check(comp(memcmp("abd", "abc", 3)) ==  1, "memcmp: greater");
  check(comp(memcmp("abc", "abc", 0)) ==  0, "memcmp: zero length");
  check(comp(memcmp("\xff", "\x01", 1)) == 1, "memcmp: unsigned bytes");
}

void test_strlen() {
  check(strlen("") == 0,      "strlen: empty");
  check(strlen("a") == 1,     "strlen: single char");
  check(strlen("hello") == 5, "strlen: normal");
}

void test_strcpy() {
  char buf[8];
  char *r;
  r = strcpy(buf, "hello");
  check(r == buf,                  "strcpy: returns dest");
  check(strcmp(buf, "hello") == 0, "strcpy: copies string");
  strcpy(buf, "");
  check(buf[0] == 0,               "strcpy: empty string");
}

void test_strcat() {
  char buf[16];
  char *r;
  strcpy(buf, "hello");
  r = strcat(buf, " world");
  check(r == buf,                        "strcat: returns dest");
  check(strcmp(buf, "hello world") == 0, "strcat: appends");
  strcat(buf, "");
  check(strcmp(buf, "hello world") == 0, "strcat: empty append");
}

void test_strchr() {
  const char *s = "abcabc";
  check(strchr(s, 'a') == s,   "strchr: first occurrence");
  check(strchr(s, 'c') == s+2, "strchr: finds char");
  check(strchr(s, 0)   == s+6, "strchr: finds null terminator");
  check(strchr(s, 'z') == 0,   "strchr: not found");
}

void test_strrchr() {
  const char *s = "abcabc";
  check(strrchr(s, 'a') == s+3, "strrchr: last occurrence");
  check(strrchr(s, 'c') == s+5, "strrchr: finds last char");
  check(strrchr(s, 0)   == s+6, "strrchr: finds null terminator");
  check(strrchr(s, 'z') == 0,   "strrchr: not found");
}

void test_strcmp() {
  check(comp(strcmp("abc", "abc"))   ==  0, "strcmp: equal");
  check(comp(strcmp("abc", "ab"))    ==  1, "strcmp: longer");
  check(comp(strcmp("ab",  "abc"))   == -1, "strcmp: shorter");
  check(comp(strcmp("\xff", "\x01")) ==  1, "strcmp: unsigned bytes");
  check(comp(strcmp("", ""))         ==  0, "strcmp: both empty");
}

void test_strncmp() {
  check(comp(strncmp("abc", "abc", 3)) ==  0, "strncmp: equal full");
  check(comp(strncmp("abc", "abd", 2)) ==  0, "strncmp: equal prefix");
  check(comp(strncmp("abc", "abd", 3)) == -1, "strncmp: different");
  check(comp(strncmp("abc", "xyz", 0)) ==  0, "strncmp: zero length");
}

void test_strstr() {
  const char *h = "hello world";
  check(strstr(h, "world") != 0,                  "strstr: found");
  check(strcmp(strstr(h, "world"), "world") == 0, "strstr: correct pointer");
  check(strstr(h, "xyz") == 0,                    "strstr: not found");
  check(strstr(h, "hello world!!!") == 0,         "strstr: needle longer");
}

void test_memmem() {
  const char *h = "abcdefgh";
  check(memmem(h, 8, "cde", 3) == h+2,    "memmem: found");
  check(memmem(h, 8, "xyz", 3) == 0,      "memmem: not found");
  check(memmem(h, 3, "abcdefgh", 8) == 0, "memmem: needle longer than haystack");
}

/* ---- stdio.c: sprintf/snprintf ---- */

void test_sprintf() {
  char buf[64];
  int n;

  n = sprintf(buf, "hello");
  check(n==5 && strcmp(buf,"hello")==0,   "sprintf: plain string");

  n = sprintf(buf, "%d", 42);
  check(n==2 && strcmp(buf,"42")==0,      "sprintf: decimal");

  n = sprintf(buf, "%d", -42);
  check(n==3 && strcmp(buf,"-42")==0,     "sprintf: negative");

  // -2147483648 is not a valid positive int, so avoid compiler warning
  { int int_min = -2147483647; int_min -= 1;
    sprintf(buf, "%d", int_min);
    check(strcmp(buf,"-2147483648")==0,   "sprintf: INT_MIN"); }

  n = sprintf(buf, "%x", 255);
  check(n==2 && strcmp(buf,"ff")==0,      "sprintf: hex");

  n = sprintf(buf, "%o", 8);
  check(n==2 && strcmp(buf,"10")==0,      "sprintf: octal");

  n = sprintf(buf, "%s", "world");
  check(n==5 && strcmp(buf,"world")==0,   "sprintf: string arg");

  n = sprintf(buf, "%c", 'A');
  check(n==1 && buf[0]=='A',              "sprintf: char");

  n = sprintf(buf, "%%");
  check(n==1 && buf[0]=='%',             "sprintf: percent literal");

  sprintf(buf, "%05d", 42);
  check(strcmp(buf,"00042")==0,           "sprintf: zero pad");

  sprintf(buf, "%5d", 42);
  check(strcmp(buf,"   42")==0,           "sprintf: right justify");

  sprintf(buf, "%-5d|", 42);
  check(strcmp(buf,"42   |")==0,          "sprintf: left justify");
}

void test_snprintf() {
  char buf[8];
  int n;
  n = snprintf(buf, 4, "hello");
  check(n == 5,                                "snprintf: returns full length");
  check(strncmp(buf,"hel",3)==0 && buf[3]==0,  "snprintf: truncates with null");
}

void test_fprintf() {
  FILE *f;
  char buf[64];
  int n;

  f = fopen("/tmp/test_pnut_fprintf.txt", "w");
  check(f != 0, "fprintf: fopen");
  if (!f) return;

  n = fprintf(f, "%d + %d = %d", 2, 3, 5);
  check(n == 9, "fprintf: returns char count");

  n = fprintf(f, " %s!", "ok");
  check(n == 4, "fprintf: string arg");

  fclose(f);

  f = fopen("/tmp/test_pnut_fprintf.txt", "r");
  if (!f) return;
  n = fread(buf, 1, 63, f);
  buf[n] = 0;
  check(strcmp(buf, "2 + 3 = 5 ok!") == 0, "fprintf: file contents match");
  fclose(f);
  remove("/tmp/test_pnut_fprintf.txt");
}

void test_printf() {
  int n;
  /* printf uses the same vfprintf as sprintf/fprintf, so just check return value */
  n = printf("");
  check(n == 0, "printf: empty returns 0");
  n = printf("%d", 42);
  check(n == 2, "printf: returns char count");
}

void test_fputs_puts() {
  FILE *f;
  char buf[64];
  int n;

  f = fopen("/tmp/test_pnut_fputs.txt", "w");
  check(f != 0, "fputs: fopen");
  if (!f) return;

  fputs("hello", f);
  fputs(" world", f);
  fclose(f);

  f = fopen("/tmp/test_pnut_fputs.txt", "r");
  if (!f) return;
  n = fread(buf, 1, 63, f);
  buf[n] = 0;
  check(strcmp(buf, "hello world") == 0, "fputs: writes strings");
  fclose(f);
  remove("/tmp/test_pnut_fputs.txt");

  /* puts writes to stdout; just check it succeeds (non-negative) */
  n = puts("puts_test");
  check(n >= 0, "puts: returns non-negative");
}

void test_fdopen() {
  int fd;
  FILE *f;
  char buf[16];
  int n;

  fd = open("/tmp/test_pnut_fdopen.txt", 01 | 0100 | 01000, 0666);
  check(fd >= 0, "fdopen: open succeeds");
  if (fd < 0) return;

  f = fdopen(fd, "w");
  check(f != 0, "fdopen: returns non-null");
  fputs("abc", f);
  fclose(f);

  f = fopen("/tmp/test_pnut_fdopen.txt", "r");
  if (!f) return;
  n = fread(buf, 1, 3, f);
  check(n == 3 && strncmp(buf, "abc", 3) == 0, "fdopen: wrote correctly");
  fclose(f);
  remove("/tmp/test_pnut_fdopen.txt");
}

void test_fflush_fclose() {
  FILE *f;
  f = fopen("/tmp/test_pnut_flush.txt", "w");
  if (!f) return;
  check(fflush(f) == 0, "fflush: returns 0");
  check(fclose(f) == 0, "fclose: returns 0");
  remove("/tmp/test_pnut_flush.txt");
}

/* ---- stdlib.c ---- */

void test_atoi() {
  check(atoi("42")   == 42,  "atoi: positive");
  check(atoi("-42")  == -42, "atoi: negative");
  check(atoi("0")    == 0,   "atoi: zero");
  check(atoi("  42") == 42,  "atoi: leading spaces");
}

void test_strtol() {
  char *end;
  check(strtol("42",   0, 10) == 42,   "strtol: decimal");
  check(strtol("-42",  0, 10) == -42,  "strtol: negative");
  check(strtol("0xff", 0,  0) == 255,  "strtol: hex auto");
  check(strtol("ff",   0, 16) == 255,  "strtol: hex explicit");
  check(strtol("010",  0,  0) == 8,    "strtol: octal auto");
  check(strtol("10",   0,  8) == 8,    "strtol: octal explicit");
  strtol("42xyz", &end, 10);
  check(strcmp(end, "xyz") == 0,       "strtol: endptr after digits");
  strtol("xyz", &end, 10);
  check(strcmp(end, "xyz") == 0,       "strtol: endptr on invalid");
  check(strtol("10",   0,  26) == 26,  "strtol: base 26");
}

void test_strtoul() {
  check(strtoul("42",   0, 10) == 42,  "strtoul: decimal");
  check(strtoul("0xff", 0,  0) == 255, "strtoul: hex auto");
  check(strtoul("010",  0,  0) == 8,   "strtoul: octal auto");
  check(strtoul("ff",   0, 16) == 255, "strtoul: hex explicit");
}

void test_strtoll() {
  check(strtoll("1234567890",  0, 10) == 1234567890,   "strtoll: decimal");
  check(strtoll("-1234567890", 0, 10) == -1234567890,  "strtoll: negative");
  check(strtoll("0xff",        0,  0) == 255,          "strtoll: hex auto");
  check(strtoll("010",         0,  0) == 8,            "strtoll: octal auto");
}

void test_strtoull() {
  check(strtoull("42",   0, 10) == 42,  "strtoull: decimal");
  check(strtoull("0xff", 0,  0) == 255, "strtoull: hex auto");
  check(strtoull("010",  0,  0) == 8,   "strtoull: octal auto");
  check(strtoull("ff",   0, 16) == 255, "strtoull: hex explicit");
}

void test_getenv() {
#ifdef USE_PORTABLE_LIBC
  /* Portable libc stub always returns NULL */
  check(getenv("PATH") == 0, "getenv: returns null");
#endif
  check(getenv("UNLIKELY_VAR_XYZ_999") == 0, "getenv: unset var returns null");
}

void test_malloc() {
  int *p = malloc(10 * sizeof(int));
  int i;
  int ok;
  check(p != 0, "malloc: non-null for positive size");
  for (i = 0; i < 10; i++) p[i] = i;
  ok = 1;
  for (i = 0; i < 10; i++) if (p[i] != i) ok = 0;
  check(ok, "malloc: memory is writable and readable");
  free(p);

  free(malloc(0)); /* should not crash */
}

void test_realloc() {
  int *p = malloc(sizeof(int));
  int *p2;
  int *p3;
  *p = 42;
  p2 = realloc(p, 2 * sizeof(int));
  check(p2 != 0,   "realloc: returns non-null");
  check(*p2 == 42, "realloc: preserves data");
  p3 = realloc(0, sizeof(int));
  check(p3 != 0,   "realloc: from null acts like malloc");
}

int int_cmp(const void *a, const void *b) {
  int x = *(int*)a, y = *(int*)b;
  return x < y ? -1 : x > y ? 1 : 0;
}

void test_qsort() {
  int arr[]  = {5, 3, 1, 4, 2};
  int arr2[] = {42};
  int arr3[] = {3, 1, 2, 3, 1};

  qsort(arr, 5, sizeof(int), int_cmp);
  check(arr[0]==1 && arr[1]==2 && arr[2]==3 && arr[3]==4 && arr[4]==5,
        "qsort: ascending order");

  qsort(arr2, 1, sizeof(int), int_cmp);
  check(arr2[0] == 42, "qsort: single element");

  qsort(arr3, 5, sizeof(int), int_cmp);
  check(arr3[0]==1 && arr3[1]==1 && arr3[2]==2 && arr3[3]==3 && arr3[4]==3,
        "qsort: with duplicates");
}

struct pair { int key; int val; };

int pair_cmp(const void *a, const void *b) {
  int x = ((struct pair*)a)->key;
  int y = ((struct pair*)b)->key;
  return x < y ? -1 : x > y ? 1 : 0;
}

void test_qsort_structs() {
  struct pair pairs[4];
  pairs[0].key = 30; pairs[0].val = 100;
  pairs[1].key = 10; pairs[1].val = 200;
  pairs[2].key = 20; pairs[2].val = 300;
  pairs[3].key = 10; pairs[3].val = 400;

  qsort(pairs, 4, sizeof(struct pair), pair_cmp);
  check(pairs[0].key==10 && pairs[1].key==10 && pairs[2].key==20 && pairs[3].key==30,
        "qsort structs: keys sorted");
  /* Verify payload traveled with its key */
  check((pairs[0].val==200 || pairs[0].val==400) &&
        (pairs[1].val==200 || pairs[1].val==400),
        "qsort structs: vals follow keys");
  check(pairs[2].val==300 && pairs[3].val==100,
        "qsort structs: vals correct for unique keys");
}

/* ---- ctype.c ---- */

void test_ctype() {
  check( isdigit('0') && isdigit('9') && !isdigit('a'),                       "isdigit");
  check( isxdigit('0') && isxdigit('a') && isxdigit('F') && !isxdigit('g'),   "isxdigit");
  check( islower('a') && islower('z') && !islower('A'),                        "islower");
  check( isupper('A') && isupper('Z') && !isupper('a'),                        "isupper");
  check( isalpha('a') && isalpha('Z') && !isalpha('1'),                        "isalpha");
  check( isalnum('a') && isalnum('0') && !isalnum('!'),                        "isalnum");
  check( tolower('A')=='a' && tolower('a')=='a' && tolower('1')=='1',          "tolower");
  check( toupper('a')=='A' && toupper('A')=='A' && toupper('1')=='1',          "toupper");
  check( isspace(' ') && isspace('\n') && isspace('\t') && !isspace('a'),      "isspace");
  check( isprint(' ') && isprint('~') && !isprint('\n'),                       "isprint");
  check( isgraph('a') && !isgraph(' ') && !isgraph('\n'),                      "isgraph");
  check( iscntrl('\n') && iscntrl('\t') && !iscntrl('a'),                      "iscntrl");
  check( isascii('a') && isascii(0) && !isascii(128),                          "isascii");
  check( ispunct('!') && ispunct('.') && !ispunct('a') && !ispunct(' '),        "ispunct");
  check( isblank(' ') && isblank('\t') && !isblank('\n'),                       "isblank");
}

/* ---- file I/O ---- */

void test_file_io() {
  FILE *f;
  char buf[16];
  int n;
  int c;

  f = fopen("/tmp/test_pnut_libc.txt", "w");
  check(f != 0, "fopen: write mode");
  if (!f) return;

  n = fwrite("hello\n", 1, 6, f);
  check(n == 6, "fwrite: returns count");
  fclose(f);

  f = fopen("/tmp/test_pnut_libc.txt", "r");
  check(f != 0, "fopen: read mode");
  if (!f) return;

  n = fread(buf, 1, 6, f);
  check(n == 6, "fread: returns count");
  check(strncmp(buf, "hello\n", 6) == 0, "fread: correct data");

  fseek(f, 0, 0 /* SEEK_SET */);
  check(ftell(f) == 0, "fseek/ftell: seek to start");

  fseek(f, 3, 0 /* SEEK_SET */);
  check(ftell(f) == 3, "fseek: seek to offset 3");

  c = fgetc(f);
  check(c == 'l', "fgetc: reads correct char after seek");

  fclose(f);
  check(remove("/tmp/test_pnut_libc.txt") == 0, "remove: success");

  // Then try to reopen the file for reading, which should fail since it was removed.
  f = fopen("/tmp/test_pnut_libc.txt", "r");
  check(f == 0, "fopen: file should not exist after remove");
}

int main() {
  test_memset();
  test_memcpy();
  test_memmove();
  test_memcmp();
  test_strlen();
  test_strcpy();
  test_strcat();
  test_strchr();
  test_strrchr();
  test_strcmp();
  test_strncmp();
  test_strstr();
  test_memmem();
  test_sprintf();
  test_snprintf();
  test_fprintf();
  test_printf();
  test_fputs_puts();
  test_fdopen();
  test_fflush_fclose();
  test_atoi();
  test_strtol();
  test_strtoul();
  test_strtoll();
  test_strtoull();
  test_getenv();
  test_malloc();
  test_realloc();
  test_qsort();
  test_qsort_structs();
  test_ctype();
  test_file_io();

  if (failed == 0) {
    printf("All tests passed!\n");
  } else {
    printf("FAILED: %d test(s).\n", failed);
  }
  return failed;
}

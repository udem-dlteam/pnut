#include "../include/sys/types.h"

void *memset(void *dest, int c, size_t n) {

  char *s = dest;

  while (n) {
    *s = c;
    ++s;
    --n;
  }

  return dest;
}

void *memcpy(void *dest, const void *src, size_t n) {

  char *d = dest;
  const char *s = src;

  while (n) {
    *d = *s;
    ++d;
    ++s;
    --n;
  }

  return dest;
}

void *memmove(void *dest, const void *src, size_t n) {

  char *d = dest;
  const char *s = src;

  if (d < s) {
    while (n) {
      *d = *s;
      ++d;
      ++s;
      --n;
    }
  } else {
    while (n) {
      --n;
      d[n] = s[n];
    }
  }

  return dest;
}

int memcmp(const void *vl, const void *vr, size_t n) {

  const char *l=vl;
  const char *r=vr;

  while (n && *l == *r) {
    --n;
    ++l;
    ++r;
  }

  return n ? (*l & 255) - (*r & 255) : 0;
}

size_t strlen(const char *s) {

  const char *end = s;

  while (*end) ++end;

  return end - s;
}

char *strcpy(char *dest, const char *src) {

  char *d = dest;
  const char *s = src;

  while ((*d = *s)) {
    ++d;
    ++s;
  }

  return dest;
}

char *strcat(char *dest, const char *src) {
  strcpy(dest + strlen(dest), src);
  return dest;
}

char *strchr(const char *s, int c) {

  while (*s) {
    if (*s == c) return s;
    ++s;
  }

  return c == 0 ? s : 0;
}

char *strrchr(const char *s, int c) {

  char *result = 0;

  while (*s != 0) {
    if (*s == c) result = s;
    ++s;
  }

  return c == 0 ? s : result;
}

int strcmp(const char *l, const char *r) {

  while (*l == *r && *l) {
    ++l;
    ++r;
  }

  return (*l & 255) - (*r & 255);
}

int strncmp(const char *s1, const char *s2, size_t n) {

  while (n && *s1 && *s1 == *s2) {
    ++s1;
    ++s2;
    --n;
  }

  return n ? (*s1 & 255) - (*s2 & 255) : 0;
}

void *memmem(const void* haystack, size_t hl, const void* needle, size_t nl) {
  int i;
  if (nl>hl) return 0;
  for (i=hl-nl+1; i; --i) {
    if (!memcmp(haystack,needle,nl))
      return (char*)haystack;
    ++haystack;
  }
  return 0;
}

char *strstr(char const *haystack, char const *needle)
{
  return memmem(haystack, strlen(haystack), needle, strlen(needle));
}

void *memset(void *b, int c, int len)
{
  char *p = b;
  while (len--) *p++ = c;
  return b;
}

int memcmp(const void *vl, const void *vr, int n) {

  const char *l = vl;
  const char *r = vr;

  while (n && *l == *r) {
    --n;
    ++l;
    ++r;
  }

  return n ? (*l & 255) - (*r & 255) : 0;
}

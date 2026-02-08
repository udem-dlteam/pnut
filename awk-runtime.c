// Produce the AWK runtime library

#define DEFAULT_USE 0

// Bitwise operations

bool runtime_use_and = DEFAULT_USE;
bool runtime_and_defined = false;
void runtime_and() {
  if (runtime_and_defined++) return;
  putstr("function and(a, b,    r, m) {\n");
  putstr("    a = int(a); b = int(b)\n");
  putstr("    if (a < 0) a += 4294967296\n");
  putstr("    if (b < 0) b += 4294967296\n");
  putstr("    r = 0; m = 1\n");
  putstr("    while (a > 0 && b > 0) {\n");
  putstr("        if ((a % 2) == 1 && (b % 2) == 1) r += m\n");
  putstr("        a = int(a / 2); b = int(b / 2)\n");
  putstr("        m *= 2\n");
  putstr("    }\n");
  putstr("    if (r >= 2147483648) r -= 4294967296\n");
  putstr("    return r\n");
  putstr("}\n\n");
}

bool runtime_use_or = DEFAULT_USE;
bool runtime_or_defined = false;
void runtime_or() {
  if (runtime_or_defined++) return;
  putstr("function or(a, b,    r, m) {\n");
  putstr("    a = int(a); b = int(b)\n");
  putstr("    if (a < 0) a += 4294967296\n");
  putstr("    if (b < 0) b += 4294967296\n");
  putstr("    r = 0; m = 1\n");
  putstr("    while (a > 0 || b > 0) {\n");
  putstr("        if ((a % 2) == 1 || (b % 2) == 1) r += m\n");
  putstr("        a = int(a / 2); b = int(b / 2)\n");
  putstr("        m *= 2\n");
  putstr("    }\n");
  putstr("    if (r >= 2147483648) r -= 4294967296\n");
  putstr("    return r\n");
  putstr("}\n\n");
}

bool runtime_use_xor = DEFAULT_USE;
bool runtime_xor_defined = false;
void runtime_xor() {
  if (runtime_xor_defined++) return;
  putstr("function xor(a, b,    r, m) {\n");
  putstr("    a = int(a); b = int(b)\n");
  putstr("    if (a < 0) a += 4294967296\n");
  putstr("    if (b < 0) b += 4294967296\n");
  putstr("    r = 0; m = 1\n");
  putstr("    while (a > 0 || b > 0) {\n");
  putstr("        if ((a % 2) != (b % 2)) r += m\n");
  putstr("        a = int(a / 2); b = int(b / 2)\n");
  putstr("        m *= 2\n");
  putstr("    }\n");
  putstr("    if (r >= 2147483648) r -= 4294967296\n");
  putstr("    return r\n");
  putstr("}\n\n");
}

bool runtime_use_compl = DEFAULT_USE;
bool runtime_compl_defined = false;
void runtime_compl() {
  if (runtime_compl_defined++) return;
  putstr("function compl(a) {\n");
  putstr("    return -int(a) - 1\n");
  putstr("}\n\n");
}

bool runtime_use_lshift = DEFAULT_USE;
bool runtime_lshift_defined = false;
void runtime_lshift() {
  if (runtime_lshift_defined++) return;
  putstr("function lshift(a, b,    r) {\n");
  putstr("    r = int(int(a) * (2 ^ int(b)))\n");
  putstr("    r = r % 4294967296\n");
  putstr("    if (r >= 2147483648) r -= 4294967296\n");
  putstr("    return r\n");
  putstr("}\n\n");
}

bool runtime_use_rshift = DEFAULT_USE;
bool runtime_rshift_defined = false;
void runtime_rshift() {
  if (runtime_rshift_defined++) return;
  putstr("function rshift(a, b,    r, i, m) {\n");
  putstr("    a = int(a); b = int(b)\n");
  putstr("    if (a >= 0) {\n");
  putstr("        r = int(a / (2 ^ b))\n");
  putstr("    } else {\n");
  putstr("        a += 4294967296\n");
  putstr("        r = int(a / (2 ^ b))\n");
  putstr("        # Sign extension\n");
  putstr("        m = 2147483648\n");
  putstr("        for (i = 0; i < b; i++) {\n");
  putstr("            r += m\n");
  putstr("            m = int(m / 2)\n");
  putstr("        }\n");
  putstr("        if (r >= 2147483648) r -= 4294967296\n");
  putstr("    }\n");
  putstr("    return r\n");
  putstr("}\n\n");
}

bool runtime_use_comma = DEFAULT_USE;
bool runtime_comma_defined = false;
void runtime_comma() {
  if (runtime_comma_defined++) return;
  putstr("function comma(v1, v2) {\n");
  putstr("  return v2\n");
  putstr("}\n\n");
}

// memory allocation

bool runtime_use_malloc = DEFAULT_USE;
bool runtime_malloc_defined = false;
void runtime_malloc() {
  if (runtime_malloc_defined++) return;
  putstr("function _malloc(size) {\n");
  putstr("  return (__ALLOC += size) - size;\n");
  putstr("}\n\n");
}

bool runtime_use_free = DEFAULT_USE;
bool runtime_free_defined = false;
void runtime_free() {
  if (runtime_free_defined++) return;
  putstr("function _free(ptr) {\n");
  putstr("  return 0\n");
  putstr("}\n\n");
}

// helpers

bool runtime_use_defstr = DEFAULT_USE;
bool runtime_defstr_defined = false;
void runtime_defstr() {
  if (runtime_defstr_defined++) return;
  runtime_malloc();
  putstr("function unpack_string_to_buf(str, addr,    len, chars, i, c, v) {\n");
  putstr("  len = split(str, chars, \"\")\n");
  putstr("  for (i = 1; i <= len; i++) {\n");
  putstr("    c = chars[i]\n");
  putstr("    if (c == \"\\\\\") {\n");
  putstr("      i++\n");
  putstr("      c = chars[i]\n");
  putstr("      if      (c == \"0\") v = 0\n");
  putstr("      else if (c == \"n\") v = 10\n");
  putstr("      else if (c == \"r\") v = 13\n");
  putstr("      else if (c == \"t\") v = 9\n");
  putstr("      else if (c == \"v\") v = 11\n");
  putstr("      else if (c == \"f\") v = 12\n");
  putstr("      else if (c == \"a\") v = 7\n");
  putstr("      else if (c == \"b\") v = 8\n");
  putstr("      else if (c == \"\\\\\") v = 92\n");
  putstr("      else if (c == \"\\\"\") v = 34\n");
  putstr("      else if (c == \"'\") v = 39\n");
  putstr("      else if (c == \"$\") v = 36\n");
  putstr("      else if (c == \"`\") v = 96\n");
  putstr("      else v = ord[c]\n");
  putstr("    } else {\n");
  putstr("      v = ord[c]\n");
  putstr("    }\n");
  putstr("    _[addr++] = v\n");
  putstr("  }\n");
  putstr("  _[addr] = 0\n");
  putstr("  return addr\n");
  putstr("}\n");
  putstr("\n");
  putstr("function defstr(str,    addr) {\n");
  putstr("  if (str in __str_cache) return __str_cache[str]\n");
  putstr("  addr = _malloc(length(str) + 1)\n");
  putstr("  unpack_string_to_buf(str, addr)\n");
  putstr("  __str_cache[str] = addr\n");
  putstr("  return addr\n");
  putstr("}\n\n");
}

// An implementation of puts, used to replace printf("%s", ...) calls.
bool runtime_use_put_pstr = DEFAULT_USE;
bool runtime_put_pstr_defined = false;
void runtime_put_pstr() {
  if (runtime_put_pstr_defined++) return;
  putstr("function _put_pstr(addr,    c) {\n");
  putstr("  while ((c = _[addr]) != 0) {\n");
  putstr("    printf(\"%c\", c)\n");
  putstr("    addr++\n");
  putstr("  }\n");
  putstr("}\n");
  putstr("\n");
}

// Input / output

bool runtime_use_open = DEFAULT_USE;
bool runtime_open_defined = false;
void runtime_open() {
  if (runtime_open_defined++) return;
  putstr("function get_pstr(addr,    s, c) {\n");
  putstr("  s = \"\"\n");
  putstr("  while ((c = _[addr]) != 0) {\n");
  putstr("    s = s sprintf(\"%c\", c)\n");
  putstr("    addr++\n");
  putstr("  }\n");
  putstr("  return s\n");
  putstr("}\n");
  putstr("\n");
  putstr("function _open(path_ptr, flags, mode,    path, fd) {\n");
  putstr("  path = get_pstr(path_ptr)\n");
  putstr("  fd = __next_fd++\n");
  putstr("  __rt_file[fd] = path\n");
  putstr("  if (flags == 1) { # O_WRONLY\n");
  putstr("    __rt_mode[fd] = \"w\"\n");
  putstr("    printf \"\" > path # Truncate\n");
  putstr("  } else if (flags == 2) { # O_RDWR (not really supported, using append)\n");
  putstr("    __rt_mode[fd] = \"a\"\n");
  putstr("  } else {\n");
  putstr("    __rt_mode[fd] = \"r\"\n");
  putstr("  }\n");
  putstr("  return fd\n");
  putstr("}\n\n");
}

bool runtime_use_close = DEFAULT_USE;
bool runtime_close_defined = false;
void runtime_close() {
  if (runtime_close_defined++) return;
  putstr("function _close(fd) {\n");
  putstr("  if (fd > 2) close(__rt_file[fd])\n");
  putstr("  delete __rt_file[fd]\n");
  putstr("  delete __rt_mode[fd]\n");
  putstr("  delete __fgetc_idx[fd]\n");
  putstr("  delete __fgetc_len[fd]\n");
  putstr("  return 0\n");
  putstr("}\n\n");
}

bool runtime_use_write = DEFAULT_USE;
bool runtime_write_defined = false;
void runtime_write() {
  if (runtime_write_defined++) return;
  putstr("function _write(fd, buf_ptr, count,    path, s, i) {\n");
  putstr("  path = __rt_file[fd]\n");
  putstr("  s = \"\"\n");
  putstr("  for (i = 0; i < count; i++) s = s sprintf(\"%c\", _[buf_ptr + i])\n");
  putstr("  if (fd == 1) printf \"%s\", s\n");
  putstr("  else if (fd == 2) printf \"%s\", s > \"/dev/stderr\"\n");
  putstr("  else printf \"%s\", s >> path\n");
  putstr("  return count\n");
  putstr("}\n\n");
}

bool runtime_use_fgetc = DEFAULT_USE;
bool runtime_fgetc_defined = false;
void runtime_fgetc() {
  if (runtime_fgetc_defined++) return;
  putstr("function _fgetc(fd,    path, status, line, i, len) {\n");
  putstr("  if (!(fd in __fgetc_idx) || __fgetc_idx[fd] > __fgetc_len[fd]) {\n");
  putstr("    path = __rt_file[fd]\n");
  putstr("    if (fd == 0) status = getline line\n");
  putstr("    else status = (getline line < path)\n");
  putstr("    if (status < 0) return -1 # Read error\n");
  putstr("    len = length(line)\n");
  putstr("    for (i = 1; i <= len; i++) __fgetc_buf[fd, i] = ord[substr(line, i, 1)]\n");
  putstr("    __fgetc_buf[fd, len + 1] = status >= 1 ? 10 : -1 # Add newline if we read a line successfully, otherwise mark EOF\n");
  putstr("    __fgetc_len[fd] = len + 1\n");
  putstr("    __fgetc_idx[fd] = 1\n");
  putstr("  }\n");
  putstr("  return __fgetc_buf[fd, __fgetc_idx[fd]++]\n");
  putstr("}\n\n");
}

bool runtime_use_read = DEFAULT_USE;
bool runtime_read_defined = false;
void runtime_read() {
  if (runtime_read_defined++) return;
  runtime_use_fgetc = true; // Use _fgetc to read characters one by one
  putstr("function _read(fd, buf_ptr, count,    c, i) {\n");
  putstr("  for (i = 0; i < count; i++) {\n");
  putstr("    c = _fgetc(fd)\n");
  putstr("    if (c == -1) return i\n");
  putstr("    _[buf_ptr + i] = c\n");
  putstr("  }\n");
  putstr("  return count\n");
  putstr("}\n\n");
}

bool runtime_use_fopen = DEFAULT_USE;
bool runtime_fopen_defined = false;
void runtime_fopen() {
  if (runtime_fopen_defined++) return;
  runtime_open();
  putstr("function _fopen(path_ptr, mode_ptr,    mode_str) {\n");
  putstr("  mode_str = get_pstr(mode_ptr)\n");
  putstr("  return _open(path_ptr, (mode_str == \"w\" ? 1 : 0), 0)\n");
  putstr("}\n\n");
}

bool runtime_use_fclose = DEFAULT_USE;
bool runtime_fclose_defined = false;
void runtime_fclose() {
  if (runtime_fclose_defined++) return;
  runtime_close();
  putstr("function _fclose(fd) {\n");
  putstr("  return _close(fd)\n");
  putstr("}\n\n");
}

#ifdef AWK_INLINE_PUTCHAR

bool runtime_use_putchar = DEFAULT_USE;
bool runtime_putchar_defined = false;
void runtime_putchar() {
  if (runtime_putchar_defined++) return;
  putstr("function _putchar(c) {\n");
  putstr("  printf(\"%c\", c)\n");
  putstr("  return 0\n");
  putstr("}\n\n");
}

#endif // AWK_INLINE_PUTCHAR

// other stubs

bool runtime_use_make_argv = DEFAULT_USE;
bool runtime_make_argv_defined = false;
void runtime_make_argv() {
  if (runtime_make_argv_defined++) return;
  runtime_malloc();
  runtime_defstr();
  putstr("function make_argv(    i, argv_ptr) {\n");
  putstr("  argv_ptr = _malloc(ARGC)\n");
  putstr("  for (i = 0; i < ARGC; i++) {\n");
  putstr("    _[argv_ptr + i] = defstr(ARGV[i])\n");
  putstr("  }\n");
  putstr("  return argv_ptr\n");
  putstr("}\n\n");
}

bool runtime_use_exit = DEFAULT_USE;
bool runtime_exit_defined = false;
void runtime_exit() {
  if (runtime_exit_defined++) return;
  putstr("function _exit(status) {\n");
  putstr("  exit status\n");
  putstr("}\n\n");
}

#ifndef MINIMAL_RUNTIME

bool runtime_use_getchar = DEFAULT_USE;
bool runtime_getchar_defined = false;
void runtime_getchar() {
  if (runtime_getchar_defined++) return;
  runtime_use_fgetc = true;
  putstr("function _getchar() {\n");
  putstr("  return _fgetc(0)\n");
  putstr("}\n\n");
}

#endif

bool runtime_use_isatty = DEFAULT_USE;
bool runtime_isatty_defined = false;
void runtime_isatty() {
  if (runtime_isatty_defined++) return;
  putstr("function _isatty(fd) {\n");
  putstr("  return 0\n");
  putstr("}\n\n");
}

void produce_runtime() {
  if (runtime_use_and)                  runtime_and();
  if (runtime_use_or)                   runtime_or();
  if (runtime_use_xor)                  runtime_xor();
  if (runtime_use_compl)                runtime_compl();
  if (runtime_use_lshift)               runtime_lshift();
  if (runtime_use_rshift)               runtime_rshift();
  if (runtime_use_comma)                runtime_comma();

  if (runtime_use_malloc)               runtime_malloc();
  if (runtime_use_free)                 runtime_free();
  if (runtime_use_defstr)               runtime_defstr();
  if (runtime_use_put_pstr)             runtime_put_pstr();
  if (runtime_use_open)                 runtime_open();
  if (runtime_use_close)                runtime_close();
  if (runtime_use_read)                 runtime_read();
  if (runtime_use_write)                runtime_write();
  if (runtime_use_fopen)                runtime_fopen();
  if (runtime_use_fclose)               runtime_fclose();
  if (runtime_use_fgetc)                runtime_fgetc();
  if (runtime_use_make_argv)            runtime_make_argv();

#ifdef AWK_INLINE_PUTCHAR
  if (runtime_use_putchar)              runtime_putchar();
#endif
#ifdef AWK_INLINE_EXIT
  if (runtime_use_exit)                 runtime_exit();
#endif

#ifndef MINIMAL_RUNTIME
  if (runtime_use_getchar)              runtime_getchar();
#endif
#if !defined(MINIMAL_RUNTIME) || defined(SUPPORT_STDIN_INPUT)
  if (runtime_use_isatty)               runtime_isatty();
#endif
}

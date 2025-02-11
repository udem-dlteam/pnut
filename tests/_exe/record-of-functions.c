#include <stdio.h>
#include <stdlib.h>

#define EOF (-1)
#ifndef NULL
#define NULL (0)
#endif

void putstr(char *s) {
  while (*s) {
    putchar(*s);
    s += 1;
  }
}

// Record that holds two functions for input and output
// Used to parameterize generic input/output functions
struct IOFunctions {
  char (*in_char)(void *);
  void (*out_char)(void *, char);
  void *param; // Parameter passed to in_char and out_char
};

struct IOBuffer {
  char *buf;
  int len;
  int pos;
};

// State for stubbed input/output functions
struct StubbedIO {
  struct IOBuffer input;
  struct IOBuffer output;
};

// Default putchar/getchar functions, ignores the extra parameter
void putchar_(void *param, char c) { putchar(c); }
char getchar_(void *param) { return getchar(); }

// Stubbed putchar/getchar functions
// Their extra param is a StubbedIO struct that's used to keep state between calls.
void putchar_stub(struct StubbedIO *stubData, char c) {
  struct IOBuffer *buf = &stubData->output;
  if (buf->pos < buf->len) {
    buf->buf[buf->pos] = c;
    buf->pos += 1;
  } else {
    putstr("Output buffer overflow\n");
    exit(1);
  }
}

char getchar_stub(struct StubbedIO *stubData) {
  struct IOBuffer *buf = &stubData->input;
  if (buf->pos >= buf->len) return EOF;

  buf->pos += 1;
  return buf->buf[buf->pos - 1];
}

// Parameterized input/output functions, using the IOFunctions record.
int in_string(struct IOFunctions *record, char delim, char *buf, int len) {
  int i = 0;
  char c;
  while (i < len) {
    c = record->in_char(record->param);
    if (c == EOF || c == delim) break;

    buf[i] = c;
    i += 1;
  }

  return i;
}

void out_string(struct IOFunctions *record, char *s) {
  while (*s) {
    record->out_char(record->param, *s);
    s += 1;
  }
}

struct IOFunctions stdin_stdout = { &getchar_, &putchar_, NULL };

struct IOFunctions *stub_io(char *input, int input_buf_len, int output_buf_len) {
  struct IOFunctions *record = malloc(sizeof(struct IOFunctions));
  struct StubbedIO *stubData = malloc(sizeof(struct StubbedIO));
  record->in_char = (char (*)(void *)) &getchar_stub;
  record->out_char = (void (*)(void *, char)) &putchar_stub;
  record->param = stubData;

  stubData->input.buf = input;
  stubData->input.len = input_buf_len;
  stubData->input.pos = 0;

  stubData->output.buf = malloc(output_buf_len);
  stubData->output.len = output_buf_len;
  stubData->output.pos = 0;

  return record;
}

int main() {
  // Testing default output, but not input since it requires user interaction
  char in_buf[100];
  out_string(&stdin_stdout, "hello\n");

  // Testing stubbed input/output, no user interaction required
  struct IOFunctions *stubbedIO = stub_io("WOOO WEEE\n", 14, 10);
  out_string(stubbedIO, "WEEE WOOO\n");
  in_string(stubbedIO, '\0', in_buf, 100);
  putstr(((struct StubbedIO *)stubbedIO->param)->output.buf); // WEEE WOOO
  putstr(in_buf);                                             // WOOO WEEE
  return 0;
}

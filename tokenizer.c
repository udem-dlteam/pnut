#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;

#endif


#define AVOID_AMPAMP_BARBAR
#define USE_IN_RANGE_FUNCTION_not

#ifdef AVOID_AMPAMP_BARBAR
#define AND &
#define OR |
#else
#define AND &&
#define OR ||
#endif

#ifdef USE_IN_RANGE_FUNCTION
int in_range(int x, int lo, int hi) {
  if (x < lo) return 0;
  if (x > hi) return 0;
  return 1;
}
#else
#define in_range(x, lo, hi) ((x >= lo) AND (x <= hi))
#endif


int NEWLINE = 10;
int SPACE = 32;
int SHARP = 35;
int DIGIT_0 = 48;
int DIGIT_9 = 57;
int UPPER_A = 65;
int UPPER_Z = 90;
int LOWER_A = 97;
int LOWER_Z = 122;
int UNDERSCORE = 95;

int END_OF_FILE = 256;
int INTEGER = 257;
int IDENTIFIER = 258;
int OTHER = 259;

void fatal_error(char_ptr msg) {
  printf("%s\n", msg);
  exit(1);
}

/* tokenizer */

void handle_preprocessor_directive() {
  return;
}

int ch;
int token;
int value;

void get_ch() {

  ch = getchar();
  if (ch == EOF) ch = END_OF_FILE;

}

void get_identifier() {

  while (in_range(ch, UPPER_A, UPPER_Z) OR
         in_range(ch, LOWER_A, LOWER_Z) OR
         in_range(ch, DIGIT_0, DIGIT_9) OR
         (ch == UNDERSCORE)) {
    get_ch();
  }

  /* TODO: accumulate in a string pool, check for keywords */
}

void get_token() {

  value = 0; /* value of token */

  while (1) {

    token = ch;
    get_ch();

    if (token <= SPACE) {

      /* skip whitespace, detecting when it is at start of line */

      while (ch <= SPACE) {
        if (ch == NEWLINE) token = ch;
        get_ch();
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((token == NEWLINE) AND (ch == SHARP))
        handle_preprocessor_directive();

    } else if (in_range(token, DIGIT_0, DIGIT_9)) {

      value = token - DIGIT_0;

      token = INTEGER;

      while (in_range(ch, DIGIT_0, DIGIT_9)) {
        value = value * 10 + (ch - DIGIT_0);
        get_ch();
      }

      break;

    } else if (in_range(token, UPPER_A, UPPER_Z) OR
               in_range(token, LOWER_A, LOWER_Z) OR
               (token == UNDERSCORE)) {

      get_identifier();

      token = IDENTIFIER;

      break;

    } else if (token == END_OF_FILE) {

      break;

    } else {

      token = OTHER;

      break;

    }
  }
}

int main() {

  get_ch();
  get_token();

  while (token != END_OF_FILE) {
    /*
    if (token == IDENTIFIER)
      printf("IDENTIFIER\n");
    else if (token == INTEGER)
      printf("INTEGER\n");
    else
      printf("OTHER\n");
    */
    get_token();
  }

  return 0;
}

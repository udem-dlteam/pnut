#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;

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
  /* TODO */
}

int ch;
int token;
int value;

void get_ch() {

  ch = getchar();
  if (ch == EOF) ch = END_OF_FILE;

}

void get_identifier() {

  while ((ch >= UPPER_A && ch <= UPPER_Z) ||
         (ch >= LOWER_A && ch <= LOWER_Z) ||
         (ch >= DIGIT_0 && ch <= DIGIT_9) ||
         ch == UNDERSCORE) {
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

      if (token == NEWLINE && ch == SHARP)
        handle_preprocessor_directive();

    } else if (token >= DIGIT_0 && token <= DIGIT_9) {

      value = token - DIGIT_0;

      token = INTEGER;

      while (ch >= DIGIT_0 && ch <= DIGIT_9) {
        value = value * 10 + (ch - DIGIT_0);
        get_ch();
      }

      break;

    } else if ((token >= UPPER_A && token <= UPPER_Z) ||
               (token >= LOWER_A && token <= LOWER_Z) ||
               token == UNDERSCORE) {

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

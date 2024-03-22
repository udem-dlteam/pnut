#ifndef SIX_CC

#include <stdio.h>
#include <stdlib.h>
typedef FILE *FILE_ptr;
typedef int *int_ptr;
typedef char *char_ptr;

#endif


#define AVOID_AMPAMP_BARBAR_not
#define USE_IN_RANGE_FUNCTION_not
#define INLINE_get_ch_not

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

int INTEGER = 256;
int IDENTIFIER = 257;
int OTHER = 258;

void fatal_error(char_ptr msg) {
  printf("%s\n", msg);
  exit(1);
}

/* tokenizer */

void handle_preprocessor_directive() {
  return;
}

int ch;
int tok;
int val;

#ifdef INLINE_get_ch

#define get_ch() ch = getchar()

#else

void get_ch() {
  ch = getchar();
}

#endif

void get_identifier() {

  while (in_range(ch, UPPER_A, UPPER_Z) OR
         in_range(ch, LOWER_A, LOWER_Z) OR
         in_range(ch, DIGIT_0, DIGIT_9) OR
         (ch == UNDERSCORE)) {
    get_ch();
  }

  /* TODO: accumulate in a string pool, check for keywords */
}

void get_tok() {

  val = 0; /* value of token */

  while (1) {

    if (ch <= SPACE) {

      if (ch == EOF) {
        tok = EOF;
        return;
      }

      /* skip whitespace, detecting when it is at start of line */

      while (ch <= SPACE) {
        if (ch == NEWLINE) tok = ch;
        get_ch();
        if (ch == EOF) break;
      }

      /* detect '#' at start of line, possibly preceded by whitespace */

      if ((tok == NEWLINE) AND (ch == SHARP))
        handle_preprocessor_directive();

      tok = OTHER;

    } else {

      tok = ch;
      get_ch();

      switch (tok) {

      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
      case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
      case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
      case 'V': case 'W': case 'X': case 'Y': case 'Z':
      case '_':
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
      case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
      case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
      case 'v': case 'w': case 'x': case 'y': case 'z':

        get_identifier();

        tok = IDENTIFIER;

        return;

      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':

        val = tok - DIGIT_0;

        tok = INTEGER;

        while (in_range(ch, DIGIT_0, DIGIT_9)) {
          val = val * 10 + (ch - DIGIT_0);
          get_ch();
        }

        return;

      default:

        tok = OTHER;

        return;
      }
    }
  }
}

int main() {

  ch = NEWLINE;
  get_tok();

  while (tok != EOF) {
    /*
    if (tok == IDENTIFIER)
      printf("IDENTIFIER\n");
    else if (tok == INTEGER)
      printf("INTEGER\n");
    else
      printf("OTHER\n");
    */
    get_tok();
  }

  return 0;
}

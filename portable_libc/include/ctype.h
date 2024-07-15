#ifndef _CTYPE_H
#define _CTYPE_H

int isdigit(int c);
int isxdigit(int c);
int isnumber(int c, int base);
int islower(int c);
int isupper(int c);
int isalpha(int c);
int isalnum(int c);
int tolower(int c);
int toupper(int c);
int isascii(int c);
int iscntrl(int c);
int isgraph(int c);
int isprint(int c);
int isspace(int c);
int ispunct(int c);

#endif

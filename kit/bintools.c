/*
 * bintools.c: A bootstrap utility for various binary tools.
 *
 * Supported commands:
 *  cp: Copy files
 *  cat: Concatenate files
 *  mkdir: Create directories
 *  sha256sum: Compute SHA256 checksums
 *  simple-patch: Apply simple patches
 *  ungz: Uncompress .gz files
 *  untar: Extract .tar files
 */

#include <string.h>

// Add includes for all subcommands so process-includes.sh puts them at the top
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>

#define ENTRY_POINT cat_main
/*
 * cat.c: Output the contents of files passed as arguments or stdin
 *
 * Usage: ./cat.sh <files>
 *        ./cat.sh < input
 */


#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#define BUF_SIZE 1024

void cat_fd(int fd) {
  char buf[BUF_SIZE];
  int n = BUF_SIZE;
  while (n == BUF_SIZE) {
    n = read(fd, buf, BUF_SIZE);
    if (n < 0 || write(1, buf, n) != n) exit(1);
  }
}

void cat_file(char *filename) {
  int fd = open(filename, 0);
  if (fd < 0) exit(1);
  cat_fd(fd);
  close(fd);
}

int ENTRY_POINT(int argc, char **argv) {

  int i;

  if (argc >= 2) {
    for (i = 1; i < argc; ++i) {
      if (argv[i][0] == '-' && argv[i][1] == '\0') {
        cat_fd(0);
      } else {
        cat_file(argv[i]);
      }
    }
  } else {
    cat_fd(0);
  }

  return 0;
}
#undef ENTRY_POINT
#define ENTRY_POINT cp_main
/*
 * cp.c: Copy the contents of one file to another
 *
 * Warning: This is a very minimal implementation of the 'cp' command.
 * It does __not__ propagate file permissions.
 *
 * Usage: ./cp.sh <source> <destination>
 */


#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

void file_error(char *filename) {
  printf("cp: %s: no such file or directory\n", filename);
  exit(1);
}

#define BUF_SIZE 1024

int ENTRY_POINT(int argc, char **argv) {
  int src, dst;
  int len;
  char buffer[BUF_SIZE];

  if (argc != 3) {
    printf("Usage: cp <source> <destination>\n");
    return 1;
  }

  // TODO: Propagate permissions from source file
  src = open(argv[1], O_RDONLY);
  dst = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);

  if (src == -1) { file_error(argv[1]); }
  if (dst == -1) { file_error(argv[2]); }

  while ((len = read(src, buffer, BUF_SIZE)) != 0) {
    write(dst, buffer, len);
  }
}
#undef ENTRY_POINT
#define ENTRY_POINT mkdir_main
/*
 * mkdir.c: Create directories
 *
 * Usage: ./mkdir.sh [-p|--parents] <directory>...
 *
 * Known issues: Blows up if the directory already exists.
 *
 * Options:
 *   -p, --parents   Create parent directories as needed
 *
 */

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif


void create_dir_recursive(int create_parent_dirs, char *pathname, int mode) {
	int res;

	// Try creating the directory, if it fails, we'll try to create the parent
  // directories and then retry. This is not guaranteed to succeed since it
  // could be failing for other reasons.
	res = mkdir(pathname, mode);

	if(res != 0 && create_parent_dirs) {
    // Find the last slash in the pathname, then temporarily remove the last
    // part of the pathname while creating the parent directories.
    // If there are no slashes in the path, then we are already at the root
    // directory and something wrong happened.
    char *parent_dirs_path = strrchr(pathname, '/');

    if (parent_dirs_path) {
      parent_dirs_path[0] = '\0';
      create_dir_recursive(create_parent_dirs, pathname, mode);

      // Restore the original pathname and try creating the directory again.
      parent_dirs_path[0] = '/';
      res = mkdir(pathname, mode);
    } else {
      printf("Could not create directory %s. It probably already exists.\n", pathname);
      exit(1);
    }
	}

	if(res != 0) {
		printf("Could not create directory %s. It probably already exists.\n", pathname);
    exit(1);
	}
}

int ENTRY_POINT(int argc, char **argv) {
	int create_parent_dirs = 0;

  while (argc > 1) {
    if (strcmp(argv[1], "-p") == 0 || strcmp(argv[1], "--parents") == 0) {
			create_parent_dirs = 1;
      argv += 1;
      argc -= 1;
		}
		else {
      // Strip trailing '/' if present
      if(argv[1][strlen(argv[1]) - 1] == '/') {
        argv[1][strlen(argv[1]) - 1] = '\0';
      }
      create_dir_recursive(create_parent_dirs, argv[1], 0755);
      argv += 1;
      argc -= 1;
    }
	}

	return 0;
}
#undef ENTRY_POINT
#define ENTRY_POINT sha256sum_main
/*
 * sha256sum.c: Compute the SHA-256 hash of files passed as arguments
 *
 * Usage: ./sha256sum.sh <files>
 */


#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#define BLOCK_SIZE 64

int k[64];

void sha256_setup() {
  k[ 0] = 0x428a2f98;
  k[ 1] = 0x71374491;
  k[ 2] = 0xb5c0fbcf;
  k[ 3] = 0xe9b5dba5;
  k[ 4] = 0x3956c25b;
  k[ 5] = 0x59f111f1;
  k[ 6] = 0x923f82a4;
  k[ 7] = 0xab1c5ed5;
  k[ 8] = 0xd807aa98;
  k[ 9] = 0x12835b01;
  k[10] = 0x243185be;
  k[11] = 0x550c7dc3;
  k[12] = 0x72be5d74;
  k[13] = 0x80deb1fe;
  k[14] = 0x9bdc06a7;
  k[15] = 0xc19bf174;
  k[16] = 0xe49b69c1;
  k[17] = 0xefbe4786;
  k[18] = 0x0fc19dc6;
  k[19] = 0x240ca1cc;
  k[20] = 0x2de92c6f;
  k[21] = 0x4a7484aa;
  k[22] = 0x5cb0a9dc;
  k[23] = 0x76f988da;
  k[24] = 0x983e5152;
  k[25] = 0xa831c66d;
  k[26] = 0xb00327c8;
  k[27] = 0xbf597fc7;
  k[28] = 0xc6e00bf3;
  k[29] = 0xd5a79147;
  k[30] = 0x06ca6351;
  k[31] = 0x14292967;
  k[32] = 0x27b70a85;
  k[33] = 0x2e1b2138;
  k[34] = 0x4d2c6dfc;
  k[35] = 0x53380d13;
  k[36] = 0x650a7354;
  k[37] = 0x766a0abb;
  k[38] = 0x81c2c92e;
  k[39] = 0x92722c85;
  k[40] = 0xa2bfe8a1;
  k[41] = 0xa81a664b;
  k[42] = 0xc24b8b70;
  k[43] = 0xc76c51a3;
  k[44] = 0xd192e819;
  k[45] = 0xd6990624;
  k[46] = 0xf40e3585;
  k[47] = 0x106aa070;
  k[48] = 0x19a4c116;
  k[49] = 0x1e376c08;
  k[50] = 0x2748774c;
  k[51] = 0x34b0bcb5;
  k[52] = 0x391c0cb3;
  k[53] = 0x4ed8aa4a;
  k[54] = 0x5b9cca4f;
  k[55] = 0x682e6ff3;
  k[56] = 0x748f82ee;
  k[57] = 0x78a5636f;
  k[58] = 0x84c87814;
  k[59] = 0x8cc70208;
  k[60] = 0x90befffa;
  k[61] = 0xa4506ceb;
  k[62] = 0xbef9a3f7;
  k[63] = 0xc67178f2;
}

#define u32mask 0xffffffff
#define u31mask 0x7fffffff
#define u29mask 0x1fffffff
#define u22mask 0x003fffff

#define rot32(n, shift) (((n >> shift) & (u31mask >> (shift-1))) | ((n << (32 - shift)) & u32mask))

int w[64];
int nbits;
int hash[8];
int temp[8];

void sha256_init() {
  nbits = 0;
  hash[0] = 0x6a09e667;
  hash[1] = 0xbb67ae85;
  hash[2] = 0x3c6ef372;
  hash[3] = 0xa54ff53a;
  hash[4] = 0x510e527f;
  hash[5] = 0x9b05688c;
  hash[6] = 0x1f83d9ab;
  hash[7] = 0x5be0cd19;
}

void sha256_add_block(char *bytes) {

  int b0;
  int b1;
  int b2;
  int b3;
  int s0;
  int s1;
  int i;
  int ch;
  int t1;
  int ma;
  int t2;

  for (i=0; i<16; ++i) {
    b0 = 0xff & bytes[i*4];
    b1 = 0xff & bytes[i*4+1];
    b2 = 0xff & bytes[i*4+2];
    b3 = 0xff & bytes[i*4+3];
    w[i] = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
  }

  for (i=16; i<64; ++i) {
    s0 = rot32(w[i-15], 7) ^ rot32(w[i-15], 18) ^ ((w[i-15] >> 3) & u29mask);
    s1 = rot32(w[i-2], 17) ^ rot32(w[i-2], 19) ^ ((w[i-2] >> 10) & u22mask);
    w[i] = (w[i-16] + s0 + w[i-7] + s1) & u32mask;
  }

  for (i=0; i<8; ++i) temp[i] = hash[i];

  for (i=0; i<64; ++i) {

    s1 = rot32(temp[4], 6) ^ rot32(temp[4], 11) ^ rot32(temp[4], 25);
    ch = (temp[4] & temp[5]) ^ (~temp[4] & temp[6]);
    t1 = (temp[7] + s1 + ch + k[i] + w[i]) & u32mask;
    s0 = rot32(temp[0], 2) ^ rot32(temp[0], 13) ^ rot32(temp[0], 22);
    ma = (temp[0] & temp[1]) ^ (temp[0] & temp[2]) ^ (temp[1] & temp[2]);
    t2 = (s0 + ma) & u32mask;

    temp[7] = temp[6];
    temp[6] = temp[5];
    temp[5] = temp[4];
    temp[4] = (temp[3] + t1) & u32mask;
    temp[3] = temp[2];
    temp[2] = temp[1];
    temp[1] = temp[0];
    temp[0] = (t1 + t2) & u32mask;
  }

  for (i=0; i<8; ++i) hash[i] = (hash[i] + temp[i]) & u32mask;
}

void hex(int byte) {
  char *digits = "0123456789abcdef";
  putchar(digits[0xf & (byte >> 4)]);
  putchar(digits[0xf & byte]);
}

int process_file(char *filename) {

  int i;
  int fd;
  int n = BLOCK_SIZE;
  int h;
  char buf[BLOCK_SIZE];

  sha256_setup();
  sha256_init();

  fd = open(filename, 0);

  while (n == BLOCK_SIZE) {

    n = read(fd, buf, BLOCK_SIZE);

    if (n < 0) return 1;

    nbits += 8*n;

    if (n < BLOCK_SIZE) {

      buf[n] = 0x80;

      for (i=n+1; i<BLOCK_SIZE; ++i) buf[i] = 0;

      if (n >= BLOCK_SIZE-9) {
        sha256_add_block(buf);
        for (i=0; i<BLOCK_SIZE-8; ++i) buf[i] = 0;
      }

      for (i=1; i<=8; ++i) {
        buf[BLOCK_SIZE-i] = 0xff & nbits;
        nbits >>= 8;
      }
    }

    sha256_add_block(buf);
  }

  close(fd);

  for (i=0; i<8; ++i) {
    h = hash[i];
    hex(h >> 24);
    hex(h >> 16);
    hex(h >> 8);
    hex(h);
  }

  putchar(' ');
  putchar(' ');

  while (*filename) {
    putchar(*filename);
    ++filename;
  }

  putchar('\n');

  return 0;
}

int ENTRY_POINT(int argc, char **myargv) {

  int i;

  for (i = 1; i < argc; ++i) {
    if (process_file(myargv[i]) != 0) break;
  }

  return 0;
}
#undef ENTRY_POINT
#define ENTRY_POINT simple_patch_main
/*
 * simple-patch.c
 * A simple patch utility that replaces a specific string in a file with another string.
 * Usage: ./simple-patch file before-patch after-patch
 */


#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#define ASSERT(x, msg) \
  if (!(x)) { \
    fprintf(stderr, msg "\n"); \
    exit(1); \
  }

char *read_file(char *filename) {
  // Open file for reading
  int fd = open(filename, O_RDONLY);
  ASSERT(fd >= 0, "Error opening file");

  // Allocate buffer for file content
  int size = lseek(fd, 0, SEEK_END); // Get file size
  char *content = malloc(size + 1); // Allocate memory for file content and \0
  ASSERT(content != NULL, "Error allocating memory for file content");

  // Read file content
  lseek(fd, 0, SEEK_SET); // Reset file pointer to the beginning
  ASSERT(read(fd, content, size) == size, "Error reading file content");
  content[size] = '\0'; // Null-terminate the string

  // Close file descriptor
  close(fd);
  return content;
}

// Usage: simple-patch file before-patch after-patch
int ENTRY_POINT(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "Usage: %s file before-patch after-patch\n", argv[0]);
    return 1;
  }

  char *file_content = read_file(argv[1]);
  int file_len = strlen(file_content);

  char *patch_before = read_file(argv[2]);
  int before_patch_len = strlen(patch_before);

  char *patch_after = read_file(argv[3]);
  int after_patch_len = strlen(patch_after);

  // Find the before patch string in the file content
  char *pos = strstr(file_content, patch_before);
  ASSERT(pos != NULL, "Error finding patch string");
  int pos_index = pos - file_content;

  int fd_output = open(argv[1], O_WRONLY | O_TRUNC); // Open the output file, truncating it
  ASSERT(fd_output >= 0, "Error opening after patch file");

  // Create a new file content with the after patch
  write(fd_output, file_content, pos_index); // Write before patch
  write(fd_output, patch_after, after_patch_len); // Write after patch
  write(fd_output, file_content + pos_index + before_patch_len, file_len - pos_index - before_patch_len); // Write the rest of the file

  // Clean up
  free(file_content);
  free(patch_before);
  free(patch_after);
  close(fd_output);

  return 0;
}
#undef ENTRY_POINT
#define ENTRY_POINT ungz_main
/*
 * ungz.c
 * A simple GZIP decompressor that reads from a file or stdin and writes to a file or stdout.
 * Usage: ./ungz --file input.gz --output output.txt
 */


/*
 * puff.c
 * Copyright (C) 2002-2013 Mark Adler
 * For conditions of distribution and use, see copyright notice in puff.h
 * version 2.3, 21 Jan 2013
 *
 * puff.c is a simple inflate written to be an unambiguous way to specify the
 * deflate format.  It is not written for speed but rather simplicity.  As a
 * side benefit, this code might actually be useful when small code is more
 * important than speed, such as bootstrap applications.  For typical deflate
 * data, zlib's inflate() is about four times as fast as puff().  zlib's
 * inflate compiles to around 20K on my machine, whereas puff.c compiles to
 * around 4K on my machine (a PowerPC using GNU cc).  If the faster decode()
 * function here is used, then puff() is only twice as slow as zlib's
 * inflate().
 *
 * All dynamically allocated memory comes from the stack.  The stack required
 * is less than 2K bytes.  This code is compatible with 16-bit int's and
 * assumes that long's are at least 32 bits.  puff.c uses the short data type,
 * assumed to be 16 bits, for arrays in order to conserve memory.  The code
 * works whether integers are stored big endian or little endian.
 *
 * In the comments below are "Format notes" that describe the inflate process
 * and document some of the less obvious aspects of the format.  This source
 * code is meant to supplement RFC 1951, which formally describes the deflate
 * format:
 *
 *    http://www.zlib.org/rfc-deflate.html
 */

/*
 * Change history:
 *
 * 1.0  10 Feb 2002     - First version
 * 1.1  17 Feb 2002     - Clarifications of some comments and notes
 *                      - Update puff() dest and source pointers on negative
 *                        errors to facilitate debugging deflators
 *                      - Remove longest from struct huffman -- not needed
 *                      - Simplify offs[] index in construct()
 *                      - Add input size and checking, using longjmp() to
 *                        maintain easy readability
 *                      - Use short data type for large arrays
 *                      - Use pointers instead of long to specify source and
 *                        destination sizes to avoid arbitrary 4 GB limits
 * 1.2  17 Mar 2002     - Add faster version of decode(), doubles speed (!),
 *                        but leave simple version for readabilty
 *                      - Make sure invalid distances detected if pointers
 *                        are 16 bits
 *                      - Fix fixed codes table error
 *                      - Provide a scanning mode for determining size of
 *                        uncompressed data
 * 1.3  20 Mar 2002     - Go back to lengths for puff() parameters [Gailly]
 *                      - Add a puff.h file for the interface
 *                      - Add braces in puff() for else do [Gailly]
 *                      - Use indexes instead of pointers for readability
 * 1.4  31 Mar 2002     - Simplify construct() code set check
 *                      - Fix some comments
 *                      - Add FIXLCODES #define
 * 1.5   6 Apr 2002     - Minor comment fixes
 * 1.6   7 Aug 2002     - Minor format changes
 * 1.7   3 Mar 2003     - Added test code for distribution
 *                      - Added zlib-like license
 * 1.8   9 Jan 2004     - Added some comments on no distance codes case
 * 1.9  21 Feb 2008     - Fix bug on 16-bit integer architectures [Pohland]
 *                      - Catch missing end-of-block symbol error
 * 2.0  25 Jul 2008     - Add #define to permit distance too far back
 *                      - Add option in TEST code for puff to write the data
 *                      - Add option in TEST code to skip input bytes
 *                      - Allow TEST code to read from piped stdin
 * 2.1   4 Apr 2010     - Avoid variable initialization for happier compilers
 *                      - Avoid unsigned comparisons for even happier compilers
 * 2.2  25 Apr 2010     - Fix bug in variable initializations [Oberhumer]
 *                      - Add const where appropriate [Oberhumer]
 *                      - Split if's and ?'s for coverage testing
 *                      - Break out test code to separate file
 *                      - Move NIL to puff.h
 *                      - Allow incomplete code only if single code length is 1
 *                      - Add full code coverage test to Makefile
 * 2.3  21 Jan 2013     - Check for invalid code length codes in dynamic blocks
 */

/* puff.h
  Copyright (C) 2002-2013 Mark Adler, all rights reserved
  version 2.3, 21 Jan 2013

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Mark Adler    madler@alumni.caltech.edu
 */


/*
 * See puff.c for purpose and usage.
 */
#ifndef NIL
#  define NIL ((unsigned char *)0)      /* for no output option */
#endif

int puff(unsigned char *dest,           /* pointer to destination pointer */
         unsigned long *destlen,        /* amount of output space */
         const unsigned char *source,   /* pointer to source data pointer */
         unsigned long *sourcelen);     /* amount of input available */

#define local static            /* for local function definitions */

/*
 * Maximums for allocations and loops.  It is not useful to change these --
 * they are fixed by the deflate format.
 */
#define MAXBITS 15              /* maximum bits in a code */
#define MAXLCODES 286           /* maximum number of literal/length codes */
#define MAXDCODES 30            /* maximum number of distance codes */
#define MAXCODES (MAXLCODES+MAXDCODES)  /* maximum codes lengths to read */
#define FIXLCODES 288           /* number of fixed literal/length codes */

/* input and output state */
struct state {
    /* output state */
    unsigned char *out;         /* output buffer */
    unsigned long outlen;       /* available space at out */
    unsigned long outcnt;       /* bytes written to out so far */

    /* input state */
    const unsigned char *in;    /* input buffer */
    unsigned long inlen;        /* available input at in */
    unsigned long incnt;        /* bytes read so far */
    int bitbuf;                 /* bit buffer */
    int bitcnt;                 /* number of bits in bit buffer */

    /* input limit error return state for bits() and decode() */
    jmp_buf env;
};

/*
 * Return need bits from the input stream.  This always leaves less than
 * eight bits in the buffer.  bits() works properly for need == 0.
 *
 * Format notes:
 *
 * - Bits are stored in bytes from the least significant bit to the most
 *   significant bit.  Therefore bits are dropped from the bottom of the bit
 *   buffer, using shift right, and new bytes are appended to the top of the
 *   bit buffer, using shift left.
 */
local int bits(struct state *s, int need)
{
    long val;           /* bit accumulator (can use up to 20 bits) */

    /* load at least need bits into val */
    val = s->bitbuf;
    while (s->bitcnt < need) {
        if (s->incnt == s->inlen)
            longjmp(s->env, 1);         /* out of input */
        val |= (long)(s->in[s->incnt++]) << s->bitcnt;  /* load eight bits */
        s->bitcnt += 8;
    }

    /* drop need bits and update buffer, always zero to seven bits left */
    s->bitbuf = (int)(val >> need);
    s->bitcnt -= need;

    /* return need bits, zeroing the bits above that */
    return (int)(val & ((1L << need) - 1));
}

/*
 * Process a stored block.
 *
 * Format notes:
 *
 * - After the two-bit stored block type (00), the stored block length and
 *   stored bytes are byte-aligned for fast copying.  Therefore any leftover
 *   bits in the byte that has the last bit of the type, as many as seven, are
 *   discarded.  The value of the discarded bits are not defined and should not
 *   be checked against any expectation.
 *
 * - The second inverted copy of the stored block length does not have to be
 *   checked, but it's probably a good idea to do so anyway.
 *
 * - A stored block can have zero length.  This is sometimes used to byte-align
 *   subsets of the compressed data for random access or partial recovery.
 */
local int stored(struct state *s)
{
    unsigned len;       /* length of stored block */

    /* discard leftover bits from current byte (assumes s->bitcnt < 8) */
    s->bitbuf = 0;
    s->bitcnt = 0;

    /* get length and check against its one's complement */
    if (s->incnt + 4 > s->inlen)
        return 2;                               /* not enough input */
    len = s->in[s->incnt++];
    len |= s->in[s->incnt++] << 8;
    if (s->in[s->incnt++] != (~len & 0xff) ||
        s->in[s->incnt++] != ((~len >> 8) & 0xff))
        return -2;                              /* didn't match complement! */

    /* copy len bytes from in to out */
    if (s->incnt + len > s->inlen)
        return 2;                               /* not enough input */
    if (s->out != NIL) {
        if (s->outcnt + len > s->outlen)
            return 1;                           /* not enough output space */
        while (len--)
            s->out[s->outcnt++] = s->in[s->incnt++];
    }
    else {                                      /* just scanning */
        s->outcnt += len;
        s->incnt += len;
    }

    /* done with a valid stored block */
    return 0;
}

/*
 * Huffman code decoding tables.  count[1..MAXBITS] is the number of symbols of
 * each length, which for a canonical code are stepped through in order.
 * symbol[] are the symbol values in canonical order, where the number of
 * entries is the sum of the counts in count[].  The decoding process can be
 * seen in the function decode() below.
 */
struct huffman {
    short *count;       /* number of symbols of each length */
    short *symbol;      /* canonically ordered symbols */
};

/*
 * Decode a code from the stream s using huffman table h.  Return the symbol or
 * a negative value if there is an error.  If all of the lengths are zero, i.e.
 * an empty code, or if the code is incomplete and an invalid code is received,
 * then -10 is returned after reading MAXBITS bits.
 *
 * Format notes:
 *
 * - The codes as stored in the compressed data are bit-reversed relative to
 *   a simple integer ordering of codes of the same lengths.  Hence below the
 *   bits are pulled from the compressed data one at a time and used to
 *   build the code value reversed from what is in the stream in order to
 *   permit simple integer comparisons for decoding.  A table-based decoding
 *   scheme (as used in zlib) does not need to do this reversal.
 *
 * - The first code for the shortest length is all zeros.  Subsequent codes of
 *   the same length are simply integer increments of the previous code.  When
 *   moving up a length, a zero bit is appended to the code.  For a complete
 *   code, the last code of the longest length will be all ones.
 *
 * - Incomplete codes are handled by this decoder, since they are permitted
 *   in the deflate format.  See the format notes for fixed() and dynamic().
 */
#ifdef SLOW
local int decode(struct state *s, const struct huffman *h)
{
    int len;            /* current number of bits in code */
    int code;           /* len bits being decoded */
    int first;          /* first code of length len */
    int count;          /* number of codes of length len */
    int index;          /* index of first code of length len in symbol table */

    code = first = index = 0;
    for (len = 1; len <= MAXBITS; len++) {
        code |= bits(s, 1);             /* get next bit */
        count = h->count[len];
        if (code - count < first)       /* if length len, return symbol */
            return h->symbol[index + (code - first)];
        index += count;                 /* else update for next length */
        first += count;
        first <<= 1;
        code <<= 1;
    }
    return -10;                         /* ran out of codes */
}

/*
 * A faster version of decode() for real applications of this code.   It's not
 * as readable, but it makes puff() twice as fast.  And it only makes the code
 * a few percent larger.
 */
#else /* !SLOW */
local int decode(struct state *s, const struct huffman *h)
{
    int len;            /* current number of bits in code */
    int code;           /* len bits being decoded */
    int first;          /* first code of length len */
    int count;          /* number of codes of length len */
    int index;          /* index of first code of length len in symbol table */
    int bitbuf;         /* bits from stream */
    int left;           /* bits left in next or left to process */
    short *next;        /* next number of codes */

    bitbuf = s->bitbuf;
    left = s->bitcnt;
    code = first = index = 0;
    len = 1;
    next = h->count + 1;
    while (1) {
        while (left--) {
            code |= bitbuf & 1;
            bitbuf >>= 1;
            count = *next++;
            if (code - count < first) { /* if length len, return symbol */
                s->bitbuf = bitbuf;
                s->bitcnt = (s->bitcnt - len) & 7;
                return h->symbol[index + (code - first)];
            }
            index += count;             /* else update for next length */
            first += count;
            first <<= 1;
            code <<= 1;
            len++;
        }
        left = (MAXBITS+1) - len;
        if (left == 0)
            break;
        if (s->incnt == s->inlen)
            longjmp(s->env, 1);         /* out of input */
        bitbuf = s->in[s->incnt++];
        if (left > 8)
            left = 8;
    }
    return -10;                         /* ran out of codes */
}
#endif /* SLOW */

/*
 * Given the list of code lengths length[0..n-1] representing a canonical
 * Huffman code for n symbols, construct the tables required to decode those
 * codes.  Those tables are the number of codes of each length, and the symbols
 * sorted by length, retaining their original order within each length.  The
 * return value is zero for a complete code set, negative for an over-
 * subscribed code set, and positive for an incomplete code set.  The tables
 * can be used if the return value is zero or positive, but they cannot be used
 * if the return value is negative.  If the return value is zero, it is not
 * possible for decode() using that table to return an error--any stream of
 * enough bits will resolve to a symbol.  If the return value is positive, then
 * it is possible for decode() using that table to return an error for received
 * codes past the end of the incomplete lengths.
 *
 * Not used by decode(), but used for error checking, h->count[0] is the number
 * of the n symbols not in the code.  So n - h->count[0] is the number of
 * codes.  This is useful for checking for incomplete codes that have more than
 * one symbol, which is an error in a dynamic block.
 *
 * Assumption: for all i in 0..n-1, 0 <= length[i] <= MAXBITS
 * This is assured by the construction of the length arrays in dynamic() and
 * fixed() and is not verified by construct().
 *
 * Format notes:
 *
 * - Permitted and expected examples of incomplete codes are one of the fixed
 *   codes and any code with a single symbol which in deflate is coded as one
 *   bit instead of zero bits.  See the format notes for fixed() and dynamic().
 *
 * - Within a given code length, the symbols are kept in ascending order for
 *   the code bits definition.
 */
local int construct(struct huffman *h, const short *length, int n)
{
    int symbol;         /* current symbol when stepping through length[] */
    int len;            /* current length when stepping through h->count[] */
    int left;           /* number of possible codes left of current length */
    short offs[MAXBITS+1];      /* offsets in symbol table for each length */

    /* count number of codes of each length */
    for (len = 0; len <= MAXBITS; len++)
        h->count[len] = 0;
    for (symbol = 0; symbol < n; symbol++)
        (h->count[length[symbol]])++;   /* assumes lengths are within bounds */
    if (h->count[0] == n)               /* no codes! */
        return 0;                       /* complete, but decode() will fail */

    /* check for an over-subscribed or incomplete set of lengths */
    left = 1;                           /* one possible code of zero length */
    for (len = 1; len <= MAXBITS; len++) {
        left <<= 1;                     /* one more bit, double codes left */
        left -= h->count[len];          /* deduct count from possible codes */
        if (left < 0)
            return left;                /* over-subscribed--return negative */
    }                                   /* left > 0 means incomplete */

    /* generate offsets into symbol table for each length for sorting */
    offs[1] = 0;
    for (len = 1; len < MAXBITS; len++)
        offs[len + 1] = offs[len] + h->count[len];

    /*
     * put symbols in table sorted by length, by symbol order within each
     * length
     */
    for (symbol = 0; symbol < n; symbol++)
        if (length[symbol] != 0)
            h->symbol[offs[length[symbol]]++] = symbol;

    /* return zero for complete set, positive for incomplete set */
    return left;
}

/*
 * Decode literal/length and distance codes until an end-of-block code.
 *
 * Format notes:
 *
 * - Compressed data that is after the block type if fixed or after the code
 *   description if dynamic is a combination of literals and length/distance
 *   pairs terminated by and end-of-block code.  Literals are simply Huffman
 *   coded bytes.  A length/distance pair is a coded length followed by a
 *   coded distance to represent a string that occurs earlier in the
 *   uncompressed data that occurs again at the current location.
 *
 * - Literals, lengths, and the end-of-block code are combined into a single
 *   code of up to 286 symbols.  They are 256 literals (0..255), 29 length
 *   symbols (257..285), and the end-of-block symbol (256).
 *
 * - There are 256 possible lengths (3..258), and so 29 symbols are not enough
 *   to represent all of those.  Lengths 3..10 and 258 are in fact represented
 *   by just a length symbol.  Lengths 11..257 are represented as a symbol and
 *   some number of extra bits that are added as an integer to the base length
 *   of the length symbol.  The number of extra bits is determined by the base
 *   length symbol.  These are in the static arrays below, lens[] for the base
 *   lengths and lext[] for the corresponding number of extra bits.
 *
 * - The reason that 258 gets its own symbol is that the longest length is used
 *   often in highly redundant files.  Note that 258 can also be coded as the
 *   base value 227 plus the maximum extra value of 31.  While a good deflate
 *   should never do this, it is not an error, and should be decoded properly.
 *
 * - If a length is decoded, including its extra bits if any, then it is
 *   followed a distance code.  There are up to 30 distance symbols.  Again
 *   there are many more possible distances (1..32768), so extra bits are added
 *   to a base value represented by the symbol.  The distances 1..4 get their
 *   own symbol, but the rest require extra bits.  The base distances and
 *   corresponding number of extra bits are below in the static arrays dist[]
 *   and dext[].
 *
 * - Literal bytes are simply written to the output.  A length/distance pair is
 *   an instruction to copy previously uncompressed bytes to the output.  The
 *   copy is from distance bytes back in the output stream, copying for length
 *   bytes.
 *
 * - Distances pointing before the beginning of the output data are not
 *   permitted.
 *
 * - Overlapped copies, where the length is greater than the distance, are
 *   allowed and common.  For example, a distance of one and a length of 258
 *   simply copies the last byte 258 times.  A distance of four and a length of
 *   twelve copies the last four bytes three times.  A simple forward copy
 *   ignoring whether the length is greater than the distance or not implements
 *   this correctly.  You should not use memcpy() since its behavior is not
 *   defined for overlapped arrays.  You should not use memmove() or bcopy()
 *   since though their behavior -is- defined for overlapping arrays, it is
 *   defined to do the wrong thing in this case.
 */
local int codes(struct state *s,
                const struct huffman *lencode,
                const struct huffman *distcode)
{
    int symbol;         /* decoded symbol */
    int len;            /* length for copy */
    unsigned dist;      /* distance for copy */
    static const short lens[29] = { /* Size base for length codes 257..285 */
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258};
    static const short lext[29] = { /* Extra bits for length codes 257..285 */
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0};
    static const short dists[30] = { /* Offset base for distance codes 0..29 */
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577};
    static const short dext[30] = { /* Extra bits for distance codes 0..29 */
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13};

    /* decode literals and length/distance pairs */
    do {
        symbol = decode(s, lencode);
        if (symbol < 0)
            return symbol;              /* invalid symbol */
        if (symbol < 256) {             /* literal: symbol is the byte */
            /* write out the literal */
            if (s->out != NIL) {
                if (s->outcnt == s->outlen)
                    return 1;
                s->out[s->outcnt] = symbol;
            }
            s->outcnt++;
        }
        else if (symbol > 256) {        /* length */
            /* get and compute length */
            symbol -= 257;
            if (symbol >= 29)
                return -10;             /* invalid fixed code */
            len = lens[symbol] + bits(s, lext[symbol]);

            /* get and check distance */
            symbol = decode(s, distcode);
            if (symbol < 0)
                return symbol;          /* invalid symbol */
            dist = dists[symbol] + bits(s, dext[symbol]);
#ifndef INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
            if (dist > s->outcnt)
                return -11;     /* distance too far back */
#endif

            /* copy length bytes from distance bytes back */
            if (s->out != NIL) {
                if (s->outcnt + len > s->outlen)
                    return 1;
                while (len--) {
                    s->out[s->outcnt] =
#ifdef INFLATE_ALLOW_INVALID_DISTANCE_TOOFAR_ARRR
                        dist > s->outcnt ?
                            0 :
#endif
                            s->out[s->outcnt - dist];
                    s->outcnt++;
                }
            }
            else
                s->outcnt += len;
        }
    } while (symbol != 256);            /* end of block symbol */

    /* done with a valid fixed or dynamic block */
    return 0;
}

/*
 * Process a fixed codes block.
 *
 * Format notes:
 *
 * - This block type can be useful for compressing small amounts of data for
 *   which the size of the code descriptions in a dynamic block exceeds the
 *   benefit of custom codes for that block.  For fixed codes, no bits are
 *   spent on code descriptions.  Instead the code lengths for literal/length
 *   codes and distance codes are fixed.  The specific lengths for each symbol
 *   can be seen in the "for" loops below.
 *
 * - The literal/length code is complete, but has two symbols that are invalid
 *   and should result in an error if received.  This cannot be implemented
 *   simply as an incomplete code since those two symbols are in the "middle"
 *   of the code.  They are eight bits long and the longest literal/length\
 *   code is nine bits.  Therefore the code must be constructed with those
 *   symbols, and the invalid symbols must be detected after decoding.
 *
 * - The fixed distance codes also have two invalid symbols that should result
 *   in an error if received.  Since all of the distance codes are the same
 *   length, this can be implemented as an incomplete code.  Then the invalid
 *   codes are detected while decoding.
 */
local int fixed(struct state *s)
{
    static int virgin = 1;
    static short lencnt[MAXBITS+1], lensym[FIXLCODES];
    static short distcnt[MAXBITS+1], distsym[MAXDCODES];
    static struct huffman lencode, distcode;

    /* build fixed huffman tables if first call (may not be thread safe) */
    if (virgin) {
        int symbol;
        short lengths[FIXLCODES];

        /* construct lencode and distcode */
        lencode.count = lencnt;
        lencode.symbol = lensym;
        distcode.count = distcnt;
        distcode.symbol = distsym;

        /* literal/length table */
        for (symbol = 0; symbol < 144; symbol++)
            lengths[symbol] = 8;
        for (; symbol < 256; symbol++)
            lengths[symbol] = 9;
        for (; symbol < 280; symbol++)
            lengths[symbol] = 7;
        for (; symbol < FIXLCODES; symbol++)
            lengths[symbol] = 8;
        construct(&lencode, lengths, FIXLCODES);

        /* distance table */
        for (symbol = 0; symbol < MAXDCODES; symbol++)
            lengths[symbol] = 5;
        construct(&distcode, lengths, MAXDCODES);

        /* do this just once */
        virgin = 0;
    }

    /* decode data until end-of-block code */
    return codes(s, &lencode, &distcode);
}

/*
 * Process a dynamic codes block.
 *
 * Format notes:
 *
 * - A dynamic block starts with a description of the literal/length and
 *   distance codes for that block.  New dynamic blocks allow the compressor to
 *   rapidly adapt to changing data with new codes optimized for that data.
 *
 * - The codes used by the deflate format are "canonical", which means that
 *   the actual bits of the codes are generated in an unambiguous way simply
 *   from the number of bits in each code.  Therefore the code descriptions
 *   are simply a list of code lengths for each symbol.
 *
 * - The code lengths are stored in order for the symbols, so lengths are
 *   provided for each of the literal/length symbols, and for each of the
 *   distance symbols.
 *
 * - If a symbol is not used in the block, this is represented by a zero as
 *   as the code length.  This does not mean a zero-length code, but rather
 *   that no code should be created for this symbol.  There is no way in the
 *   deflate format to represent a zero-length code.
 *
 * - The maximum number of bits in a code is 15, so the possible lengths for
 *   any code are 1..15.
 *
 * - The fact that a length of zero is not permitted for a code has an
 *   interesting consequence.  Normally if only one symbol is used for a given
 *   code, then in fact that code could be represented with zero bits.  However
 *   in deflate, that code has to be at least one bit.  So for example, if
 *   only a single distance base symbol appears in a block, then it will be
 *   represented by a single code of length one, in particular one 0 bit.  This
 *   is an incomplete code, since if a 1 bit is received, it has no meaning,
 *   and should result in an error.  So incomplete distance codes of one symbol
 *   should be permitted, and the receipt of invalid codes should be handled.
 *
 * - It is also possible to have a single literal/length code, but that code
 *   must be the end-of-block code, since every dynamic block has one.  This
 *   is not the most efficient way to create an empty block (an empty fixed
 *   block is fewer bits), but it is allowed by the format.  So incomplete
 *   literal/length codes of one symbol should also be permitted.
 *
 * - If there are only literal codes and no lengths, then there are no distance
 *   codes.  This is represented by one distance code with zero bits.
 *
 * - The list of up to 286 length/literal lengths and up to 30 distance lengths
 *   are themselves compressed using Huffman codes and run-length encoding.  In
 *   the list of code lengths, a 0 symbol means no code, a 1..15 symbol means
 *   that length, and the symbols 16, 17, and 18 are run-length instructions.
 *   Each of 16, 17, and 18 are follwed by extra bits to define the length of
 *   the run.  16 copies the last length 3 to 6 times.  17 represents 3 to 10
 *   zero lengths, and 18 represents 11 to 138 zero lengths.  Unused symbols
 *   are common, hence the special coding for zero lengths.
 *
 * - The symbols for 0..18 are Huffman coded, and so that code must be
 *   described first.  This is simply a sequence of up to 19 three-bit values
 *   representing no code (0) or the code length for that symbol (1..7).
 *
 * - A dynamic block starts with three fixed-size counts from which is computed
 *   the number of literal/length code lengths, the number of distance code
 *   lengths, and the number of code length code lengths (ok, you come up with
 *   a better name!) in the code descriptions.  For the literal/length and
 *   distance codes, lengths after those provided are considered zero, i.e. no
 *   code.  The code length code lengths are received in a permuted order (see
 *   the order[] array below) to make a short code length code length list more
 *   likely.  As it turns out, very short and very long codes are less likely
 *   to be seen in a dynamic code description, hence what may appear initially
 *   to be a peculiar ordering.
 *
 * - Given the number of literal/length code lengths (nlen) and distance code
 *   lengths (ndist), then they are treated as one long list of nlen + ndist
 *   code lengths.  Therefore run-length coding can and often does cross the
 *   boundary between the two sets of lengths.
 *
 * - So to summarize, the code description at the start of a dynamic block is
 *   three counts for the number of code lengths for the literal/length codes,
 *   the distance codes, and the code length codes.  This is followed by the
 *   code length code lengths, three bits each.  This is used to construct the
 *   code length code which is used to read the remainder of the lengths.  Then
 *   the literal/length code lengths and distance lengths are read as a single
 *   set of lengths using the code length codes.  Codes are constructed from
 *   the resulting two sets of lengths, and then finally you can start
 *   decoding actual compressed data in the block.
 *
 * - For reference, a "typical" size for the code description in a dynamic
 *   block is around 80 bytes.
 */
local int dynamic(struct state *s)
{
    int nlen, ndist, ncode;             /* number of lengths in descriptor */
    int index;                          /* index of lengths[] */
    int err;                            /* construct() return value */
    short lengths[MAXCODES];            /* descriptor code lengths */
    short lencnt[MAXBITS+1], lensym[MAXLCODES];         /* lencode memory */
    short distcnt[MAXBITS+1], distsym[MAXDCODES];       /* distcode memory */
    struct huffman lencode, distcode;   /* length and distance codes */
    static const short order[19] =      /* permutation of code length codes */
        {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

    /* construct lencode and distcode */
    lencode.count = lencnt;
    lencode.symbol = lensym;
    distcode.count = distcnt;
    distcode.symbol = distsym;

    /* get number of lengths in each table, check lengths */
    nlen = bits(s, 5) + 257;
    ndist = bits(s, 5) + 1;
    ncode = bits(s, 4) + 4;
    if (nlen > MAXLCODES || ndist > MAXDCODES)
        return -3;                      /* bad counts */

    /* read code length code lengths (really), missing lengths are zero */
    for (index = 0; index < ncode; index++)
        lengths[order[index]] = bits(s, 3);
    for (; index < 19; index++)
        lengths[order[index]] = 0;

    /* build huffman table for code lengths codes (use lencode temporarily) */
    err = construct(&lencode, lengths, 19);
    if (err != 0)               /* require complete code set here */
        return -4;

    /* read length/literal and distance code length tables */
    index = 0;
    while (index < nlen + ndist) {
        int symbol;             /* decoded value */
        int len;                /* last length to repeat */

        symbol = decode(s, &lencode);
        if (symbol < 0)
            return symbol;          /* invalid symbol */
        if (symbol < 16)                /* length in 0..15 */
            lengths[index++] = symbol;
        else {                          /* repeat instruction */
            len = 0;                    /* assume repeating zeros */
            if (symbol == 16) {         /* repeat last length 3..6 times */
                if (index == 0)
                    return -5;          /* no last length! */
                len = lengths[index - 1];       /* last length */
                symbol = 3 + bits(s, 2);
            }
            else if (symbol == 17)      /* repeat zero 3..10 times */
                symbol = 3 + bits(s, 3);
            else                        /* == 18, repeat zero 11..138 times */
                symbol = 11 + bits(s, 7);
            if (index + symbol > nlen + ndist)
                return -6;              /* too many lengths! */
            while (symbol--)            /* repeat last or zero symbol times */
                lengths[index++] = len;
        }
    }

    /* check for end-of-block code -- there better be one! */
    if (lengths[256] == 0)
        return -9;

    /* build huffman table for literal/length codes */
    err = construct(&lencode, lengths, nlen);
    if (err && (err < 0 || nlen != lencode.count[0] + lencode.count[1]))
        return -7;      /* incomplete code ok only for single length 1 code */

    /* build huffman table for distance codes */
    err = construct(&distcode, lengths + nlen, ndist);
    if (err && (err < 0 || ndist != distcode.count[0] + distcode.count[1]))
        return -8;      /* incomplete code ok only for single length 1 code */

    /* decode data until end-of-block code */
    return codes(s, &lencode, &distcode);
}

/*
 * Inflate source to dest.  On return, destlen and sourcelen are updated to the
 * size of the uncompressed data and the size of the deflate data respectively.
 * On success, the return value of puff() is zero.  If there is an error in the
 * source data, i.e. it is not in the deflate format, then a negative value is
 * returned.  If there is not enough input available or there is not enough
 * output space, then a positive error is returned.  In that case, destlen and
 * sourcelen are not updated to facilitate retrying from the beginning with the
 * provision of more input data or more output space.  In the case of invalid
 * inflate data (a negative error), the dest and source pointers are updated to
 * facilitate the debugging of deflators.
 *
 * puff() also has a mode to determine the size of the uncompressed output with
 * no output written.  For this dest must be (unsigned char *)0.  In this case,
 * the input value of *destlen is ignored, and on return *destlen is set to the
 * size of the uncompressed output.
 *
 * The return codes are:
 *
 *   2:  available inflate data did not terminate
 *   1:  output space exhausted before completing inflate
 *   0:  successful inflate
 *  -1:  invalid block type (type == 3)
 *  -2:  stored block length did not match one's complement
 *  -3:  dynamic block code description: too many length or distance codes
 *  -4:  dynamic block code description: code lengths codes incomplete
 *  -5:  dynamic block code description: repeat lengths with no first length
 *  -6:  dynamic block code description: repeat more than specified lengths
 *  -7:  dynamic block code description: invalid literal/length code lengths
 *  -8:  dynamic block code description: invalid distance code lengths
 *  -9:  dynamic block code description: missing end-of-block code
 * -10:  invalid literal/length or distance code in fixed or dynamic block
 * -11:  distance is too far back in fixed or dynamic block
 *
 * Format notes:
 *
 * - Three bits are read for each block to determine the kind of block and
 *   whether or not it is the last block.  Then the block is decoded and the
 *   process repeated if it was not the last block.
 *
 * - The leftover bits in the last byte of the deflate data after the last
 *   block (if it was a fixed or dynamic block) are undefined and have no
 *   expected values to check.
 */
int puff(unsigned char *dest,           /* pointer to destination pointer */
         unsigned long *destlen,        /* amount of output space */
         const unsigned char *source,   /* pointer to source data pointer */
         unsigned long *sourcelen)      /* amount of input available */
{
    struct state s;             /* input/output state */
    int last, type;             /* block information */
    int err;                    /* return value */

    /* initialize output state */
    s.out = dest;
    s.outlen = *destlen;                /* ignored if dest is NIL */
    s.outcnt = 0;

    /* initialize input state */
    s.in = source;
    s.inlen = *sourcelen;
    s.incnt = 0;
    s.bitbuf = 0;
    s.bitcnt = 0;

    /* return if bits() or decode() tries to read past available input */
    if (setjmp(s.env) != 0)             /* if came back here via longjmp() */
        err = 2;                        /* then skip do-loop, return error */
    else {
        /* process blocks until last block or error */
        do {
            last = bits(&s, 1);         /* one if last block */
            type = bits(&s, 2);         /* block type 0..3 */
            err = type == 0 ?
                    stored(&s) :
                    (type == 1 ?
                        fixed(&s) :
                        (type == 2 ?
                            dynamic(&s) :
                            -1));       /* type == 3, invalid */
            if (err != 0)
                break;                  /* return with error */
        } while (!last);
    }

    /* update the lengths and return */
    if (err <= 0) {
        *destlen = s.outcnt;
        *sourcelen = s.incnt;
    }
    return err;
}
// RFC 1952 Appendix: Sample CRC Code

/* Table of CRCs of all 8-bit messages. */
unsigned long crc_table[256];
/* Flag: has the table been computed? Initially false. */
int crc_table_computed = 0;
/* Make the table for a fast CRC. */
void make_crc_table(void)
{
  unsigned long c;
  int n, k;
  for (n = 0; n < 256; n++) {
    c = (unsigned long) n;
    for (k = 0; k < 8; k++) {
      if (c & 1) {
        c = 0xedb88320L ^ (c >> 1);
      } else {
        c = c >> 1;
      }
    }
    crc_table[n] = c;
  }
  crc_table_computed = 1;
}

/*
    Update a running crc with the bytes buf[0..len-1] and return
  the updated crc. The crc should be initialized to zero. Pre- and
  post-conditioning (one’s complement) is performed within this
  function so it shouldn’t be done by the caller. Usage example:

    unsigned long crc = 0L;

    while (read_buffer(buffer, length) != EOF) {
      crc = update_crc(crc, buffer, length);
    }
    if (crc != original_crc) error();
*/
unsigned long update_crc(unsigned long crc,
                         unsigned char *buf, int len)
{
  unsigned long c = crc ^ 0xffffffffL;
  int n;
  if (!crc_table_computed)
    make_crc_table();
  for (n = 0; n < len; n++) {
    c = crc_table[(c ^ buf[n]) & 0xff] ^ (c >> 8);
  }
  return c ^ 0xffffffffL;
}

/* Return the CRC of the bytes buf[0..len-1]. */
unsigned long crc(unsigned char *buf, int len)
{
  return update_crc(0L, buf, len);
}

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

// GZIP file format is specified in RFC 1952
// https://web.archive.org/web/20240421215142/https://dl.acm.org/doi/pdf/10.17487/RFC1952

struct GZ_FILE {
  char id1; // 0x1f
  char id2; // 0x8b
  char compression_method;  // 0x08 = DEFLATE
  char flags; // Each bit is a flag corresponding to the F* defines below
  char modification_time[4]; // Modification time
  char extra_flags; // Extra flags
  char os; // Operating system (used to determine the end-of-line convention)
};

#define FTEXT     0x1  // If FTEXT is set, the file is probably ASCII text.
#define FHCRC     0x2  // If FHCRC is set, a CRC16 for the gzip header is present, immediately before the compressed data.
#define FEXTRA    0x4  // If FEXTRA is set, an extra field is present, starting with a 16-bit length field.
#define FNAME     0x8  // If FNAME is set, an original file name is present, terminated by a zero byte.
#define FCOMMENT  0x10 // If FCOMMENT is set, a zero-terminated file comment is present.
#define FRESERVED 0xE0 // Reserved bits, must be zero.

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define ASSERT(x, msg) \
  if (!(x)) { \
    fprintf(stderr, msg "\n"); \
    exit(1); \
  }

#ifndef SOURCE_LEN
#define SOURCE_LEN 10485760 // 100MB
#endif

#ifndef DEST_LEN
#define DEST_LEN 10485760 // 100MB
#endif

unsigned char gz_source[SOURCE_LEN];
unsigned char gz_dest[DEST_LEN];

int ENTRY_POINT(int argc, char **argv) {
  int in_fd  = STDIN;
  int out_fd = STDOUT;

  while (argc > 1) {
    if (strcmp(argv[1], "--file") == 0 && argc > 2) {
      in_fd = open(argv[2], O_RDONLY);
      if (in_fd < 0) {
        printf("Error opening file %s\n", argv[2]);
        return 1;
      }
      argv += 2;
      argc -= 2;
    } else if (strcmp(argv[1], "--output") == 0 && argc > 2) {
      out_fd = open(argv[2], O_RDWR | O_CREAT | O_TRUNC, 0666);
      if (out_fd < 0) {
        printf("Error opening output file %s\n", argv[2]);
        return 1;
      }
      argv += 2;
      argc -= 2;
    } else {
      printf("Unknown option %s\n", argv[1]);
      return 1;
    }
  }

  // Read the GZIP header
  struct GZ_FILE gz_header;
  ASSERT(read(in_fd, &gz_header, sizeof(gz_header)) == sizeof(gz_header),
         "Error reading GZIP header");

  // Check the GZIP header
  ASSERT(gz_header.id1 != 0x1f || gz_header.id2 != 0x8b, "Invalid GZIP header");

  // Check the compression method
  ASSERT(gz_header.compression_method == 0x08, "Unsupported compression method");

  // We ignore the CM/MTIME/XFL/OS fields for now.

  // If flags.extra is set, the following 2 bytes are XLEN, the length of the extra fields.
  char *extra = 0;
  char *filename = NULL; // pointer in the extra array
  char *comment = NULL;  // pointer in the extra array
  if (gz_header.flags & FEXTRA) {
    unsigned char len_bytes[2];
    ASSERT(read(in_fd, len_bytes, sizeof(len_bytes)) == sizeof(len_bytes),
         "Error reading extra field length");
    int extra_len = len_bytes[0] | (len_bytes[1] << 8);

    // XLEN doesn't include the 2 bytes for the CRC16 data if FHCRC is set.
    if (gz_header.flags & FHCRC) extra_len += 2;
    extra = malloc(extra_len); // Unknown size so heap allocated instead of stack allocated
    ASSERT(extra != NULL, "Error allocating memory for extra field");

    // We then read the extra fields, which are extra_len bytes long.
    ASSERT(read(in_fd, extra, extra_len) == extra_len, "Error reading extra field");

    // If the FNAME flag is set, read the original file name
    int filename_len = 0;
    if (gz_header.flags & FNAME) {
      filename = extra;
      filename_len = strlen(filename);
    }

    // If the FCOMMENT flag is set, read the original file comment
    int comment_len = 0;
    if (gz_header.flags & FCOMMENT) {
      comment = extra + filename_len + 1; // Skip filename + NUL byte
      comment_len = strlen(comment);
    }

    // If the FHCRC flag is set, read the CRC16
    int crc16 = 0;
    if (gz_header.flags & FHCRC) {
      crc16 = extra[filename_len + comment_len + 2] | (extra[filename_len + comment_len + 3] << 8);
      (void) crc16;
      printf("CRC16 not yet implemented\n");
      exit(1);
    }
  }

  // Required for compliance with RFC 1952, the reserved bits must be zero
  ASSERT((gz_header.flags & FRESERVED) == 0, "Reserved bits in GZIP header must be zero");

  // Now we can read the compressed data
  unsigned long src_len = SOURCE_LEN;
  unsigned long dest_len = DEST_LEN;
  // We read the file and feed it to the decompressor. The decompressor
  // signals with its return value:
  //  2:  available inflate data did not terminate
  //  1:  output space exhausted before completing inflate
  //  0:  successful inflate
  // <0:  error
  //
  int compressed_size = read(in_fd, gz_source, SOURCE_LEN);
  ASSERT((compressed_size < SOURCE_LEN), "Input space exhausted when reading file. Recompile ungz.c with a larger SOURCE_LEN.");
  int puff_ret = puff(gz_dest, &dest_len, gz_source, &src_len);
  switch (puff_ret) {
    case 0:
      // Successful inflate, we can go and check the CRC then output
      ASSERT(src_len + 8 <= compressed_size, "Input file is missing CRC32 and ISIZE");

      // CRC32 is at the end of the compressed data
      unsigned long crc32 =  gz_source[src_len + 0]        | (gz_source[src_len + 1] << 8)
                          | (gz_source[src_len + 2] << 16) | (gz_source[src_len + 3] << 24);

      unsigned long isize =  gz_source[src_len + 4]        | (gz_source[src_len + 5] << 8)
                          | (gz_source[src_len + 6] << 16) | (gz_source[src_len + 7] << 24);

      // Check CRC32 and ISIZE
      unsigned long computed_crc = crc(gz_dest, dest_len);
      ASSERT(computed_crc == crc32, "CRC32 mismatch");
      ASSERT(isize == dest_len, "ISIZE mismatch");
      write(out_fd, gz_dest, dest_len);
      break;

    case 1:
      // Output space exhausted before completing inflate
      printf("Output space exhausted before completing inflate. Recompile ungz.c with a larger DEST_LEN.\n");
      exit(1);

    case 2:
      // Available inflate data did not terminate
      printf("Available inflate data did not terminate. Perhaps the file is corrupted.\n");
      exit(1);

    default:
      // Error in the compressed data
      printf("Error %d in the compressed data\n", puff_ret);
      exit(1);
  }

  // Cleanup
  if (extra) free(extra);

  return 0;
}
#undef ENTRY_POINT
#define ENTRY_POINT untar_main
/*
 * This file is in the public domain.  Use it as you see fit.
 */

/*
 * "untar" is an extremely simple tar extractor:
 *  * A single C source file, so it should be easy to compile
 *    and run on any system with a C compiler.
 *  * Extremely portable standard C.  The only non-ANSI function
 *    used is mkdir().
 *  * Reads basic ustar tar archives.
 *  * Does not require libarchive or any other special library.
 *
 * To compile: cc -o untar untar.c
 *
 * Usage:  untar <archive>
 *
 * In particular, this program should be sufficient to extract the
 * distribution for libarchive, allowing people to bootstrap
 * libarchive on systems that do not already have a tar program.
 *
 * To unpack libarchive-x.y.z.tar.gz:
 *    * gunzip libarchive-x.y.z.tar.gz
 *    * untar libarchive-x.y.z.tar
 *
 * Written by Tim Kientzle, March 2009.
 *
 * Released into the public domain.
 */

/* These are all highly standard and portable headers. */

/* This is for mkdir(); this may need to be changed for some platforms. */

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <windows.h>
#endif

#ifndef ENTRY_POINT
#define ENTRY_POINT main
#endif

#define BLOCKSIZE 512

/* System call to create a directory. */
static int
system_mkdir(char *pathname, int mode)
{
#if defined(_WIN32) && !defined(__CYGWIN__)
	(void)mode; /* UNUSED */
	return _mkdir(pathname);
#else
	return mkdir(pathname, mode);
#endif
}

/* Parse an octal number, ignoring leading and trailing nonsense. */
static unsigned long
parseoct(const char *p, size_t n)
{
	unsigned long i = 0;

	while ((*p < '0' || *p > '7') && n > 0) {
		++p;
		--n;
	}
	while (*p >= '0' && *p <= '7' && n > 0) {
		i *= 8;
		i += *p - '0';
		++p;
		--n;
	}
	return (i);
}

/* Returns true if this is 512 zero bytes. */
static int
is_end_of_archive(const char *p)
{
	int n;
	for (n = 0; n < BLOCKSIZE; ++n)
		if (p[n] != '\0')
			return (0);
	return (1);
}

/* Create a directory, including parent directories as necessary. */
static void
create_dir(char *pathname, int mode)
{
	char *p;
	int r;

	/* Strip trailing '/' */
	if (pathname[strlen(pathname) - 1] == '/')
		pathname[strlen(pathname) - 1] = '\0';

	/* Try creating the directory. */
	r = system_mkdir(pathname, mode);
	if (r != 0) {
		/* On failure, try creating parent directory. */
		p = strrchr(pathname, '/');
		if (p != NULL) {
			*p = '\0';
			create_dir(pathname, 0755);
			*p = '/';
			r = system_mkdir(pathname, mode);
		}
	}
	if (r != 0)
		fprintf(stderr, "Could not create directory %s\n", pathname);
}

/* Create a file, including parent directory as necessary. */
static FILE *
create_file(char *pathname, int mode)
{
	FILE *f;
	f = fopen(pathname, "wb+");
	if (f == NULL) {
		/* Try creating parent dir and then creating file. */
		char *p = strrchr(pathname, '/');
		if (p != NULL) {
			*p = '\0';
			create_dir(pathname, 0755);
			*p = '/';
			f = fopen(pathname, "wb+");
		}
	}
	return (f);
}

/* Verify the tar checksum. */
static int
verify_checksum(const char *p)
{
	int n, u = 0;
	for (n = 0; n < BLOCKSIZE; ++n) {
		if (n < 148 || n > 155)
			/* Standard tar checksum adds unsigned bytes. */
			u += ((unsigned char *)p)[n];
		else
			u += 0x20;

	}
	return (u == (int)parseoct(p + 148, 8));
}

/* Extract a tar archive. */
static void
untar(FILE *a, const char *path)
{
	char buff[BLOCKSIZE];
	FILE *f = NULL;
	size_t bytes_read;
	unsigned long filesize;

	printf("Extracting from %s\n", path);
	for (;;) {
		bytes_read = fread(buff, 1, BLOCKSIZE, a);
		if (bytes_read < BLOCKSIZE) {
			fprintf(stderr,
			    "Short read on %s: expected %d, got %d\n",
			    path, BLOCKSIZE, (int)bytes_read);
			return;
		}
		if (is_end_of_archive(buff)) {
			printf("End of %s\n", path);
			return;
		}
		if (!verify_checksum(buff)) {
			fprintf(stderr, "Checksum failure\n");
			return;
		}
		filesize = parseoct(buff + 124, 12);
		switch (buff[156]) {
		case '1':
			printf(" Ignoring hardlink %s\n", buff);
			break;
		case '2':
			printf(" Ignoring symlink %s\n", buff);
			break;
		case '3':
			printf(" Ignoring character device %s\n", buff);
				break;
		case '4':
			printf(" Ignoring block device %s\n", buff);
			break;
		case '5':
			printf(" Extracting dir %s\n", buff);
			create_dir(buff, (int)parseoct(buff + 100, 8));
			filesize = 0;
			break;
		case '6':
			printf(" Ignoring FIFO %s\n", buff);
			break;
		default:
			printf(" Extracting file %s\n", buff);
			f = create_file(buff, (int)parseoct(buff + 100, 8));
			break;
		}
		while (filesize > 0) {
			bytes_read = fread(buff, 1, BLOCKSIZE, a);
			if (bytes_read < BLOCKSIZE) {
				fprintf(stderr,
				    "Short read on %s: Expected %d, got %d\n",
				    path, BLOCKSIZE, (int)bytes_read);
				return;
			}
			if (filesize < BLOCKSIZE)
				bytes_read = (size_t)filesize;
			if (f != NULL) {
				if (fwrite(buff, 1, bytes_read, f)
				    != bytes_read)
				{
					fprintf(stderr, "Failed write\n");
					fclose(f);
					f = NULL;
				}
			}
			filesize -= bytes_read;
		}
		if (f != NULL) {
			fclose(f);
			f = NULL;
		}
	}
}

int
ENTRY_POINT(int argc, char **argv)
{
	FILE *a;

	++argv; /* Skip program name */
	for ( ;*argv != NULL; ++argv) {
		a = fopen(*argv, "rb");
		if (a == NULL)
			fprintf(stderr, "Unable to open %s\n", *argv);
		else {
			untar(a, *argv);
			fclose(a);
		}
	}
	return (0);
}
#undef ENTRY_POINT

// Function to invoke the appropriate subcommand based on the command name.
// Takes the command name and the arguments passed to it. argc and argv include
// the command name as the first argument.
// Returns the exit code of the invoked command.
int invoke_subcommand(char* name, int argc, char **argv) {
  if (strcmp(name, "bintools") == 0) {
    // If no command is provided, show usage
    if (argc < 2) {
      // No command provided, show usage
      static const char *usage = "Usage: bintools <command> [<args>]\n"
                          "Commands:\n"
                          "  cp <source> <destination>   Copy files\n"
                          "  cat <files>                 Concatenate files\n"
                          "  mkdir <directory>           Create directories\n"
                          "  sha256sum <files>           Compute SHA256 checksums\n"
                          "  simple-patch <patch> <file> Apply simple patches\n"
                          "  ungz <file.gz>              Uncompress .gz files\n"
                          "  untar <file.tar>            Extract .tar files\n";
      printf("%s\n", usage);
    } else {
      // Shift arguments to pass to subcommand
      return invoke_subcommand(argv[1], argc - 1, argv + 1);
    }
  } else if (strcmp(name, "cp") == 0) {
    return cp_main(argc, argv);
  } else if (strcmp(name, "cat") == 0) {
    return cat_main(argc, argv);
  } else if (strcmp(name, "mkdir") == 0) {
    return mkdir_main(argc, argv);
  } else if (strcmp(name, "sha256sum") == 0) {
    return sha256sum_main(argc, argv);
  } else if (strcmp(name, "simple-patch") == 0) {
    return simple_patch_main(argc, argv);
  } else if (strcmp(name, "ungz") == 0) {
    return ungz_main(argc, argv);
  } else if (strcmp(name, "untar") == 0) {
    return untar_main(argc, argv);
  } else {
    printf("bintools: unknown command '%s'\n", name);
    return 1;
  }
}

// bintools may be called with ./path/to/bintools or /usr/bin/bintools
// We want to extract the actual command name (bintools) from the path.
char *strip_path(char *path) {
  char *base = strrchr(path, '/');
  return base ? base + 1 : path;
}

int main(int argc, char **argv) {
  // The tool looks at argv[0] to determine which sub-command to pass control to.
  // If no command is provided, it shows usage information.
  return invoke_subcommand(strip_path(argv[0]), argc, argv);
}

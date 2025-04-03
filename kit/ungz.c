#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "puff.c"
#include "crc.c"

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
#define SOURCE_LEN 104857600 // 100MB
#endif

#ifndef DEST_LEN
#define DEST_LEN 104857600 // 100MB
#endif

unsigned char source[SOURCE_LEN];
unsigned char dest[DEST_LEN];

int main(int argc, char **argv) {
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
  int compressed_size = read(in_fd, source, SOURCE_LEN);
  ASSERT((compressed_size < SOURCE_LEN), "Input space exhausted when reading file. Recompile ungz.c with a larger SOURCE_LEN.");
  int puff_ret = puff(dest, &dest_len, source, &src_len);
  switch (puff_ret) {
    case 0:
      // Successful inflate, we can go and check the CRC then output
      ASSERT(src_len + 8 <= compressed_size, "Input file is missing CRC32 and ISIZE");

      // CRC32 is at the end of the compressed data
      unsigned long crc32 =  source[src_len + 0]        | (source[src_len + 1] << 8)
                          | (source[src_len + 2] << 16) | (source[src_len + 3] << 24);

      unsigned long isize =  source[src_len + 4]        | (source[src_len + 5] << 8)
                          | (source[src_len + 6] << 16) | (source[src_len + 7] << 24);

      // Check CRC32 and ISIZE
      unsigned long computed_crc = crc(dest, dest_len);
      ASSERT(computed_crc == crc32, "CRC32 mismatch");
      ASSERT(isize == dest_len, "ISIZE mismatch");
      write(out_fd, dest, dest_len);
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

set -e -u

#################################### C code ####################################
# int main() {
#   printf("Hello, world\n");
# }
################################# End of C code ################################
_main() {
  printf "Hello, world\n"
}

# Runtime library
_code=0; # Success exit code
_main _code; exit $_code

# string_pool_alloc=365 heap_alloc=349 text_alloc=35

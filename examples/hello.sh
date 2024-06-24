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

# string_pool_alloc=387 heap_alloc=365 max_text_alloc=45 cumul_text_alloc=45

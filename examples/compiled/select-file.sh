#!/bin/sh
set -e -u -f
LC_ALL=C

################################################################################
#
#                      Bindings for POSIX shell utilities
#
# ############################# Calling convention #############################
#
# The calling convention is the following:
#
# 1. C functions are prefixed with an underscore when compiled to shell.
#
# 2. The first argument is the return variable, it indicates where the return
#    value should be stored. The other arguments are the function arguments.
#
# 3. Variables are globally scoped, and the callee is responsible for restoring
#    the initial values of local variables before returning. Variables starting
#    with `_` are global variables, variables starting with `__` are reserved
#    for the runtime library, and `___` is free to be used by the user.
#
# 4. The arguments and return value can either be immediates (int) or pointers.
#    Shell strings must always be converted to the C string format before being
#    returned using the `unpack_string` function.
#
# ############################# String conversions #############################
#
# To help with the calling convention, the `pack_string` and `unpack_string`
# functions can be used to convert to/from shell strings and C strings. For
# functions that return multiple strings, the `unpack_lines` function can be
# used to unpack them into an array of strings.
#
# By default, the `unpack_string` function from the runtime library is included.
# This is because the implementation of `unpack_string` depends on the
# compilation options of pnut and may be optimized for different use cases.
#
# Similarly, the `pack_string` function calls the `_put_pstr` function from the
# runtime library so it doesn't have to be included twice (one for the runtime
# library and one for this script).
#
# ############################# Memory allocation ##############################
#
# Memory can be dynamically allocated using the `_malloc` function, and freed
# using the `_free` function. For long running scripts, it is recommended to
# free memory as it tends to slow down the execution of scripts. Because POSIX
# shell requires 32-bit signed integers, the maximum amount of memory that can
# be allocated is 2^31 - 1 words. In practice, most shells will become unusably
# slow or run out of memory before reaching this limit.
#
# Because memory is allocated using a simple bump allocator, address space is
# never reclaimed. Programs that allocate over 2^31 - 1 words may need to manage
# blocks of memory manually to avoid running out of memory.
#
# Because the memory is word-addressable, pointers, integers and characters all
# occupy the same amount of memory. Keep that in mind when allocating
# structures.
#
################################################################################

# Convert a C string to a shell string. The result is stored in $__res.
pack_string() { # $1 = string address
  __res=$(_put_pstr __ $1)
}

# Like `unpack_string` but for multiple strings separated by newlines.
# Returns a pointer to the first string in the array, and null terminates the
# array.
unpack_lines() {
  ___i=1 # Account for null delimiter
  IFS="
"
  for ___line in $2; do
    : $((___i += 1))
  done
  _malloc $1 $___i
  ___i=0
  for ___line in $2; do
    unpack_string _$(($1 + ___i))  "$___line"
    : $((___i += 1))
  done
  : $((_$(($1 + ___i)) = 0)) # Null delimiter
}

_cat() { # $2 = file (char*)
  pack_string $2
  cat $__res
}

# Return the current date with the format "YYYY-MM-DDTHH:MM:SS"
_date() {
  ___date=$(date -Iseconds)
  unpack_string $1 "$___date"
}

_pwd() {
  ___pwd=$(pwd)
  unpack_string $1 "$___pwd"
}

# Example of a variadic function.
# This function can take an optional string argument for the directory to list.
_ls() { # $2 = dir (char*)
  __res=
  if [ $# -eq 2 ]; then
    pack_string $2
  fi
  ___lines=$(ls $__res)
  unpack_lines $1 "$___lines"
}

_touch() { # $2 = file (char*)
  pack_string $2
  touch $__res
}

# Create a directory and return the exit code.
_mkdir() { # $2 = dir (char*)
  pack_string $2
  set +e # Ignore errors
  mkdir -p $__res
  : $(($1 = $?)) # Return the exit code
  set -e # Restore set -e
}

_file_permission() { # $2 = file (char*)
  pack_string $2
  ___perms=$(ls -l $__res | cut -c1-10) # Produce -rwxr-xr-x
  unpack_string $1 "$___perms"
}

_chmod() { # $2 = mode (int), $3 = file (char*)
  pack_string $3
  set +e # Ignore errors
  chmod "$2" $__res
  : $(($1 = $?)) # Return the exit code
  set -e # Restore set -e
}

_wc() { # $2 = file (char*), $3 = lines addr (int*), $4 = words addr (int*), $5 = chars addr (int*)
  pack_string $2
  __res=$(wc $__res | read -r ___lines ___words ___chars _)
  # Write result to the addresses
  : $((_$3 = ___lines))
  : $((_$4 = ___words))
  : $((_$5 = ___chars))
}

: $((i = arr = 0))
_print_array() { let arr $2
  let i
  i=0
  while [ $((_$((arr + i)))) != 0 ] ; do
    printf "%d: " $i
    _put_pstr __ $((_$((arr + i))))
    printf "\n" $i
    : $((i += 1))
  done
  endlet $1 i arr
}

: $((i = arr = 0))
_array_len() { let arr $2
  let i
  i=0
  while [ $((_$((arr + i)))) != 0 ] ; do
    : $((i += 1))
  done
  : $(($1 = i))
  endlet $1 i arr
}

: $((c = n = 0))
_read_int() {
  let n; let c
  n=0
  while [ 1 != 0 ] ; do
    _getchar c 
    if [ $c -ge $__0__ ] && [ $c -le $__9__ ] ; then
      n=$((((10 * n) + c) - __0__))
    else
      break
    fi
  done
  : $(($1 = n))
  endlet $1 c n
}

: $((__t1 = len = ix = files = 0))
_main() {
  let files; let ix; let len; let __t1
  printf "Files in current directory (" 
  _pwd __t1 
  _put_pstr __ $__t1
  printf ")\n" 
  _ls files 
  _array_len len $files
  _print_array __ $files
  while [ 1 != 0 ] ; do
    printf "Select a file to print: " 
    _read_int ix 
    if [ 0 -le $ix ] && [ $ix -lt $len ] ; then
      break
    fi
    printf "Invalid index.\n" 
  done
  _cat __ $((_$((files + ix))))
  endlet $1 __t1 len ix files
}

# Character constants
readonly __0__=48
readonly __9__=57
# Runtime library

__stdin_buf=
__stdin_line_ending=0 # Line ending, either -1 (EOF) or 10 ('\n')
_getchar() {
  if [ -z "$__stdin_buf" ]; then          # need to get next line when buffer empty
    if [ $__stdin_line_ending != 0 ]; then  # Line is empty, return line ending
      : $(($1 = __stdin_line_ending))
      __stdin_line_ending=0                  # Reset line ending for next getchar call
      return
    fi
    IFS=                                            # don't split input
    if read -r __stdin_buf ; then                   # read next line into $__stdin_buf
      if [ -z "$__stdin_buf" ] ; then               # an empty line implies a newline character
        : $(($1 = 10))                              # next getchar call will read next line
        return
      fi
      __stdin_line_ending=10
    else
      if [ -z "$__stdin_buf" ] ; then               # EOF reached when read fails
        : $(($1 = -1))
        return
      else
        __stdin_line_ending=-1
      fi
    fi
  fi
  __c=$(printf "%d" "'${__stdin_buf%"${__stdin_buf#?}"}"); __c=$((__c > 0 ? __c : 256 + __c))
  : $(($1 = __c))
    __stdin_buf="${__stdin_buf#?}"                  # remove the current char from $__stdin_buf
}

_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((_$__addr)) != 0 ]; do
    printf \\$((_$__addr/64))$((_$__addr/8%8))$((_$__addr%8))
    : $((__addr += 1))
  done
}

# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__SP += 1)) $((__$__SP=$1)) # Push
  : $(($1=${2-0}))                 # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$__SP)) $((__SP -= 1)); # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

# Convert a Shell string to a C string
unpack_string() {
  __str="$2"
  _malloc $1 $((${#__str} + 1))
  __ptr=$(($1))
  while [ -n "$__str" ] ; do
    # Remove first char from string
    __tail="${__str#?}"
    # Remove all but first char
    __char="${__str%"$__tail"}"
    # Convert char to ASCII
    __c=$(printf "%d" "'$__char"); __c=$((__c > 0 ? __c : 256 + __c))
    # Write character to memory
    : $((_$__ptr = __c))
    # Continue with rest of string
    : $((__ptr += 1))
    __str="$__tail"
  done
  : $((_$__ptr = 0))
}

_main __

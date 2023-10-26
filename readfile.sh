#!/bin/ksh

_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }

# The source code can be read using a buffer that is managed like a stack
# (where $BP is the stack pointer). Using a stack is convenient for handling
# #include directives. When an #include is encountered, the content of the
# file is added to the top of the stack so that the compiler will first
# see the content of the included file, and then the source code immediately
# after the #include directive.

BP=0  # Source code buffer pointer

prepend_file()
{
  : $((tmp1 = BP))                            # remember beginning of section
  IFS=""                                      # avoid word splitting by "read"
  while : ; do
    if read -r tmp2 ; then                    # read a line of input into $tmp2
      while [ ! -z "$tmp2" ] ; do             # iterate over all chars in $tmp2
        tmp3=$tmp2                            # remember original $tmp2
        tmp2=${tmp2#?}                        # remove first char from $tmp2
        tmp3=${tmp3%"$tmp2"}                  # extract first char
        tmp3=$(LC_CTYPE=C printf %d "'$tmp3") # convert char to int
        : $((B$BP = tmp3)) $((BP = BP+1))     # add char to buffer
      done
      : $((B$BP = 10)) $((BP = BP+1))         # add newline char at end of line
    else
      break
    fi
  done

  # reverse the characters that were read from file so that characters
  # can be iterated over by using $BP to "pop" the next character

  : $((tmp2 = BP))
  while [ $tmp2 -gt $tmp1 ] ; do
    : $((tmp2 = tmp2-1)) $((tmp3 = B$tmp2)) $((B$tmp2 = B$tmp1)) $((B$tmp1 = tmp3)) $((tmp1 = tmp1+1))
  done
}

prepend_file < README.md
prepend_file < readfile.sh

# iterate over all buffered characters and copy them to stdout

while [ 0 != $BP ] ; do
  : $((BP = BP-1)) $((c = B$BP))
  _putchar $c
done

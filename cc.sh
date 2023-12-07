#!/bin/ksh

# Sample execution:
#
# $ cat test.c
# #include <stdio.h>
#
# #define NL 10
# #define ZERO '0'
#
# void main() {
#
#   /* print a number to stdout */
#
#   int n = 31416;
#   int p = 1;
#
#   while (p * 10 <= n) p *= 10;
#
#   while (p > 0) {
#     int digit = n / p;
#     putchar(ZERO + digit);
#     n %= p;
#     p /= 10;
#   }
#
#   putchar(NL);
# }
# $ ./cc.sh test.c
# #!/bin/ksh
# push() { SP=$((SP+1)) ; eval STACK$SP=\$$1 ; }
# pop()  { eval $1=\$STACK$SP ; SP=$((SP-1)) ; }
# _putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }
# _main() {
#   push n
#   n=31416
#   push p
#   p=1
#   while [ 0 != $(( p * 10 <= n )) ] ; do
#     : $(( p *= 10 ))
#   done
#   while [ 0 != $(( p > 0 )) ] ; do
#     push digit
#     digit=$(( n / p ))
#     _putchar $(( 48 + digit ))
#     : $(( n %= p ))
#     : $(( p /= 10 ))
#     pop digit
#   done
#   _putchar 10
#   pop p ; pop n
# }
# _main

# Some interesting links:
# https://www.quut.com/c/ANSI-C-grammar-l-1995.html
# https://github.com/rswier/c4/tree/master


get_char()                           # get next char from source into $char
{
  if [ -z "$src_buf" ] ; then        # need to get next line when buffer empty
    IFS=                             # don't split input
    if read -r src_buf ; then        # read next line into $src_buf
      if [ -z "$src_buf" ] ; then    # an empty line implies a newline character
        char=NEWLINE                 # next get_char call will read next line
        return
      fi
    else
      char=EOF                       # EOF reached when read fails
      return
    fi
  else
    src_buf="${src_buf#?}"           # remove the current char from $src_buf
    if [ -z "$src_buf" ] ; then      # end of line if the buffer is now empty
      char=NEWLINE
      return
    fi
  fi

  # current character is at the head of $src_buf

  char="$src_buf"                    # remember current buffer
  rest="${src_buf#?}"                # remove the first char
  char="${char%"$rest"}"             # remove all but first char
}

get_token()                          # get next token from source into $token
{
  value=                             # $value is token's "value", often empty

  while : ; do

    token=$char                      # often the token is a single char
    get_char                         # advance to next char

    case "$token" in

      ' '|NEWLINE)                   # skip whitespace
        while : ; do
          case "$char" in
            ' ') get_char ;;
            NEWLINE) token=$char ; get_char ;;
            *) break ;;
          esac
        done
        if [ NEWLINE = "$token" -a '#' = "$char" ] ; then
          handle_preprocessor_directive
        fi
        ;;

      [0-9])                         # parse integer
        value="$token"
        token=INTEGER
        while : ; do
          case "$char" in
            [0-9])
              value="$value$char"
              get_char
              ;;
            *)
              break
              ;;
          esac
        done
        break
        ;;

      [a-zA-Z_])                     # parse identifier or C keyword
        parse_identifier
        eval macro=\$MACRO_$token    # check if there's a macro by that name
        if [ -n "$macro" ] ; then
          src_buf=" $macro$src_buf"  # inject macro definition in source buffer
          char=" "
        else
          # detect ANSI C keywords
          case "$token" in
            auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|int|long|register|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while)
              ;;
            *)
              value="$token"
              token=IDENTIFIER
              ;;
          esac
          break
        fi
        ;;

      '/')                           # possibly the start of a /*...*/ comment
        if [ '*' = "$char" ] ; then
          get_char
          while : ; do
            prev="$char"
            get_char
            if [ EOF = "$char" -o '*/' = "$prev$char" ] ; then
              break
            fi
          done
          get_char
        else
          case "$char" in
            '=') get_char ; token='/=' ;;
          esac
          break
        fi
        ;;

      "'")                           # parse a character literal
        parse_string_char
        if [ "'" != "$char" ] ; then
          syntax_error "\"'\" missing"
        fi
        value=$code
        token=INTEGER
        get_char
        break
        ;;

      '"')                           # parse a string literal
        value=
        token=STRING
        while : ; do
          if [ '"' = "$char" -o EOF = "$char" ] ; then
            break
          fi
          parse_string_char
          value="$value$code,"
        done
        get_char
#        IFS=' '
#        for c in $value ; do
#          printf \\$(($c/64))$(($c/8%8))$(($c%8))
#        done
#        printf '\n'
        break
        ;;

      '<')
        case "$char" in
          '=') get_char ; token='<=' ;;
          '<') get_char ; token='<<'
               case "$char" in
                 '=') get_char ; token='<<=' ;;
               esac
               ;;
        esac
        break
        ;;

      '>')
        case "$char" in
          '=') get_char ; token='>=' ;;
          '>') get_char ; token='>>'
               case "$char" in
                 '=') get_char ; token='>>=' ;;
               esac
               ;;
        esac
        break
        ;;

      '!')
        case "$char" in
          '=') get_char ; token='!=' ;;
        esac
        break
        ;;

      '=')
        case "$char" in
          '=') get_char ; token='==' ;;
        esac
        break
        ;;

      '&')
        case "$char" in
          '&') get_char ; token='&&' ;;
        esac
        break
        ;;

      '|')
        case "$char" in
          '|') get_char ; token='||' ;;
        esac
        break
        ;;

      '+')
        case "$char" in
          '=') get_char ; token='+=' ;;
        esac
        break
        ;;

      '-')
        case "$char" in
          '=') get_char ; token='-=' ;;
        esac
        break
        ;;

      '*')
        case "$char" in
          '=') get_char ; token='*=' ;;
        esac
        break
        ;;

      '%')
        case "$char" in
          '=') get_char ; token='%=' ;;
        esac
        break
        ;;

      '&')
        case "$char" in
          '=') get_char ; token='&=' ;;
        esac
        break
        ;;

      '^')
        case "$char" in
          '=') get_char ; token='^=' ;;
        esac
        break
        ;;

      '|')
        case "$char" in
          '=') get_char ; token='|=' ;;
        esac
        break
        ;;

      *)                             # all else is treated as single char token
        break                        # (if there is no such token the parser
        ;;                           # will detect it and give an error)

    esac
  done
}

expect_token()
{
  if [ "$1" != "$token" ] ; then
    syntax_error "'$1' missing"
  fi
  get_token
}

handle_preprocessor_directive()
{
  get_char
  skip_blanks
  token=""
  parse_identifier
  case "$token" in

    define)
      skip_blanks
      token=""
      parse_identifier
      if [ '_(' = "_$char" ] ; then
        syntax_error "unsupported preprocessor macro with parameters"
      fi
      skip_blanks
      get_rest_of_line
      eval MACRO_$token=\"\$value \"
      ;;

    include)
      skip_blanks
      get_rest_of_line
      # TODO: include the file "$value"
      ;;

    *)
      syntax_error "unsupported preprocessor directive '#$token'"
      ;;

  esac
}

skip_blanks()
{
  while [ " " = "$char" ] ; do
    get_char
  done
}

get_rest_of_line()
{
  value=""
  while [ NEWLINE != "$char" -a EOF != "$char" ] ; do
    value="$value$char"
    get_char
  done
}

parse_identifier()
{
  while : ; do
    case "$char" in
      [0-9a-zA-Z_])
        token="$token$char"
        get_char
        ;;
      *)
        break
        ;;
    esac
  done
}

parse_string_char()
{
  if [ '\' = "$char" ] ; then
    get_char
    case "$char" in
      'a')
        get_char
        code=7
        ;;
      'b')
        get_char
        code=8
        ;;
      'f')
        get_char
        code=12
        ;;
      'n')
        get_char
        code=10
        ;;
      'r')
        get_char
        code=13
        ;;
      't')
        get_char
        code=9
        ;;
      'v')
        get_char
        code=11
        ;;
      '\')
        get_char
        code=92
        ;;
      '"')
        get_char
        code=34
        ;;
      "'")
        get_char
        code=39
        ;;
      '?')
        get_char
        code=63
        ;;
      [0-7])
        code=$char
        get_char
        for i in 1 2 ; do
          case "$char" in
            [0-7])
              code=$((code*8+$char))
              ;;
            *)
              break
              ;;
          esac
          get_char
        done
        ;;
      'x')
        code=0
        get_char
        for i in 0 1 ; do
          case "$char" in
            [0-9a-fA-F])
              code=$((code*16+0x$char))
              ;;
            *)
              syntax_error "invalid hex escape in string"
              ;;
          esac
          get_char
        done
        ;;
      *)
        syntax_error "invalid escape in string"
        ;;
    esac
  else
    code=$(LC_CTYPE=C printf "%d" "'$char") # convert to integer code
    get_char
  fi
}

HP=0  # heap allocation pointer (0 is reserved for NULL pointer)
SP=0  # stack allocation pointer

ast_alloc()
{
  HP=$((HP+1))
  ast=$HP
}

push() { SP=$((SP+1)) ; eval STACK$SP=\$$1 ; }
pop()  { eval $1=\$STACK$SP ; SP=$((SP-1)) ; }

syntax_error()
{
  >&2 printf "SYNTAX ERROR: %s\n" "$1"
  exit 1
}

missing_feature_error()
{
  >&2 printf "Not yet implemented: %s\n" "$1"
  exit 1
}

constant_fold_unary()
{
  eval kind_x=\$AST_$x
  if [ INTEGER = "$kind_x" ] ; then
    result=$(($op VAL_$x))
    ast_alloc ; eval AST_$ast=INTEGER VAL_$ast=\$result
  fi
}

constant_fold_binary()
{
  eval kind_x=\$AST_$x kind_y=\$AST_$y
  if [ INTEGER/INTEGER = "$kind_x/$kind_y" ] ; then
    eval val_x=\$VAL_$x val_y=\$VAL_$y
    if [ '/' != "$op" -a '%' != "$op" -o 0 != "$val_y" ] ; then
      result=$((val_x $op val_y))
      ast_alloc ; eval AST_$ast=INTEGER VAL_$ast=\$result
    fi
  fi
}

parse_primary_expression()
{
  case "$token" in

    IDENTIFIER)
      ast_alloc ; eval AST_$ast=IDENTIFIER VAL_$ast=\$value
      get_token
      ;;

    INTEGER)
      ast_alloc ; eval AST_$ast=INTEGER VAL_$ast=\$value
      get_token
      ;;

    STRING)
      accum=$value
      get_token
      while [ STRING = "$token" ] ; do  # accumulate contiguous strings
        accum=$accum$value
        get_token
      done
      ast_alloc ; eval AST_$ast=STRING VAL_$ast=\$accum
      ;;

    '(')
      push x
      parse_parenthesized_expression
      x=$ast
      ast_alloc ; eval AST_$ast='\(x\)' X_$ast=\$x
      pop x
      ;;

    *)
      syntax_error "identifier, constant, or '(' expected"
      ;;

  esac
}

parse_parenthesized_expression()
{
  if [ '_(' != "_$token" ] ; then  # expect '('
    syntax_error "'(' missing"
  fi
  get_token                      # skip '('
  parse_comma_expression
  if [ ')' != "$token" ] ; then  # expect ')' after comma_expression
    syntax_error "')' missing"
  fi
  get_token                      # skip ')'
}

parse_postfix_expression()
{
  parse_primary_expression
  while : ; do
    case "$token" in

      '[')
        push ast
        get_token  # skip '['
        parse_comma_expression
        y=$ast
        pop x
        if [ ']' != "$token" ] ; then  # expect ']' after comma_expression
          syntax_error "']' missing"
        fi
        get_token                      # skip ']'
        ast_alloc ; eval AST_$ast='x[y]' X_$ast=\$x Y_$ast=\$y
        ;;

      '(')
        push ast
        get_token  # skip '('
        if [ ')' = "$token" ] ; then
          y=0
        else
          parse_comma_expression
          y=$ast
        fi
        if [ ')' != "$token" ] ; then  # expect ')' after arguments
          syntax_error "')' missing"
        fi
        pop x
        get_token                        # skip ')'
        ast_alloc ; eval AST_$ast='x\(y\)' X_$ast=\$x Y_$ast=\$y
        ;;

      '.')
        syntax_error "TODO 222"
        ;;

      '->')
        syntax_error "TODO 333"
        ;;

      '++'|'--')
        syntax_error "TODO 444"
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_unary_expression()
{
  case "$token" in

    '++'|'--')
      push token
      get_token
      parse_unary_expression
      x=$ast
      pop op
      ast_alloc ; eval AST_$ast=\${op}x X_$ast=\$x
      ;;

    '&'|'*'|'+'|'-'|'~'|'!')
      push token
      get_token
      parse_cast_expression
      x=$ast
      pop op
      ast_alloc ; eval AST_$ast=\${op}x X_$ast=\$x
      case "$op" in
        '+'|'-'|'~'|'!')
          constant_fold_unary
          ;;
      esac
      ;;

    'sizeof')
      missing_feature_error "sizeof"
      ;;

    *)
      parse_postfix_expression
      ;;

  esac
}

parse_cast_expression()
{
  parse_unary_expression
}

parse_multiplicative_expression()
{
  parse_cast_expression
  while : ; do
    case "$token" in

      '*'|'/'|'%')
        push token ; push ast
        get_token  # skip operator
        parse_cast_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_additive_expression()
{
  parse_multiplicative_expression
  while : ; do
    case "$token" in

      '+'|'-')
        push token ; push ast
        get_token  # skip operator
        parse_multiplicative_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_shift_expression()
{
  parse_additive_expression
  while : ; do
    case "$token" in

      '<<'|'>>')
        push token ; push ast
        get_token  # skip operator
        parse_additive_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_relational_expression()
{
  parse_shift_expression
  while : ; do
    case "$token" in

      '<'|'>'|'<='|'>=')
        push token ; push ast
        get_token  # skip operator
        parse_shift_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_equality_expression()
{
  parse_relational_expression
  while : ; do
    case "$token" in

      '=='|'!=')
        push token ; push ast
        get_token  # skip operator
        parse_relational_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_AND_expression()
{
  parse_equality_expression
  while : ; do
    case "$token" in

      '&')
        push token ; push ast
        get_token  # skip operator
        parse_equality_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_exclusive_OR_expression()
{
  parse_AND_expression
  while : ; do
    case "$token" in

      '^')
        push token ; push ast
        get_token  # skip operator
        parse_AND_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_inclusive_OR_expression()
{
  parse_exclusive_OR_expression
  while : ; do
    case "$token" in

      '|')
        push token ; push ast
        get_token  # skip operator
        parse_exclusive_OR_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_logical_AND_expression()
{
  parse_inclusive_OR_expression
  while : ; do
    case "$token" in

      '&&')
        push token ; push ast
        get_token  # skip operator
        parse_inclusive_OR_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_logical_OR_expression()
{
  parse_logical_AND_expression
  while : ; do
    case "$token" in

      '||')
        push token ; push ast
        get_token  # skip operator
        parse_logical_AND_expression
        y=$ast
        pop x ; pop op
        ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
        constant_fold_binary
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_conditional_expression()
{
  parse_logical_OR_expression
  if [ '?' = "$token" ] ; then
    push ast
    get_token  # skip '?'
    parse_comma_expression
    push ast
    if [ ':' != "$token" ] ; then  # expect ':' after middle expression
      syntax_error "':' missing"
    fi
    parse_conditional_expression
    z=$ast
    pop y ; pop x
    ast_alloc ; eval AST_$ast='x?y:z' X_$ast=\$x Y_$ast=\$y Z_$ast=\$z
  fi
}

parse_assignment_expression()
{
  parse_conditional_expression
  case "$token" in

    '='|'+='|'-='|'*='|'/='|'%='|'<<='|'>>='|'&='|'^='|'|=')
      push token ; push ast
      get_token  # skip operator
      parse_assignment_expression
      y=$ast
      pop x ; pop op
      ast_alloc ; eval AST_$ast=x\${op}y X_$ast=\$x Y_$ast=\$y
      ;;

  esac
}

parse_comma_expression()
{
  parse_assignment_expression
  while : ; do
    case "$token" in

      ',')
        push ast
        get_token  # skip ','
        parse_assignment_expression
        y=$ast
        pop x
        ast_alloc ; eval AST_$ast='x,y' X_$ast=\$x Y_$ast=\$y
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_constant_expression()
{
  parse_conditional_expression
  eval kind=\$AST_$ast
  if [ INTEGER != "$kind" ] ; then
    syntax_error "constant expression expected"
  fi
}

parse_comma_expression_opt()
{
  parse_comma_expression
}


parse_type()
{
  type=
  while : ; do
    case "$token" in

      int|char|short|long|signed)
        if [ "$type" != "" -a "$type" != "int" ] ; then
          syntax_error "inconsistent type"
        else
          type=int
          get_token
        fi
        ;;

      unsigned|float|double)
        syntax_error "$token type not supported"
        ;;

      void)
        if [ "$type" != "" ] ; then
          syntax_error "inconsistent type"
        else
          type=void
          get_token
        fi
        ;;

      *)
        break
        ;;

    esac
  done
  if [ "$type" = "" ] ; then
    syntax_error "type expected"
  fi
}


parse_statement()
{
  case "$token" in

    if)
      get_token
      parse_parenthesized_expression
      push ast
      parse_statement
      push ast
      if [ else = "$token" ] ; then
        get_token
        parse_statement
        no=$ast
      else
        no=0
      fi
      pop yes ; pop test
      ast_alloc ; eval AST_$ast=if TEST_$ast=\$test YES_$ast=\$yes NO_$ast=\$no
      ;;

    switch)
      get_token
      parse_parenthesized_expression
      push ast
      parse_statement
      y=$ast
      pop x
      ast_alloc ; eval AST_$ast=switch X_$ast=\$x Y_$ast=\$y
      ;;

    case)
      get_token
      parse_constant_expression
      push ast
      expect_token ':'
      parse_statement
      y=$ast
      pop x
      ast_alloc ; eval AST_$ast=case X_$ast=\$x Y_$ast=\$y
      ;;

    default)
      get_token
      expect_token ':'
      parse_statement
      y=$ast
      x=0
      ast_alloc ; eval AST_$ast=case X_$ast=\$x Y_$ast=\$y
      ;;

    while)
      get_token
      parse_parenthesized_expression
      push ast
      parse_statement
      body=$ast
      pop test
      ast_alloc ; eval AST_$ast=while TEST_$ast=\$test BODY_$ast=\$body
      ;;

    do)
      get_token
      parse_statement
      push ast
      expect_token while
      parse_parenthesized_expression
      expect_token ';'
      y=$ast
      pop x
      ast_alloc ; eval AST_$ast=do X_$ast=\$x Y_$ast=\$y
      ;;

    for)
      get_token
      expect_token '('
      parse_comma_expression_opt
      push ast
      expect_token ';'
      parse_comma_expression_opt
      push ast
      expect_token ';'
      parse_comma_expression_opt
      push ast
      expect_token ')'
      parse_statement
      z=$ast
      pop y
      pop z
      pop ast
      ;;

    goto)
      get_token
      if [ IDENTIFIER != "$token" ] ; then
        syntax_error "identifier expected"
      fi
      ast_alloc ; eval AST_$ast=goto NAME_$ast=\$value
      get_token
      expect_token ';'
      ;;

    continue)
      get_token
      expect_token ';'
      ast_alloc ; eval AST_$ast=continue
      ;;

    break)
      get_token
      expect_token ';'
      ast_alloc ; eval AST_$ast=break
      ;;

    return)
      get_token
      parse_comma_expression_opt
      expect_token ';'
      x=$ast
      ast_alloc ; eval AST_$ast=return X_$ast=\$x
      ;;

    '{')
      parse_compound_statement
      ;;

    *)
      start=$token
      parse_comma_expression_opt
      eval kind=\$AST_$ast
      if [ IDENTIFIER = "$kind" -a '_(' != "_$start" -a ':' = "$token" ] ; then
        get_token  # skip ':'
        parse_statement
      else
        expect_token ';'
        expr=$ast
        ast_alloc ; eval AST_$ast=expr EXPR_$ast=\$expr
      fi
      ;;

  esac
}

parse_compound_statement()
{
  list_construction_begin
  expect_token '{'
  parse_declaration_list_opt
  parse_statement_list_opt
  expect_token '}'
  ast=$list_head
  list_construction_end
}

list_construction_begin()
{
  push list_head ; push list_tail
  list_head=0  # start with an empty list
}

list_construction_end()
{
  pop list_tail ; pop list_head
}

list_add()
{
  ast_alloc ; eval AST_$ast=list ELEM_$ast=$1 NEXT_$ast=
  if [ 0 = "$list_head" ] ; then
    list_head=$ast  # new list cell is the only one in the list
    list_tail=$ast  # so it is both the head and the tail
  else
    eval NEXT_$list_tail=\$ast  # add new list cell to tail
    list_tail=$ast
  fi
}

parse_declaration_list_opt()
{
  while : ; do
    case "$token" in

      int|char|short|long|signed|unsigned|float|double|void)
        parse_definition "body"
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_statement_list_opt()
{
  while [ '}' != "$token" ] ; do
    parse_statement
    list_add $ast
  done
}

parse_translation_unit()
{
  list_construction_begin
  while : ; do
    parse_external_definition
    if [ EOF = "$token" ] ; then
      break
    fi
  done
  ast=$list_head
  list_construction_end
}

parse_stars()
{
  stars=
  while : ; do
    case "$token" in

      '*')
        get_token
        stars="$stars*"
        ;;

      *)
        break
        ;;

    esac
  done
}

parse_external_definition()
{
  parse_definition "global"
}

parse_definition()
{
  push where ; push name
  where=$1

  # use a simplified syntax for definitions

  case "$token" in

    int|char|short|long|signed|unsigned|float|double|void)

      parse_type

      while : ; do
        parse_stars
        if [ IDENTIFIER != "$token" ] ; then
          syntax_error "identifier expected"
        else
          name=$value
          get_token

          if [ '(' = "$token" ] ; then

            if [ "global" != "$where" ] ; then
              syntax_error "function declaration only allowed at top level"
            fi

            get_token
            expect_token ')'  # TODO: parse parameter list
            parse_compound_statement

            body=$ast

            ast_alloc ; eval AST_$ast=fun_decl NAME_$ast=\$name TYPE_$ast=\$stars\$type BODY_$ast=\$body

            list_add $ast

            break

          else

            if [ void = "$type" ] ; then
              syntax_error "variable with void type"
            fi

            val=0  # default is variable with no initializer

            if [ '=' = "$token" ] ; then
              get_token
              parse_conditional_expression  # parse initializer
              val=$ast
            fi

            ast_alloc ; eval AST_$ast=var_decl NAME_$ast=\$name TYPE_$ast=\$stars\$type VAL_$ast=\$val

            list_add $ast

            case "$token" in

              ';')
                get_token
                break
                ;;

              ',')
                get_token
                ;;

              *)
                syntax_error "';' or ',' expected"
                ;;

            esac

          fi

        fi

      done
      ;;

    *)
      syntax_error "type expected"
      ;;

  esac
  pop name ; pop where
}

dump_ast()
{
  push ast ; push kind ; push indent
  what=$1
  ast=$2
  indent="$indent|"

  if [ 0 = "$ast" ] ; then
    printf "$indent$what<none>\n"
  else
    eval kind=\$AST_$ast
#    printf "$indent ast=%s kind=%s\n" "$ast" "$kind"
    case "$kind" in

      IDENTIFIER)
        push val
        eval val=\$VAL_$ast
        printf "$indent$what%s %s\n" "IDENTIFIER" "$val"
        pop val
        ;;

      INTEGER)
        push val
        eval val=\$VAL_$ast
        printf "$indent$what%s %s\n" "INTEGER" "$val"
        pop val
        ;;

      STRING)
        push val
        eval val=\$VAL_$ast
        printf "$indent$what%s %s\n" "STRING" "$val"
        pop val
        ;;

      '++x'|'--x'|'&x'|'*x'|'+x'|'-x'|'~x'|'!x'|'(x)')
        push x
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval x=\$X_$ast
        dump_ast "x= " "$x"
        pop x
        ;;

      'x[y]'|'x(y)'|\
      'x*y'|'x/y'|'x%y'|'x+y'|'x-y'|'x<<y'|'x>>y'|\
      'x<y'|'x>y'|'x<=y'|'x>=y'|'x==y'|'x!=y'|\
      'x&y'|'x|y'|'x^y'|\
      'x&&y'|'x||y'|'x,y')
        push x ; push y
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval x=\$X_$ast
        dump_ast "x= " "$x"
        eval y=\$Y_$ast
        dump_ast "y= " "$y"
        pop y ; pop x
        ;;

      'x=y'|'x+=y'|'x-=y'|'x*=y'|'x/=y'|'x%=y'|\
      'x<<=y'|'x>>=y'|'x&=y'|'x^=y'|'|x=y')
        push x ; push y
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval x=\$X_$ast
        dump_ast "x= " "$x"
        eval y=\$Y_$ast
        dump_ast "y= " "$y"
        pop y ; pop x
        ;;

      'x?y:z')
        push x ; push y ; push z
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval x=\$X_$ast
        dump_ast "x= " "$x"
        eval y=\$Y_$ast
        dump_ast "y= " "$y"
        eval z=\$Z_$ast
        dump_ast "z= " "$z"
        pop z ; pop y ; pop x
        ;;

      if)
        push test ; push yes ; push no
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval test=\$TEST_$ast
        dump_ast "test= " "$test"
        eval yes=\$YES_$ast
        dump_ast "yes= " "$yes"
        eval no=\$NO_$ast
        dump_ast "no= " "$no"
        pop no ; pop yes ; pop test
        ;;

      while)
        push test ; push body
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval test=\$TEST_$ast
        dump_ast "test= " "$test"
        eval body=\$BODY_$ast
        dump_ast "body= " "$body"
        pop body ; pop test
        ;;

      expr)
        push expr
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval expr=\$EXPR_$ast
        dump_ast "expr= " "$expr"
        pop expr
        ;;

      list)
        push elem ; push index
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        index=1
        while [ "" != "$ast" ] ; do
          eval elem=\$ELEM_$ast
          dump_ast "$index= " "$elem"
          eval ast=\$NEXT_$ast
          index=$((index+1))
        done
        pop index ; pop elem
        ;;

      var_decl)
        push name ; push type ; push val
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval name=\$NAME_$ast
        printf "%s%s%s\n" "$indent|" "name= " "$name"
        eval type=\$TYPE_$ast
        printf "%s%s%s\n" "$indent|" "type= " "$type"
        eval val=\$VAL_$ast
        dump_ast "val= " "$val"
        pop val ; pop type ; pop name
        ;;

      fun_decl)
        push name ; push type ; push body
        printf "%s%s%s\n" "$indent" "$what" "$kind"
        eval name=\$NAME_$ast
        printf "%s%s%s\n" "$indent|" "name= " "$name"
        eval type=\$TYPE_$ast
        printf "%s%s%s\n" "$indent|" "type= " "$type"
        eval body=\$BODY_$ast
        dump_ast "body= " "$body"
        pop body ; pop type ; pop name
        ;;

      *)
        printf "$indent$what%s ***UNIMPLEMENTED***\n" "$kind"
        ;;

    esac
  fi
  pop indent ; pop kind ; pop ast
}

comp_translation_unit()
{
  push ast ; push kind ; push elem
  ast=$1
  eval kind=\$AST_$ast

#  dump_ast "ast=" "$ast"

  printf "%s\n" '#!/bin/ksh'

  # runtime system
  printf "%s\n" 'var() { : $((SP = SP+1)) $((_$SP = $1)) $(($1 = $2)) ; }'
  printf "%s\n" 'unvar() { : $(($1 = _$SP)) $((SP = SP-1)) ; }'
  printf "%s\n" '_putchar() { printf \\$(($1/64))$(($1/8%8))$(($1%8)) ; }'

  # a translation unit is a list of declarations
  while [ "" != "$ast" ] ; do
    eval elem=\$ELEM_$ast
    comp_top_decl "$elem"
    eval ast=\$NEXT_$ast
  done

  printf "%s\n" "_main"  # start execution with a call to main function

  pop elem; pop kind ; pop ast
}

comp_top_decl()
{
  push ast ; push kind
  ast=$1
  eval kind=\$AST_$ast
  case "$kind" in

      var_decl)
        push name ; push type ; push val
        eval name=\$NAME_$ast
        eval type=\$TYPE_$ast
        eval val=\$VAL_$ast
        if [ 0 != "$val" ] ; then
          comp_expr "$val"
        else
          result=0  # initialize to 0 by default
        fi
        printf "%s%s\n" "_$name=$result"
        pop val ; pop type ; pop name
        ;;

      fun_decl)
        push name ; push type ; push body ; push local_vars
        local_vars=
        eval name=\$NAME_$ast
        eval type=\$TYPE_$ast
        eval body=\$BODY_$ast
        printf "%s\n" "_$name() {"
        eval k=\$AST_$body
        comp_stmt "$body"
        printf "%s\n" "}"
        pop local_vars ; pop body ; pop type ; pop name
        ;;

  esac
  pop kind ; pop ast
}

comp_stmt()
{
  push ast ; push kind ; push indent
  ast=$1
  eval kind=\$AST_$ast
  indent="$indent  "
  case "$kind" in

      if)
        push test ; push yes ; push no
        eval test=\$TEST_$ast
        comp_test "$test"
        printf "%s%s\n" "$indent" "if $result ; then"
        eval yes=\$YES_$ast
        comp_stmt "$yes"
        eval no=\$NO_$ast
        if [ 0 != "$no" ] ; then
          printf "%s%s\n" "$indent" "else"
          comp_stmt "$no"
        fi
        printf "%s%s\n" "$indent" "fi"
        pop no ; pop yes ; pop test
        ;;

      while)
        push test ; push body
        eval test=\$TEST_$ast
        comp_test "$test"
        printf "%s%s\n" "$indent" "while $result ; do"
        eval body=\$BODY_$ast
        comp_stmt "$body"
        printf "%s%s\n" "$indent" "done"
        pop body ; pop test
        ;;

      expr)
        push expr ; push k
        eval expr=\$EXPR_$ast
        eval k=\$AST_$expr
        case "$k" in
          'x(y)')
            push x ; push y ; push name
            eval x=\$X_$expr
            eval y=\$Y_$expr
            eval k=\$AST_$x
            if [ IDENTIFIER = "$k" ] ; then
              eval name=\$VAL_$x
              if [ putchar = "$name" ] ; then
                comp_expr "$y"
                printf "%s%s\n" "$indent" "_putchar $result"
              else
                echo "error call identifier != putchar"
              fi
            else
              echo "error call non-identifier"
            fi
            pop name ; pop y ; pop x
            ;;
          *)
            comp_expr "$expr"
            printf "%s%s\n" "$indent" ": $result"
        esac
        pop k ; pop expr
        ;;

      var_decl)
        push name ; push type ; push val
        eval name=\$NAME_$ast
        eval type=\$TYPE_$ast
        eval val=\$VAL_$ast
        local_vars="$local_vars $name"
        block_vars="$block_vars $name"
        if [ 0 != "$val" ] ; then
          comp_expr "$val"
        else
          result=0  # initialize to 0 by default
        fi
        printf "%s%s\n" "$indent" "var $name $result"
        pop val ; pop type ; pop name
        ;;

      list)
        pop indent ; push indent # recover previous indentation
        push elem ; push block_vars
        block_vars=
        while [ "" != "$ast" ] ; do
          eval elem=\$ELEM_$ast
          comp_stmt "$elem"
          eval ast=\$NEXT_$ast
        done
        if [ "" != "$block_vars" ] ; then
          pops=
          IFS=" "
          for var in $block_vars ; do
            if [ "" = "$pops" ] ; then
              pops="unvar $var"
            else
              pops="unvar $var ; $pops"
            fi
          done
          printf "%s%s\n" "$indent  " "$pops"
        fi
        pop block_vars ; pop elem
        ;;

  esac
  pop indent ; pop kind ; pop ast
}

comp_test()
{
  push ast ; push kind
  ast=$1
  eval kind=\$AST_$ast
  case "$kind" in

      INTEGER)
        eval result=\$VAL_$ast
        if [ 0 != "$result" ] ; then
          result=":"
          return
        fi
        ;;
  esac
  comp_expr "$ast"
  result="[ 0 != $result ]"
  pop kind ; pop ast
}

comp_expr()
{
  push ast ; push kind
  ast=$1
  eval kind=\$AST_$ast
  case "$kind" in

      IDENTIFIER)
        eval result=\$VAL_$ast
        get_var "$result"
        result="\$$result"
        ;;

      INTEGER)
        eval result=\$VAL_$ast
        ;;

      *)
        comp_subexpr "$ast"
        result="\$(( $result ))"
        ;;

  esac
  pop kind ; pop ast
}

comp_subexpr()
{
  push ast ; push kind
  ast=$1
  eval kind=\$AST_$ast
  case "$kind" in

      IDENTIFIER)
        eval result=\$VAL_$ast
        get_var "$result"
        ;;

      INTEGER)
        eval result=\$VAL_$ast
        ;;

      STRING)
        missing_feature_error "string subexpression"
        ;;

      '++x'|'--x'|'&x'|'*x')
        missing_feature_error "increment/decrement/address/dereference"
        ;;

      '+x'|'-x'|'~x'|'!x')
        push op ; push x
        op="${kind%x}"
        eval x=\$X_$ast
        comp_subexpr $x
        x=$result
        result="$op $x"
        pop x ; pop op
        ;;

      '(x)')
        push x
        eval x=\$X_$ast
        comp_subexpr $x
        x=$result
        result="($x)"
        pop x
        ;;

      'x*y'|'x/y'|'x%y'|'x+y'|'x-y'|'x<<y'|'x>>y'|\
      'x<y'|'x>y'|'x<=y'|'x>=y'|'x==y'|'x!=y'|\
      'x&y'|'x|y'|'x^y')
        push op ; push x ; push y
        op="${kind#x}"
        op="${op%y}"
        eval x=\$X_$ast
        comp_subexpr $x
        push result
        eval y=\$Y_$ast
        comp_subexpr $y
        y=$result
        pop x
        result="$x $op $y"
        pop y ; pop x ; pop op
        ;;

      'x[y]'|'x(y)'|\
      'x&&y'|'x||y'|'x,y')
        missing_feature_error "array subscript/function call/logical operators/comma operator"
        ;;

      'x=y'|'x+=y'|'x-=y'|'x*=y'|'x/=y'|'x%=y'|\
      'x<<=y'|'x>>=y'|'x&=y'|'x^=y'|'|x=y')
        push op ; push x ; push y
        op="${kind#x}"
        op="${op%y}"
        eval x=\$X_$ast
        comp_lvalue $x
        push result
        eval y=\$Y_$ast
        comp_subexpr $y
        y=$result
        pop x
        result="$x $op $y"
        pop y ; pop x ; pop op
        ;;

      'x?y:z')
        missing_feature_error "ternary operator"
        ;;

      *)
        printf "$indent$what%s ***UNIMPLEMENTED***\n" "$kind"
        ;;

    esac

  pop kind ; pop ast
}

get_var()
{
  IFS=" "
  for var in $local_vars ; do
    if [ "$var" = "$1" ] ; then
      result="$1"
      return
    fi
  done
  result="_$1" # global var is prefixed with "_"
}

comp_lvalue()
{
  push ast ; push kind
  ast=$1
  eval kind=\$AST_$ast
  case "$kind" in

      IDENTIFIER)
        eval result=\$VAL_$ast
        get_var "$result"
        ;;

      *)
        missing_feature_error "lvalue compound expression"
        ;;

  esac
  pop kind ; pop ast
}


compile()
{
  src_buf=                # initialize buffer for reading source
  char=NEWLINE            # first character read is at the start of a line
  get_token               # read the first token

  parse_translation_unit  # parse the rest of the file

  comp_translation_unit "$ast"
}

compile < "$1"

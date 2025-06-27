#!/bin/sh
set -e -u -f
LC_ALL=C

_fp=0
_fp_filepath=0
_fp_dirname=0
_include_search_path=0
_output_fd=1
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

defarr() { _malloc $1 $2; }

defarr _include_stack 30
_include_stack_top=0
: $((str = 0))
_putstr() { let str $2
  while [ $((_$str)) != 0 ]; do
    printf \\$(((_$str)/64))$(((_$str)/8%8))$(((_$str)%8))
    : $((str += 1))
  done
  endlet $1 str
}

: $((msg = 0))
_fatal_error() { let msg $2
  _putstr __ $msg
  printf "\n"
  exit 1
  endlet $1 msg
}

: $((msg = 0))
_syntax_error() { let msg $2
  printf "syntax error: "
  _putstr __ $msg
  printf "\n"
  exit 1
  endlet $1 msg
}

_save_include_context() {
  if [ $_include_stack_top -ge $((10 * 3)) ] ; then
    defstr __str_0 "Include stack overflow"
    _fatal_error __ $__str_0
  fi
  if [ $_fp != 0 ] ; then
    : $((_$((_include_stack + _include_stack_top)) = _fp))
    : $((_$((_include_stack + _include_stack_top + 1)) = _fp_filepath))
    : $((_$((_include_stack + _include_stack_top + 2)) = _fp_dirname))
    : $((_include_stack_top += 3))
  fi
}

_restore_include_context() {
  if [ $_include_stack_top = 0 ] ; then
    defstr __str_1 "Include stack is empty"
    _fatal_error __ $__str_1
  fi
  _fclose __ $_fp
  _free __ $_fp_dirname
  : $((_include_stack_top -= 3))
  _fp=$((_$((_include_stack + _include_stack_top))))
  _fp_filepath=$((_$((_include_stack + _include_stack_top + 1))))
  _fp_dirname=$((_$((_include_stack + _include_stack_top + 2))))
}

# Enum declaration
readonly _AUTO_KW=300
readonly _BREAK_KW=301
readonly _CASE_KW=302
readonly _CHAR_KW=303
readonly _CONST_KW=304
readonly _CONTINUE_KW=305
readonly _DEFAULT_KW=306
readonly _DO_KW=307
readonly _DOUBLE_KW=308
readonly _ELSE_KW=309
readonly _ENUM_KW=310
readonly _EXTERN_KW=311
readonly _FLOAT_KW=312
readonly _FOR_KW=313
readonly _GOTO_KW=314
readonly _IF_KW=315
readonly _INLINE_KW=316
readonly _INT_KW=317
readonly _LONG_KW=318
readonly _REGISTER_KW=319
readonly _RETURN_KW=320
readonly _SHORT_KW=321
readonly _SIGNED_KW=322
readonly _SIZEOF_KW=323
readonly _STATIC_KW=324
readonly _STRUCT_KW=325
readonly _SWITCH_KW=326
readonly _TYPEDEF_KW=327
readonly _UNION_KW=328
readonly _UNSIGNED_KW=329
readonly _VOID_KW=330
readonly _VOLATILE_KW=331
readonly _WHILE_KW=332
readonly _INTEGER=401
readonly _INTEGER_HEX=402
readonly _INTEGER_OCT=403
readonly _CHARACTER=410
readonly _STRING=411
readonly _AMP_AMP=450
readonly _AMP_EQ=451
readonly _ARROW=452
readonly _BAR_BAR=453
readonly _BAR_EQ=454
readonly _CARET_EQ=455
readonly _EQ_EQ=456
readonly _GT_EQ=457
readonly _LSHIFT_EQ=458
readonly _LSHIFT=459
readonly _LT_EQ=460
readonly _MINUS_EQ=461
readonly _MINUS_MINUS=462
readonly _EXCL_EQ=463
readonly _PERCENT_EQ=464
readonly _PLUS_EQ=465
readonly _PLUS_PLUS=466
readonly _RSHIFT_EQ=467
readonly _RSHIFT=468
readonly _SLASH_EQ=469
readonly _STAR_EQ=470
readonly _HASH_HASH=471
readonly _PLUS_PLUS_PRE=472
readonly _MINUS_MINUS_PRE=473
readonly _PLUS_PLUS_POST=474
readonly _MINUS_MINUS_POST=475
readonly _ELLIPSIS=476
readonly _PARENS=477
readonly _INITIALIZER_LIST=478
readonly _DECL=479
readonly _DECLS=480
readonly _FUN_DECL=481
readonly _CAST=482
readonly _MACRO_ARG=499
readonly _IDENTIFIER=500
readonly _TYPE=501
readonly _MACRO=502
readonly _LIST=600
_ch=0
_tok=0
_val=0
defarr _string_pool 250000
_string_pool_alloc=0
_string_start=0
_hash=0
defarr _heap 786432
_heap_alloc=1009
_alloc_result=0
_alloc_obj() { # size: $2
  _alloc_result=$_heap_alloc
  : $((_heap_alloc += $2))
  if [ $_heap_alloc -gt 786432 ] ; then
    defstr __str_2 "heap overflow"
    _fatal_error __ $__str_2
  fi
  : $(($1 = _alloc_result))
}

_get_op() { # node: $2
  : $(($1 = _$((_heap + $2)) & 1023))
}

_get_nb_children() { # node: $2
  : $(($1 = _$((_heap + $2)) >> 10))
}

_get_val() { # node: $2
  : $(($1 = _$((_heap + $2 + 1))))
}

_set_val() { # node: $2, val: $3
  : $((_$((_heap + $2 + 1)) = $3))
}

_get_child() { # node: $2, i: $3
  : $(($1 = _$((_heap + $2 + $3 + 1))))
}

_set_child() { # node: $2, i: $3, child: $4
  : $((_$((_heap + $2 + $3 + 1)) = $4))
}

_ast_result=0
_new_ast0() { # op: $2, val: $3
  _alloc_obj _ast_result 2
  : $((_$((_heap + _ast_result)) = $2))
  _set_val __ $_ast_result $3
  : $(($1 = _ast_result))
}

_new_ast1() { # op: $2, child0: $3
  _alloc_obj _ast_result 2
  : $((_$((_heap + _ast_result)) = $2 + 1024))
  _set_child __ $_ast_result 0 $3
  : $(($1 = _ast_result))
}

_new_ast2() { # op: $2, child0: $3, child1: $4
  _alloc_obj _ast_result 3
  : $((_$((_heap + _ast_result)) = $2 + 2048))
  _set_child __ $_ast_result 0 $3
  _set_child __ $_ast_result 1 $4
  : $(($1 = _ast_result))
}

_new_ast3() { # op: $2, child0: $3, child1: $4, child2: $5
  _alloc_obj _ast_result 4
  : $((_$((_heap + _ast_result)) = $2 + 3072))
  _set_child __ $_ast_result 0 $3
  _set_child __ $_ast_result 1 $4
  _set_child __ $_ast_result 2 $5
  : $(($1 = _ast_result))
}

_new_ast4() { # op: $2, child0: $3, child1: $4, child2: $5, child3: $6
  _alloc_obj _ast_result 5
  : $((_$((_heap + _ast_result)) = $2 + 4096))
  _set_child __ $_ast_result 0 $3
  _set_child __ $_ast_result 1 $4
  _set_child __ $_ast_result 2 $5
  _set_child __ $_ast_result 3 $6
  : $(($1 = _ast_result))
}

: $((__t1 = i = nb_children = 0))
_clone_ast() { # orig: $2
  let nb_children; let i; let __t1
  _get_nb_children nb_children $2
  if [ $nb_children = 0 ] ; then
    nb_children=1
  fi
  _alloc_obj _ast_result $((nb_children + 1))
  : $((_$((_heap + _ast_result)) = _$((_heap + $2))))
  i=0
  while [ $i -lt $nb_children ]; do
    _get_child __t1 $2 $i
    _set_child __ $_ast_result $i $__t1
    : $((i += 1))
  done
  : $(($1 = _ast_result))
  endlet $1 __t1 i nb_children
}

_cons() { # child0: $2, child1: $3
  _new_ast2 $1 $_LIST $2 $3
}

_car() { # pair: $2
  _get_child $1 $2 0
}

_cdr() { # pair: $2
  _get_child $1 $2 1
}

_car_() { # expected_op: $2, pair: $3
  _get_child $1 $3 0
}

_cdr_() { # expected_op: $2, pair: $3
  _get_child $1 $3 1
}

_set_car() { # pair: $2, value: $3
  _set_child $1 $2 0 $3
}

_set_cdr() { # pair: $2, value: $3
  _set_child $1 $2 1 $3
}

_list1() { # child0: $2
  _new_ast2 $1 $_LIST $2 0
}

: $((__t1 = 0))
_list2() { # child0: $2, child1: $3
  let __t1
  _new_ast2 __t1 $_LIST $3 0
  _new_ast2 $1 $_LIST $2 $__t1
  endlet $1 __t1
}

: $((__t1 = 0))
_list3() { # child0: $2, child1: $3, child2: $4
  let __t1
  _new_ast2 __t1 $_LIST $4 0
  _new_ast2 __t1 $_LIST $3 $__t1
  _new_ast2 $1 $_LIST $2 $__t1
  endlet $1 __t1
}

: $((__t1 = 0))
_list_singleton() { # list: $2
  let __t1
  if [ $2 != 0 ] && { _cdr_ __t1 $_LIST $2; [ $__t1 = 0 ]; } ; then
    _car $1 $2
  else
    : $(($1 = 0))
  fi
  endlet $1 __t1
}

_begin_string() {
  _string_start=$_string_pool_alloc
  _hash=0
}

_accum_string() {
  _hash=$(((_ch + (_hash ^ 1026)) % 1009))
  : $((_$((_string_pool + _string_pool_alloc)) = _ch))
  : $((_string_pool_alloc += 1))
  if [ $_string_pool_alloc -ge 250000 ] ; then
    defstr __str_3 "string pool overflow"
    _fatal_error __ $__str_3
  fi
}

: $((c = 0))
_accum_string_char() { let c $2
  _hash=$(((c + (_hash ^ 1026)) % 1009))
  : $((_$((_string_pool + _string_pool_alloc)) = c))
  : $((_string_pool_alloc += 1))
  if [ $_string_pool_alloc -ge 250000 ] ; then
    defstr __str_3 "string pool overflow"
    _fatal_error __ $__str_3
  fi
  endlet $1 c
}

: $((string_end = string_start = string_probe = 0))
_accum_string_string() { let string_probe $2
  let string_start; let string_end
  string_start=$((_string_pool + _$((_heap + string_probe + 1))))
  string_end=$((string_start + _$((_heap + string_probe + 4))))
  while [ $string_start -lt $string_end ]; do
    _accum_string_char __ $((_$string_start))
    : $((string_start += 1))
  done
  endlet $1 string_end string_start string_probe
}

: $((n = 0))
_accum_string_integer() { let n $2
  if [ $n -lt 0 ] ; then
    _accum_string_char __ $__MINUS__
    _accum_string_integer __ $((- n))
  else
    if [ $n -gt 9 ] ; then
      _accum_string_integer __ $((n / 10))
    fi
    _accum_string_char __ $((__0__ + (n % 10)))
  fi
  endlet $1 n
}

_probe=0
_probe_start=0
_c1=0
_c2=0
_end_ident_i=0
_end_ident() {
  : $((_$((_string_pool + _string_pool_alloc)) = 0))
  : $((_string_pool_alloc += 1))
  _probe=$((_$((_heap + _hash))))
  while [ $_probe != 0 ]; do
    _probe_start=$((_$((_heap + _probe + 1))))
    _end_ident_i=0
    _c1=$((_$((_string_pool + _string_start + _end_ident_i))))
    _c2=$((_$((_string_pool + _probe_start + _end_ident_i))))
    while [ $_c1 = $_c2 ]; do
      if [ $_c1 = 0 ] ; then
        _string_pool_alloc=$_string_start
        : $(($1 = _probe))
        return
      fi
      : $((_end_ident_i += 1))
      _c1=$((_$((_string_pool + _string_start + _end_ident_i))))
      _c2=$((_$((_string_pool + _probe_start + _end_ident_i))))
    done
    _hash=$_probe
    _probe=$((_$((_heap + _probe))))
  done
  _alloc_obj _probe 5
  : $((_$((_heap + _hash)) = _probe))
  : $((_$((_heap + _probe)) = 0))
  : $((_$((_heap + _probe + 1)) = _string_start))
  : $((_$((_heap + _probe + 2)) = _IDENTIFIER))
  : $((_$((_heap + _probe + 3)) = 0))
  : $((_$((_heap + _probe + 4)) = (_string_pool_alloc - _string_start) - 1))
  : $(($1 = _probe))
}

: $((probe = 0))
_probe_string() { let probe $2
  : $(($1 = _$((_heap + probe + 1))))
  endlet $1 probe
}

defarr _if_macro_stack 20
_if_macro_stack_ix=0
_if_macro_mask=1
_if_macro_executed=0
_expand_macro=1
_expand_macro_arg=1
_skip_newlines=1
defarr _macro_stack 180
_macro_stack_ix=0
_macro_tok_lst=0
_macro_args=0
_macro_ident=0
_macro_args_count=0
_paste_last_token=0
_prev_macro_mask() {
  : $(($1 = (_if_macro_stack_ix == 0) || _$((_if_macro_stack + (_if_macro_stack_ix - 2)))))
}

: $((new_mask = 0))
_push_if_macro_mask() { let new_mask $2
  if [ $_if_macro_stack_ix -ge 20 ] ; then
    defstr __str_4 "Too many nested #ifdef/#ifndef directives. Maximum supported is 20."
    _fatal_error __ $__str_4
  fi
  : $((_$((_if_macro_stack + _if_macro_stack_ix)) = _if_macro_mask))
  : $((_$((_if_macro_stack + _if_macro_stack_ix + 1)) = _if_macro_executed))
  : $((_if_macro_stack_ix += 2))
  new_mask=$((_if_macro_mask & new_mask))
  : $((_if_macro_mask = _if_macro_executed = new_mask))
  endlet $1 new_mask
}

_pop_if_macro_mask() {
  if [ $_if_macro_stack_ix = 0 ] ; then
    defstr __str_5 "Unbalanced #ifdef/#ifndef/#else/#endif directives."
    _fatal_error __ $__str_5
  fi
  : $((_if_macro_stack_ix -= 2))
  _if_macro_mask=$((_$((_if_macro_stack + _if_macro_stack_ix))))
  _if_macro_executed=$((_$((_if_macro_stack + _if_macro_stack_ix + 1))))
}

_get_ch() {
  _fgetc _ch $_fp
  if [ $_ch = -1 ] ; then
    if [ $_include_stack_top != 0 ] ; then
      _restore_include_context __
      _ch=$__NEWLINE__
    fi
  fi
}

: $((i = str = 0))
_strlen() { let str $2
  let i
  i=0
  while [ $((_$((str + i)))) != $__NUL__ ]; do
    : $((i += 1))
  done
  : $(($1 = i))
  endlet $1 i str
}

: $((i = n = src = dest = 0))
_memcpy() { let dest $2; let src $3; let n $4
  let i
  i=0
  while [ $i -lt $n ]; do
    : $((_$((dest + i)) = _$((src + i))))
    : $((i += 1))
  done
  endlet $1 i n src dest
}

: $((temp = len = end = start = str = 0))
_substr() { let str $2; let start $3; let end $4
  let len; let temp
  len=$((end - start))
  _malloc temp $((len + 1))
  _memcpy __ $temp $((str + start)) $len
  : $((_$((temp + len)) = __NUL__))
  : $(($1 = temp))
  endlet $1 temp len end start str
}

: $((temp = s2_len = s1_len = s2 = s1 = 0))
_str_concat() { let s1 $2; let s2 $3
  let s1_len; let s2_len; let temp
  _strlen s1_len $s1
  _strlen s2_len $s2
  _malloc temp $((s1_len + s2_len + 1))
  _memcpy __ $temp $s1 $s1_len
  _memcpy __ $((temp + s1_len)) $s2 $s2_len
  : $((_$((temp + s1_len + s2_len)) = __NUL__))
  : $(($1 = temp))
  endlet $1 temp s2_len s1_len s2 s1
}

: $((last_slash = i = path = 0))
_file_parent_directory() { let path $2
  let i; let last_slash
  i=0
  last_slash=-1
  while [ $((_$((path + i)))) != $__NUL__ ]; do
    if [ $((_$((path + i)))) = $__SLASH__ ] ; then
      last_slash=$i
    fi
    : $((i += 1))
  done
  if [ $last_slash = -1 ] ; then
    _malloc path 1
    : $((_$((path + 0)) = __NUL__))
  else
    _substr path $path 0 $((last_slash + 1))
  fi
  : $(($1 = path))
  endlet $1 last_slash i path
}

: $((relative_to = file_name = 0))
_include_file() { let file_name $2; let relative_to $3
  _save_include_context __
  _fp_filepath=$file_name
  if [ $relative_to != 0 ] ; then
    _str_concat _fp_filepath $relative_to $_fp_filepath
  fi
  defstr __str_6 "r"
  _fopen _fp $_fp_filepath $__str_6
  if [ $_fp = 0 ] ; then
    _putstr __ $_fp_filepath
    printf "\n"
    defstr __str_7 "Could not open file"
    _fatal_error __ $__str_7
  fi
  _file_parent_directory _fp_dirname $_fp_filepath
  endlet $1 relative_to file_name
}

: $((digit = base = 0))
_accum_digit() { let base $2
  let digit
  digit=99
  if [ $__0__ -le $_ch ] && [ $_ch -le $__9__ ] ; then
    digit=$((_ch - __0__))
  elif [ $__A__ -le $_ch ] && [ $_ch -le $__Z__ ] ; then
    digit=$(((_ch - __A__) + 10))
  elif [ $__a__ -le $_ch ] && [ $_ch -le $__z__ ] ; then
    digit=$(((_ch - __a__) + 10))
  fi
  if [ $digit -ge $base ] ; then
    : $(($1 = 0))
  else
    _val=$(((_val * base) - digit))
    _get_ch __
    : $(($1 = 1))
  fi
  endlet $1 digit base
}

: $((__t1 = 0))
_get_string_char() {
  let __t1
  _val=$_ch
  _get_ch __
  if [ $_val = $__BACKSLASH__ ] ; then
    if [ $__0__ -le $_ch ] && [ $_ch -le $__7__ ] ; then
      _val=0
      _accum_digit __ 8
      _accum_digit __ 8
      _accum_digit __ 8
      _val=$((- _val % 256))
    elif [ $_ch = $__x__ ] || [ $_ch = $__X__ ] ; then
      _get_ch __
      _val=0
      if _accum_digit __t1 16; [ $__t1 != 0 ] ; then
        _accum_digit __ 16
      else
        defstr __str_8 "invalid hex escape -- it must have at least one digit"
        _syntax_error __ $__str_8
      fi
      _val=$((- _val % 256))
    else
      if [ $_ch = $__a__ ] ; then
        _val=7
      elif [ $_ch = $__b__ ] ; then
        _val=8
      elif [ $_ch = $__f__ ] ; then
        _val=12
      elif [ $_ch = $__n__ ] ; then
        _val=10
      elif [ $_ch = $__r__ ] ; then
        _val=13
      elif [ $_ch = $__t__ ] ; then
        _val=9
      elif [ $_ch = $__v__ ] ; then
        _val=11
      elif [ $_ch = $__BACKSLASH__ ] || [ $_ch = $__QUOTE__ ] || [ $_ch = $__DQUOTE__ ] ; then
        _val=$_ch
      else
        defstr __str_9 "unimplemented string character escape"
        _syntax_error __ $__str_9
      fi
      _get_ch __
    fi
  fi
  endlet $1 __t1
}

: $((end = 0))
_accum_string_until() { let end $2
  while [ $_ch != $end ] && [ $_ch != -1 ]; do
    _get_string_char __
    _tok=$_ch
    _ch=$_val
    _accum_string __
    _ch=$_tok
  done
  if [ $_ch != $end ] ; then
    defstr __str_10 "unterminated string literal"
    _syntax_error __ $__str_10
  fi
  _get_ch __
  endlet $1 end
}

_IFDEF_ID=0
_IFNDEF_ID=0
_ELIF_ID=0
_ENDIF_ID=0
_DEFINE_ID=0
_UNDEF_ID=0
_INCLUDE_ID=0
_DEFINED_ID=0
_WARNING_ID=0
_ERROR_ID=0
_INCLUDE_SHELL_ID=0
_NOT_SUPPORTED_ID=0
_ARGV__ID=0
_ARGV_ID=0
_IFS_ID=0
_MAIN_ID=0
_PUTCHAR_ID=0
_GETCHAR_ID=0
_EXIT_ID=0
_MALLOC_ID=0
_FREE_ID=0
_PRINTF_ID=0
_FOPEN_ID=0
_FCLOSE_ID=0
_FGETC_ID=0
_PUTSTR_ID=0
_PUTS_ID=0
_READ_ID=0
_WRITE_ID=0
_OPEN_ID=0
_CLOSE_ID=0
_FILE__ID=0
_LINE__ID=0
: $((skip_newlines_prev = prev_macro_mask = prev_expand_macro = 0))
_get_tok_macro() {
  let prev_expand_macro; let prev_macro_mask; let skip_newlines_prev
  prev_expand_macro=$_expand_macro
  prev_macro_mask=$_if_macro_mask
  skip_newlines_prev=$_skip_newlines
  _expand_macro=0
  _if_macro_mask=1
  _skip_newlines=0
  _get_tok __
  _expand_macro=$prev_expand_macro
  _if_macro_mask=$prev_macro_mask
  _skip_newlines=$skip_newlines_prev
  endlet $1 skip_newlines_prev prev_macro_mask prev_expand_macro
}

: $((prev_macro_mask = prev_expand_macro = 0))
_get_tok_macro_expand() {
  let prev_expand_macro; let prev_macro_mask
  prev_expand_macro=$_expand_macro
  prev_macro_mask=$_if_macro_mask
  _expand_macro=0
  _if_macro_mask=1
  _get_tok __
  _expand_macro=$prev_expand_macro
  _if_macro_mask=$prev_macro_mask
  endlet $1 prev_macro_mask prev_expand_macro
}

: $((__t1 = ix = val = tok = args = 0))
_lookup_macro_token() { let args $2; let tok $3; let val $4
  let ix; let __t1
  ix=0
  if [ $tok -lt $_IDENTIFIER ] ; then
    _cons $1 $tok $val
    endlet $1 __t1 ix val tok args
    return
  fi
  while [ $args != 0 ]; do
    if _car __t1 $args; [ $__t1 = $val ] ; then
      break
    fi
    _cdr args $args
    : $((ix += 1))
  done
  if [ $args = 0 ] ; then
    _cons $1 $tok $val
  else
    _cons $1 $_MACRO_ARG $ix
  fi
  endlet $1 __t1 ix val tok args
}

: $((__t1 = tail = toks = args = 0))
_read_macro_tokens() { let args $2
  let toks; let tail; let __t1
  toks=0
  if [ $_tok != $__NEWLINE__ ] && [ $_tok != -1 ] ; then
    _lookup_macro_token __t1 $args $_tok $_val
    _cons toks $__t1 0
    tail=$toks
    _get_tok_macro __
    while [ $_tok != $__NEWLINE__ ] && [ $_tok != -1 ]; do
      _lookup_macro_token __t1 $args $_tok $_val
      _cons __t1 $__t1 0
      _set_cdr __ $tail $__t1
      _cdr tail $tail
      _get_tok_macro __
    done
    if { _car __t1 $toks; _car __t1 $__t1; [ $__t1 = $_HASH_HASH ]; } || { _car __t1 $tail; _car __t1 $__t1; [ $__t1 = $_HASH_HASH ]; } ; then
      defstr __str_11 "'##' cannot appear at either end of a macro expansion"
      _syntax_error __ $__str_11
    fi
  fi
  : $(($1 = toks))
  endlet $1 __t1 tail toks args
}

: $((__t1 = args_count = args = macro = 0))
_handle_define() {
  let macro; let args; let args_count; let __t1
  args=0
  args_count=-1
  if [ $_tok != $_IDENTIFIER ] && [ $_tok != $_MACRO ] && { [ $_tok -lt $_AUTO_KW ] || [ $_tok -gt $_WHILE_KW ]; } ; then
    printf "tok="
    printf "%d" $_tok
    printf "\n"
    defstr __str_12 "#define directive can only be followed by a identifier"
    _syntax_error __ $__str_12
  fi
  : $((_$((_heap + _val + 2)) = _MACRO))
  macro=$_val
  if [ $_ch = $__LPAREN__ ] ; then
    args_count=0
    _get_tok_macro __
    _get_tok_macro __
    while [ $_tok != $__NEWLINE__ ] && [ $_tok != -1 ]; do
      if [ $_tok = $__COMMA__ ] ; then
        _get_tok_macro __
        continue
      elif [ $_tok = $__RPAREN__ ] ; then
        _get_tok_macro __
        break
      fi
      _get_tok_macro __
      _cons args $_val $args
      : $((args_count += 1))
    done
  else
    _get_tok_macro __
  fi
  _read_macro_tokens __t1 $args
  _cons _$((_heap + macro + 3)) $__t1 $args_count
  endlet $1 __t1 args_count args macro
}

: $((__t1 = node = 0))
_non_parenthesized_operand() { let node $2
  let __t1
  while _get_op __t1 $node; [ $__t1 = $_PARENS ]; do
    _get_child node $node 0
  done
  : $(($1 = node))
  endlet $1 __t1 node
}

: $((__t1 = child1 = child0 = op2 = op1 = op = if_macro = expr = 0))
_eval_constant() { let expr $2; let if_macro $3
  let op; let op1; let op2; let child0; let child1; let __t1
  _get_op op $expr
  if _get_nb_children __t1 $expr; [ $__t1 -ge 1 ] ; then
    _get_child child0 $expr 0
  fi
  if _get_nb_children __t1 $expr; [ $__t1 -ge 2 ] ; then
    _get_child child1 $expr 1
  fi
  case $op in
    $_PARENS)
      _eval_constant $1 $child0 $if_macro
    ;;
    $_INTEGER|$_INTEGER_HEX|$_INTEGER_OCT)
      _get_val __t1 $expr
      : $(($1 = -__t1))
    ;;
    $_CHARACTER)
      _get_val $1 $expr
    ;;
    $__TILDE__)
      _eval_constant __t1 $child0 $if_macro
      : $(($1 = ~__t1))
    ;;
    $__EXCL__)
      _eval_constant __t1 $child0 $if_macro
      : $(($1 = !__t1))
    ;;
    $__MINUS__|$__PLUS__)
      _eval_constant op1 $child0 $if_macro
      if _get_nb_children __t1 $expr; [ $__t1 = 1 ] ; then
        : $(($1 = (op == __MINUS__) ? - op1: op1))
      else
        _eval_constant op2 $child1 $if_macro
        : $(($1 = (op == __MINUS__) ? (op1 - op2): (op1 + op2)))
      fi
    ;;
    $__QUESTION__)
      _eval_constant op1 $child0 $if_macro
      if [ $op1 != 0 ] ; then
        _eval_constant $1 $child1 $if_macro
      else
        _get_child __t1 $expr 2
        _eval_constant $1 $__t1 $if_macro
      fi
    ;;
    $__STAR__|$__SLASH__|$__PERCENT__|$__AMP__|$__BAR__|$__CARET__|$_LSHIFT|$_RSHIFT|$_EQ_EQ|$_EXCL_EQ|$_LT_EQ|$_GT_EQ|$__LT__|$__GT__)
      _eval_constant op1 $child0 $if_macro
      _eval_constant op2 $child1 $if_macro
      case $op in
        $__STAR__)
          : $(($1 = op1 * op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__SLASH__)
          : $(($1 = op1 / op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__PERCENT__)
          : $(($1 = op1 % op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__AMP__)
          : $(($1 = op1 & op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__BAR__)
          : $(($1 = op1 | op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__CARET__)
          : $(($1 = op1 ^ op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_LSHIFT)
          : $(($1 = op1 << op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_RSHIFT)
          : $(($1 = op1 >> op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_EQ_EQ)
          : $(($1 = op1 == op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_EXCL_EQ)
          : $(($1 = op1 != op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_LT_EQ)
          : $(($1 = op1 <= op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $_GT_EQ)
          : $(($1 = op1 >= op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__LT__)
          : $(($1 = op1 < op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
        $__GT__)
          : $(($1 = op1 > op2))
          endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
          return
        ;;
      esac
      : $(($1 = 0))
    ;;
    $_AMP_AMP)
      _eval_constant op1 $child0 $if_macro
      if [ $((! op1)) != 0 ] ; then
        : $(($1 = 0))
      else
        _eval_constant $1 $child1 $if_macro
      fi
    ;;
    $_BAR_BAR)
      _eval_constant op1 $child0 $if_macro
      if [ $op1 != 0 ] ; then
        : $(($1 = 1))
      else
        _eval_constant $1 $child1 $if_macro
      fi
    ;;
    $__LPAREN__)
      if [ $if_macro != 0 ] && { _get_val __t1 $child0; [ $__t1 = $_DEFINED_ID ]; } ; then
        : $(($1 = child1 == _MACRO))
      else
        defstr __str_13 "unknown function call in constant expressions"
        _fatal_error __ $__str_13
        : $(($1 = 0))
      fi
    ;;
    $_IDENTIFIER)
      if [ $if_macro != 0 ] ; then
        : $(($1 = 0))
      else
        defstr __str_14 "identifiers are not allowed in constant expression"
        _fatal_error __ $__str_14
        : $(($1 = 0))
      fi
    ;;
    *)
      printf "op="
      printf "%d" $op
      printf "\n"
      defstr __str_15 "unsupported operator in constant expression"
      _fatal_error __ $__str_15
      : $(($1 = 0))
    ;;
  esac
  endlet $1 __t1 child1 child0 op2 op1 op if_macro expr
}

: $((expr = previous_mask = prev_skip_newlines = 0))
_evaluate_if_condition() {
  let prev_skip_newlines; let previous_mask; let expr
  prev_skip_newlines=$_skip_newlines
  previous_mask=$_if_macro_mask
  _if_macro_mask=1
  _skip_newlines=0
  _get_tok __
  _parse_assignment_expression expr
  _if_macro_mask=$previous_mask
  _skip_newlines=$prev_skip_newlines
  _eval_constant $1 $expr 1
  endlet $1 expr previous_mask prev_skip_newlines
}

_handle_include() {
  if [ $_tok = $_STRING ] ; then
    _include_file __ $((_string_pool + _$((_heap + _val + 1)))) $_fp_dirname
    _get_tok_macro __
  elif [ $_tok = $__LT__ ] ; then
    _accum_string_until __ $__GT__
    _end_ident _val
    if [ $_include_search_path != 0 ] ; then
      _include_file __ $((_string_pool + _$((_heap + _val + 1)))) $_include_search_path
    fi
    _get_tok_macro __
  else
    printf "tok="
    printf "%d" $_tok
    printf "\n"
    defstr __str_16 "expected string to #include directive"
    _syntax_error __ $__str_16
  fi
}

: $((__t1 = temp = 0))
_handle_preprocessor_directive() {
  let temp; let __t1
  _get_tok_macro __
  _get_tok_macro __
  if [ $_tok = $_IDENTIFIER ] && { [ $_val = $_IFDEF_ID ] || [ $_val = $_IFNDEF_ID ]; } ; then
    temp=$_val
    _get_tok_macro __
    _push_if_macro_mask __ $(((temp == _IFDEF_ID) ? (_tok == _MACRO): (_tok != _MACRO)))
    _get_tok_macro __
  elif [ $_tok = $_IF_KW ] ; then
    _evaluate_if_condition __t1
    temp=$((__t1 != 0))
    _push_if_macro_mask __ $temp
  elif [ $_tok = $_IDENTIFIER ] && [ $_val = $_ELIF_ID ] ; then
    _evaluate_if_condition __t1
    temp=$((__t1 != 0))
    if { _prev_macro_mask __t1; [ $__t1 != 0 ]; } && [ $((! _if_macro_executed)) != 0 ] ; then
      : $((_if_macro_executed |= temp))
      _if_macro_mask=$temp
    else
      _if_macro_mask=0
    fi
  elif [ $_tok = $_ELSE_KW ] ; then
    if _prev_macro_mask __t1; [ $__t1 != 0 ] ; then
      _if_macro_mask=$((! _if_macro_executed))
      _if_macro_executed=1
    else
      _if_macro_mask=0
    fi
    _get_tok_macro __
  elif [ $_tok = $_IDENTIFIER ] && [ $_val = $_ENDIF_ID ] ; then
    _pop_if_macro_mask __
    _get_tok_macro __
  elif [ $_if_macro_mask != 0 ] ; then
    if [ $_tok = $_IDENTIFIER ] && [ $_val = $_INCLUDE_ID ] ; then
      _get_tok_macro __
      _handle_include __
    elif [ $_tok = $_IDENTIFIER ] && [ $_val = $_INCLUDE_SHELL_ID ] ; then
      _get_tok_macro __
      _handle_shell_include __
    elif [ $_tok = $_IDENTIFIER ] && [ $_val = $_UNDEF_ID ] ; then
      _get_tok_macro __
      if [ $_tok = $_IDENTIFIER ] || [ $_tok = $_MACRO ] ; then
        : $((_$((_heap + _val + 2)) = _IDENTIFIER))
        _get_tok_macro __
      else
        printf "tok="
        printf "%d" $_tok
        printf "\n"
        defstr __str_17 "#undef directive can only be followed by a identifier"
        _syntax_error __ $__str_17
      fi
    elif [ $_tok = $_IDENTIFIER ] && [ $_val = $_DEFINE_ID ] ; then
      _get_tok_macro __
      _handle_define __
    elif [ $_tok = $_IDENTIFIER ] && { [ $_val = $_WARNING_ID ] || [ $_val = $_ERROR_ID ]; } ; then
      temp=$_val
      defstr __str_19 "error:"
      defstr __str_18 "warning:"
      _putstr __ $(((temp == _WARNING_ID) ? __str_18: __str_19))
      while [ $_ch != $__NEWLINE__ ] && [ $_ch != -1 ]; do
        printf \\$((_ch/64))$((_ch/8%8))$((_ch%8))
        _get_ch __
      done
      printf "\n"
      _tok=$__NEWLINE__
      if [ $temp = $_ERROR_ID ] ; then
        exit 1
      fi
    else
      printf "tok="
      printf "%d" $_tok
      printf ": "
      _putstr __ $((_string_pool + _$((_heap + _val + 1))))
      printf "\n"
      defstr __str_20 "unsupported preprocessor directive"
      _syntax_error __ $__str_20
    fi
  else
    while [ $_tok != $__NEWLINE__ ] && [ $_tok != -1 ]; do
      _get_tok_macro __
    done
  fi
  if [ $_tok != $__NEWLINE__ ] && [ $_tok != -1 ] ; then
    printf "tok="
    printf "%d" $_tok
    printf "\n"
    printf "directive="
    printf "%d" $_tok
    printf "\n"
    if [ $_tok = $_IDENTIFIER ] || [ $_tok = $_MACRO ] ; then
      printf "string = "
      _putstr __ $((_string_pool + _$((_heap + _val + 1))))
      printf "\n"
    fi
    defstr __str_21 "preprocessor expected end of line"
    _syntax_error __ $__str_21
  fi
  endlet $1 __t1 temp
}

_get_ident() {
  _begin_string __
  while { [ $__A__ -le $_ch ] && [ $_ch -le $__Z__ ]; } || { [ $__a__ -le $_ch ] && [ $_ch -le $__z__ ]; } || { [ $__0__ -le $_ch ] && [ $_ch -le $__9__ ]; } || [ $_ch = $__UNDERSCORE__ ]; do
    _accum_string __
    _get_ch __
  done
  _end_ident _val
  _tok=$((_$((_heap + _val + 2))))
}

: $((prev_ch = i = name = 0))
_intern_str() { let name $2
  let i; let prev_ch
  i=0
  prev_ch=$_ch
  _begin_string __
  while [ $((_$((name + i)))) != 0 ]; do
    _ch=$((_$((name + i))))
    _accum_string __
    : $((i += 1))
  done
  _end_ident i
  _ch=$prev_ch
  : $(($1 = i))
  endlet $1 prev_ch i name
}

: $((i = name = tok = 0))
_init_ident() { let tok $2; let name $3
  let i
  _intern_str i $name
  : $((_$((_heap + i + 2)) = tok))
  : $(($1 = i))
  endlet $1 i name tok
}

: $((i = 0))
_init_ident_table() {
  let i
  i=0
  while [ $i -lt 1009 ]; do
    : $((_$((_heap + i)) = 0))
    : $((i += 1))
  done
  defstr __str_22 "auto"
  _init_ident __ $_AUTO_KW $__str_22
  defstr __str_23 "break"
  _init_ident __ $_BREAK_KW $__str_23
  defstr __str_24 "case"
  _init_ident __ $_CASE_KW $__str_24
  defstr __str_25 "char"
  _init_ident __ $_CHAR_KW $__str_25
  defstr __str_26 "const"
  _init_ident __ $_CONST_KW $__str_26
  defstr __str_27 "continue"
  _init_ident __ $_CONTINUE_KW $__str_27
  defstr __str_28 "default"
  _init_ident __ $_DEFAULT_KW $__str_28
  defstr __str_29 "do"
  _init_ident __ $_DO_KW $__str_29
  defstr __str_30 "double"
  _init_ident __ $_DOUBLE_KW $__str_30
  defstr __str_31 "else"
  _init_ident __ $_ELSE_KW $__str_31
  defstr __str_32 "enum"
  _init_ident __ $_ENUM_KW $__str_32
  defstr __str_33 "extern"
  _init_ident __ $_EXTERN_KW $__str_33
  defstr __str_34 "float"
  _init_ident __ $_FLOAT_KW $__str_34
  defstr __str_35 "for"
  _init_ident __ $_FOR_KW $__str_35
  defstr __str_36 "goto"
  _init_ident __ $_GOTO_KW $__str_36
  defstr __str_37 "if"
  _init_ident __ $_IF_KW $__str_37
  defstr __str_38 "inline"
  _init_ident __ $_INLINE_KW $__str_38
  defstr __str_39 "int"
  _init_ident __ $_INT_KW $__str_39
  defstr __str_40 "long"
  _init_ident __ $_LONG_KW $__str_40
  defstr __str_41 "register"
  _init_ident __ $_REGISTER_KW $__str_41
  defstr __str_42 "return"
  _init_ident __ $_RETURN_KW $__str_42
  defstr __str_43 "short"
  _init_ident __ $_SHORT_KW $__str_43
  defstr __str_44 "signed"
  _init_ident __ $_SIGNED_KW $__str_44
  defstr __str_45 "sizeof"
  _init_ident __ $_SIZEOF_KW $__str_45
  defstr __str_46 "static"
  _init_ident __ $_STATIC_KW $__str_46
  defstr __str_47 "struct"
  _init_ident __ $_STRUCT_KW $__str_47
  defstr __str_48 "switch"
  _init_ident __ $_SWITCH_KW $__str_48
  defstr __str_49 "typedef"
  _init_ident __ $_TYPEDEF_KW $__str_49
  defstr __str_50 "union"
  _init_ident __ $_UNION_KW $__str_50
  defstr __str_51 "unsigned"
  _init_ident __ $_UNSIGNED_KW $__str_51
  defstr __str_52 "void"
  _init_ident __ $_VOID_KW $__str_52
  defstr __str_53 "volatile"
  _init_ident __ $_VOLATILE_KW $__str_53
  defstr __str_54 "while"
  _init_ident __ $_WHILE_KW $__str_54
  defstr __str_55 "ifdef"
  _init_ident _IFDEF_ID $_IDENTIFIER $__str_55
  defstr __str_56 "ifndef"
  _init_ident _IFNDEF_ID $_IDENTIFIER $__str_56
  defstr __str_57 "elif"
  _init_ident _ELIF_ID $_IDENTIFIER $__str_57
  defstr __str_58 "endif"
  _init_ident _ENDIF_ID $_IDENTIFIER $__str_58
  defstr __str_59 "define"
  _init_ident _DEFINE_ID $_IDENTIFIER $__str_59
  defstr __str_60 "warning"
  _init_ident _WARNING_ID $_IDENTIFIER $__str_60
  defstr __str_61 "error"
  _init_ident _ERROR_ID $_IDENTIFIER $__str_61
  defstr __str_62 "undef"
  _init_ident _UNDEF_ID $_IDENTIFIER $__str_62
  defstr __str_63 "include"
  _init_ident _INCLUDE_ID $_IDENTIFIER $__str_63
  defstr __str_64 "defined"
  _init_ident _DEFINED_ID $_IDENTIFIER $__str_64
  defstr __str_65 "include_shell"
  _init_ident _INCLUDE_SHELL_ID $_IDENTIFIER $__str_65
  defstr __str_66 "argv"
  _init_ident _ARGV_ID $_IDENTIFIER $__str_66
  defstr __str_67 "argv_"
  _init_ident _ARGV__ID $_IDENTIFIER $__str_67
  defstr __str_68 "IFS"
  _init_ident _IFS_ID $_IDENTIFIER $__str_68
  defstr __str_69 "main"
  _init_ident _MAIN_ID $_IDENTIFIER $__str_69
  defstr __str_70 "putchar"
  _init_ident _PUTCHAR_ID $_IDENTIFIER $__str_70
  defstr __str_71 "getchar"
  _init_ident _GETCHAR_ID $_IDENTIFIER $__str_71
  defstr __str_72 "exit"
  _init_ident _EXIT_ID $_IDENTIFIER $__str_72
  defstr __str_73 "malloc"
  _init_ident _MALLOC_ID $_IDENTIFIER $__str_73
  defstr __str_74 "free"
  _init_ident _FREE_ID $_IDENTIFIER $__str_74
  defstr __str_75 "printf"
  _init_ident _PRINTF_ID $_IDENTIFIER $__str_75
  defstr __str_76 "fopen"
  _init_ident _FOPEN_ID $_IDENTIFIER $__str_76
  defstr __str_77 "fclose"
  _init_ident _FCLOSE_ID $_IDENTIFIER $__str_77
  defstr __str_78 "fgetc"
  _init_ident _FGETC_ID $_IDENTIFIER $__str_78
  defstr __str_79 "putstr"
  _init_ident _PUTSTR_ID $_IDENTIFIER $__str_79
  defstr __str_80 "puts"
  _init_ident _PUTS_ID $_IDENTIFIER $__str_80
  defstr __str_81 "read"
  _init_ident _READ_ID $_IDENTIFIER $__str_81
  defstr __str_82 "write"
  _init_ident _WRITE_ID $_IDENTIFIER $__str_82
  defstr __str_83 "open"
  _init_ident _OPEN_ID $_IDENTIFIER $__str_83
  defstr __str_84 "close"
  _init_ident _CLOSE_ID $_IDENTIFIER $__str_84
  defstr __str_85 "NOT_SUPPORTED"
  _init_ident _NOT_SUPPORTED_ID $_IDENTIFIER $__str_85
  endlet $1 i
}

: $((__t1 = macro_id = value = macro_str = 0))
_init_builtin_string_macro() { let macro_str $2; let value $3
  let macro_id; let __t1
  _init_ident macro_id $_MACRO $macro_str
  _intern_str __t1 $value
  _cons __t1 $_STRING $__t1
  _cons __t1 $__t1 0
  _cons _$((_heap + macro_id + 3)) $__t1 -1
  : $(($1 = macro_id))
  endlet $1 __t1 macro_id value macro_str
}

: $((__t1 = macro_id = value = macro_str = 0))
_init_builtin_int_macro() { let macro_str $2; let value $3
  let macro_id; let __t1
  _init_ident macro_id $_MACRO $macro_str
  _cons __t1 $_INTEGER $((- value))
  _cons __t1 $__t1 0
  _cons _$((_heap + macro_id + 3)) $__t1 -1
  : $(($1 = macro_id))
  endlet $1 __t1 macro_id value macro_str
}

: $((macro_id = macro_str = 0))
_init_builtin_empty_macro() { let macro_str $2
  let macro_id
  _init_ident macro_id $_MACRO $macro_str
  _cons _$((_heap + macro_id + 3)) 0 -1
  : $(($1 = macro_id))
  endlet $1 macro_id macro_str
}

_init_pnut_macros() {
  defstr __str_1340 "PNUT_CC"
  _init_builtin_int_macro __ $__str_1340 1
  defstr __str_1359 "__DATE__"
  defstr __str_86 "Jan  1 1970"
  _init_builtin_string_macro __ $__str_1359 $__str_86
  defstr __str_1378 "__TIME__"
  defstr __str_87 "00:00:00"
  _init_builtin_string_macro __ $__str_1378 $__str_87
  defstr __str_1397 "__TIMESTAMP__"
  defstr __str_88 "Jan  1 1970 00:00:00"
  _init_builtin_string_macro __ $__str_1397 $__str_88
  defstr __str_1416 "__FILE__"
  defstr __str_89 "<unknown>"
  _init_builtin_string_macro _FILE__ID $__str_1416 $__str_89
  defstr __str_1430 "__LINE__"
  _init_builtin_int_macro _LINE__ID $__str_1430 0
  defstr __str_1444 "PNUT_SH"
  _init_builtin_int_macro __ $__str_1444 1
}

: $((__t1 = tail = parens_depth = arg_tokens = 0))
_macro_parse_argument() {
  let arg_tokens; let parens_depth; let tail; let __t1
  arg_tokens=0
  parens_depth=0
  while { [ $parens_depth -gt 0 ] || { [ $_tok != $__COMMA__ ] && [ $_tok != $__RPAREN__ ]; }; } && [ $_tok != -1 ]; do
    if [ $_tok = $__LPAREN__ ] ; then
      : $((parens_depth += 1))
    fi
    if [ $_tok = $__RPAREN__ ] ; then
      : $((parens_depth -= 1))
    fi
    if [ $arg_tokens = 0 ] ; then
      _cons __t1 $_tok $_val
      _cons arg_tokens $__t1 0
      tail=$arg_tokens
    else
      _cons __t1 $_tok $_val
      _cons __t1 $__t1 0
      _set_cdr __ $tail $__t1
      _cdr tail $tail
    fi
    _get_tok_macro_expand __
  done
  : $(($1 = arg_tokens))
  endlet $1 __t1 tail parens_depth arg_tokens
}

: $((expected_argc = macro = macro_args_count = 0))
_check_macro_arity() { let macro_args_count $2; let macro $3
  let expected_argc
  _cdr expected_argc $((_$((_heap + macro + 3))))
  if [ $macro_args_count != $expected_argc ] ; then
    printf "expected_argc="
    printf "%d" $expected_argc
    printf " != macro_args_count="
    printf "%d" $macro_args_count
    printf "\n"
    printf "macro="
    _putstr __ $((_string_pool + _$((_heap + macro + 1))))
    printf "\n"
    defstr __str_90 "macro argument count mismatch"
    _syntax_error __ $__str_90
  fi
  endlet $1 expected_argc macro macro_args_count
}

: $((__t1 = prev_is_comma = macro_args_count = args = macro = 0))
_get_macro_args_toks() { let macro $2
  let args; let macro_args_count; let prev_is_comma; let __t1
  args=0
  macro_args_count=0
  prev_is_comma=$((_tok == __COMMA__))
  _get_tok_macro_expand __
  while [ $_tok != $__RPAREN__ ] && [ $_tok != -1 ]; do
    if [ $_tok = $__COMMA__ ] ; then
      _get_tok_macro_expand __
      if [ $prev_is_comma != 0 ] ; then
        _cons args 0 $args
        : $((macro_args_count += 1))
      fi
      prev_is_comma=1
      continue
    else
      prev_is_comma=0
    fi
    _macro_parse_argument __t1
    _cons args $__t1 $args
    : $((macro_args_count += 1))
  done
  if [ $_tok != $__RPAREN__ ] ; then
    defstr __str_91 "unterminated macro argument list"
    _syntax_error __ $__str_91
  fi
  if [ $prev_is_comma != 0 ] ; then
    _cons args 0 $args
    : $((macro_args_count += 1))
  fi
  _check_macro_arity __ $macro_args_count $macro
  : $(($1 = args))
  endlet $1 __t1 prev_is_comma macro_args_count args macro
}

: $((arg = ix = 0))
_get_macro_arg() { let ix $2
  let arg
  arg=$_macro_args
  while [ $ix -gt 0 ]; do
    if [ $arg = 0 ] ; then
      defstr __str_92 "too few arguments to macro"
      _syntax_error __ $__str_92
    fi
    _cdr arg $arg
    : $((ix -= 1))
  done
  _car $1 $arg
  endlet $1 arg ix
}

_return_to_parent_macro() {
  if [ $_macro_stack_ix = 0 ] ; then
    defstr __str_93 "return_to_parent_macro: no parent macro"
    _fatal_error __ $__str_93
  fi
  : $((_macro_stack_ix -= 3))
  _macro_tok_lst=$((_$((_macro_stack + _macro_stack_ix))))
  _macro_args=$((_$((_macro_stack + _macro_stack_ix + 1))))
  _macro_ident=$((_$((_macro_stack + _macro_stack_ix + 2))))
}

: $((args = tokens = ident = 0))
_begin_macro_expansion() { let ident $2; let tokens $3; let args $4
  if [ $((_macro_stack_ix + 3)) -ge 180 ] ; then
    defstr __str_94 "Macro recursion depth exceeded."
    _fatal_error __ $__str_94
  fi
  : $((_$((_macro_stack + _macro_stack_ix)) = _macro_tok_lst))
  : $((_$((_macro_stack + _macro_stack_ix + 1)) = _macro_args))
  : $((_$((_macro_stack + _macro_stack_ix + 2)) = _macro_ident))
  : $((_macro_stack_ix += 3))
  _macro_ident=$ident
  _macro_tok_lst=$tokens
  _macro_args=$args
  endlet $1 args tokens ident
}

: $((i = ident = 0))
_macro_is_already_expanding() { let ident $2
  let i
  i=$_macro_stack_ix
  if [ $ident = 0 ] || [ $_macro_ident = 0 ] ; then
    : $(($1 = 0))
    endlet $1 i ident
    return
  fi
  if [ $ident = $_macro_ident ] ; then
    : $(($1 = 1))
    endlet $1 i ident
    return
  fi
  while [ $i -gt 0 ]; do
    : $((i -= 3))
    if [ $((_$((_macro_stack + i + 2)))) = $ident ] ; then
      : $(($1 = 1))
      endlet $1 i ident
      return
    fi
  done
  : $(($1 = 0))
  endlet $1 i ident
}

: $((__t1 = prev_val = prev_tok = 0))
_undo_token() { let prev_tok $2; let prev_val $3
  let __t1
  _cons __t1 $_tok $_val
  _cons __t1 $__t1 0
  _begin_macro_expansion __ 0 $__t1 0
  _tok=$prev_tok
  _val=$prev_val
  endlet $1 __t1 prev_val prev_tok
}

: $((__t1 = tokens = macro = 0))
_attempt_macro_expansion() { let macro $2
  let tokens; let __t1
  _car tokens $((_$((_heap + macro + 3))))
  if _macro_is_already_expanding __t1 $macro; [ $__t1 != 0 ] ; then
    _tok=$_IDENTIFIER
    _val=$macro
    : $(($1 = 0))
  elif _cdr __t1 $((_$((_heap + macro + 3)))); [ $__t1 = -1 ] ; then
    if [ $macro = $_FILE__ID ] ; then
      _intern_str __t1 $_fp_filepath
      _cons __t1 $_STRING $__t1
      _cons tokens $__t1 0
    fi
    _begin_macro_expansion __ $macro $tokens 0
    : $(($1 = 1))
  else
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $_MACRO $__str_95 0
    if [ $_tok = $__LPAREN__ ] ; then
      _get_macro_args_toks __t1 $macro
      _begin_macro_expansion __ $macro $tokens $__t1
      : $(($1 = 1))
    else
      _undo_token __ $_IDENTIFIER $macro
      : $(($1 = 0))
    fi
  fi
  endlet $1 __t1 tokens macro
}

: $((__t1 = arg = 0))
_stringify() {
  let arg; let __t1
  _expand_macro_arg=0
  _get_tok_macro __
  _expand_macro_arg=1
  if [ $_tok != $_MACRO_ARG ] ; then
    printf "tok="
    printf "%d" $_tok
    printf "\n"
    defstr __str_96 "expected macro argument after #"
    _syntax_error __ $__str_96
  fi
  _get_macro_arg arg $_val
  _tok=$_STRING
  if { { _car __t1 $arg; _car __t1 $__t1; [ $__t1 = $_IDENTIFIER ]; } || { _car __t1 $arg; _car __t1 $__t1; [ $__t1 = $_MACRO ]; } || { { _car __t1 $arg; _car __t1 $__t1; [ $_AUTO_KW -le $__t1 ]; } && { _car __t1 $arg; _car __t1 $__t1; [ $__t1 -le $_WHILE_KW ]; }; }; } && { _cdr __t1 $arg; [ $__t1 = 0 ]; } ; then
    _car __t1 $arg
    _cdr _val $__t1
  else
    _val=$_NOT_SUPPORTED_ID
  fi
  endlet $1 __t1 arg
}

: $((right_digits = result = right_val = left_val = 0))
_paste_integers() { let left_val $2; let right_val $3
  let result; let right_digits
  result=$left_val
  right_digits=$right_val
  while [ $right_digits -gt 0 ]; do
    : $((result *= 10))
    : $((right_digits /= 10))
  done
  : $(($1 = result + right_val))
  endlet $1 right_digits result right_val left_val
}

: $((__t1 = right_val = right_tok = left_val = left_tok = 0))
_paste_tokens() { let left_tok $2; let left_val $3
  let right_tok; let right_val; let __t1
  _expand_macro_arg=0
  _get_tok_macro __
  _expand_macro_arg=1
  if [ $_tok = $_MACRO_ARG ] ; then
    if _get_macro_arg __t1 $_val; [ $__t1 = 0 ] ; then
      _tok=$left_tok
      _val=$left_val
      endlet $1 __t1 right_val right_tok left_val left_tok
      return
    else
      _get_macro_arg __t1 $_val
      _begin_macro_expansion __ 0 $__t1 0
      _get_tok_macro __
    fi
  fi
  right_tok=$_tok
  right_val=$_val
  if [ $left_tok = $_IDENTIFIER ] || [ $left_tok = $_TYPE ] || [ $left_tok = $_MACRO ] || [ $left_tok -le $_WHILE_KW ] ; then
    _begin_string __
    _accum_string_string __ $left_val
    if [ $right_tok = $_IDENTIFIER ] || [ $right_tok = $_TYPE ] || [ $right_tok = $_MACRO ] || [ $right_tok -le $_WHILE_KW ] ; then
      _accum_string_string __ $right_val
    elif [ $right_tok = $_INTEGER ] || [ $right_tok = $_INTEGER_HEX ] || [ $right_tok = $_INTEGER_OCT ] ; then
      _accum_string_integer __ $((- right_val))
    else
      printf "left_tok="
      printf "%d" $left_tok
      printf ", right_tok="
      printf "%d" $right_tok
      printf "\n"
      printf "left="
      _putstr __ $((_string_pool + _$((_heap + left_val + 1))))
      printf "\n"
      defstr __str_97 "cannot paste an identifier with a non-identifier or non-negative integer"
      _syntax_error __ $__str_97
    fi
    _end_ident _val
    _tok=$((_$((_heap + _val + 2))))
  elif [ $left_tok = $_INTEGER ] || [ $left_tok = $_INTEGER_HEX ] || [ $left_tok = $_INTEGER_OCT ] ; then
    if [ $right_tok = $_INTEGER ] || [ $right_tok = $_INTEGER_HEX ] || [ $right_tok = $_INTEGER_OCT ] ; then
      _paste_integers __t1 $((- left_val)) $((- right_val))
      _val=$((-__t1))
    elif [ $right_tok = $_IDENTIFIER ] || [ $right_tok = $_MACRO ] || [ $right_tok -le $_WHILE_KW ] ; then
      _begin_string __
      _accum_string_integer __ $((- left_val))
      _accum_string_string __ $right_val
      _end_ident _val
      _tok=$((_$((_heap + _val + 2))))
    else
      printf "left_tok="
      printf "%d" $left_tok
      printf ", right_tok="
      printf "%d" $right_tok
      printf "\n"
      defstr __str_98 "cannot paste an integer with a non-integer"
      _syntax_error __ $__str_98
    fi
  else
    printf "left_tok="
    printf "%d" $left_tok
    printf ", right_tok="
    printf "%d" $right_tok
    printf "\n"
    defstr __str_99 "cannot paste a non-identifier or non-integer"
    _syntax_error __ $__str_99
  fi
  endlet $1 __t1 right_val right_tok left_val left_tok
}

: $((__t1 = 0))
_get_tok() {
  let __t1
  while :; do
    while [ 1 != 0 ]; do
      if [ $_macro_tok_lst != 0 ] ; then
        _car __t1 $_macro_tok_lst
        _car _tok $__t1
        _car __t1 $_macro_tok_lst
        _cdr _val $__t1
        _cdr _macro_tok_lst $_macro_tok_lst
        if [ $_tok -ge $_IDENTIFIER ] ; then
          _tok=$((_$((_heap + _val + 2))))
        fi
        if [ $_macro_tok_lst != 0 ] && { _car __t1 $_macro_tok_lst; _car __t1 $__t1; [ $__t1 = $_HASH_HASH ]; } ; then
          if [ $_tok = $_MACRO ] || [ $_tok = $_MACRO_ARG ] ; then
            _cdr _macro_tok_lst $_macro_tok_lst
            _paste_last_token=1
          else
            _cdr _macro_tok_lst $_macro_tok_lst
            _paste_tokens __ $_tok $_val
          fi
        elif [ $_macro_tok_lst = 0 ] && [ $_paste_last_token != 0 ] ; then
          if [ $_macro_stack_ix = 0 ] ; then
            defstr __str_100 "## cannot appear at the end of a macro expansion"
            _syntax_error __ $__str_100
          fi
          _return_to_parent_macro __
          _paste_last_token=0
          _paste_tokens __ $_tok $_val
        fi
        if [ $_tok = $_MACRO ] ; then
          if _attempt_macro_expansion __t1 $_val; [ $__t1 != 0 ] ; then
            continue
          fi
          break
        elif [ $_tok = $_MACRO_ARG ] && [ $_expand_macro_arg != 0 ] ; then
          _get_macro_arg __t1 $_val
          _begin_macro_expansion __ 0 $__t1 0
          continue
        elif [ $_tok = $__SHARP__ ] ; then
          _stringify __
          break
        fi
        break
      elif [ $_macro_stack_ix != 0 ] ; then
        _return_to_parent_macro __
        continue
      elif [ $_ch -le $__SPACE__ ] ; then
        if [ $_ch = -1 ] ; then
          _tok=-1
          break
        fi
        _tok=0
        while [ 0 -le $_ch ] && [ $_ch -le $__SPACE__ ]; do
          if [ $_ch = $__NEWLINE__ ] ; then
            _tok=$_ch
          fi
          _get_ch __
        done
        if [ $_tok = $__NEWLINE__ ] && [ $((! _skip_newlines)) != 0 ] ; then
          break
        fi
      elif [ $_tok = $__NEWLINE__ ] && [ $_ch = $__SHARP__ ] ; then
        _tok=0
        _handle_preprocessor_directive __
      elif { [ $__a__ -le $_ch ] && [ $_ch -le $__z__ ]; } || { [ $__A__ -le $_ch ] && [ $_ch -le $__Z__ ]; } || [ $_ch = $__UNDERSCORE__ ] ; then
        _get_ident __
        if [ $_tok = $_MACRO ] ; then
          if [ $_if_macro_mask != 0 ] && [ $_expand_macro != 0 ] ; then
            if _attempt_macro_expansion __t1 $_val; [ $__t1 != 0 ] ; then
              continue
            fi
            break
          fi
        fi
        break
      elif [ $__0__ -le $_ch ] && [ $_ch -le $__9__ ] ; then
        _val=0
        _tok=$_INTEGER
        if [ $_ch = $__0__ ] ; then
          _get_ch __
          if [ $_ch = $__x__ ] || [ $_ch = $__X__ ] ; then
            _tok=$_INTEGER_HEX
            _get_ch __
            if _accum_digit __t1 16; [ $__t1 != 0 ] ; then
              while _accum_digit __t1 16; [ $__t1 != 0 ]; do
                :
              done
            else
              defstr __str_101 "invalid hex integer -- it must have at least one digit"
              _syntax_error __ $__str_101
            fi
          else
            while _accum_digit __t1 8; [ $__t1 != 0 ]; do
              :
            done
            _tok=$(((_val == 0) ? _INTEGER: _INTEGER_OCT))
          fi
        else
          while _accum_digit __t1 10; [ $__t1 != 0 ]; do
            :
          done
        fi
        break
      elif [ $_ch = $__QUOTE__ ] ; then
        _get_ch __
        _get_string_char __
        if [ $_ch != $__QUOTE__ ] ; then
          defstr __str_102 "unterminated character literal"
          _syntax_error __ $__str_102
        fi
        _get_ch __
        _tok=$_CHARACTER
        break
      elif [ $_ch = $__DQUOTE__ ] ; then
        _get_ch __
        _begin_string __
        _accum_string_until __ $__DQUOTE__
        _end_ident _val
        _tok=$_STRING
        break
      else
        _tok=$_ch
        if [ $_ch = $__SLASH__ ] ; then
          _get_ch __
          if [ $_ch = $__STAR__ ] ; then
            _get_ch __
            _tok=$_ch
            while { [ $_tok != $__STAR__ ] || [ $_ch != $__SLASH__ ]; } && [ $_ch != -1 ]; do
              _tok=$_ch
              _get_ch __
            done
            if [ $_ch = -1 ] ; then
              defstr __str_103 "unterminated comment"
              _syntax_error __ $__str_103
            fi
            _get_ch __
          elif [ $_ch = $__SLASH__ ] ; then
            while [ $_ch != $__NEWLINE__ ] && [ $_ch != -1 ]; do
              _get_ch __
            done
          else
            if [ $_ch = $__EQ__ ] ; then
              _get_ch __
              _tok=$_SLASH_EQ
            fi
            break
          fi
        elif [ $_ch = $__AMP__ ] ; then
          _get_ch __
          if [ $_ch = $__AMP__ ] ; then
            _get_ch __
            _tok=$_AMP_AMP
          elif [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_AMP_EQ
          fi
          break
        elif [ $_ch = $__BAR__ ] ; then
          _get_ch __
          if [ $_ch = $__BAR__ ] ; then
            _get_ch __
            _tok=$_BAR_BAR
          elif [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_BAR_EQ
          fi
          break
        elif [ $_ch = $__LT__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_LT_EQ
          elif [ $_ch = $__LT__ ] ; then
            _get_ch __
            if [ $_ch = $__EQ__ ] ; then
              _get_ch __
              _tok=$_LSHIFT_EQ
            else
              _tok=$_LSHIFT
            fi
          fi
          break
        elif [ $_ch = $__GT__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_GT_EQ
          elif [ $_ch = $__GT__ ] ; then
            _get_ch __
            if [ $_ch = $__EQ__ ] ; then
              _get_ch __
              _tok=$_RSHIFT_EQ
            else
              _tok=$_RSHIFT
            fi
          fi
          break
        elif [ $_ch = $__EQ__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_EQ_EQ
          fi
          break
        elif [ $_ch = $__EXCL__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_EXCL_EQ
          fi
          break
        elif [ $_ch = $__PLUS__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_PLUS_EQ
          elif [ $_ch = $__PLUS__ ] ; then
            _get_ch __
            _tok=$_PLUS_PLUS
          fi
          break
        elif [ $_ch = $__MINUS__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_MINUS_EQ
          elif [ $_ch = $__GT__ ] ; then
            _get_ch __
            _tok=$_ARROW
          elif [ $_ch = $__MINUS__ ] ; then
            _get_ch __
            _tok=$_MINUS_MINUS
          fi
          break
        elif [ $_ch = $__STAR__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_STAR_EQ
          fi
          break
        elif [ $_ch = $__PERCENT__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_PERCENT_EQ
          fi
          break
        elif [ $_ch = $__CARET__ ] ; then
          _get_ch __
          if [ $_ch = $__EQ__ ] ; then
            _get_ch __
            _tok=$_CARET_EQ
          fi
          break
        elif [ $_ch = $__SHARP__ ] ; then
          _get_ch __
          if [ $_ch = $__SHARP__ ] ; then
            _get_ch __
            _tok=$_HASH_HASH
          fi
          break
        elif [ $_ch = $__PERIOD__ ] ; then
          _get_ch __
          if [ $_ch = $__PERIOD__ ] ; then
            _get_ch __
            if [ $_ch = $__PERIOD__ ] ; then
              _get_ch __
              _tok=$_ELLIPSIS
            else
              defstr __str_104 "invalid token"
              _syntax_error __ $__str_104
            fi
          else
            _tok=$__PERIOD__
          fi
          break
        elif [ $_ch = $__TILDE__ ] || [ $_ch = $__PERIOD__ ] || [ $_ch = $__QUESTION__ ] || [ $_ch = $__COMMA__ ] || [ $_ch = $__COLON__ ] || [ $_ch = $__SEMICOLON__ ] || [ $_ch = $__LPAREN__ ] || [ $_ch = $__RPAREN__ ] || [ $_ch = $__LBRACK__ ] || [ $_ch = $__RBRACK__ ] || [ $_ch = $__LBRACE__ ] || [ $_ch = $__RBRACE__ ] ; then
          _tok=$_ch
          _get_ch __
          break
        elif [ $_ch = $__BACKSLASH__ ] ; then
          _get_ch __
          if [ $_ch = $__NEWLINE__ ] ; then
            _get_ch __
          else
            printf "ch="
            printf "%d" $_ch
            printf "\n"
            defstr __str_105 "unexpected character after backslash"
            _syntax_error __ $__str_105
          fi
        else
          printf "ch="
          printf "%d" $_ch
          printf "\n"
          defstr __str_104 "invalid token"
          _syntax_error __ $__str_104
        fi
      fi
    done
    [ $((! _if_macro_mask)) != 0 ] || break
  done
  endlet $1 __t1
}

: $((line = file = token = msg = 0))
_parse_error_internal() { let msg $2; let token $3; let file $4; let line $5
  _putstr __ $msg
  exit 1
  endlet $1 line file token msg
}

: $((line = file = expected_tok = 0))
_expect_tok_() { let expected_tok $2; let file $3; let line $4
  if [ $_tok != $expected_tok ] ; then
    printf "expected tok="
    printf "%d" $expected_tok
    printf "\ncurrent tok="
    printf "%d" $_tok
    printf "\n"
    defstr __str_106 "unexpected token"
    _parse_error_internal __ $__str_106 $_tok $file $line
  fi
  _get_tok __
  endlet $1 line file expected_tok
}

: $((__t1 = type_or_decl = 0))
_get_type_specifier() { let type_or_decl $2
  let __t1
  while [ 1 != 0 ]; do
    _get_op __t1 $type_or_decl
    case $__t1 in
      $_DECL)
        _get_child type_or_decl $type_or_decl 1
      ;;
      $__LBRACK__)
        _get_child type_or_decl $type_or_decl 0
      ;;
      $__STAR__)
        _get_child type_or_decl $type_or_decl 0
      ;;
      *)
        : $(($1 = type_or_decl))
        break
      ;;
    esac
  done
  endlet $1 __t1 type_or_decl
}

: $((is_const = parent_type = 0))
_pointer_type() { let parent_type $2; let is_const $3
  _new_ast2 $1 $__STAR__ $((is_const ? (1 << (_CONST_KW - _AUTO_KW)): 0)) $parent_type
  endlet $1 is_const parent_type
}

: $((params = parent_type = 0))
_function_type() { let parent_type $2; let params $3
  _new_ast3 $1 $__LPAREN__ $parent_type $params 0
  endlet $1 params parent_type
}

: $((func_type = 0))
_make_variadic_func() { let func_type $2
  _set_child __ $func_type 2 1
  : $(($1 = func_type))
  endlet $1 func_type
}

: $((__t1 = type = 0))
_is_constant_type() { let type $2
  let __t1
  _get_op __t1 $type
  case $__t1 in
    $__LBRACK__)
      : $(($1 = 0))
    ;;
    $__LPAREN__)
      : $(($1 = 0))
    ;;
    $__STAR__)
      _get_child __t1 $type 0
      : $(($1 = __t1 & (1 << (_CONST_KW - _AUTO_KW))))
    ;;
    *)
      _get_child __t1 $type 0
      : $(($1 = __t1 & (1 << (_CONST_KW - _AUTO_KW))))
    ;;
  esac
  endlet $1 __t1 type
}

: $((tok = 0))
_is_type_starter() { let tok $2
  case $tok in
    $_INT_KW|$_CHAR_KW|$_SHORT_KW|$_LONG_KW|$_VOID_KW|$_FLOAT_KW|$_DOUBLE_KW|$_SIGNED_KW|$_UNSIGNED_KW|$_TYPE|$_CONST_KW|$_VOLATILE_KW|$_ENUM_KW|$_STRUCT_KW|$_UNION_KW|$_TYPEDEF_KW|$_STATIC_KW|$_AUTO_KW|$_REGISTER_KW|$_EXTERN_KW|$_INLINE_KW)
      : $(($1 = 1))
    ;;
    *)
      : $(($1 = 0))
    ;;
  esac
  endlet $1 tok
}

: $((__t1 = last_literal_type = next_value = value = tail = result = ident = name = 0))
_parse_enum() {
  let name; let ident; let result; let tail; let value; let next_value; let last_literal_type; let __t1
  result=0
  value=0
  next_value=0
  last_literal_type=$_INTEGER
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $_ENUM_KW $__str_95 0
  if [ $_tok = $_IDENTIFIER ] || [ $_tok = $_TYPE ] ; then
    _new_ast0 name $_IDENTIFIER $_val
    _get_tok __
  else
    name=0
  fi
  if [ $_tok = $__LBRACE__ ] ; then
    _get_tok __
    while [ $_tok != $__RBRACE__ ]; do
      if [ $_tok != $_IDENTIFIER ] ; then
        defstr __str_107 "identifier expected"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_107 $_tok $__str_95 0
      fi
      _new_ast0 ident $_IDENTIFIER $_val
      _get_tok __
      if [ $_tok = $__EQ__ ] ; then
        _get_tok __
        _parse_assignment_expression value
        if [ $value = 0 ] ; then
          defstr __str_108 "Enum value must be a constant expression"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_108 $_tok $__str_95 0
        fi
        _non_parenthesized_operand value $value
        if { _get_op __t1 $value; [ $__t1 != $_INTEGER ]; } && { _get_op __t1 $value; [ $__t1 != $_INTEGER_HEX ]; } && { _get_op __t1 $value; [ $__t1 != $_INTEGER_OCT ]; } ; then
          _eval_constant __t1 $value 0
          _new_ast0 value $last_literal_type $((-__t1))
        fi
        _get_op last_literal_type $value
        _get_val __t1 $value
        next_value=$((__t1 - 1))
      else
        _new_ast0 value $last_literal_type $next_value
        : $((next_value -= 1))
      fi
      if [ $result = 0 ] ; then
        _new_ast2 __t1 $__EQ__ $ident $value
        _cons result $__t1 0
        tail=$result
      else
        _new_ast2 __t1 $__EQ__ $ident $value
        _cons __t1 $__t1 0
        _set_child __ $tail 1 $__t1
        _get_child tail $tail 1
      fi
      if [ $_tok = $__COMMA__ ] ; then
        _get_tok __
      else
        break
      fi
    done
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__RBRACE__ $__str_95 0
  fi
  _new_ast3 $1 $_ENUM_KW 0 $name $result
  endlet $1 __t1 last_literal_type next_value value tail result ident name
}

: $((__t1 = ends_in_flex_array = tail = result = decl = type_specifier = name = struct_or_union_tok = 0))
_parse_struct_or_union() { let struct_or_union_tok $2
  let name; let type_specifier; let decl; let result; let tail; let ends_in_flex_array; let __t1
  result=0
  ends_in_flex_array=0
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $struct_or_union_tok $__str_95 0
  if [ $_tok = $_IDENTIFIER ] || [ $_tok = $_TYPE ] ; then
    _new_ast0 name $_IDENTIFIER $_val
    _get_tok __
  else
    name=0
  fi
  if [ $_tok = $__LBRACE__ ] ; then
    _get_tok __
    while [ $_tok != $__RBRACE__ ]; do
      if _is_type_starter __t1 $_tok; [ $((!__t1)) != 0 ] ; then
        defstr __str_109 "type expected in struct declaration"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_109 $_tok $__str_95 0
      fi
      if [ $ends_in_flex_array != 0 ] ; then
        defstr __str_110 "flexible array member must be last"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_110 $_tok $__str_95 0
      fi
      _parse_declaration_specifiers type_specifier 0
      if [ $_tok = $__SEMICOLON__ ] ; then
        if { _get_op __t1 $type_specifier; [ $__t1 != $_ENUM_KW ]; } && { _get_op __t1 $type_specifier; [ $__t1 != $_STRUCT_KW ]; } && { _get_op __t1 $type_specifier; [ $__t1 != $_UNION_KW ]; } ; then
          defstr __str_111 "Anonymous struct/union member must be a struct or union type"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_111 $_tok $__str_95 0
        fi
        _new_ast3 decl $_DECL 0 $type_specifier 0
        if [ $result = 0 ] ; then
          _cons result $decl 0
          : $((tail = result))
        else
          _cons __t1 $decl 0
          _set_child __ $tail 1 $__t1
          _get_child tail $tail 1
        fi
      else
        while [ 1 != 0 ]; do
          _parse_declarator decl 0 $type_specifier
          if [ $result = 0 ] ; then
            _cons result $decl 0
            : $((tail = result))
          else
            _cons __t1 $decl 0
            _set_child __ $tail 1 $__t1
            _get_child tail $tail 1
          fi
          if _get_child __t1 $decl 1; [ $__t1 = $_VOID_KW ] ; then
            defstr __str_112 "member with void type not allowed in struct/union"
            defstr __str_95 "pnut.c"
            _parse_error_internal __ $__str_112 $_tok $__str_95 0
          fi
          if { _get_child __t1 $decl 1; [ $__t1 = $__LBRACK__ ]; } && { _get_child __t1 $decl 1; _get_child __t1 $__t1 1; [ $__t1 = 0 ]; } ; then
            ends_in_flex_array=1
            break
          fi
          if [ $_tok = $__COMMA__ ] ; then
            _get_tok __
          else
            break
          fi
        done
      fi
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    done
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__RBRACE__ $__str_95 0
  fi
  _new_ast3 $1 $struct_or_union_tok 0 $name $result
  endlet $1 __t1 ends_in_flex_array tail result decl type_specifier name struct_or_union_tok
}

: $((type_specifier = 0))
_parse_type_specifier() {
  let type_specifier
  type_specifier=0
  case $_tok in
    $_CHAR_KW|$_INT_KW|$_VOID_KW)
      _new_ast0 type_specifier $_tok 0
      _get_tok __
      : $(($1 = type_specifier))
    ;;
    $_SHORT_KW)
      _get_tok __
      if [ $_tok = $_INT_KW ] ; then
        _get_tok __
      fi
      _new_ast0 $1 $_SHORT_KW 0
    ;;
    $_SIGNED_KW)
      _get_tok __
      _parse_type_specifier type_specifier
      if [ $type_specifier = 0 ] ; then
        _new_ast0 type_specifier $_INT_KW 0
      fi
      : $(($1 = type_specifier))
    ;;
    $_LONG_KW)
      _get_tok __
      if [ $_tok = $_LONG_KW ] ; then
        _get_tok __
        if [ $_tok = $_INT_KW ] ; then
          _get_tok __
        fi
        _new_ast0 $1 $_LONG_KW 0
      elif [ $_tok = $_INT_KW ] ; then
        _get_tok __
        _new_ast0 $1 $_INT_KW 0
      else
        _new_ast0 $1 $_INT_KW 0
      fi
    ;;
    *)
      : $(($1 = 0))
    ;;
  esac
  endlet $1 type_specifier
}

_glo_specifier_storage_class=0
: $((__t1 = specifier_storage_class = loop = type_qualifier = type_specifier = allow_typedef = 0))
_parse_declaration_specifiers() { let allow_typedef $2
  let type_specifier; let type_qualifier; let loop; let specifier_storage_class; let __t1
  type_specifier=0
  type_qualifier=0
  loop=1
  specifier_storage_class=0
  while [ $loop != 0 ]; do
    case $_tok in
      $_AUTO_KW|$_REGISTER_KW|$_STATIC_KW|$_EXTERN_KW|$_TYPEDEF_KW)
        if [ $specifier_storage_class != 0 ] ; then
          defstr __str_113 "Multiple storage classes not supported"
          _fatal_error __ $__str_113
        fi
        if [ $_tok = $_TYPEDEF_KW ] && [ $((! allow_typedef)) != 0 ] ; then
          defstr __str_114 "Unexpected typedef"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_114 $_tok $__str_95 0
        fi
        specifier_storage_class=$_tok
        _get_tok __
      ;;
      $_INLINE_KW)
        _get_tok __
      ;;
      $_CONST_KW|$_VOLATILE_KW)
        : $((type_qualifier |= (1 << (_tok - _AUTO_KW))))
        _get_tok __
      ;;
      $_CHAR_KW|$_INT_KW|$_VOID_KW|$_SHORT_KW|$_SIGNED_KW|$_UNSIGNED_KW|$_LONG_KW|$_FLOAT_KW|$_DOUBLE_KW)
        if [ $type_specifier != 0 ] ; then
          defstr __str_115 "Unexpected C type specifier"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_115 $_tok $__str_95 0
        fi
        _parse_type_specifier type_specifier
        if [ $type_specifier = 0 ] ; then
          defstr __str_116 "Failed to parse type specifier"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_116 $_tok $__str_95 0
        fi
      ;;
      $_STRUCT_KW|$_UNION_KW)
        if [ $type_specifier != 0 ] ; then
          defstr __str_117 "Multiple types not supported"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_117 $_tok $__str_95 0
        fi
        _parse_struct_or_union type_specifier $_tok
      ;;
      $_ENUM_KW)
        if [ $type_specifier != 0 ] ; then
          defstr __str_117 "Multiple types not supported"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_117 $_tok $__str_95 0
        fi
        _parse_enum type_specifier
      ;;
      $_TYPE)
        if [ $type_specifier != 0 ] ; then
          defstr __str_117 "Multiple types not supported"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_117 $_tok $__str_95 0
        fi
        _clone_ast type_specifier $((_$((_heap + _val + 3))))
        _get_tok __
      ;;
      *)
        loop=0
      ;;
    esac
  done
  if [ $type_specifier = 0 ] ; then
    defstr __str_118 "Type expected"
    defstr __str_95 "pnut.c"
    _parse_error_internal __ $__str_118 $_tok $__str_95 0
  fi
  if [ $type_qualifier != 0 ] ; then
    if { _get_op __t1 $type_specifier; [ $__t1 = $__LBRACK__ ]; } || { _get_op __t1 $type_specifier; [ $__t1 = $__LPAREN__ ]; } ; then
      defstr __str_119 "Type qualifiers not allowed on typedef'ed array or function type"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_119 $_tok $__str_95 0
    fi
    _get_child __t1 $type_specifier 0
    _set_child __ $type_specifier 0 $((__t1 | type_qualifier))
  fi
  _glo_specifier_storage_class=$specifier_storage_class
  : $(($1 = type_specifier))
  endlet $1 __t1 specifier_storage_class loop type_qualifier type_specifier allow_typedef
}

_parse_param_list_is_variadic=0
: $((__t2 = __t1 = decl = tail = result = 0))
_parse_param_list() {
  let result; let tail; let decl; let __t1; let __t2
  result=0
  _parse_param_list_is_variadic=0
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__LPAREN__ $__str_95 0
  while [ $_tok != $__RPAREN__ ] && [ $_tok != -1 ]; do
    if _is_type_starter __t1 $_tok; [ $__t1 != 0 ] ; then
      _parse_declaration_specifiers __t1 0
      _parse_declarator decl 1 $__t1
      if _get_child __t1 $decl 1; _get_op __t1 $__t1; [ $__t1 = $_VOID_KW ] ; then
        if [ $_tok != $__RPAREN__ ] || [ $result != 0 ] ; then
          defstr __str_120 "void must be the only parameter"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_120 $_tok $__str_95 0
        fi
        break
      fi
    elif [ $_tok = $_IDENTIFIER ] ; then
      _new_ast0 __t1 $_IDENTIFIER $_val
      _new_ast0 __t2 $_INT_KW 0
      _new_ast3 decl $_DECL $__t1 $__t2 0
      _get_tok __
    elif [ $_tok = $_ELLIPSIS ] ; then
      if [ $result = 0 ] ; then
        defstr __str_121 "Function must have a named parameter before ellipsis parameter"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_121 $_tok $__str_95 0
      fi
      _get_tok __
      _parse_param_list_is_variadic=1
      break
    else
      defstr __str_122 "Parameter declaration expected"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_122 $_tok $__str_95 0
    fi
    if [ $_tok = $__COMMA__ ] ; then
      _get_tok __
    fi
    if [ $result = 0 ] ; then
      _cons result $decl 0
      : $((tail = result))
    else
      _cons __t1 $decl 0
      _set_child __ $tail 1 $__t1
      _get_child tail $tail 1
    fi
  done
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__RPAREN__ $__str_95 0
  : $(($1 = result))
  endlet $1 __t2 __t1 decl tail result
}

: $((__t1 = type = 0))
_get_inner_type() { let type $2
  let __t1
  _get_op __t1 $type
  case $__t1 in
    $_DECL|$__STAR__)
      _get_child $1 $type 1
    ;;
    $__LBRACK__|$__LPAREN__)
      _get_child $1 $type 0
    ;;
    *)
      defstr __str_123 "Invalid type"
      _fatal_error __ $__str_123
      : $(($1 = 0))
    ;;
  esac
  endlet $1 __t1 type
}

: $((__t1 = inner_type = parent_type = 0))
_update_inner_type() { let parent_type $2; let inner_type $3
  let __t1
  _get_op __t1 $parent_type
  case $__t1 in
    $_DECL|$__STAR__)
      _set_child __ $parent_type 1 $inner_type
    ;;
    $__LBRACK__|$__LPAREN__)
      _set_child __ $parent_type 0 $inner_type
    ;;
  esac
  endlet $1 __t1 inner_type parent_type
}

_parse_declarator_parent_type_parent=0
: $((__t2 = __t1 = parent_type_parent = arr_size_expr = decl = result = first_tok = parent_type = abstract_decl = 0))
_parse_declarator() { let abstract_decl $2; let parent_type $3
  let first_tok; let result; let decl; let arr_size_expr; let parent_type_parent; let __t1; let __t2
  first_tok=$_tok
  result=0
  case $_tok in
    $_IDENTIFIER)
      _new_ast0 __t1 $_IDENTIFIER $_val
      _new_ast3 result $_DECL $__t1 $parent_type 0
      parent_type_parent=$result
      _get_tok __
    ;;
    $__STAR__)
      _get_tok __
      _pointer_type parent_type_parent $parent_type $((_tok == _CONST_KW))
      if [ $_tok = $_CONST_KW ] ; then
        _get_tok __
      fi
      _parse_declarator result $abstract_decl $parent_type_parent
    ;;
    $__LPAREN__)
      _get_tok __
      _parse_declarator result $abstract_decl $parent_type
      parent_type_parent=$_parse_declarator_parent_type_parent
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RPAREN__ $__str_95 0
    ;;
    *)
      if [ $abstract_decl != 0 ] ; then
        _new_ast3 result $_DECL 0 $parent_type 0
        parent_type_parent=$result
      else
        defstr __str_124 "Invalid declarator, expected an identifier but declarator doesn't have one"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_124 $_tok $__str_95 0
      fi
    ;;
  esac
  decl=$result
  _get_child result $decl 1
  while [ $first_tok != $__STAR__ ]; do
    if [ $_tok = $__LBRACK__ ] ; then
      if _get_op __t1 $result; [ $__t1 = $_VOID_KW ] ; then
        defstr __str_125 "void array not allowed"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_125 $_tok $__str_95 0
      fi
      _get_tok __
      if [ $_tok = $__RBRACK__ ] ; then
        _val=0
      else
        _parse_assignment_expression arr_size_expr
        if [ $arr_size_expr = 0 ] ; then
          defstr __str_126 "Array size must be an integer constant"
          defstr __str_95 "pnut.c"
          _parse_error_internal __ $__str_126 $_tok $__str_95 0
        fi
        _eval_constant _val $arr_size_expr 0
      fi
      _get_inner_type __t1 $parent_type_parent
      _new_ast2 result $__LBRACK__ $__t1 $_val
      _update_inner_type __ $parent_type_parent $result
      parent_type_parent=$result
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RBRACK__ $__str_95 0
    elif [ $_tok = $__LPAREN__ ] ; then
      _get_inner_type __t1 $parent_type_parent
      _parse_param_list __t2
      _new_ast3 result $__LPAREN__ $__t1 $__t2 0
      if [ $_parse_param_list_is_variadic != 0 ] ; then
        _make_variadic_func result $result
      fi
      _update_inner_type __ $parent_type_parent $result
      parent_type_parent=$result
    else
      break
    fi
  done
  _parse_declarator_parent_type_parent=$parent_type_parent
  : $(($1 = decl))
  endlet $1 __t2 __t1 parent_type_parent arr_size_expr decl result first_tok parent_type abstract_decl
}

: $((__t1 = tail = result = 0))
_parse_initializer_list() {
  let result; let tail; let __t1
  result=0
  tail=0
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__LBRACE__ $__str_95 0
  while [ $_tok != $__RBRACE__ ] && [ $_tok != -1 ]; do
    if [ $_tok = $__LBRACE__ ] ; then
      defstr __str_127 "nested initializer lists not supported"
      _fatal_error __ $__str_127
    fi
    if [ $result = 0 ] ; then
      _parse_initializer __t1
      _cons result $__t1 0
      : $((tail = result))
    else
      _parse_initializer __t1
      _cons __t1 $__t1 0
      _set_child __ $tail 1 $__t1
      _get_child tail $tail 1
    fi
    if [ $_tok = $__COMMA__ ] ; then
      _get_tok __
    else
      break
    fi
  done
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__RBRACE__ $__str_95 0
  _new_ast1 $1 $_INITIALIZER_LIST $result
  endlet $1 __t1 tail result
}

_parse_initializer() {
  if [ $_tok = $__LBRACE__ ] ; then
    _parse_initializer_list $1
  else
    _parse_assignment_expression $1
  fi
}

: $((__t1 = declarator = type_specifier = is_for_typedef = 0))
_parse_declarator_and_initializer() { let is_for_typedef $2; let type_specifier $3
  let declarator; let __t1
  _parse_declarator declarator 0 $type_specifier
  if [ $is_for_typedef = 0 ] ; then
    if [ $_tok = $__EQ__ ] ; then
      _get_tok __
      _parse_initializer __t1
      _set_child __ $declarator 2 $__t1
    fi
  fi
  : $(($1 = declarator))
  endlet $1 __t1 declarator type_specifier is_for_typedef
}

: $((__t1 = tail = declarators = first_declarator = type_specifier = is_for_typedef = 0))
_parse_declarators() { let is_for_typedef $2; let type_specifier $3; let first_declarator $4
  let declarators; let tail; let __t1
  _cons declarators $first_declarator 0
  tail=$declarators
  while [ $_tok != $__SEMICOLON__ ]; do
    if [ $_tok = $__COMMA__ ] ; then
      _get_tok __
      _parse_declarator_and_initializer __t1 $is_for_typedef $type_specifier
      _cons __t1 $__t1 0
      _set_child __ $tail 1 $__t1
      _get_child tail $tail 1
    else
      defstr __str_128 "';' or ',' expected"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_128 $_tok $__str_95 0
    fi
  done
  : $(($1 = declarators))
  endlet $1 __t1 tail declarators first_declarator type_specifier is_for_typedef
}

: $((__t1 = decl_type = decl_ident = declarator = 0))
_add_typedef() { let declarator $2
  let decl_ident; let decl_type; let __t1
  _get_child __t1 $declarator 0
  _get_val decl_ident $__t1
  _get_child decl_type $declarator 1
  if { _get_op __t1 $decl_type; [ $__t1 = $_STRUCT_KW ]; } || { _get_op __t1 $decl_type; [ $__t1 = $_UNION_KW ]; } || { _get_op __t1 $decl_type; [ $__t1 = $_ENUM_KW ]; } ; then
    if { _get_child __t1 $decl_type 1; [ $__t1 != 0 ]; } && { _get_child __t1 $decl_type 1; _get_val __t1 $__t1; [ $__t1 != $decl_ident ]; } ; then
      defstr __str_129 "typedef name must match struct/union/enum name"
      _syntax_error __ $__str_129
    fi
    _new_ast0 __t1 $_IDENTIFIER $decl_ident
    _set_child __ $decl_type 1 $__t1
  fi
  : $((_$((_heap + decl_ident + 2)) = _TYPE))
  : $((_$((_heap + decl_ident + 3)) = decl_type))
  endlet $1 __t1 decl_type decl_ident declarator
}

: $((__t1 = params = fun_type = declarator = 0))
_parse_fun_def() { let declarator $2
  let fun_type; let params; let __t1
  _get_child fun_type $declarator 1
  _get_child params $fun_type 1
  while [ $params != 0 ]; do
    if _get_child __t1 $params 0; _get_child __t1 $__t1 0; [ $__t1 = 0 ] ; then
      defstr __str_130 "Parameter name expected"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_130 $_tok $__str_95 0
    fi
    _get_child params $params 1
  done
  if _get_child __t1 $declarator 2; [ $__t1 != 0 ] ; then
    defstr __str_131 "Initializer not allowed in function definition"
    defstr __str_95 "pnut.c"
    _parse_error_internal __ $__str_131 $_tok $__str_95 0
  fi
  _parse_compound_statement __t1
  _new_ast2 $1 $_FUN_DECL $declarator $__t1
  endlet $1 __t1 params fun_type declarator
}

: $((__t1 = type_specifier = declarators = declarator = result = local = 0))
_parse_declaration() { let local $2
  let result; let declarator; let declarators; let type_specifier; let __t1
  _parse_declaration_specifiers type_specifier 1
  if [ $_tok = $__SEMICOLON__ ] ; then
    if { _get_op __t1 $type_specifier; [ $__t1 != $_ENUM_KW ]; } && { _get_op __t1 $type_specifier; [ $__t1 != $_STRUCT_KW ]; } && { _get_op __t1 $type_specifier; [ $__t1 != $_UNION_KW ]; } ; then
      defstr __str_132 "enum/struct/union declaration expected"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_132 $_tok $__str_95 0
    fi
    if [ $_glo_specifier_storage_class = $_TYPEDEF_KW ] ; then
      _new_ast3 __t1 $_DECL 0 $type_specifier 0
      _add_typedef __ $__t1
    fi
    result=$type_specifier
  elif [ $_glo_specifier_storage_class = $_TYPEDEF_KW ] ; then
    _parse_declarator_and_initializer declarator 1 $type_specifier
    _parse_declarators declarators 1 $type_specifier $declarator
    type_specifier=$declarators
    while [ $declarators != 0 ]; do
      _get_child __t1 $declarators 0
      _add_typedef __ $__t1
      _get_child declarators $declarators 1
    done
    _new_ast1 result $_TYPEDEF_KW $type_specifier
  else
    _parse_declarator_and_initializer declarator 0 $type_specifier
    if { _get_child __t1 $declarator 1; _get_op __t1 $__t1; [ $__t1 = $__LPAREN__ ]; } && [ $_tok = $__LBRACE__ ] ; then
      if [ $local != 0 ] ; then
        defstr __str_133 "Function definition not allowed in local scope"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_133 $_tok $__str_95 0
      fi
      _parse_fun_def $1 $declarator
      endlet $1 __t1 type_specifier declarators declarator result local
      return
    fi
    _parse_declarators declarators 0 $type_specifier $declarator
    _new_ast2 result $_DECLS $declarators $_glo_specifier_storage_class
  fi
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__SEMICOLON__ $__str_95 0
  : $(($1 = result))
  endlet $1 __t1 type_specifier declarators declarator result local
}

: $((result = first_par_already_consumed = 0))
_parse_parenthesized_expression() { let first_par_already_consumed $2
  let result
  if [ $((! first_par_already_consumed)) != 0 ] ; then
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__LPAREN__ $__str_95 0
  fi
  _parse_comma_expression result
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__RPAREN__ $__str_95 0
  _new_ast1 $1 $_PARENS $result
  endlet $1 result first_par_already_consumed
}

: $((__t1 = tail = result = 0))
_parse_primary_expression() {
  let result; let tail; let __t1
  result=0
  if [ $_tok = $_IDENTIFIER ] || [ $_tok = $_CHARACTER ] || [ $_tok = $_INTEGER ] || [ $_tok = $_INTEGER_HEX ] || [ $_tok = $_INTEGER_OCT ] ; then
    _new_ast0 result $_tok $_val
    _get_tok __
  elif [ $_tok = $_STRING ] ; then
    _new_ast0 result $_STRING $_val
    _get_tok __
    if [ $_tok = $_STRING ] ; then
      _get_val __t1 $result
      _cons result $__t1 0
      tail=$result
      while [ $_tok = $_STRING ]; do
        _cons __t1 $_val 0
        _set_cdr __ $tail $__t1
        _cdr tail $tail
        _get_tok __
      done
      _begin_string __
      while [ $result != 0 ]; do
        _car __t1 $result
        _accum_string_string __ $__t1
        _cdr result $result
      done
      _end_ident __t1
      _new_ast0 result $_STRING $__t1
    fi
  elif [ $_tok = $__LPAREN__ ] ; then
    _parse_parenthesized_expression result 0
  else
    defstr __str_134 "identifier, literal, or '(' expected"
    defstr __str_95 "pnut.c"
    _parse_error_internal __ $__str_134 $_tok $__str_95 0
  fi
  : $(($1 = result))
  endlet $1 __t1 tail result
}

: $((__t1 = child = result = 0))
_parse_postfix_expression() { let result $2
  let child; let __t1
  if [ $result = 0 ] ; then
    _parse_primary_expression result
  fi
  while [ 1 != 0 ]; do
    if [ $_tok = $__LBRACK__ ] ; then
      _get_tok __
      _parse_comma_expression child
      _new_ast2 result $__LBRACK__ $result $child
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RBRACK__ $__str_95 0
    elif [ $_tok = $__LPAREN__ ] ; then
      _get_tok __
      if [ $_tok = $__RPAREN__ ] ; then
        child=0
      else
        _parse_call_params child
      fi
      _new_ast2 result $__LPAREN__ $result $child
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RPAREN__ $__str_95 0
    elif [ $_tok = $__PERIOD__ ] ; then
      _get_tok __
      if [ $_tok != $_IDENTIFIER ] ; then
        defstr __str_107 "identifier expected"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_107 $_tok $__str_95 0
      fi
      _new_ast0 __t1 $_IDENTIFIER $_val
      _new_ast2 result $__PERIOD__ $result $__t1
      _get_tok __
    elif [ $_tok = $_ARROW ] ; then
      _get_tok __
      if [ $_tok != $_IDENTIFIER ] ; then
        defstr __str_107 "identifier expected"
        defstr __str_95 "pnut.c"
        _parse_error_internal __ $__str_107 $_tok $__str_95 0
      fi
      _new_ast0 __t1 $_IDENTIFIER $_val
      _new_ast2 result $_ARROW $result $__t1
      _get_tok __
    elif [ $_tok = $_PLUS_PLUS ] ; then
      _get_tok __
      _new_ast1 result $_PLUS_PLUS_POST $result
    elif [ $_tok = $_MINUS_MINUS ] ; then
      _get_tok __
      _new_ast1 result $_MINUS_MINUS_POST $result
    else
      break
    fi
  done
  : $(($1 = result))
  endlet $1 __t1 child result
}

: $((__t1 = op = result = 0))
_parse_unary_expression() {
  let result; let op; let __t1
  if [ $_tok = $_PLUS_PLUS ] ; then
    _get_tok __
    _parse_unary_expression result
    _new_ast1 result $_PLUS_PLUS_PRE $result
  elif [ $_tok = $_MINUS_MINUS ] ; then
    _get_tok __
    _parse_unary_expression result
    _new_ast1 result $_MINUS_MINUS_PRE $result
  elif [ $_tok = $__AMP__ ] || [ $_tok = $__STAR__ ] || [ $_tok = $__PLUS__ ] || [ $_tok = $__MINUS__ ] || [ $_tok = $__TILDE__ ] || [ $_tok = $__EXCL__ ] ; then
    op=$_tok
    _get_tok __
    _parse_cast_expression result
    _new_ast1 result $op $result
  elif [ $_skip_newlines != 0 ] && [ $_tok = $_SIZEOF_KW ] ; then
    _get_tok __
    if [ $_tok = $__LPAREN__ ] ; then
      _get_tok __
      if _is_type_starter __t1 $_tok; [ $__t1 != 0 ] ; then
        _parse_declaration_specifiers __t1 0
        _parse_declarator result 1 $__t1
        defstr __str_95 "pnut.c"
        _expect_tok_ __ $__RPAREN__ $__str_95 0
      else
        _parse_parenthesized_expression __t1 1
        _parse_postfix_expression result $__t1
      fi
    else
      _parse_unary_expression result
    fi
    _new_ast1 result $_SIZEOF_KW $result
  elif [ $((! _skip_newlines)) != 0 ] && [ $_tok = $_IDENTIFIER ] && [ $_val = $_DEFINED_ID ] ; then
    _get_tok_macro __
    if [ $_tok = $__LPAREN__ ] ; then
      _get_tok_macro __
      _new_ast0 __t1 $_IDENTIFIER $_DEFINED_ID
      _new_ast2 result $__LPAREN__ $__t1 $_tok
      _get_tok_macro __
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RPAREN__ $__str_95 0
    elif [ $_tok = $_IDENTIFIER ] || [ $_tok = $_MACRO ] ; then
      _new_ast0 __t1 $_IDENTIFIER $_DEFINED_ID
      _new_ast2 result $__LPAREN__ $__t1 $_tok
      _get_tok_macro __
    else
      defstr __str_135 "identifier or '(' expected"
      defstr __str_95 "pnut.c"
      _parse_error_internal __ $__str_135 $_tok $__str_95 0
      : $(($1 = 0))
      endlet $1 __t1 op result
      return
    fi
  else
    _parse_postfix_expression result 0
  fi
  : $(($1 = result))
  endlet $1 __t1 op result
}

: $((__t1 = type = result = 0))
_parse_cast_expression() {
  let result; let type; let __t1
  if [ $_tok = $__LPAREN__ ] ; then
    _get_tok __
    if _is_type_starter __t1 $_tok; [ $__t1 != 0 ] ; then
      _parse_declaration_specifiers __t1 0
      _parse_declarator type 1 $__t1
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__RPAREN__ $__str_95 0
      _parse_cast_expression __t1
      _new_ast2 result $_CAST $type $__t1
      : $(($1 = result))
    else
      _parse_parenthesized_expression __t1 1
      _parse_postfix_expression $1 $__t1
      endlet $1 __t1 type result
      return
    fi
  else
    _parse_unary_expression $1
    endlet $1 __t1 type result
    return
  fi
  endlet $1 __t1 type result
}

: $((op = child = result = 0))
_parse_multiplicative_expression() {
  let result; let child; let op
  _parse_cast_expression result
  while [ $_tok = $__STAR__ ] || [ $_tok = $__SLASH__ ] || [ $_tok = $__PERCENT__ ]; do
    op=$_tok
    _get_tok __
    _parse_cast_expression child
    _new_ast2 result $op $result $child
  done
  : $(($1 = result))
  endlet $1 op child result
}

: $((op = child = result = 0))
_parse_additive_expression() {
  let result; let child; let op
  _parse_multiplicative_expression result
  while [ $_tok = $__PLUS__ ] || [ $_tok = $__MINUS__ ]; do
    op=$_tok
    _get_tok __
    _parse_multiplicative_expression child
    _new_ast2 result $op $result $child
  done
  : $(($1 = result))
  endlet $1 op child result
}

: $((op = child = result = 0))
_parse_shift_expression() {
  let result; let child; let op
  _parse_additive_expression result
  while [ $_tok = $_LSHIFT ] || [ $_tok = $_RSHIFT ]; do
    op=$_tok
    _get_tok __
    _parse_additive_expression child
    _new_ast2 result $op $result $child
  done
  : $(($1 = result))
  endlet $1 op child result
}

: $((op = child = result = 0))
_parse_relational_expression() {
  let result; let child; let op
  _parse_shift_expression result
  while [ $_tok = $__LT__ ] || [ $_tok = $__GT__ ] || [ $_tok = $_LT_EQ ] || [ $_tok = $_GT_EQ ]; do
    op=$_tok
    _get_tok __
    _parse_shift_expression child
    _new_ast2 result $op $result $child
  done
  : $(($1 = result))
  endlet $1 op child result
}

: $((op = child = result = 0))
_parse_equality_expression() {
  let result; let child; let op
  _parse_relational_expression result
  while [ $_tok = $_EQ_EQ ] || [ $_tok = $_EXCL_EQ ]; do
    op=$_tok
    _get_tok __
    _parse_relational_expression child
    _new_ast2 result $op $result $child
  done
  : $(($1 = result))
  endlet $1 op child result
}

: $((child = result = 0))
_parse_AND_expression() {
  let result; let child
  _parse_equality_expression result
  while [ $_tok = $__AMP__ ]; do
    _get_tok __
    _parse_equality_expression child
    _new_ast2 result $__AMP__ $result $child
  done
  : $(($1 = result))
  endlet $1 child result
}

: $((child = result = 0))
_parse_exclusive_OR_expression() {
  let result; let child
  _parse_AND_expression result
  while [ $_tok = $__CARET__ ]; do
    _get_tok __
    _parse_AND_expression child
    _new_ast2 result $__CARET__ $result $child
  done
  : $(($1 = result))
  endlet $1 child result
}

: $((child = result = 0))
_parse_inclusive_OR_expression() {
  let result; let child
  _parse_exclusive_OR_expression result
  while [ $_tok = $__BAR__ ]; do
    _get_tok __
    _parse_exclusive_OR_expression child
    _new_ast2 result $__BAR__ $result $child
  done
  : $(($1 = result))
  endlet $1 child result
}

: $((child = result = 0))
_parse_logical_AND_expression() {
  let result; let child
  _parse_inclusive_OR_expression result
  while [ $_tok = $_AMP_AMP ]; do
    _get_tok __
    _parse_inclusive_OR_expression child
    _new_ast2 result $_AMP_AMP $result $child
  done
  : $(($1 = result))
  endlet $1 child result
}

: $((child = result = 0))
_parse_logical_OR_expression() {
  let result; let child
  _parse_logical_AND_expression result
  while [ $_tok = $_BAR_BAR ]; do
    _get_tok __
    _parse_logical_AND_expression child
    _new_ast2 result $_BAR_BAR $result $child
  done
  : $(($1 = result))
  endlet $1 child result
}

: $((child2 = child1 = result = 0))
_parse_conditional_expression() {
  let result; let child1; let child2
  _parse_logical_OR_expression result
  if [ $_tok = $__QUESTION__ ] ; then
    _get_tok __
    _parse_comma_expression child1
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__COLON__ $__str_95 0
    _parse_conditional_expression child2
    _new_ast3 result $__QUESTION__ $result $child1 $child2
  fi
  : $(($1 = result))
  endlet $1 child2 child1 result
}

: $((op = child = result = 0))
_parse_assignment_expression() {
  let result; let child; let op
  _parse_conditional_expression result
  if [ $_tok = $__EQ__ ] || [ $_tok = $_PLUS_EQ ] || [ $_tok = $_MINUS_EQ ] || [ $_tok = $_STAR_EQ ] || [ $_tok = $_SLASH_EQ ] || [ $_tok = $_PERCENT_EQ ] || [ $_tok = $_LSHIFT_EQ ] || [ $_tok = $_RSHIFT_EQ ] || [ $_tok = $_AMP_EQ ] || [ $_tok = $_CARET_EQ ] || [ $_tok = $_BAR_EQ ] ; then
    op=$_tok
    _get_tok __
    _parse_assignment_expression child
    _new_ast2 result $op $result $child
  fi
  : $(($1 = result))
  endlet $1 op child result
}

: $((__t1 = result = 0))
_parse_comma_expression() {
  let result; let __t1
  _parse_assignment_expression result
  if [ $_tok = $__COMMA__ ] ; then
    _get_tok __
    _new_ast2 result $__COMMA__ $result 0
    _parse_comma_expression __t1
    _set_child __ $result 1 $__t1
  fi
  : $(($1 = result))
  endlet $1 __t1 result
}

: $((__t1 = result = 0))
_parse_call_params() {
  let result; let __t1
  _parse_assignment_expression result
  _new_ast2 result $_LIST $result 0
  if [ $_tok = $__COMMA__ ] ; then
    _get_tok __
    _parse_call_params __t1
    _set_child __ $result 1 $__t1
  fi
  : $(($1 = result))
  endlet $1 __t1 result
}

: $((result = 0))
_parse_comma_expression_opt() {
  let result
  if [ $_tok = $__COLON__ ] || [ $_tok = $__SEMICOLON__ ] || [ $_tok = $__RPAREN__ ] ; then
    result=0
  else
    _parse_comma_expression result
  fi
  : $(($1 = result))
  endlet $1 result
}

_parse_expression() {
  _parse_comma_expression $1
}

_parse_constant_expression() {
  _parse_expression $1
}

: $((__t1 = start_tok = child3 = child2 = child1 = result = 0))
_parse_statement() {
  let result; let child1; let child2; let child3; let start_tok; let __t1
  if [ $_tok = $_IF_KW ] ; then
    _get_tok __
    _parse_parenthesized_expression result 0
    _parse_statement child1
    if [ $_tok = $_ELSE_KW ] ; then
      _get_tok __
      _parse_statement child2
    else
      child2=0
    fi
    _new_ast3 result $_IF_KW $result $child1 $child2
  elif [ $_tok = $_SWITCH_KW ] ; then
    _get_tok __
    _parse_parenthesized_expression result 0
    _parse_statement child1
    _new_ast2 result $_SWITCH_KW $result $child1
  elif [ $_tok = $_CASE_KW ] ; then
    _get_tok __
    _parse_constant_expression result
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__COLON__ $__str_95 0
    _parse_statement child1
    _new_ast2 result $_CASE_KW $result $child1
  elif [ $_tok = $_DEFAULT_KW ] ; then
    _get_tok __
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__COLON__ $__str_95 0
    _parse_statement result
    _new_ast1 result $_DEFAULT_KW $result
  elif [ $_tok = $_WHILE_KW ] ; then
    _get_tok __
    _parse_parenthesized_expression result 0
    _parse_statement child1
    _new_ast2 result $_WHILE_KW $result $child1
  elif [ $_tok = $_DO_KW ] ; then
    _get_tok __
    _parse_statement result
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $_WHILE_KW $__str_95 0
    _parse_parenthesized_expression child1 0
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _new_ast2 result $_DO_KW $result $child1
  elif [ $_tok = $_FOR_KW ] ; then
    _get_tok __
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__LPAREN__ $__str_95 0
    _parse_comma_expression_opt result
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _parse_comma_expression_opt child1
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _parse_comma_expression_opt child2
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__RPAREN__ $__str_95 0
    _parse_statement child3
    _new_ast4 result $_FOR_KW $result $child1 $child2 $child3
  elif [ $_tok = $_GOTO_KW ] ; then
    _get_tok __
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $_IDENTIFIER $__str_95 0
    _new_ast0 __t1 $_IDENTIFIER $_val
    _new_ast1 result $_GOTO_KW $__t1
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
  elif [ $_tok = $_CONTINUE_KW ] ; then
    _get_tok __
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _new_ast0 result $_CONTINUE_KW 0
  elif [ $_tok = $_BREAK_KW ] ; then
    _get_tok __
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _new_ast0 result $_BREAK_KW 0
  elif [ $_tok = $_RETURN_KW ] ; then
    _get_tok __
    _parse_comma_expression_opt result
    defstr __str_95 "pnut.c"
    _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    _new_ast1 result $_RETURN_KW $result
  elif [ $_tok = $__LBRACE__ ] ; then
    _parse_compound_statement result
  else
    start_tok=$_tok
    _parse_comma_expression_opt result
    if [ $_tok = $__COLON__ ] && [ $start_tok != $__LPAREN__ ] && { _get_op __t1 $result; [ $__t1 = $_IDENTIFIER ]; } ; then
      _get_tok __
      _parse_statement child1
      _new_ast2 result $__COLON__ $result $child1
    else
      defstr __str_95 "pnut.c"
      _expect_tok_ __ $__SEMICOLON__ $__str_95 0
    fi
  fi
  : $(($1 = result))
  endlet $1 __t1 start_tok child3 child2 child1 result
}

: $((__t1 = tail = child1 = result = 0))
_parse_compound_statement() {
  let result; let child1; let tail; let __t1
  result=0
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__LBRACE__ $__str_95 0
  if [ $_tok != $__RBRACE__ ] && [ $_tok != -1 ] ; then
    if _is_type_starter __t1 $_tok; [ $__t1 != 0 ] ; then
      _parse_declaration child1 1
    else
      _parse_statement child1
    fi
    _new_ast2 result $__LBRACE__ $child1 0
    tail=$result
    while [ $_tok != $__RBRACE__ ] && [ $_tok != -1 ]; do
      if _is_type_starter __t1 $_tok; [ $__t1 != 0 ] ; then
        _parse_declaration child1 1
      else
        _parse_statement child1
      fi
      _new_ast2 child1 $__LBRACE__ $child1 0
      _set_child __ $tail 1 $child1
      tail=$child1
    done
  fi
  defstr __str_95 "pnut.c"
  _expect_tok_ __ $__RBRACE__ $__str_95 0
  : $(($1 = result))
  endlet $1 __t1 tail child1 result
}

_runtime_use_local_vars=0
_runtime_local_vars_defined=0
_runtime_local_vars() {
  if [ $_runtime_local_vars_defined != 0 ] ; then
    return
  fi
  _runtime_local_vars_defined=1
  printf "# Local variables\n"
  printf "__=0\n"
  printf "__SP=0\n"
  printf "let() { # \$1: variable name, \$2: value (optional)\n"
  printf "  : \$((__\$((__SP += 1))=\$1)) # Push\n"
  printf "  : \$((\$1=\${2-0}))           # Init\n"
  printf "}\n"
  printf "endlet() { # \$1: return variable\n"
  printf "           # \$2...: function local variables\n"
  printf "  __ret=\$1 # Don't overwrite return value\n"
  printf "  : \$((__tmp = \$__ret))\n"
  printf "  while [ \$# -ge 2 ]; do\n"
  printf "    : \$((\$2 = __\$(((__SP -= 1) + 1)))) # Pop\n"
  printf "    shift;\n"
  printf "  done\n"
  printf "  : \$((\$__ret=__tmp))   # Restore return value\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_char_to_int=0
_runtime_char_to_int_defined=0
_runtime_char_to_int() {
  if [ $_runtime_char_to_int_defined != 0 ] ; then
    return
  fi
  _runtime_char_to_int_defined=1
  printf "__c2i_0=48\n"
  printf "__c2i_1=49\n"
  printf "__c2i_2=50\n"
  printf "__c2i_3=51\n"
  printf "__c2i_4=52\n"
  printf "__c2i_5=53\n"
  printf "__c2i_6=54\n"
  printf "__c2i_7=55\n"
  printf "__c2i_8=56\n"
  printf "__c2i_9=57\n"
  printf "__c2i_a=97\n"
  printf "__c2i_b=98\n"
  printf "__c2i_c=99\n"
  printf "__c2i_d=100\n"
  printf "__c2i_e=101\n"
  printf "__c2i_f=102\n"
  printf "__c2i_g=103\n"
  printf "__c2i_h=104\n"
  printf "__c2i_i=105\n"
  printf "__c2i_j=106\n"
  printf "__c2i_k=107\n"
  printf "__c2i_l=108\n"
  printf "__c2i_m=109\n"
  printf "__c2i_n=110\n"
  printf "__c2i_o=111\n"
  printf "__c2i_p=112\n"
  printf "__c2i_q=113\n"
  printf "__c2i_r=114\n"
  printf "__c2i_s=115\n"
  printf "__c2i_t=116\n"
  printf "__c2i_u=117\n"
  printf "__c2i_v=118\n"
  printf "__c2i_w=119\n"
  printf "__c2i_x=120\n"
  printf "__c2i_y=121\n"
  printf "__c2i_z=122\n"
  printf "__c2i_A=65\n"
  printf "__c2i_B=66\n"
  printf "__c2i_C=67\n"
  printf "__c2i_D=68\n"
  printf "__c2i_E=69\n"
  printf "__c2i_F=70\n"
  printf "__c2i_G=71\n"
  printf "__c2i_H=72\n"
  printf "__c2i_I=73\n"
  printf "__c2i_J=74\n"
  printf "__c2i_K=75\n"
  printf "__c2i_L=76\n"
  printf "__c2i_M=77\n"
  printf "__c2i_N=78\n"
  printf "__c2i_O=79\n"
  printf "__c2i_P=80\n"
  printf "__c2i_Q=81\n"
  printf "__c2i_R=82\n"
  printf "__c2i_S=83\n"
  printf "__c2i_T=84\n"
  printf "__c2i_U=85\n"
  printf "__c2i_V=86\n"
  printf "__c2i_W=87\n"
  printf "__c2i_X=88\n"
  printf "__c2i_Y=89\n"
  printf "__c2i_Z=90\n"
  printf "char_to_int() {\n"
  printf "  case \$1 in\n"
  printf "    [[:alnum:]]) __c=\$((__c2i_\$1)) ;;\n"
  printf "    ' ') __c=32 ;;\n"
  printf "    '!') __c=33 ;;\n"
  printf "    '\"') __c=34 ;;\n"
  printf "    '#') __c=35 ;;\n"
  printf "    '\$') __c=36 ;;\n"
  printf "    '%%') __c=37 ;;\n"
  printf "    '&') __c=38 ;;\n"
  printf "    \"'\") __c=39 ;;\n"
  printf "    '(') __c=40 ;;\n"
  printf "    ')') __c=41 ;;\n"
  printf "    '*') __c=42 ;;\n"
  printf "    '+') __c=43 ;;\n"
  printf "    ',') __c=44 ;;\n"
  printf "    '-') __c=45 ;;\n"
  printf "    '.') __c=46 ;;\n"
  printf "    '/') __c=47 ;;\n"
  printf "    ':') __c=58 ;;\n"
  printf "    ';') __c=59 ;;\n"
  printf "    '<') __c=60 ;;\n"
  printf "    '=') __c=61 ;;\n"
  printf "    '>') __c=62 ;;\n"
  printf "    '?') __c=63 ;;\n"
  printf "    '@') __c=64 ;;\n"
  printf "    '[') __c=91 ;;\n"
  printf "    '\\\\') __c=92 ;;\n"
  printf "    ']') __c=93 ;;\n"
  printf "    '^') __c=94 ;;\n"
  printf "    '_') __c=95 ;;\n"
  printf "    '\`') __c=96 ;;\n"
  printf "    '{') __c=123 ;;\n"
  printf "    '|') __c=124 ;;\n"
  printf "    '}') __c=125 ;;\n"
  printf "    '~') __c=126 ;;\n"
  printf "    *)\n"
  printf "      __c=\$(printf \"%%d\" \"'\$1\"); __c=\$((__c > 0 ? __c : 256 + __c)) ;;\n"
  printf "  esac\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_unpack_string=0
_runtime_unpack_string_defined=0
_runtime_unpack_string() {
  if [ $_runtime_unpack_string_defined != 0 ] ; then
    return
  fi
  _runtime_unpack_string_defined=1
  _runtime_char_to_int __
  printf "# Unpack a Shell string into an appropriately sized buffer\n"
  printf "unpack_string() { # \$1: Shell string, \$2: Buffer, \$3: Ends with EOF?\n"
  printf "  __fgetc_buf=\$1\n"
  printf "  __buffer=\$2\n"
  printf "  __ends_with_eof=\$3\n"
  printf "  while [ ! -z \"\$__fgetc_buf\" ]; do\n"
  printf "    case \"\$__fgetc_buf\" in\n"
  printf "      \" \"*) : \$((_\$__buffer = 32))  ;;\n"
  printf "      \"e\"*) : \$((_\$__buffer = 101)) ;;\n"
  printf "      \"=\"*) : \$((_\$__buffer = 61))  ;;\n"
  printf "      \"t\"*) : \$((_\$__buffer = 116)) ;;\n"
  printf "      \";\"*) : \$((_\$__buffer = 59))  ;;\n"
  printf "      \"i\"*) : \$((_\$__buffer = 105)) ;;\n"
  printf "      \")\"*) : \$((_\$__buffer = 41))  ;;\n"
  printf "      \"(\"*) : \$((_\$__buffer = 40))  ;;\n"
  printf "      \"n\"*) : \$((_\$__buffer = 110)) ;;\n"
  printf "      \"s\"*) : \$((_\$__buffer = 115)) ;;\n"
  printf "      \"l\"*) : \$((_\$__buffer = 108)) ;;\n"
  printf "      \"+\"*) : \$((_\$__buffer = 43))  ;;\n"
  printf "      \"p\"*) : \$((_\$__buffer = 112)) ;;\n"
  printf "      \"a\"*) : \$((_\$__buffer = 97))  ;;\n"
  printf "      \"r\"*) : \$((_\$__buffer = 114)) ;;\n"
  printf "      \"f\"*) : \$((_\$__buffer = 102)) ;;\n"
  printf "      \"d\"*) : \$((_\$__buffer = 100)) ;;\n"
  printf "      \"*\"*) : \$((_\$__buffer = 42))  ;;\n"
  printf "      *)\n"
  printf "        char_to_int \"\${__fgetc_buf%%\"\${__fgetc_buf#?}\"}\"\n"
  printf "        : \$((_\$__buffer = __c))\n"
  printf "        ;;\n"
  printf "    esac\n"
  printf "    __fgetc_buf=\${__fgetc_buf#?}      # Remove the first character\n"
  printf "    : \$((__buffer += 1))              # Move to the next buffer position\n"
  printf "  done\n"
  printf "\n"
  printf "  if [ \$__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?\n"
  printf "    : \$((_\$__buffer = 10))            # Line ends with newline\n"
  printf "    : \$((__buffer += 1))\n"
  printf "  fi\n"
  printf "  : \$((_\$__buffer = 0))               # Then \\\\0\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_malloc=0
_runtime_malloc_defined=0
_runtime_malloc() {
  if [ $_runtime_malloc_defined != 0 ] ; then
    return
  fi
  _runtime_malloc_defined=1
  printf "__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.\n\n"
  printf "_malloc() { # \$2 = object size\n"
  printf "  : \$((_\$__ALLOC = \$2)) # Track object size\n"
  printf "  : \$((\$1 = \$__ALLOC + 1))\n"
  printf "  : \$((__ALLOC += \$2 + 1))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_initialize=0
_runtime_initialize_defined=0
_runtime_initialize() {
  if [ $_runtime_initialize_defined != 0 ] ; then
    return
  fi
  _runtime_initialize_defined=1
  printf "# Initialize memory with the list of values.\n"
  printf "# When the expected number of elements is higher than the actual number of\n"
  printf "# elements, the remaining elements are set to 0\n"
  printf "initialize() { # \$1 = var name, \$2 = length, \$3... = elements\n"
  printf "  __ptr=\$((\$1))\n"
  printf "  __size=\$2\n"
  printf "  __i=0\n"
  printf "  while [ \$# -ge 3 ]; do\n"
  printf "    : \$((_\$((__ptr + __i)) = \$3))\n"
  printf "    : \$((__i += 1))\n"
  printf "    shift\n"
  printf "  done\n"
  printf "\n"
  printf "  while [ \$__i -lt \$__size ]; do\n"
  printf "    : \$((_\$((__ptr + __i)) = 0))\n"
  printf "    : \$((__i += 1))\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_defarr=0
_runtime_defarr_defined=0
_runtime_defarr() {
  if [ $_runtime_defarr_defined != 0 ] ; then
    return
  fi
  _runtime_defarr_defined=1
  _runtime_malloc __
  if [ $_runtime_use_initialize != 0 ] ; then
    _runtime_initialize __
    printf "defarr() {\n"
    printf "  _malloc \$1 \$2;\n"
    printf "  if [ \$# -gt 2 ]; then initialize \$@; fi\n"
    printf "}\n"
  else
    printf "defarr() { _malloc \$1 \$2; }\n"
  fi
  printf "\n"
}

_runtime_use_free=0
_runtime_free_defined=0
_runtime_free() {
  if [ $_runtime_free_defined != 0 ] ; then
    return
  fi
  _runtime_free_defined=1
  printf "_free() { # \$2 = object to free\n"
  printf "  __ptr=\$((\$2 - 1))          # Start of object\n"
  printf "  __end=\$((__ptr + _\$__ptr)) # End of object\n"
  printf "  while [ \$__ptr -lt \$__end ]; do\n"
  printf "    unset \"_\$__ptr\"\n"
  printf "    : \$((__ptr += 1))\n"
  printf "  done\n"
  printf "  : \$((\$1 = 0))              # Return 0\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_make_argv=0
_runtime_make_argv_defined=0
_runtime_make_argv() {
  if [ $_runtime_make_argv_defined != 0 ] ; then
    return
  fi
  _runtime_make_argv_defined=1
  _runtime_malloc __
  _runtime_unpack_string __
  printf "make_argv() {\n"
  printf "  __argc=\$1; shift;\n"
  printf "  _malloc __argv \$__argc # Allocate enough space for all elements. No need to initialize.\n"
  printf "  __argv_ptr=\$__argv\n"
  printf "\n"
  printf "  while [ \$# -ge 1 ]; do\n"
  printf "    _malloc _\$__argv_ptr \$((\${#1} + 1))\n"
  printf "    unpack_string \"\$1\" \$((_\$__argv_ptr)) 1\n"
  printf "    : \$((__argv_ptr += 1))\n"
  printf "    shift\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_unpack_escaped_string=0
_runtime_unpack_escaped_string_defined=0
_runtime_unpack_escaped_string() {
  if [ $_runtime_unpack_escaped_string_defined != 0 ] ; then
    return
  fi
  _runtime_unpack_escaped_string_defined=1
  _runtime_malloc __
  _runtime_char_to_int __
  printf "unpack_escaped_string() { # \$1 = string, \$2 = size (optional)\n"
  printf "  __str=\"\$1\"\n"
  printf "  # Allocates enough space for all characters, assuming that no character is escaped\n"
  printf "  _malloc __addr \$((\${2:-\${#__str} + 1}))\n"
  printf "  __ptr=\$__addr\n"
  printf "  __end=\$((__ptr + \${2:-\${#__str} + 1})) # End of allocated memory\n"
  printf "  while [ -n \"\$__str\" ] ; do\n"
  printf "    case \"\$__str\" in\n"
  printf "      '\\\\'*)\n"
  printf "        __str=\"\${__str#?}\" # Remove the current char from \$__str\n"
  printf "        case \"\$__str\" in\n"
  printf "          '0'*) __c=0 ;;\n"
  printf "          'a'*) __c=7 ;;\n"
  printf "          'b'*) __c=8 ;;\n"
  printf "          'f'*) __c=12 ;;\n"
  printf "          'n'*) __c=10 ;;\n"
  printf "          'r'*) __c=13 ;;\n"
  printf "          't'*) __c=9 ;;\n"
  printf "          'v'*) __c=11 ;;\n"
  printf "          '\\\\'*) __c=92 ;;\n"
  printf "          '\"'*) __c=34 ;;\n"
  printf "          \"'\"*) __c=39 ;;\n"
  printf "          '?'*) __c=63 ;;\n"
  printf "          '\$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes\n"
  printf "          *) echo \"invalid escape in string: \$__str\"; exit 1 ;;\n"
  printf "        esac\n"
  printf "        __str=\"\${__str#?}\" # Remove the current char from \$__str\n"
  printf "        ;;\n"
  printf "      *)\n"
  printf "        char_to_int \"\${__str%%\"\${__str#?}\"}\"\n"
  printf "        __str=\"\${__str#?}\" # Remove the current char from \$__str\n"
  printf "        ;;\n"
  printf "    esac\n"
  printf "    : \$((_\$__ptr = __c))\n"
  printf "    : \$((__ptr += 1))\n"
  printf "  done\n"
  printf "  while [ \$__ptr -le \$__end ]; do\n"
  printf "    : \$((_\$__ptr = 0))\n"
  printf "    : \$((__ptr += 1))\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_defstr=0
_runtime_defstr_defined=0
_runtime_defstr() {
  if [ $_runtime_defstr_defined != 0 ] ; then
    return
  fi
  _runtime_defstr_defined=1
  _runtime_unpack_escaped_string __
  printf "# Define a string, and return a reference to it in the varible taken as argument.\n"
  printf "# If the variable is already defined, this function does nothing.\n"
  printf "# Note that it's up to the caller to ensure that no 2 strings share the same variable.\n"
  printf "defstr() { # \$1 = variable name, \$2 = string, \$3 = size (optional)\n"
  printf "  set +u # Necessary to allow the variable to be empty\n"
  printf "  if [ \$((\$1)) -eq 0 ]; then\n"
  printf "    unpack_escaped_string \"\$2\" \$3\n"
  printf "    : \$((\$1 = __addr))\n"
  printf "  fi\n"
  printf "  set -u\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_exit=0
_runtime_exit_defined=0
_runtime_exit() {
  if [ $_runtime_exit_defined != 0 ] ; then
    return
  fi
  _runtime_exit_defined=1
  printf "_exit() { # \$2: exit status\n"
  printf "  exit \$2\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_putchar=0
_runtime_putchar_defined=0
_runtime_putchar() {
  if [ $_runtime_putchar_defined != 0 ] ; then
    return
  fi
  _runtime_putchar_defined=1
  printf "_putchar() {\n"
  printf "  : \$((\$1 = 0)); shift # Return 0\n"
  printf "  printf \\\\\\\\\$((\$1/64))\$((\$1/8%%8))\$((\$1%%8))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_getchar=0
_runtime_getchar_defined=0
_runtime_getchar() {
  if [ $_runtime_getchar_defined != 0 ] ; then
    return
  fi
  _runtime_getchar_defined=1
  _runtime_char_to_int __
  printf "__stdin_buf=\n"
  printf "__stdin_line_ending=0 # Line ending, either -1 (EOF) or 10 ('\\\\n')\n"
  printf "_getchar() {\n"
  printf "  if [ -z \"\$__stdin_buf\" ]; then          # need to get next line when buffer empty\n"
  printf "    if [ \$__stdin_line_ending != 0 ]; then  # Line is empty, return line ending\n"
  printf "      : \$((\$1 = __stdin_line_ending))\n"
  printf "      __stdin_line_ending=0                  # Reset line ending for next getchar call\n"
  printf "      return\n"
  printf "    fi\n"
  printf "    IFS=                                            # don't split input\n"
  printf "    if read -r __stdin_buf ; then                   # read next line into \$__stdin_buf\n"
  printf "      if [ -z \"\$__stdin_buf\" ] ; then               # an empty line implies a newline character\n"
  printf "        : \$((\$1 = 10))                              # next getchar call will read next line\n"
  printf "        return\n"
  printf "      fi\n"
  printf "      __stdin_line_ending=10\n"
  printf "    else\n"
  printf "      if [ -z \"\$__stdin_buf\" ] ; then               # EOF reached when read fails\n"
  printf "        : \$((\$1 = -1))\n"
  printf "        return\n"
  printf "      else\n"
  printf "        __stdin_line_ending=-1\n"
  printf "      fi\n"
  printf "    fi\n"
  printf "  fi\n"
  printf "  case \"\$__stdin_buf\" in\n"
  printf "    \" \"*) : \$((\$1 = 32))  ;;\n"
  printf "    \"e\"*) : \$((\$1 = 101)) ;;\n"
  printf "    \"=\"*) : \$((\$1 = 61))  ;;\n"
  printf "    \"t\"*) : \$((\$1 = 116)) ;;\n"
  printf "    \";\"*) : \$((\$1 = 59))  ;;\n"
  printf "    \"i\"*) : \$((\$1 = 105)) ;;\n"
  printf "    \")\"*) : \$((\$1 = 41))  ;;\n"
  printf "    \"(\"*) : \$((\$1 = 40))  ;;\n"
  printf "    \"n\"*) : \$((\$1 = 110)) ;;\n"
  printf "    \"s\"*) : \$((\$1 = 115)) ;;\n"
  printf "    \"l\"*) : \$((\$1 = 108)) ;;\n"
  printf "    \"+\"*) : \$((\$1 = 43))  ;;\n"
  printf "    \"p\"*) : \$((\$1 = 112)) ;;\n"
  printf "    \"a\"*) : \$((\$1 = 97))  ;;\n"
  printf "    \"r\"*) : \$((\$1 = 114)) ;;\n"
  printf "    \"f\"*) : \$((\$1 = 102)) ;;\n"
  printf "    \"d\"*) : \$((\$1 = 100)) ;;\n"
  printf "    \"*\"*) : \$((\$1 = 42))  ;;\n"
  printf "    *)\n"
  printf "      char_to_int \"\${__stdin_buf%%\"\${__stdin_buf#?}\"}\"\n"
  printf "      : \$((\$1 = __c))\n"
  printf "      ;;\n"
  printf "  esac\n"
  printf "    __stdin_buf=\"\${__stdin_buf#?}\"                  # remove the current char from \$__stdin_buf\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_put_pstr=0
_runtime_put_pstr_defined=0
_runtime_put_pstr() {
  if [ $_runtime_put_pstr_defined != 0 ] ; then
    return
  fi
  _runtime_put_pstr_defined=1
  printf "_put_pstr() {\n"
  printf "  : \$((\$1 = 0)); shift # Return 0\n"
  printf "  __addr=\$1; shift\n"
  printf "  while [ \$((__c = _\$__addr)) != 0 ]; do\n"
  printf "    printf \\\\\\\\\$((__c/64))\$((__c/8%%8))\$((__c%%8))\n"
  printf "    : \$((__addr += 1))\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_printf=0
_runtime_printf_defined=0
_runtime_printf() {
  if [ $_runtime_printf_defined != 0 ] ; then
    return
  fi
  _runtime_printf_defined=1
  _runtime_put_pstr __
  printf "read_int() {\n"
  printf "  __int=\n"
  printf "  while [ \$((__c = _\$__fmt_ptr)) != 0 ] && [ \$__c -ge 48 ] && [ \$__c -le 57 ]; do\n"
  printf "    __int=\"\$__int\$((__c - 48))\"\n"
  printf "    : \$((__fmt_ptr += 1))\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
  printf "pad() {\n"
  printf "  if [ \$((\$1 - 1)) -ge 1 ]; then\n"
  printf "    printf \"%%\$((\$1 - 1))s\" \"\"\n"
  printf "  fi\n"
  printf "}\n"
  printf "\n"
  printf "printf_invalid_format_error() {\n"
  printf "  printf \"Invalid format specifier. %%%%\"\n"
  printf "  : \$((_\$__fmt_ptr = 0)) # Terminate string after %%...\n"
  printf "  _put_pstr __ \$__mod_start\n"
  printf "  printf \"\\\\n\"\n"
  printf "  exit 1\n"
  printf "}\n"
  printf "\n"
  printf "printf_reset() {\n"
  printf "  __mod=0\n"
  printf "  __flags=\n"
  printf "  __width=\n"
  printf "  __precision=\n"
  printf "}\n"
  printf "\n"
  printf "_printf() { # \$1 = printf format string, \$2... = printf args\n"
  printf "  : \$((\$1 = 0)); shift # Return 0\n"
  printf "  __fmt_ptr=\$1; shift\n"
  printf "  __mod_start=0\n"
  printf "  printf_reset\n"
  printf "  while [ \"\$((__head = _\$__fmt_ptr))\" != 0 ] ; do\n"
  printf "    __fmt_ptr=\$((__fmt_ptr + 1))\n"
  printf "    if [ \$__mod -eq 1 ] ; then\n"
  printf "      __char=\$(printf \"\\\\\\\\\$((\$__head/64))\$((\$__head/8%%8))\$((\$__head%%8))\")\n"
  printf "      __head_char=\$__char\n"
  printf "      case \$__head_char in\n"
  printf "        '%%')\n"
  printf "          if [ -n \"\${__flags}\${__width}\${__precision}\" ]; then printf_invalid_format_error; fi\n"
  printf "          printf \"%%%%\"\n"
  printf "          printf_reset\n"
  printf "          ;;\n"
  printf "        'd'|'i'|'o'|'u'|'x'|'X')\n"
  printf "          printf \"%%\${__flags}\${__width}\${__precision}\${__head_char}\" \$1\n"
  printf "          shift\n"
  printf "          printf_reset\n"
  printf "          ;;\n"
  printf "        'c')\n"
  printf "          case \"\$__flags\" in\n"
  printf "            *'-'*)\n"
  printf "              printf \\\\\\\\\$((\$1/64))\$((\$1/8%%8))\$((\$1%%8)); pad \${__width:-0} ;;\n"
  printf "            *) pad \${__width:-0}; printf \\\\\\\\\$((\$1/64))\$((\$1/8%%8))\$((\$1%%8)) ;;\n"
  printf "          esac\n"
  printf "          shift\n"
  printf "          printf_reset\n"
  printf "          ;;\n"
  printf "        's')\n"
  printf "          # We only want to use the shell's native printf (and _put_pstr subshell) if %%s has sub-specifiers\n"
  printf "          if [ -z \"{__flags}\${__width}{__precision}\" ]; then\n"
  printf "            _put_pstr __ \$1\n"
  printf "          else\n"
  printf "            printf \"%%\${__flags}\${__width}\${__precision}s\" \"\$(_put_pstr __ \$1)\"\n"
  printf "          fi\n"
  printf "          shift\n"
  printf "          printf_reset\n"
  printf "          ;;\n"
  printf "        '-'|'+'|' '|'#'|'0')\n"
  printf "          if [ -n \"\${__width}\${__precision}\" ]; then printf_invalid_format_error; fi\n"
  printf "          __flags=\"\$__flags\$__head_char\"\n"
  printf "          ;;\n"
  printf "        [0-9])\n"
  printf "          if [ -n \"\${__width}\${__precision}\" ]; then printf_invalid_format_error; fi\n"
  printf "          read_int\n"
  printf "          __width=\"\$__head_char\$__int\"\n"
  printf "          ;;\n"
  printf "        '*')\n"
  printf "          if [ -n \"\${__width}\${__precision}\" ]; then printf_invalid_format_error; fi\n"
  printf "          __width=\$1\n"
  printf "          shift\n"
  printf "          ;;\n"
  printf "        '.')\n"
  printf "          __head=\$((_\$__fmt_ptr))\n"
  printf "          if [ \$__head = 42 ]; then # 42 = '*'\n"
  printf "            __fmt_ptr=\$((__fmt_ptr + 1))\n"
  printf "            __precision=\".\$1\"\n"
  printf "            shift\n"
  printf "          elif [ \$__head -ge 48 ] && [ \$__head -le 57 ]; then\n"
  printf "            read_int\n"
  printf "            __precision=\".\$__int\"\n"
  printf "          else\n"
  printf "            printf_invalid_format_error\n"
  printf "          fi\n"
  printf "          ;;\n"
  printf "        *)\n"
  printf "          echo \"4: Unknown format specifier %%\$__head_char\"; exit 1\n"
  printf "      esac\n"
  printf "    elif [ \$__head = 37 ]; then # 37 == '%%'\n"
  printf "      __mod=1; __mod_start=\$__fmt_ptr\n"
  printf "    else\n"
  printf "      printf \\\\\\\\\$((\$__head/64))\$((\$__head/8%%8))\$((\$__head%%8))\n"
  printf "    fi\n"
  printf "  done\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_open=0
_runtime_open_defined=0
_runtime_open() {
  if [ $_runtime_open_defined != 0 ] ; then
    return
  fi
  _runtime_open_defined=1
  _runtime_malloc __
  _runtime_put_pstr __
  printf "_malloc __buffer_fd0 1000   # Allocate buffer\n"
  printf ": \$((_\$__buffer_fd0 = 0))   # Init buffer to \"\"\n"
  printf ": \$((__cursor_fd0 = 0))     # Make buffer empty\n"
  printf ": \$((__buflen_fd0 = 1000))  # Init buffer length\n"
  printf "__state_fd0=0 # stdin\n"
  printf "__state_fd1=1 # stdout\n"
  printf "__state_fd2=2 # stderr\n"
  printf "__state_fd3=-1\n"
  printf "__state_fd4=-1\n"
  printf "__state_fd5=-1\n"
  printf "__state_fd6=-1\n"
  printf "__state_fd7=-1\n"
  printf "__state_fd8=-1\n"
  printf "__state_fd9=-1\n"
  printf "\n"
  printf "_open() { # \$2: filename, \$3: flags, \$4: mode\n"
  printf "  # Get available fd\n"
  printf "  __fd=0\n"
  printf "  while [ \$__fd -lt 10 ]; do\n"
  printf "    if [ \$((__state_fd\$__fd)) -lt 0 ]; then\n"
  printf "      break\n"
  printf "    fi\n"
  printf "    : \$((__fd += 1))\n"
  printf "  done\n"
  printf "  if [ \$__fd -gt 9 ] ; then\n"
  printf "    # Some shells don't support fd > 9\n"
  printf "    echo \"No more file descriptors available to open \$(_put_pstr __ \$2)\" ; exit 1\n"
  printf "  else\n"
  printf "    # Because the file must be read line-by-line, and string\n"
  printf "    # values can't be assigned to dynamic variables, each line\n"
  printf "    # is read and then unpacked in the buffer.\n"
  printf "    _malloc __addr 1000                 # Allocate buffer\n"
  printf "    : \$((_\$__addr = 0))                 # Init buffer to \"\"\n"
  printf "    : \$((__buffer_fd\$__fd = __addr))    # Buffer address\n"
  printf "    : \$((__cursor_fd\$__fd = 0))         # Buffer cursor\n"
  printf "    : \$((__buflen_fd\$__fd = 1000))      # Buffer length\n"
  printf "    : \$((__state_fd\$__fd = \$3))         # Mark the fd as opened\n"
  printf "    __res=\$(_put_pstr __ \$2)\n"
  printf "    if [ \$3 = 0 ] ; then\n"
  printf "      case \$__fd in\n"
  printf "        0) exec 0< \"\$__res\" ;; 1) exec 1< \"\$__res\" ;; 2) exec 2< \"\$__res\" ;;\n"
  printf "        3) exec 3< \"\$__res\" ;; 4) exec 4< \"\$__res\" ;; 5) exec 5< \"\$__res\" ;;\n"
  printf "        6) exec 6< \"\$__res\" ;; 7) exec 7< \"\$__res\" ;; 8) exec 8< \"\$__res\" ;;\n"
  printf "        9) exec 9< \"\$__res\" ;;\n"
  printf "      esac\n"
  printf "    elif [ \$3 = 1 ] ; then\n"
  printf "      case \$__fd in\n"
  printf "        0) exec 0> \"\$__res\" ;; 1) exec 1> \"\$__res\" ;; 2) exec 2> \"\$__res\" ;;\n"
  printf "        3) exec 3> \"\$__res\" ;; 4) exec 4> \"\$__res\" ;; 5) exec 5> \"\$__res\" ;;\n"
  printf "        6) exec 6> \"\$__res\" ;; 7) exec 7> \"\$__res\" ;; 8) exec 8> \"\$__res\" ;;\n"
  printf "        9) exec 9> \"\$__res\" ;;\n"
  printf "      esac\n"
  printf "    elif [ \$3 = 2 ] ; then\n"
  printf "      case \$__fd in\n"
  printf "        0) exec 0>> \"\$__res\" ;; 1) exec 1>> \"\$__res\" ;; 2) exec 2>> \"\$__res\" ;;\n"
  printf "        3) exec 3>> \"\$__res\" ;; 4) exec 4>> \"\$__res\" ;; 5) exec 5>> \"\$__res\" ;;\n"
  printf "        6) exec 6>> \"\$__res\" ;; 7) exec 7>> \"\$__res\" ;; 8) exec 8>> \"\$__res\" ;;\n"
  printf "        9) exec 9>> \"\$__res\" ;;\n"
  printf "      esac\n"
  printf "    else\n"
  printf "      echo \"Unknown file mode\" ; exit 1\n"
  printf "    fi\n"
  printf "  fi\n"
  printf "  : \$((\$1 = __fd))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_read_byte=0
_runtime_read_byte_defined=0
_runtime_read_byte() {
  if [ $_runtime_read_byte_defined != 0 ] ; then
    return
  fi
  _runtime_read_byte_defined=1
  _runtime_malloc __
  _runtime_free __
  _runtime_char_to_int __
  _runtime_unpack_string __
  printf "refill_buffer() { # \$1: fd\n"
  printf "  __fd=\$1\n"
  printf "  __buffer=\$((__buffer_fd\$__fd))\n"
  printf "\n"
  printf "  IFS=\n"
  printf "  __ends_with_eof=0\n"
  printf "  read -r __temp_buf <&\$__fd || __ends_with_eof=1\n"
  printf "\n"
  printf "  # Check that the buffer is large enough to unpack the line\n"
  printf "  __buflen=\$((__buflen_fd\$__fd - 2)) # Minus 2 to account for newline and \\\\0\n"
  printf "  __len=\${#__temp_buf}\n"
  printf "  if [ \$__len -gt \$__buflen ]; then\n"
  printf "    # Free buffer and reallocate a new one double the line size\n"
  printf "    __buflen=\$((__len * 2))\n"
  printf "    _free __ \$__buffer\n"
  printf "    _malloc __buffer \$__buflen\n"
  printf "    : \$((__buffer_fd\$__fd = __buffer))\n"
  printf "    : \$((__buflen_fd\$__fd = __buflen))\n"
  printf "  fi\n"
  printf "  unpack_string \"\$__temp_buf\" \$__buffer \$__ends_with_eof\n"
  printf "}\n"
  printf "\n"
  printf "read_byte() { # \$2: fd\n"
  printf "  __fd=\$2\n"
  printf "  : \$((__buffer=__buffer_fd\$__fd))\n"
  printf "  : \$((__cursor=__cursor_fd\$__fd))\n"
  printf "  # The cursor is at the end of the buffer, we need to read the next line\n"
  printf "  if [ \$((_\$((__buffer + __cursor)))) -eq 0 ]; then\n"
  printf "    # Buffer has been read completely, read next line\n"
  printf "    refill_buffer \$__fd\n"
  printf "    __cursor=0 # Reset cursor and reload buffer\n"
  printf "    : \$((__buffer=__buffer_fd\$__fd))\n"
  printf "    if [ \$((_\$((__buffer + __cursor)))) -eq 0 ]; then\n"
  printf "      : \$((\$1 = -1)) # EOF\n"
  printf "      return\n"
  printf "    fi\n"
  printf "  fi\n"
  printf "  : \$((\$1 = _\$((__buffer + __cursor))))\n"
  printf "  : \$((__cursor_fd\$__fd = __cursor + 1))  # Increment cursor\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_read=0
_runtime_read_defined=0
_runtime_read() {
  if [ $_runtime_read_defined != 0 ] ; then
    return
  fi
  _runtime_read_defined=1
  _runtime_read_byte __
  _runtime_open __
  printf "_read() { : \$((__fd = \$2)) \$((__buf = \$3)) \$((__count = \$4))\n"
  printf "  : \$((__i = 0))\n"
  printf "  while [ \$__i -lt \$__count ] ; do\n"
  printf "    read_byte __byte \$__fd\n"
  printf "    if [ \$__byte -lt 0 ] ; then break; fi\n"
  printf "    : \$((_\$((__buf + __i)) = __byte))\n"
  printf "    : \$((__i += 1))\n"
  printf "  done\n"
  printf "  : \$((\$1 = __i))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_write=0
_runtime_write_defined=0
_runtime_write() {
  if [ $_runtime_write_defined != 0 ] ; then
    return
  fi
  _runtime_write_defined=1
  _runtime_open __
  printf "_write() { : \$((__fd = \$2)) \$((__buf = \$3)) \$((__count = \$4))\n"
  printf "  : \$((__i = 0))\n"
  printf "  while [ \$__i -lt \$__count ] ; do\n"
  printf "    : \$((__byte = _\$((__buf+__i))))\n"
  printf "    printf \\\\\\\\\$((\$__byte/64))\$((\$__byte/8%%8))\$((\$__byte%%8)) >&\$__fd\n"
  printf "    : \$((__i += 1))\n"
  printf "  done\n"
  printf "  : \$((\$1 = __count))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_fopen=0
_runtime_fopen_defined=0
_runtime_fopen() {
  if [ $_runtime_fopen_defined != 0 ] ; then
    return
  fi
  _runtime_fopen_defined=1
  _runtime_malloc __
  _runtime_open __
  printf "# Open the file and return a FILE* for the file.\n"
  printf "# The FILE structure contains the file descriptor.\n"
  printf "_fopen() { # \$2: File name, \$3: Mode\n"
  printf "  _open __fd \$2 \$((_\$3 == 119)) 511\n"
  printf "  _malloc __file 1        # Allocate FILE structure\n"
  printf "  : \$((_\$__file = __fd))  # Save fd\n"
  printf "  : \$((\$1 = __file))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_close=0
_runtime_close_defined=0
_runtime_close() {
  if [ $_runtime_close_defined != 0 ] ; then
    return
  fi
  _runtime_close_defined=1
  _runtime_open __
  _runtime_free __
  printf "_close() { # \$2: fd\n"
  printf "  __fd=\$2\n"
  printf "  __buf=\$((__buffer_fd\$__fd))  # Get buffer\n"
  printf "  _free __ \$__buf              # Release buffer\n"
  printf "  : \$((__state_fd\$__fd = -1))  # Mark the fd as closed\n"
  printf "  case \$__fd in\n"
  printf "    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;\n"
  printf "    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;\n"
  printf "    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;\n"
  printf "    9) exec 9<&- ;;\n"
  printf "  esac\n"
  printf "  : \$((\$1 = 0))\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_fclose=0
_runtime_fclose_defined=0
_runtime_fclose() {
  if [ $_runtime_fclose_defined != 0 ] ; then
    return
  fi
  _runtime_fclose_defined=1
  _runtime_free __
  _runtime_close __
  printf "_fclose() { # \$2: file\n"
  printf "  __file=\$2\n"
  printf "  __fd=\$((_\$__file))  # Get fd\n"
  printf "  _free __ \$__file    # Release FILE structure\n"
  printf "  _close \$1 \$__fd\n"
  printf "}\n"
  printf "\n"
}

_runtime_use_fgetc=0
_runtime_fgetc_defined=0
_runtime_fgetc() {
  if [ $_runtime_fgetc_defined != 0 ] ; then
    return
  fi
  _runtime_fgetc_defined=1
  _runtime_read_byte __
  printf "_fgetc() { # \$2: file\n"
  printf "  __file=\$2\n"
  printf "  __fd=\$((_\$__file))\n"
  printf "  read_byte \$1 \$__fd\n"
  printf "}\n"
  printf "\n"
}

_produce_runtime() {
  if [ $_runtime_use_defstr != 0 ] ; then
    _runtime_defstr __
  fi
  if [ $_runtime_use_putchar != 0 ] ; then
    _runtime_putchar __
  fi
  if [ $_runtime_use_getchar != 0 ] ; then
    _runtime_getchar __
  fi
  if [ $_runtime_use_exit != 0 ] ; then
    _runtime_exit __
  fi
  if [ $_runtime_use_malloc != 0 ] ; then
    _runtime_malloc __
  fi
  if [ $_runtime_use_free != 0 ] ; then
    _runtime_free __
  fi
  if [ $_runtime_use_put_pstr != 0 ] ; then
    _runtime_put_pstr __
  fi
  if [ $_runtime_use_printf != 0 ] ; then
    _runtime_printf __
  fi
  if [ $_runtime_use_fopen != 0 ] ; then
    _runtime_fopen __
  fi
  if [ $_runtime_use_fclose != 0 ] ; then
    _runtime_fclose __
  fi
  if [ $_runtime_use_fgetc != 0 ] ; then
    _runtime_fgetc __
  fi
  if [ $_runtime_use_read != 0 ] ; then
    _runtime_read __
  fi
  if [ $_runtime_use_write != 0 ] ; then
    _runtime_write __
  fi
  if [ $_runtime_use_open != 0 ] ; then
    _runtime_open __
  fi
  if [ $_runtime_use_close != 0 ] ; then
    _runtime_close __
  fi
  if [ $_runtime_use_make_argv != 0 ] ; then
    _runtime_make_argv __
  fi
  if [ $_runtime_use_local_vars != 0 ] ; then
    _runtime_local_vars __
  fi
  if [ $_runtime_use_unpack_string != 0 ] ; then
    _runtime_unpack_string __
  fi
}

: $((c = 0))
_handle_shell_include() {
  let c
  if [ $_tok = $_STRING ] ; then
    _runtime_use_put_pstr=1
    _runtime_use_unpack_string=1
    _include_file __ $((_string_pool + _$((_heap + _val + 1)))) $_fp_dirname
    while _fgetc c $_fp; [ $c != -1 ]; do
      printf \\$((c/64))$((c/8%8))$((c%8))
    done
    printf "\n"
    _restore_include_context __
    _get_tok_macro __
  else
    printf "tok="
    printf "%d" $_tok
    printf "\n"
    defstr __str_136 "expected string to #include_shell directive"
    _syntax_error __ $__str_136
  fi
  endlet $1 c
}

defarr _text_pool 1000000
_text_alloc=1
# TEXT_NODES enum declaration
readonly _TEXT_TREE=0
readonly _TEXT_INTEGER=1
readonly _TEXT_INTEGER_HEX=2
readonly _TEXT_INTEGER_OCT=3
readonly _TEXT_STRING=4
readonly _TEXT_ESCAPED=5
# STMT_CTX enum declaration
readonly _STMT_CTX_DEFAULT=0
readonly _STMT_CTX_ELSE_IF=1
readonly _STMT_CTX_SWITCH=2
: $((i = 0))
_wrap_int() { let i $2
  if [ $((_text_alloc + 2)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_INTEGER))
  : $((_$((_text_pool + _text_alloc + 1)) = i))
  : $(($1 = (_text_alloc += 2) - 2))
  endlet $1 i
}

: $((i = 0))
_wrap_int_hex() { let i $2
  if [ $((_text_alloc + 2)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_INTEGER_HEX))
  : $((_$((_text_pool + _text_alloc + 1)) = i))
  : $(($1 = (_text_alloc += 2) - 2))
  endlet $1 i
}

: $((i = 0))
_wrap_int_oct() { let i $2
  if [ $((_text_alloc + 2)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_INTEGER_OCT))
  : $((_$((_text_pool + _text_alloc + 1)) = i))
  : $(($1 = (_text_alloc += 2) - 2))
  endlet $1 i
}

: $((__t1 = obj = multiply = 0))
_wrap_integer() { let multiply $2; let obj $3
  let __t1
  _get_op __t1 $obj
  case $__t1 in
    $_INTEGER)
      _get_val __t1 $obj
      _wrap_int $1 $((multiply * -__t1))
    ;;
    $_INTEGER_HEX)
      _get_val __t1 $obj
      _wrap_int_hex $1 $((multiply * -__t1))
    ;;
    $_INTEGER_OCT)
      _get_val __t1 $obj
      _wrap_int_oct $1 $((multiply * -__t1))
    ;;
    *)
      defstr __str_138 "wrap_integer: unknown integer type"
      _fatal_error __ $__str_138
      : $(($1 = 0))
    ;;
  esac
  endlet $1 __t1 obj multiply
}

: $((for_printf = t = 0))
_escape_text() { let t $2; let for_printf $3
  if [ $((_text_alloc + 3)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_ESCAPED))
  : $((_$((_text_pool + _text_alloc + 1)) = t))
  : $((_$((_text_pool + _text_alloc + 2)) = for_printf))
  : $(($1 = (_text_alloc += 3) - 3))
  endlet $1 for_printf t
}

: $((t2 = t1 = 0))
_string_concat() { let t1 $2; let t2 $3
  if [ $((_text_alloc + 4)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_TREE))
  : $((_$((_text_pool + _text_alloc + 1)) = 2))
  : $((_$((_text_pool + _text_alloc + 2)) = t1))
  : $((_$((_text_pool + _text_alloc + 3)) = t2))
  : $(($1 = (_text_alloc += 4) - 4))
  endlet $1 t2 t1
}

: $((t3 = t2 = t1 = 0))
_string_concat3() { let t1 $2; let t2 $3; let t3 $4
  if [ $((_text_alloc + 5)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_TREE))
  : $((_$((_text_pool + _text_alloc + 1)) = 3))
  : $((_$((_text_pool + _text_alloc + 2)) = t1))
  : $((_$((_text_pool + _text_alloc + 3)) = t2))
  : $((_$((_text_pool + _text_alloc + 4)) = t3))
  : $(($1 = (_text_alloc += 5) - 5))
  endlet $1 t3 t2 t1
}

: $((t4 = t3 = t2 = t1 = 0))
_string_concat4() { let t1 $2; let t2 $3; let t3 $4; let t4 $5
  if [ $((_text_alloc + 6)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_TREE))
  : $((_$((_text_pool + _text_alloc + 1)) = 4))
  : $((_$((_text_pool + _text_alloc + 2)) = t1))
  : $((_$((_text_pool + _text_alloc + 3)) = t2))
  : $((_$((_text_pool + _text_alloc + 4)) = t3))
  : $((_$((_text_pool + _text_alloc + 5)) = t4))
  : $(($1 = (_text_alloc += 6) - 6))
  endlet $1 t4 t3 t2 t1
}

: $((t5 = t4 = t3 = t2 = t1 = 0))
_string_concat5() { let t1 $2; let t2 $3; let t3 $4; let t4 $5; let t5 $6
  if [ $((_text_alloc + 7)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_TREE))
  : $((_$((_text_pool + _text_alloc + 1)) = 5))
  : $((_$((_text_pool + _text_alloc + 2)) = t1))
  : $((_$((_text_pool + _text_alloc + 3)) = t2))
  : $((_$((_text_pool + _text_alloc + 4)) = t3))
  : $((_$((_text_pool + _text_alloc + 5)) = t4))
  : $((_$((_text_pool + _text_alloc + 6)) = t5))
  : $(($1 = (_text_alloc += 7) - 7))
  endlet $1 t5 t4 t3 t2 t1
}

: $((end = s = 0))
_wrap_str_imm() { let s $2; let end $3
  if [ $((_text_alloc + 3)) -ge 1000000 ] ; then
    defstr __str_137 "string tree pool overflow"
    _fatal_error __ $__str_137
  fi
  : $((_$((_text_pool + _text_alloc)) = _TEXT_STRING))
  : $((_$((_text_pool + _text_alloc + 1)) = s))
  : $((_$((_text_pool + _text_alloc + 2)) = end))
  : $(($1 = (_text_alloc += 3) - 3))
  endlet $1 end s
}

: $((s = 0))
_wrap_str_lit() { let s $2
  _wrap_str_imm $1 $s 0
  endlet $1 s
}

: $((ident_probe = 0))
_wrap_str_pool() { let ident_probe $2
  _wrap_str_imm $1 $((_string_pool + _$((_heap + ident_probe + 1)))) 0
  endlet $1 ident_probe
}

: $((sep = t2 = t1 = 0))
_concatenate_strings_with() { let t1 $2; let t2 $3; let sep $4
  if [ $t1 = 0 ] ; then
    : $(($1 = t2))
    endlet $1 sep t2 t1
    return
  fi
  if [ $t2 = 0 ] ; then
    : $(($1 = t1))
    endlet $1 sep t2 t1
    return
  fi
  _string_concat3 $1 $t1 $sep $t2
  endlet $1 sep t2 t1
}

: $((for_printf = c = 0))
_print_escaped_char() { let c $2; let for_printf $3
  if [ $c = $__NUL__ ] ; then
    printf "\\\\"
    printf "0"
  elif [ $c = $__ALARM__ ] ; then
    printf "\\\\"
    printf "a"
  elif [ $c = $__BACKSPACE__ ] ; then
    printf "\\\\"
    printf "b"
  elif [ $c = $__PAGE__ ] ; then
    printf "\\\\"
    printf "f"
  elif [ $c = $__NEWLINE__ ] ; then
    printf "\\\\"
    printf "n"
  elif [ $c = $__RET__ ] ; then
    printf "\\\\"
    printf "r"
  elif [ $c = $__TAB__ ] ; then
    printf "\\\\"
    printf "t"
  elif [ $c = $__VTAB__ ] ; then
    printf "\\\\"
    printf "v"
  elif [ $c = $__BACKSLASH__ ] ; then
    printf "\\\\"
    printf "\\\\"
    printf "\\\\"
    printf "\\\\"
  elif [ $c = $__DOLLAR__ ] ; then
    printf "\\\\"
    printf "\$"
  elif [ $c = $__BACKTICK__ ] ; then
    printf "\\\\"
    printf "\`"
  elif [ $c = $__DQUOTE__ ] ; then
    printf "\\\\"
    printf "\""
  elif [ $c = $__PERCENT__ ] && [ $for_printf != 0 ] ; then
    printf "%%"
    printf "%%"
  else
    printf \\$((c/64))$((c/8%8))$((c%8))
  fi
  endlet $1 for_printf c
}

: $((for_printf = string_end = string_start = 0))
_print_escaped_string() { let string_start $2; let string_end $3; let for_printf $4
  if [ $string_end != 0 ] ; then
    while [ $string_start -lt $string_end ]; do
      _print_escaped_char __ $((_$string_start)) $for_printf
      : $((string_start += 1))
    done
  else
    while [ $((_$string_start)) != 0 ]; do
      _print_escaped_char __ $((_$string_start)) $for_printf
      : $((string_start += 1))
    done
  fi
  endlet $1 for_printf string_end string_start
}

: $((i = for_printf = t = 0))
_print_escaped_text() { let t $2; let for_printf $3
  let i
  if [ $t = 0 ] ; then
    endlet $1 i for_printf t
    return
  fi
  if [ $t -lt 0 ] ; then
    _print_escaped_char __ $((- t)) $for_printf
  elif [ $((_$((_text_pool + t)))) = $_TEXT_TREE ] ; then
    i=0
    while [ $i -lt $((_$((_text_pool + t + 1)))) ]; do
      if [ $((_$((_text_pool + t + i + 2)))) -lt 0 ] ; then
        _print_escaped_char __ $((-_$((_text_pool + t + i + 2)))) $for_printf
      else
        _print_escaped_text __ $((_$((_text_pool + t + i + 2)))) $for_printf
      fi
      : $((i += 1))
    done
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER ] ; then
    printf "%d" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER_HEX ] ; then
    printf "0"
    printf "x"
    printf "%x" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER_OCT ] ; then
    printf "0"
    printf "%o" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_STRING ] ; then
    _print_escaped_string __ $((_$((_text_pool + t + 1)))) $((_$((_text_pool + t + 2)))) $for_printf
  elif [ $((_$((_text_pool + t)))) = $_TEXT_ESCAPED ] ; then
    defstr __str_139 "Cannot escape a string that is already escaped"
    _fatal_error __ $__str_139
  else
    defstr __str_140 "print_escaped_text: unexpected string tree node"
    _fatal_error __ $__str_140
  fi
  endlet $1 i for_printf t
}

: $((s = i = t = 0))
_print_text() { let t $2
  let i; let s
  if [ $t = 0 ] ; then
    endlet $1 s i t
    return
  fi
  if [ $t -lt 0 ] ; then
    printf \\$(((- t)/64))$(((- t)/8%8))$(((- t)%8))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_TREE ] ; then
    i=0
    while [ $i -lt $((_$((_text_pool + t + 1)))) ]; do
      if [ $((_$((_text_pool + t + i + 2)))) -lt 0 ] ; then
        printf \\$(((-_$((_text_pool + t + i + 2)))/64))$(((-_$((_text_pool + t + i + 2)))/8%8))$(((-_$((_text_pool + t + i + 2)))%8))
      else
        _print_text __ $((_$((_text_pool + t + i + 2))))
      fi
      : $((i += 1))
    done
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER ] ; then
    printf "%d" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER_HEX ] ; then
    printf "0"
    printf "x"
    printf "%x" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_INTEGER_OCT ] ; then
    printf "0"
    printf "%o" $((_$((_text_pool + t + 1))))
  elif [ $((_$((_text_pool + t)))) = $_TEXT_STRING ] ; then
    if [ $((_$((_text_pool + t + 2)))) = 0 ] ; then
      _putstr __ $((_$((_text_pool + t + 1))))
    else
      s=$((_$((_text_pool + t + 1))))
      while [ $s -lt $((_$((_text_pool + t + 2)))) ] || [ $((_$s)) != 0 ]; do
        printf \\$(((_$s)/64))$(((_$s)/8%8))$(((_$s)%8))
        : $((s += 1))
      done
    fi
  elif [ $((_$((_text_pool + t)))) = $_TEXT_ESCAPED ] ; then
    _print_escaped_text __ $((_$((_text_pool + t + 1)))) $((_$((_text_pool + t + 2))))
  else
    defstr __str_141 "print_text: unexpected string tree node"
    _fatal_error __ $__str_141
  fi
  endlet $1 s i t
}

defarr _glo_decls 100000
_glo_decl_ix=0
_nest_level=0
_in_tail_position=0
_gensym_ix=0
_fun_gensym_ix=0
_max_gensym_ix=0
_string_counter=0
defarr _characters_useds 16
_any_character_used=0
_rest_loc_var_fixups=0
_main_defined=0
_top_level_stmt=1
# IDENTIFIER_TYPE enum declaration
readonly _IDENTIFIER_INTERNAL=600
readonly _IDENTIFIER_STRING=601
readonly _IDENTIFIER_DOLLAR=602
defarr _preallocated_fresh_idents 10
defarr _preallocated_dollar_idents 10
: $((i = 0))
_init_comp_context() {
  let i
  i=0
  while [ $i -lt 16 ]; do
    : $((_$((_characters_useds + i)) = 0))
    : $((i += 1))
  done
  i=0
  while [ $i -lt 10 ]; do
    _new_ast0 _$((_preallocated_fresh_idents + i)) $_IDENTIFIER_INTERNAL $i
    : $((i += 1))
  done
  i=0
  while [ $i -lt 10 ]; do
    _new_ast0 _$((_preallocated_dollar_idents + i)) $_IDENTIFIER_DOLLAR $i
    : $((i += 1))
  done
  endlet $1 i
}

: $((decl = 0))
_append_glo_decl() { let decl $2
  : $((_$((_glo_decls + _glo_decl_ix)) = _nest_level))
  : $((_$((_glo_decls + _glo_decl_ix + 1)) = 1))
  : $((_$((_glo_decls + _glo_decl_ix + 2)) = decl))
  : $((_glo_decl_ix += 3))
  endlet $1 decl
}

_append_glo_decl_fixup() {
  : $((_$((_glo_decls + _glo_decl_ix)) = - _nest_level))
  : $((_$((_glo_decls + _glo_decl_ix + 1)) = 1))
  : $((_$((_glo_decls + _glo_decl_ix + 2)) = 0))
  : $((_glo_decl_ix += 3))
  : $(($1 = _glo_decl_ix - 3))
}

: $((decl = fixup_ix = 0))
_fixup_glo_decl() { let fixup_ix $2; let decl $3
  if [ $((_$((_glo_decls + fixup_ix)))) -ge 0 ] ; then
    defstr __str_142 "fixup_glo_decl: invalid fixup"
    _fatal_error __ $__str_142
  fi
  : $((_$((_glo_decls + fixup_ix)) = -_$((_glo_decls + fixup_ix))))
  : $((_$((_glo_decls + fixup_ix + 2)) = decl))
  endlet $1 decl fixup_ix
}

: $((start = 0))
_undo_glo_decls() { let start $2
  while [ $start -lt $_glo_decl_ix ]; do
    : $((_$((_glo_decls + start + 1)) -= 1))
    : $((start += 3))
  done
  endlet $1 start
}

: $((start = 0))
_any_active_glo_decls() { let start $2
  while [ $start -lt $_glo_decl_ix ]; do
    if [ $((_$((_glo_decls + start + 1)))) != 0 ] && [ $((_$((_glo_decls + start + 2)))) != 0 ] ; then
      : $(($1 = 1))
      endlet $1 start
      return
    fi
    : $((start += 3))
  done
  : $(($1 = 0))
  endlet $1 start
}

: $((reindent = end = start = 0))
_replay_glo_decls() { let start $2; let end $3; let reindent $4
  while [ $start -lt $end ]; do
    if [ $((_$((_glo_decls + start + 1)))) = 0 ] ; then
      _append_glo_decl __ $((_$((_glo_decls + start + 2))))
      if [ $((! reindent)) != 0 ] ; then
        : $((_$((_glo_decls + _glo_decl_ix - 3)) = _$((_glo_decls + start))))
      fi
    fi
    : $((start += 3))
  done
  endlet $1 reindent end start
}

: $((__t1 = res = end = start = 0))
_replay_glo_decls_inline() { let start $2; let end $3
  let res; let __t1
  res=0
  while [ $start -lt $end ]; do
    if [ $((_$((_glo_decls + start + 1)))) = 0 ] ; then
      defstr __str_143 "; "
      _wrap_str_lit __t1 $__str_143
      _concatenate_strings_with res $res $((_$((_glo_decls + start + 2)))) $__t1
    fi
    : $((start += 3))
  done
  if [ $res != 0 ] ; then
    defstr __str_143 "; "
    _wrap_str_lit __t1 $__str_143
    _string_concat res $res $__t1
  fi
  : $(($1 = res))
  endlet $1 __t1 res end start
}

: $((level = i = 0))
_print_glo_decls() {
  let i; let level
  i=0
  while [ $i -lt $_glo_decl_ix ]; do
    if [ $((_$((_glo_decls + i + 1)))) = 1 ] ; then
      if [ $((_$((_glo_decls + i + 2)))) != 0 ] ; then
        level=$((_$((_glo_decls + i))))
        while [ $level -gt 0 ]; do
          printf " "
          printf " "
          : $((level -= 1))
        done
        _print_text __ $((_$((_glo_decls + i + 2))))
        printf "\n"
      fi
    fi
    : $((i += 3))
  done
  endlet $1 level i
}

_cgc_fs=0
_cgc_locals=0
_cgc_locals_fun=0
_cgc_globals=0
_cgc_global_alloc=0
# BINDING enum declaration
readonly _BINDING_PARAM_LOCAL=0
readonly _BINDING_VAR_LOCAL=1
readonly _BINDING_VAR_GLOBAL=2
readonly _BINDING_ENUM_CST=3
readonly _BINDING_LOOP=4
readonly _BINDING_SWITCH=5
readonly _BINDING_FUN=6
readonly _BINDING_GOTO_LABEL=7
readonly _BINDING_TYPE_STRUCT=8
readonly _BINDING_TYPE_UNION=9
readonly _BINDING_TYPE_ENUM=10
: $((binding = ident = binding_type = 0))
_cgc_lookup_binding_ident() { let binding_type $2; let ident $3; let binding $4
  while [ $binding != 0 ]; do
    if [ $((_$((_heap + binding + 1)))) = $binding_type ] && [ $((_$((_heap + binding + 2)))) = $ident ] ; then
      break
    fi
    binding=$((_$((_heap + binding))))
  done
  : $(($1 = binding))
  endlet $1 binding ident binding_type
}

: $((binding = binding_type = 0))
_cgc_lookup_last_binding() { let binding_type $2; let binding $3
  while [ $binding != 0 ]; do
    if [ $((_$((_heap + binding + 1)))) = $binding_type ] ; then
      break
    fi
    binding=$((_$((_heap + binding))))
  done
  : $(($1 = binding))
  endlet $1 binding binding_type
}

: $((binding = ident = 0))
_cgc_lookup_var() { let ident $2; let binding $3
  while [ $binding != 0 ]; do
    if [ $((_$((_heap + binding + 1)))) -le $_BINDING_VAR_GLOBAL ] && [ $((_$((_heap + binding + 2)))) = $ident ] ; then
      break
    fi
    binding=$((_$((_heap + binding))))
  done
  : $(($1 = binding))
  endlet $1 binding ident
}

: $((env = ident = 0))
_cgc_lookup_fun() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_FUN $ident $env
  endlet $1 env ident
}

: $((env = 0))
_cgc_lookup_enclosing_loop() { let env $2
  _cgc_lookup_last_binding $1 $_BINDING_LOOP $env
  endlet $1 env
}

: $((env = 0))
_cgc_lookup_enclosing_switch() { let env $2
  _cgc_lookup_last_binding $1 $_BINDING_SWITCH $env
  endlet $1 env
}

: $((binding = 0))
_cgc_lookup_enclosing_loop_or_switch() { let binding $2
  while [ $binding != 0 ]; do
    if [ $((_$((_heap + binding + 1)))) = $_BINDING_LOOP ] || [ $((_$((_heap + binding + 1)))) = $_BINDING_SWITCH ] ; then
      break
    fi
    binding=$((_$((_heap + binding))))
  done
  : $(($1 = binding))
  endlet $1 binding
}

: $((env = ident = 0))
_cgc_lookup_goto_label() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_GOTO_LABEL $ident $env
  endlet $1 env ident
}

: $((env = ident = 0))
_cgc_lookup_struct() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_TYPE_STRUCT $ident $env
  endlet $1 env ident
}

: $((env = ident = 0))
_cgc_lookup_union() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_TYPE_UNION $ident $env
  endlet $1 env ident
}

: $((env = ident = 0))
_cgc_lookup_enum() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_TYPE_ENUM $ident $env
  endlet $1 env ident
}

: $((env = ident = 0))
_cgc_lookup_enum_value() { let ident $2; let env $3
  _cgc_lookup_binding_ident $1 $_BINDING_ENUM_CST $ident $env
  endlet $1 env ident
}

: $((loop_depth = binding = 0))
_cgc_loop_depth() { let binding $2
  let loop_depth
  loop_depth=0
  _cgc_lookup_enclosing_loop binding $binding
  while [ $binding != 0 ]; do
    _cgc_lookup_enclosing_loop binding $((_$((_heap + binding))))
    : $((loop_depth += 1))
  done
  : $(($1 = loop_depth))
  endlet $1 loop_depth binding
}

: $((binding = env = type = ident = binding_type = 0))
_cgc_add_local() { let binding_type $2; let ident $3; let type $4; let env $5
  let binding
  _alloc_obj binding 5
  : $((_$((_heap + binding + 0)) = env))
  : $((_$((_heap + binding + 1)) = binding_type))
  : $((_$((_heap + binding + 2)) = ident))
  : $((_$((_heap + binding + 3)) = _cgc_fs))
  : $((_$((_heap + binding + 4)) = type))
  : $(($1 = binding))
  endlet $1 binding env type ident binding_type
}

: $((__t1 = type = ident = binding_type = 0))
_cgc_add_local_var() { let binding_type $2; let ident $3; let type $4
  let __t1
  : $((_cgc_fs += 1))
  _cgc_add_local _cgc_locals $binding_type $ident $type $_cgc_locals
  if _cgc_lookup_var __t1 $ident $_cgc_locals_fun; [ $__t1 = 0 ] ; then
    _cgc_add_local _cgc_locals_fun $binding_type $ident $type $_cgc_locals_fun
  fi
  endlet $1 __t1 type ident binding_type
}

: $((binding = 0))
_cgc_add_enclosing_loop() {
  let binding
  _alloc_obj binding 4
  : $((_$((_heap + binding + 0)) = _cgc_locals))
  : $((_$((_heap + binding + 1)) = _BINDING_LOOP))
  : $((_$((_heap + binding + 2)) = 0))
  : $((_$((_heap + binding + 3)) = 0))
  _cgc_locals=$binding
  endlet $1 binding
}

: $((binding = in_tail_position = 0))
_cgc_add_enclosing_switch() { let in_tail_position $2
  let binding
  _alloc_obj binding 3
  : $((_$((_heap + binding + 0)) = _cgc_locals))
  : $((_$((_heap + binding + 1)) = _BINDING_SWITCH))
  : $((_$((_heap + binding + 2)) = in_tail_position))
  _cgc_locals=$binding
  endlet $1 binding in_tail_position
}

: $((__t2 = __t1 = i = prefixed_with_dollar = ident = 0))
_format_special_var() { let ident $2; let prefixed_with_dollar $3
  let i; let __t1; let __t2
  _get_op __t1 $ident
  case $__t1 in
    $_IDENTIFIER_INTERNAL)
      defstr __str_144 "__t"
      _wrap_str_lit __t1 $__str_144
      _get_val __t2 $ident
      _wrap_int __t2 $__t2
      _string_concat $1 $__t1 $__t2
    ;;
    $_IDENTIFIER_STRING)
      defstr __str_145 "__str_"
      _wrap_str_lit __t1 $__str_145
      _get_val __t2 $ident
      _wrap_int __t2 $__t2
      _string_concat $1 $__t1 $__t2
    ;;
    $_IDENTIFIER_DOLLAR)
      _get_val i $ident
      if [ $prefixed_with_dollar != 0 ] ; then
        if [ $i -le 9 ] ; then
          _wrap_int $1 $i
        else
          _wrap_int __t1 $i
          _string_concat3 $1 $((-__LBRACE__)) $__t1 $((-__RBRACE__))
        fi
      else
        if [ $i -le 9 ] ; then
          _wrap_int __t1 $i
          _string_concat $1 $((-__DOLLAR__)) $__t1
        else
          defstr __str_146 "\${"
          _wrap_str_lit __t1 $__str_146
          _wrap_int __t2 $i
          _string_concat3 $1 $__t1 $__t2 $((-__RBRACE__))
        fi
      fi
    ;;
    *)
      defstr __str_147 "format_special_var: unknown identifier type"
      _fatal_error __ $__str_147
      : $(($1 = 0))
    ;;
  esac
  endlet $1 __t2 __t1 i prefixed_with_dollar ident
}

: $((__t1 = ident_probe = 0))
_global_var() { let ident_probe $2
  let __t1
  _wrap_str_pool __t1 $ident_probe
  _string_concat $1 $((-__UNDERSCORE__)) $__t1
  endlet $1 __t1 ident_probe
}

: $((ident_probe = 0))
_local_var() { let ident_probe $2
  if [ $ident_probe = $_ARGV_ID ] ; then
    defstr __str_67 "argv_"
    _wrap_str_lit $1 $__str_67
  else
    _wrap_str_pool $1 $ident_probe
  fi
  endlet $1 ident_probe
}

: $((__t1 = prefixed_with_dollar = binding = ident_probe = 0))
_local_var_or_param() { let ident_probe $2; let binding $3; let prefixed_with_dollar $4
  let __t1
  if [ $((_$((_heap + binding + 1)))) = $_BINDING_PARAM_LOCAL ] && { _is_constant_type __t1 $((_$((_heap + binding + 4)))); [ $__t1 != 0 ]; } ; then
    if [ $prefixed_with_dollar != 0 ] ; then
      _wrap_int $1 $((_$((_heap + binding + 3))))
    else
      _wrap_int __t1 $((_$((_heap + binding + 3))))
      _string_concat $1 $((-__DOLLAR__)) $__t1
    fi
  else
    _local_var $1 $ident_probe
  fi
  endlet $1 __t1 prefixed_with_dollar binding ident_probe
}

: $((__t1 = ident_probe = binding = prefixed_with_dollar = ident = 0))
_env_var_with_prefix() { let ident $2; let prefixed_with_dollar $3
  let binding; let ident_probe; let __t1
  if _get_op __t1 $ident; [ $__t1 = $_IDENTIFIER ] ; then
    _get_val ident_probe $ident
    if _cgc_lookup_var binding $ident_probe $_cgc_locals; [ $binding != 0 ] ; then
      _local_var_or_param $1 $ident_probe $binding $prefixed_with_dollar
    else
      _global_var $1 $ident_probe
    fi
  else
    _format_special_var $1 $ident $prefixed_with_dollar
  fi
  endlet $1 __t1 ident_probe binding prefixed_with_dollar ident
}

: $((__t2 = __t1 = member_name_ident = 0))
_struct_member_var() { let member_name_ident $2
  let __t1; let __t2
  defstr __str_148 "__"
  _wrap_str_lit __t1 $__str_148
  _get_val __t2 $member_name_ident
  _wrap_str_pool __t2 $__t2
  _string_concat $1 $__t1 $__t2
  endlet $1 __t2 __t1 member_name_ident
}

: $((__t2 = __t1 = struct_name_ident = 0))
_struct_sizeof_var() { let struct_name_ident $2
  let __t1; let __t2
  defstr __str_149 "__sizeof__"
  _wrap_str_lit __t1 $__str_149
  _get_val __t2 $struct_name_ident
  _wrap_str_pool __t2 $__t2
  _string_concat $1 $__t1 $__t2
  endlet $1 __t2 __t1 struct_name_ident
}

: $((__t1 = ident_tok = 0))
_function_name() { let ident_tok $2
  let __t1
  _wrap_str_pool __t1 $ident_tok
  _string_concat $1 $((-__UNDERSCORE__)) $__t1
  endlet $1 __t1 ident_tok
}

: $((ix = 0))
_new_dollar_ident() { let ix $2
  if [ $ix -lt 10 ] ; then
    : $(($1 = _$((_preallocated_dollar_idents + ix))))
  else
    _new_ast0 $1 $_IDENTIFIER_DOLLAR $ix
  fi
  endlet $1 ix
}

: $((ix = 0))
_new_fresh_ident() { let ix $2
  if [ $ix -lt 10 ] ; then
    : $(($1 = _$((_preallocated_fresh_idents + ix))))
  else
    _new_ast0 $1 $_IDENTIFIER_INTERNAL $ix
  fi
  endlet $1 ix
}

_fresh_ident() {
  : $((_gensym_ix += 1))
  if [ $_gensym_ix -gt $_fun_gensym_ix ] ; then
    _fun_gensym_ix=$_gensym_ix
  fi
  if [ $_gensym_ix -gt $_max_gensym_ix ] ; then
    _max_gensym_ix=$_gensym_ix
  fi
  _new_fresh_ident $1 $_gensym_ix
}

: $((string_probe = 0))
_fresh_string_ident() { let string_probe $2
  if [ $((_$((_heap + string_probe + 3)))) = 0 ] ; then
    : $((_string_counter += 1))
    : $((_$((_heap + string_probe + 3)) = _string_counter - 1))
  fi
  _new_ast0 $1 $_IDENTIFIER_STRING $((_$((_heap + string_probe + 3))))
  endlet $1 string_probe
}

: $((__t1 = ident_probe = kind = decl = 0))
_add_var_to_local_env() { let decl $2; let kind $3
  let ident_probe; let __t1
  _get_child __t1 $decl 0
  _get_val ident_probe $__t1
  if _cgc_lookup_var __t1 $ident_probe $_cgc_locals; [ $__t1 != 0 ] ; then
    printf "var="
    _putstr __ $((_string_pool + _$((_heap + ident_probe + 1))))
    printf "\n"
    defstr __str_150 "Variable is already in local environment"
    _fatal_error __ $__str_150
  fi
  _get_child __t1 $decl 1
  _cgc_add_local_var __ $kind $ident_probe $__t1
  endlet $1 __t1 ident_probe kind decl
}

: $((__t1 = lst = 0))
_add_fun_params_to_local_env() { let lst $2
  let __t1
  while [ $lst != 0 ]; do
    _car_ __t1 $_DECL $lst
    _add_var_to_local_env __ $__t1 $_BINDING_PARAM_LOCAL
    _cdr_ lst $_LIST $lst
  done
  endlet $1 __t1 lst
}

: $((__t1 = type = name = ident_probe = local = variable = 0))
_assert_var_decl_is_safe() { let variable $2; let local $3
  let ident_probe; let name; let type; let __t1
  _get_child __t1 $variable 0
  _get_val ident_probe $__t1
  name=$((_string_pool + _$((_heap + ident_probe + 1))))
  _get_child type $variable 1
  if [ $((_$((name + 0)))) = $__UNDERSCORE__ ] || { [ $((_$((name + 0)))) != $__NUL__ ] && [ $((_$((name + 1)))) = $__UNDERSCORE__ ] && [ $((_$((name + 2)))) = $__NUL__ ]; } ; then
    _put_pstr __ $name
    printf " "
    defstr __str_151 "variable name is invalid. It can't start or end with '_'."
    _fatal_error __ $__str_151
  fi
  if [ $local != 0 ] && { [ $ident_probe = $_ARGV__ID ] || [ $ident_probe = $_IFS_ID ]; } ; then
    printf "\""
    _put_pstr __ $name
    printf "\" "
    defstr __str_152 "variable name is invalid. It can't be 'IFS' or 'argv_'."
    _fatal_error __ $__str_152
  fi
  if [ $local != 0 ] ; then
    if { _get_op __t1 $type; [ $__t1 = $__LBRACK__ ]; } || { _get_op __t1 $type; [ $__t1 = $_STRUCT_KW ]; } ; then
      printf "\""
      _put_pstr __ $name
      printf "\" variable: "
      defstr __str_153 "array/struct value type is not supported for shell backend. Use a reference type instead."
      _fatal_error __ $__str_153
    fi
  else
    if { { _get_op __t1 $type; [ $__t1 = $__LBRACK__ ]; } && { _get_child __t1 $type 0; _get_op __t1 $__t1; [ $__t1 = $_STRUCT_KW ]; }; } || { { _get_op __t1 $type; [ $__t1 = $__LBRACK__ ]; } && { _get_child __t1 $type 0; _get_op __t1 $__t1; [ $__t1 = $__LBRACK__ ]; }; } || { _get_op __t1 $type; [ $__t1 = $_STRUCT_KW ]; } ; then
      printf "\""
      _put_pstr __ $name
      printf "\" variable: "
      defstr __str_154 "array of struct and struct value type are not supported in shell backend. Use a reference type instead."
      _fatal_error __ $__str_154
    fi
  fi
  endlet $1 __t1 type name ident_probe local variable
}

: $((__t1 = lst = 0))
_check_decls() { let lst $2
  let __t1
  while [ $lst != 0 ]; do
    _car_ __t1 $_DECL $lst
    _assert_var_decl_is_safe __ $__t1 1
    _cdr_ lst $_LIST $lst
  done
  endlet $1 __t1 lst
}

: $((__t3 = __t2 = __t1 = params_ix = res = decl = ident = params = 0))
_let_params() { let params $2
  let ident; let decl; let res; let params_ix; let __t1; let __t2; let __t3
  res=0
  params_ix=2
  while [ $params != 0 ]; do
    _car_ decl $_DECL $params
    if _get_child __t1 $decl 1; _is_constant_type __t1 $__t1; [ $((!__t1)) != 0 ] ; then
      _get_child __t1 $decl 0
      _get_val ident $__t1
      defstr __str_155 "let "
      _wrap_str_lit __t1 $__str_155
      _local_var __t2 $ident
      _new_dollar_ident __t3 $params_ix
      _format_special_var __t3 $__t3 0
      _string_concat4 __t1 $__t1 $__t2 $((-__SPACE__)) $__t3
      defstr __str_143 "; "
      _wrap_str_lit __t2 $__str_143
      _concatenate_strings_with res $res $__t1 $__t2
    fi
    _cdr_ params $_LIST $params
    : $((params_ix += 1))
  done
  : $((_runtime_use_local_vars |= (res != 0)))
  if [ $res != 0 ] ; then
    _string_concat res $((-__SPACE__)) $res
  fi
  : $(($1 = res))
  endlet $1 __t3 __t2 __t1 params_ix res decl ident params
}

: $((__t2 = __t1 = counter = res = ident = env = 0))
_save_local_vars() {
  let env; let ident; let res; let counter; let __t1; let __t2
  env=$_cgc_locals_fun
  res=0
  counter=$_fun_gensym_ix
  while [ $counter -gt 0 ]; do
    _new_fresh_ident ident $counter
    defstr __str_155 "let "
    _wrap_str_lit __t1 $__str_155
    _format_special_var __t2 $ident 1
    _string_concat __t1 $__t1 $__t2
    defstr __str_143 "; "
    _wrap_str_lit __t2 $__str_143
    _concatenate_strings_with res $__t1 $res $__t2
    : $((counter -= 1))
  done
  while [ $env != 0 ]; do
    if [ $((_$((_heap + env + 1)))) != $_BINDING_PARAM_LOCAL ] ; then
      ident=$((_$((_heap + env + 2))))
      defstr __str_155 "let "
      _wrap_str_lit __t1 $__str_155
      _local_var __t2 $ident
      _string_concat __t1 $__t1 $__t2
      defstr __str_143 "; "
      _wrap_str_lit __t2 $__str_143
      _concatenate_strings_with res $__t1 $res $__t2
    fi
    env=$((_$((_heap + env))))
  done
  : $((_runtime_use_local_vars |= (res != 0)))
  : $(($1 = res))
  endlet $1 __t2 __t1 counter res ident env
}

: $((__t1 = counter = res = ident = env = params_count = 0))
_restore_local_vars() { let params_count $2
  let env; let ident; let res; let counter; let __t1
  env=$_cgc_locals_fun
  res=0
  counter=$_fun_gensym_ix
  while [ $counter -gt 0 ]; do
    _new_fresh_ident ident $counter
    _format_special_var __t1 $ident 0
    _concatenate_strings_with res $res $__t1 $((-__SPACE__))
    : $((counter -= 1))
  done
  while [ $env != 0 ]; do
    ident=$((_$((_heap + env + 2))))
    if [ $((_$((_heap + env + 1)))) != $_BINDING_PARAM_LOCAL ] || { _is_constant_type __t1 $((_$((_heap + env + 4)))); [ $((!__t1)) != 0 ]; } ; then
      _local_var __t1 $ident
      _concatenate_strings_with res $res $__t1 $((-__SPACE__))
    fi
    env=$((_$((_heap + env))))
  done
  if [ $res != 0 ] ; then
    _runtime_use_local_vars=1
    defstr __str_156 "endlet \$1 "
    _wrap_str_lit __t1 $__str_156
    _string_concat $1 $__t1 $res
  else
    : $(($1 = 0))
  fi
  endlet $1 __t1 counter res ident env params_count
}

: $((op = 0))
_op_to_str() { let op $2
  if [ $op -lt 256 ] ; then
    _string_concat3 $1 $((-__SPACE__)) $((- op)) $((-__SPACE__))
  elif [ $op = $_AMP_AMP ] ; then
    defstr __str_157 " && "
    _wrap_str_lit $1 $__str_157
  elif [ $op = $_AMP_EQ ] ; then
    defstr __str_158 " &= "
    _wrap_str_lit $1 $__str_158
  elif [ $op = $_BAR_BAR ] ; then
    defstr __str_159 " || "
    _wrap_str_lit $1 $__str_159
  elif [ $op = $_BAR_EQ ] ; then
    defstr __str_160 " |= "
    _wrap_str_lit $1 $__str_160
  elif [ $op = $_CARET_EQ ] ; then
    defstr __str_161 " ^= "
    _wrap_str_lit $1 $__str_161
  elif [ $op = $_EQ_EQ ] ; then
    defstr __str_162 " == "
    _wrap_str_lit $1 $__str_162
  elif [ $op = $_GT_EQ ] ; then
    defstr __str_163 " >= "
    _wrap_str_lit $1 $__str_163
  elif [ $op = $_LSHIFT_EQ ] ; then
    defstr __str_164 " <<= "
    _wrap_str_lit $1 $__str_164
  elif [ $op = $_LT_EQ ] ; then
    defstr __str_165 " <= "
    _wrap_str_lit $1 $__str_165
  elif [ $op = $_LSHIFT ] ; then
    defstr __str_166 " << "
    _wrap_str_lit $1 $__str_166
  elif [ $op = $_MINUS_EQ ] ; then
    defstr __str_167 " -= "
    _wrap_str_lit $1 $__str_167
  elif [ $op = $_EXCL_EQ ] ; then
    defstr __str_168 " != "
    _wrap_str_lit $1 $__str_168
  elif [ $op = $_PERCENT_EQ ] ; then
    defstr __str_169 " %= "
    _wrap_str_lit $1 $__str_169
  elif [ $op = $_PLUS_EQ ] ; then
    defstr __str_170 " += "
    _wrap_str_lit $1 $__str_170
  elif [ $op = $_RSHIFT_EQ ] ; then
    defstr __str_171 " >>= "
    _wrap_str_lit $1 $__str_171
  elif [ $op = $_RSHIFT ] ; then
    defstr __str_172 " >> "
    _wrap_str_lit $1 $__str_172
  elif [ $op = $_SLASH_EQ ] ; then
    defstr __str_173 " /= "
    _wrap_str_lit $1 $__str_173
  elif [ $op = $_STAR_EQ ] ; then
    defstr __str_174 " *= "
    _wrap_str_lit $1 $__str_174
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    printf "\n"
    defstr __str_175 "op_to_str: unexpected operator"
    _fatal_error __ $__str_175
    : $(($1 = 0))
  fi
  endlet $1 op
}

: $((op = 0))
_test_op_to_str() { let op $2
  if [ $op = $_EQ_EQ ] ; then
    defstr __str_176 " = "
    _wrap_str_lit $1 $__str_176
  elif [ $op = $_EXCL_EQ ] ; then
    defstr __str_168 " != "
    _wrap_str_lit $1 $__str_168
  elif [ $op = $__LT__ ] ; then
    defstr __str_177 " -lt "
    _wrap_str_lit $1 $__str_177
  elif [ $op = $__GT__ ] ; then
    defstr __str_178 " -gt "
    _wrap_str_lit $1 $__str_178
  elif [ $op = $_LT_EQ ] ; then
    defstr __str_179 " -le "
    _wrap_str_lit $1 $__str_179
  elif [ $op = $_GT_EQ ] ; then
    defstr __str_180 " -ge "
    _wrap_str_lit $1 $__str_180
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    printf "\n"
    defstr __str_181 "test_op_to_str: unexpected operator"
    _fatal_error __ $__str_181
    : $(($1 = 0))
  fi
  endlet $1 op
}

: $((c = 0))
_character_ident() { let c $2
  : $((_$((_characters_useds + c / 16)) |= (1 << (c % 16))))
  _any_character_used=1
  if { [ $__A__ -le $c ] && [ $c -le $__Z__ ]; } || { [ $__a__ -le $c ] && [ $c -le $__z__ ]; } || { [ $__0__ -le $c ] && [ $c -le $__9__ ]; } ; then
    _string_concat5 $1 $((-__UNDERSCORE__)) $((-__UNDERSCORE__)) $((- c)) $((-__UNDERSCORE__)) $((-__UNDERSCORE__))
  else
    if [ $c = $__NUL__ ] ; then
      defstr __str_182 "__NUL__"
      _wrap_str_lit $1 $__str_182
    elif [ $c = $__NEWLINE__ ] ; then
      defstr __str_183 "__NEWLINE__"
      _wrap_str_lit $1 $__str_183
    elif [ $c = $__SPACE__ ] ; then
      defstr __str_184 "__SPACE__"
      _wrap_str_lit $1 $__str_184
    elif [ $c = $__EXCL__ ] ; then
      defstr __str_185 "__EXCL__"
      _wrap_str_lit $1 $__str_185
    elif [ $c = $__DQUOTE__ ] ; then
      defstr __str_186 "__DQUOTE__"
      _wrap_str_lit $1 $__str_186
    elif [ $c = $__SHARP__ ] ; then
      defstr __str_187 "__SHARP__"
      _wrap_str_lit $1 $__str_187
    elif [ $c = $__DOLLAR__ ] ; then
      defstr __str_188 "__DOLLAR__"
      _wrap_str_lit $1 $__str_188
    elif [ $c = $__PERCENT__ ] ; then
      defstr __str_189 "__PERCENT__"
      _wrap_str_lit $1 $__str_189
    elif [ $c = $__AMP__ ] ; then
      defstr __str_190 "__AMP__"
      _wrap_str_lit $1 $__str_190
    elif [ $c = $__QUOTE__ ] ; then
      defstr __str_191 "__QUOTE__"
      _wrap_str_lit $1 $__str_191
    elif [ $c = $__LPAREN__ ] ; then
      defstr __str_192 "__LPAREN__"
      _wrap_str_lit $1 $__str_192
    elif [ $c = $__RPAREN__ ] ; then
      defstr __str_193 "__RPAREN__"
      _wrap_str_lit $1 $__str_193
    elif [ $c = $__STAR__ ] ; then
      defstr __str_194 "__STAR__"
      _wrap_str_lit $1 $__str_194
    elif [ $c = $__PLUS__ ] ; then
      defstr __str_195 "__PLUS__"
      _wrap_str_lit $1 $__str_195
    elif [ $c = $__COMMA__ ] ; then
      defstr __str_196 "__COMMA__"
      _wrap_str_lit $1 $__str_196
    elif [ $c = $__MINUS__ ] ; then
      defstr __str_197 "__MINUS__"
      _wrap_str_lit $1 $__str_197
    elif [ $c = $__PERIOD__ ] ; then
      defstr __str_198 "__PERIOD__"
      _wrap_str_lit $1 $__str_198
    elif [ $c = $__SLASH__ ] ; then
      defstr __str_199 "__SLASH__"
      _wrap_str_lit $1 $__str_199
    elif [ $c = $__COLON__ ] ; then
      defstr __str_200 "__COLON__"
      _wrap_str_lit $1 $__str_200
    elif [ $c = $__SEMICOLON__ ] ; then
      defstr __str_201 "__SEMICOLON__"
      _wrap_str_lit $1 $__str_201
    elif [ $c = $__LT__ ] ; then
      defstr __str_202 "__LT__"
      _wrap_str_lit $1 $__str_202
    elif [ $c = $__EQ__ ] ; then
      defstr __str_203 "__EQ__"
      _wrap_str_lit $1 $__str_203
    elif [ $c = $__GT__ ] ; then
      defstr __str_204 "__GT__"
      _wrap_str_lit $1 $__str_204
    elif [ $c = $__QUESTION__ ] ; then
      defstr __str_205 "__QUESTION__"
      _wrap_str_lit $1 $__str_205
    elif [ $c = $__AT__ ] ; then
      defstr __str_206 "__AT__"
      _wrap_str_lit $1 $__str_206
    elif [ $c = $__CARET__ ] ; then
      defstr __str_207 "__CARET__"
      _wrap_str_lit $1 $__str_207
    elif [ $c = $__LBRACK__ ] ; then
      defstr __str_208 "__LBRACK__"
      _wrap_str_lit $1 $__str_208
    elif [ $c = $__BACKSLASH__ ] ; then
      defstr __str_209 "__BACKSLASH__"
      _wrap_str_lit $1 $__str_209
    elif [ $c = $__RBRACK__ ] ; then
      defstr __str_210 "__RBRACK__"
      _wrap_str_lit $1 $__str_210
    elif [ $c = $__UNDERSCORE__ ] ; then
      defstr __str_211 "__UNDERSCORE__"
      _wrap_str_lit $1 $__str_211
    elif [ $c = $__BACKTICK__ ] ; then
      defstr __str_212 "__BACKTICK__"
      _wrap_str_lit $1 $__str_212
    elif [ $c = $__LBRACE__ ] ; then
      defstr __str_213 "__LBRACE__"
      _wrap_str_lit $1 $__str_213
    elif [ $c = $__BAR__ ] ; then
      defstr __str_214 "__BAR__"
      _wrap_str_lit $1 $__str_214
    elif [ $c = $__RBRACE__ ] ; then
      defstr __str_215 "__RBRACE__"
      _wrap_str_lit $1 $__str_215
    elif [ $c = $__TILDE__ ] ; then
      defstr __str_216 "__TILDE__"
      _wrap_str_lit $1 $__str_216
    elif [ $c = $__ALARM__ ] ; then
      defstr __str_217 "__ALARM__"
      _wrap_str_lit $1 $__str_217
    elif [ $c = $__BACKSPACE__ ] ; then
      defstr __str_218 "__BACKSPACE__"
      _wrap_str_lit $1 $__str_218
    elif [ $c = $__PAGE__ ] ; then
      defstr __str_219 "__PAGE__"
      _wrap_str_lit $1 $__str_219
    elif [ $c = $__RET__ ] ; then
      defstr __str_220 "__RET__"
      _wrap_str_lit $1 $__str_220
    elif [ $c = $__TAB__ ] ; then
      defstr __str_221 "__TAB__"
      _wrap_str_lit $1 $__str_221
    elif [ $c = $__VTAB__ ] ; then
      defstr __str_222 "__VTAB__"
      _wrap_str_lit $1 $__str_222
    else
      defstr __str_223 "Unknown character"
      _fatal_error __ $__str_223
      : $(($1 = 0))
    fi
  fi
  endlet $1 c
}

_replaced_fun_calls=0
_replaced_fun_calls_tail=0
_conditional_fun_calls=0
_conditional_fun_calls_tail=0
_literals_inits=0
_contains_side_effects=0
: $((__t1 = sub1 = start_gensym_ix = executes_conditionally = assign_to = node = 0))
_handle_fun_call_side_effect() { let node $2; let assign_to $3; let executes_conditionally $4
  let start_gensym_ix; let sub1; let __t1
  start_gensym_ix=$_gensym_ix
  if [ $assign_to = 0 ] ; then
    _fresh_ident assign_to
    start_gensym_ix=$_gensym_ix
    : $((_gensym_ix -= 1))
  fi
  _get_child sub1 $node 1
  while [ $sub1 != 0 ]; do
    _car __t1 $sub1
    _handle_side_effects_go __t1 $__t1 $executes_conditionally
    _set_child __ $sub1 0 $__t1
    _cdr_ sub1 $_LIST $sub1
  done
  _gensym_ix=$start_gensym_ix
  _new_ast2 __t1 $__EQ__ $assign_to $node
  _cons sub1 $__t1 0
  if [ $executes_conditionally != 0 ] ; then
    if [ $_conditional_fun_calls = 0 ] ; then
      _conditional_fun_calls=$sub1
    else
      _set_child __ $_conditional_fun_calls_tail 1 $sub1
    fi
    _conditional_fun_calls_tail=$sub1
  else
    if [ $_replaced_fun_calls = 0 ] ; then
      _replaced_fun_calls=$sub1
    else
      _set_child __ $_replaced_fun_calls_tail 1 $sub1
    fi
    _replaced_fun_calls_tail=$sub1
  fi
  : $(($1 = assign_to))
  endlet $1 __t1 sub1 start_gensym_ix executes_conditionally assign_to node
}

: $((__t1 = child2 = child1 = child0 = start_gensym_ix = right_conditional_fun_calls = left_conditional_fun_calls = previous_conditional_fun_calls = sub2 = sub1 = nb_children = op = executes_conditionally = node = 0))
_handle_side_effects_go() { let node $2; let executes_conditionally $3
  let op; let nb_children; let sub1; let sub2; let previous_conditional_fun_calls; let left_conditional_fun_calls; let right_conditional_fun_calls; let start_gensym_ix; let child0; let child1; let child2; let __t1
  _get_op op $node
  _get_nb_children nb_children $node
  start_gensym_ix=$_gensym_ix
  if [ $nb_children -ge 1 ] ; then
    _get_child child0 $node 0
  fi
  if [ $nb_children -ge 2 ] ; then
    _get_child child1 $node 1
  fi
  if [ $nb_children -ge 3 ] ; then
    _get_child child2 $node 2
  fi
  if [ $nb_children = 0 ] ; then
    if [ $op = $_IDENTIFIER ] || [ $op = $_IDENTIFIER_INTERNAL ] || [ $op = $_IDENTIFIER_STRING ] || [ $op = $_IDENTIFIER_DOLLAR ] || [ $op = $_CHARACTER ] || [ $op = $_INTEGER ] || [ $op = $_INTEGER_HEX ] || [ $op = $_INTEGER_OCT ] ; then
      : $(($1 = node))
    elif [ $op = $_STRING ] ; then
      _get_val __t1 $node
      _fresh_string_ident sub1 $__t1
      _get_val __t1 $node
      _new_ast2 __t1 $__EQ__ $sub1 $__t1
      _cons _literals_inits $__t1 $_literals_inits
      : $(($1 = sub1))
    else
      printf "op=%d " $op
      printf \\$((op/64))$((op/8%8))$((op%8))
      defstr __str_224 "unexpected operator"
      _fatal_error __ $__str_224
      : $(($1 = 0))
    fi
  elif [ $nb_children = 1 ] ; then
    if [ $op = $__AMP__ ] || [ $op = $__STAR__ ] || [ $op = $__PLUS__ ] || [ $op = $__MINUS__ ] || [ $op = $__TILDE__ ] || [ $op = $__EXCL__ ] || [ $op = $_PARENS ] ; then
      _handle_side_effects_go __t1 $child0 $executes_conditionally
      _new_ast1 $1 $op $__t1
    elif [ $op = $_PLUS_PLUS_PRE ] || [ $op = $_MINUS_MINUS_PRE ] || [ $op = $_PLUS_PLUS_POST ] || [ $op = $_MINUS_MINUS_POST ] ; then
      _contains_side_effects=1
      _handle_side_effects_go __t1 $child0 $executes_conditionally
      _new_ast1 $1 $op $__t1
    elif [ $op = $_SIZEOF_KW ] ; then
      : $(($1 = node))
    else
      printf "op=%d " $op
      printf \\$((op/64))$((op/8%8))$((op%8))
      defstr __str_224 "unexpected operator"
      _fatal_error __ $__str_224
      : $(($1 = 0))
    fi
  elif [ $nb_children = 2 ] ; then
    if [ $op = $__LPAREN__ ] ; then
      _handle_fun_call_side_effect $1 $node 0 $executes_conditionally
    elif [ $op = $__EQ__ ] ; then
      if _get_op __t1 $child1; [ $__t1 = $__LPAREN__ ] ; then
        _handle_fun_call_side_effect $1 $child1 $child0 $executes_conditionally
      else
        _handle_side_effects_go sub1 $child0 $executes_conditionally
        _handle_side_effects_go sub2 $child1 $executes_conditionally
        _new_ast2 $1 $op $sub1 $sub2
      fi
    elif [ $op = $__AMP__ ] || [ $op = $__BAR__ ] || [ $op = $__LT__ ] || [ $op = $__GT__ ] || [ $op = $__PLUS__ ] || [ $op = $__MINUS__ ] || [ $op = $__STAR__ ] || [ $op = $__SLASH__ ] || [ $op = $__PERCENT__ ] || [ $op = $__CARET__ ] || [ $op = $__COMMA__ ] || [ $op = $_EQ_EQ ] || [ $op = $_EXCL_EQ ] || [ $op = $_LT_EQ ] || [ $op = $_GT_EQ ] || [ $op = $_LSHIFT ] || [ $op = $_RSHIFT ] || [ $op = $__LBRACK__ ] || [ $op = $__PERIOD__ ] || [ $op = $_ARROW ] ; then
      _handle_side_effects_go sub1 $child0 $executes_conditionally
      _handle_side_effects_go sub2 $child1 $executes_conditionally
      _new_ast2 $1 $op $sub1 $sub2
    elif [ $op = $_AMP_EQ ] || [ $op = $_BAR_EQ ] || [ $op = $_CARET_EQ ] || [ $op = $_LSHIFT_EQ ] || [ $op = $_MINUS_EQ ] || [ $op = $_PERCENT_EQ ] || [ $op = $_PLUS_EQ ] || [ $op = $_RSHIFT_EQ ] || [ $op = $_SLASH_EQ ] || [ $op = $_STAR_EQ ] ; then
      _contains_side_effects=1
      _handle_side_effects_go sub1 $child0 $executes_conditionally
      _handle_side_effects_go sub2 $child1 $executes_conditionally
      _new_ast2 $1 $op $sub1 $sub2
    elif [ $op = $_AMP_AMP ] || [ $op = $_BAR_BAR ] ; then
      previous_conditional_fun_calls=$_conditional_fun_calls
      _conditional_fun_calls=0
      _handle_side_effects_go sub1 $child0 1
      _gensym_ix=$start_gensym_ix
      left_conditional_fun_calls=$_conditional_fun_calls
      _conditional_fun_calls=0
      _handle_side_effects_go sub2 $child1 1
      _gensym_ix=$start_gensym_ix
      right_conditional_fun_calls=$_conditional_fun_calls
      _conditional_fun_calls=$previous_conditional_fun_calls
      _new_ast4 $1 $op $sub1 $sub2 $left_conditional_fun_calls $right_conditional_fun_calls
    elif [ $op = $_CAST ] ; then
      _handle_side_effects_go __t1 $child1 $executes_conditionally
      _new_ast2 $1 $_CAST $child0 $__t1
    else
      defstr __str_224 "unexpected operator"
      _fatal_error __ $__str_224
      : $(($1 = 0))
    fi
  elif [ $nb_children = 3 ] && [ $op = $__QUESTION__ ] ; then
    previous_conditional_fun_calls=$_conditional_fun_calls
    _conditional_fun_calls=0
    _handle_side_effects_go sub1 $child1 1
    left_conditional_fun_calls=$_conditional_fun_calls
    _conditional_fun_calls=0
    _handle_side_effects_go sub2 $child2 1
    right_conditional_fun_calls=$_conditional_fun_calls
    if [ $left_conditional_fun_calls != 0 ] || [ $right_conditional_fun_calls != 0 ] ; then
      defstr __str_225 "Conditional function calls in ternary operator not allowed"
      _fatal_error __ $__str_225
    fi
    _handle_side_effects_go __t1 $child0 $executes_conditionally
    _new_ast3 $1 $__QUESTION__ $__t1 $sub1 $sub2
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    _get_nb_children __t1 $node
    printf " with %d children\n" $__t1
    defstr __str_224 "unexpected operator"
    _fatal_error __ $__str_224
    : $(($1 = 0))
  fi
  endlet $1 __t1 child2 child1 child0 start_gensym_ix right_conditional_fun_calls left_conditional_fun_calls previous_conditional_fun_calls sub2 sub1 nb_children op executes_conditionally node
}

: $((node = 0))
_handle_side_effects() { let node $2
  _replaced_fun_calls=0
  _conditional_fun_calls=0
  _literals_inits=0
  _contains_side_effects=0
  _handle_side_effects_go $1 $node 0
  endlet $1 node
}

: $((__t4 = __t3 = __t2 = __t1 = array_size_text = string_end = string_start = array_size = string_probe = ident = 0))
_comp_defstr() { let ident $2; let string_probe $3; let array_size $4
  let string_start; let string_end; let array_size_text; let __t1; let __t2; let __t3; let __t4
  string_start=$((_string_pool + _$((_heap + string_probe + 1))))
  string_end=$((_string_pool + _$((_heap + string_probe + 1)) + _$((_heap + string_probe + 4))))
  array_size_text=0
  if [ $array_size != -1 ] ; then
    _wrap_int __t1 $array_size
    _string_concat array_size_text $((-__SPACE__)) $__t1
  fi
  if [ $_top_level_stmt != 0 ] ; then
    _runtime_defstr __
  else
    _runtime_use_defstr=1
  fi
  defstr __str_227 " \""
  defstr __str_226 "defstr "
  _wrap_str_lit __t1 $__str_226
  _env_var_with_prefix __t2 $ident 0
  _wrap_str_lit __t3 $__str_227
  _wrap_str_imm __t4 $string_start $string_end
  _escape_text __t4 $__t4 0
  _string_concat3 __t3 $__t3 $__t4 $((-__DQUOTE__))
  _string_concat4 __t1 $__t1 $__t2 $__t3 $array_size_text
  _append_glo_decl __ $__t1
  endlet $1 __t4 __t3 __t2 __t1 array_size_text string_end string_start array_size string_probe ident
}

: $((res = node = 0))
_initializer_list_len() { let node $2
  let res
  res=0
  while [ $node != 0 ]; do
    : $((res += 1))
    _cdr_ node $_LIST $node
  done
  : $(($1 = res))
  endlet $1 res node
}

: $((__t3 = __t2 = __t1 = str_ident = element = args = expected_len = initializer_list = 0))
_comp_initializer_list() { let initializer_list $2; let expected_len $3
  let args; let element; let str_ident; let __t1; let __t2; let __t3
  args=0
  _runtime_use_initialize=1
  while [ $initializer_list != 0 ]; do
    _car element $initializer_list
    _get_op __t1 $element
    case $__t1 in
      $_INTEGER)
        _get_val __t1 $element
        _wrap_int __t1 $((-__t1))
        _concatenate_strings_with args $args $__t1 $((-__SPACE__))
      ;;
      $_INTEGER_HEX|$_INTEGER_OCT)
        defstr __str_229 "))"
        defstr __str_228 "\$(("
        _wrap_str_lit __t1 $__str_228
        _wrap_integer __t2 1 $element
        _wrap_str_lit __t3 $__str_229
        _string_concat3 __t1 $__t1 $__t2 $__t3
        _concatenate_strings_with args $args $__t1 $((-__SPACE__))
      ;;
      $_CHARACTER)
        _get_val __t1 $element
        _wrap_int __t1 $__t1
        _concatenate_strings_with args $args $__t1 $((-__SPACE__))
      ;;
      $_STRING)
        _get_val __t1 $element
        _fresh_string_ident str_ident $__t1
        _get_val __t1 $element
        _comp_defstr __ $str_ident $__t1 -1
        _format_special_var __t1 $str_ident 1
        _string_concat __t1 $((-__DOLLAR__)) $__t1
        _concatenate_strings_with args $args $__t1 $((-__SPACE__))
      ;;
      *)
        defstr __str_230 "comp_initializer: unexpected operator"
        _fatal_error __ $__str_230
      ;;
    esac
    _cdr_ initializer_list $_LIST $initializer_list
  done
  : $(($1 = args))
  endlet $1 __t3 __t2 __t1 str_ident element args expected_len initializer_list
}

# VALUE_CTX enum declaration
readonly _RVALUE_CTX_BASE=0
readonly _RVALUE_CTX_ARITH_EXPANSION=1
readonly _RVALUE_CTX_TEST=2
readonly _RVALUE_CTX_TEST_ELSEIF=3
: $((__t2 = __t1 = side_effect = test_side_effects_code = code = test_side_effects = 0))
_with_prefixed_side_effects() { let test_side_effects $2; let code $3
  let test_side_effects_code; let side_effect; let __t1; let __t2
  test_side_effects_code=0
  while [ $test_side_effects != 0 ]; do
    _car_ side_effect $__EQ__ $test_side_effects
    _get_child __t1 $side_effect 1
    _get_child __t2 $side_effect 0
    _comp_fun_call_code __t1 $__t1 $__t2
    defstr __str_143 "; "
    _wrap_str_lit __t2 $__str_143
    _string_concat3 test_side_effects_code $test_side_effects_code $__t1 $__t2
    _cdr_ test_side_effects $_LIST $test_side_effects
  done
  if [ $test_side_effects_code != 0 ] ; then
    defstr __str_231 "{ "
    _wrap_str_lit __t1 $__str_231
    defstr __str_232 "; }"
    _wrap_str_lit __t2 $__str_232
    _string_concat4 $1 $__t1 $test_side_effects_code $code $__t2
  else
    : $(($1 = code))
  fi
  endlet $1 __t2 __t1 side_effect test_side_effects_code code test_side_effects
}

: $((op = 0))
_is_associative_operator() { let op $2
  : $(($1 = (op == __PLUS__) | (op == __STAR__) | (op == __AMP__) | (op == __BAR__) | (op == __CARET__) | (op == _EQ_EQ) | (op == _AMP_AMP) | (op == _BAR_BAR)))
  endlet $1 op
}

: $((__t2 = __t1 = inner_op = outer_op = code = test_side_effects = context = parens_otherwise = 0))
_wrap_if_needed() { let parens_otherwise $2; let context $3; let test_side_effects $4; let code $5; let outer_op $6; let inner_op $7
  let __t1; let __t2
  if [ $context = $_RVALUE_CTX_ARITH_EXPANSION ] ; then
    if [ $parens_otherwise != 0 ] && [ $outer_op != 0 ] && [ $outer_op != $__EQ__ ] && { { _is_associative_operator __t1 $inner_op; [ $((!__t1)) != 0 ]; } || [ $inner_op != $outer_op ]; } ; then
      _string_concat3 $1 $((-__LPAREN__)) $code $((-__RPAREN__))
    else
      : $(($1 = code))
    fi
  elif [ $context = $_RVALUE_CTX_TEST ] ; then
    defstr __str_234 ")) != 0 ]"
    defstr __str_233 "[ \$(("
    _wrap_str_lit __t1 $__str_233
    _wrap_str_lit __t2 $__str_234
    _string_concat3 __t1 $__t1 $code $__t2
    _with_prefixed_side_effects $1 $test_side_effects $__t1
  else
    defstr __str_228 "\$(("
    _wrap_str_lit __t1 $__str_228
    defstr __str_229 "))"
    _wrap_str_lit __t2 $__str_229
    _string_concat3 $1 $__t1 $code $__t2
  fi
  endlet $1 __t2 __t1 inner_op outer_op code test_side_effects context parens_otherwise
}

: $((__t2 = __t1 = code = test_side_effects = context = 0))
_wrap_in_condition_if_needed() { let context $2; let test_side_effects $3; let code $4
  let __t1; let __t2
  if [ $context = $_RVALUE_CTX_TEST ] ; then
    defstr __str_236 " != 0 ]"
    defstr __str_235 "[ "
    _wrap_str_lit __t1 $__str_235
    _wrap_str_lit __t2 $__str_236
    _string_concat3 __t1 $__t1 $code $__t2
    _with_prefixed_side_effects $1 $test_side_effects $__t1
  else
    : $(($1 = code))
  fi
  endlet $1 __t2 __t1 code test_side_effects context
}

: $((__t3 = __t2 = __t1 = child3 = child2 = child1 = child0 = sub3 = sub2 = sub1 = nb_children = op = outer_op = test_side_effects = context = node = 0))
_comp_rvalue_go() { let node $2; let context $3; let test_side_effects $4; let outer_op $5
  let op; let nb_children; let sub1; let sub2; let sub3; let child0; let child1; let child2; let child3; let __t1; let __t2; let __t3
  _get_op op $node
  _get_nb_children nb_children $node
  if [ $nb_children -ge 1 ] ; then
    _get_child child0 $node 0
  fi
  if [ $nb_children -ge 2 ] ; then
    _get_child child1 $node 1
  fi
  if [ $nb_children -ge 3 ] ; then
    _get_child child2 $node 2
  fi
  if [ $nb_children -ge 4 ] ; then
    _get_child child3 $node 3
  fi
  if [ $nb_children = 0 ] ; then
    if [ $op = $_INTEGER ] ; then
      _get_val __t1 $node
      _wrap_int __t1 $((-__t1))
      _wrap_in_condition_if_needed $1 $context $test_side_effects $__t1
    elif [ $op = $_INTEGER_HEX ] || [ $op = $_INTEGER_OCT ] ; then
      _wrap_integer __t1 1 $node
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $_CHARACTER ] ; then
      if [ $context = $_RVALUE_CTX_ARITH_EXPANSION ] ; then
        _get_val __t1 $node
        _character_ident $1 $__t1
      else
        _get_val __t1 $node
        _character_ident __t1 $__t1
        _string_concat __t1 $((-__DOLLAR__)) $__t1
        _wrap_in_condition_if_needed $1 $context $test_side_effects $__t1
      fi
    elif [ $op = $_IDENTIFIER ] || [ $op = $_IDENTIFIER_INTERNAL ] || [ $op = $_IDENTIFIER_STRING ] || [ $op = $_IDENTIFIER_DOLLAR ] ; then
      if [ $context = $_RVALUE_CTX_ARITH_EXPANSION ] ; then
        _env_var_with_prefix $1 $node 0
      else
        _env_var_with_prefix __t1 $node 1
        _string_concat __t1 $((-__DOLLAR__)) $__t1
        _wrap_in_condition_if_needed $1 $context $test_side_effects $__t1
      fi
    else
      printf "op=%d " $op
      printf \\$((op/64))$((op/8%8))$((op%8))
      defstr __str_237 "comp_rvalue_go: unknown rvalue with nb_children == 0"
      _fatal_error __ $__str_237
      : $(($1 = 0))
    fi
  elif [ $nb_children = 1 ] ; then
    if [ $op = $__STAR__ ] ; then
      _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_BASE 0 $op
      _string_concat __t1 $((-__UNDERSCORE__)) $sub1
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $__PLUS__ ] || [ $op = $_PARENS ] ; then
      _comp_rvalue_go $1 $child0 $context $test_side_effects $outer_op
    elif [ $op = $__MINUS__ ] || [ $op = $__TILDE__ ] || [ $op = $__EXCL__ ] ; then
      if [ $op = $__MINUS__ ] && { { _get_op __t1 $child0; [ $__t1 = $_INTEGER ]; } || [ $op = $_INTEGER_HEX ] || [ $op = $_INTEGER_OCT ]; } ; then
        _wrap_integer __t1 -1 $child0
        _wrap_in_condition_if_needed $1 $context $test_side_effects $__t1
      elif _get_op __t2 $child0; [ $__t2 = $_IDENTIFIER ] ; then
        _env_var_with_prefix __t1 $child0 0
        _string_concat3 __t1 $((- op)) $((-__SPACE__)) $__t1
        _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
      else
        _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
        _string_concat __t1 $((- op)) $sub1
        _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
      fi
    elif [ $op = $_MINUS_MINUS_PRE ] ; then
      _comp_lvalue sub1 $child0
      defstr __str_238 " -= 1"
      _wrap_str_lit __t1 $__str_238
      _string_concat __t1 $sub1 $__t1
      _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $_PLUS_PLUS_PRE ] ; then
      _comp_lvalue sub1 $child0
      defstr __str_239 " += 1"
      _wrap_str_lit __t1 $__str_239
      _string_concat __t1 $sub1 $__t1
      _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $_MINUS_MINUS_POST ] ; then
      _comp_lvalue sub1 $child0
      defstr __str_241 " + 1"
      defstr __str_240 " -= 1)"
      _wrap_str_lit __t1 $__str_240
      _wrap_str_lit __t2 $__str_241
      _string_concat4 __t1 $((-__LPAREN__)) $sub1 $__t1 $__t2
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $__PLUS__
    elif [ $op = $_PLUS_PLUS_POST ] ; then
      _comp_lvalue sub1 $child0
      defstr __str_243 " - 1"
      defstr __str_242 " += 1)"
      _wrap_str_lit __t1 $__str_242
      _wrap_str_lit __t2 $__str_243
      _string_concat4 __t1 $((-__LPAREN__)) $sub1 $__t1 $__t2
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $__MINUS__
    elif [ $op = $_SIZEOF_KW ] ; then
      if _get_op __t1 $child0; [ $__t1 = $_DECL ] ; then
        _get_child child0 $child0 1
        if { _get_op __t1 $child0; [ $__t1 = $_INT_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $_SHORT_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $_LONG_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $_CHAR_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $_VOID_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $_ENUM_KW ]; } || { _get_op __t1 $child0; [ $__t1 = $__STAR__ ]; } ; then
          _wrap_int __t1 1
          _wrap_in_condition_if_needed $1 $context $test_side_effects $__t1
        elif _get_op __t2 $child0; [ $__t2 = $_STRUCT_KW ] ; then
          _get_child __t1 $child0 1
          _struct_sizeof_var __t1 $__t1
          _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
        else
          _get_op __t1 $child0
          printf "op=%d " $__t1
          _get_op __t2 $child0
          printf \\$(((__t2)/64))$(((__t2)/8%8))$(((__t2)%8))
          printf "\n"
          _get_child __t1 $child0 1
          _get_op __t1 $__t1
          printf "op=%d " $__t1
          _get_child __t2 $child0 1
          _get_op __t2 $__t2
          printf \\$(((__t2)/64))$(((__t2)/8%8))$(((__t2)%8))
          printf "\n"
          defstr __str_244 "comp_rvalue_go: sizeof is not supported for this type or expression"
          _fatal_error __ $__str_244
          : $(($1 = 0))
        fi
      else
        _get_op __t1 $child0
        printf "op=%d " $__t1
        _get_op __t2 $child0
        printf \\$(((__t2)/64))$(((__t2)/8%8))$(((__t2)%8))
        printf "\n"
        defstr __str_244 "comp_rvalue_go: sizeof is not supported for this type or expression"
        _fatal_error __ $__str_244
        : $(($1 = 0))
      fi
    elif [ $op = $__AMP__ ] ; then
      _comp_lvalue_address __t1 $child0
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
    else
      printf "op=%d " $op
      printf \\$((op/64))$((op/8%8))$((op%8))
      defstr __str_245 "comp_rvalue_go: unexpected operator"
      _fatal_error __ $__str_245
      : $(($1 = 0))
    fi
  elif [ $nb_children = 2 ] ; then
    if [ $op = $__PLUS__ ] || [ $op = $__MINUS__ ] || [ $op = $__STAR__ ] || [ $op = $__SLASH__ ] || [ $op = $__PERCENT__ ] || [ $op = $__AMP__ ] || [ $op = $__BAR__ ] || [ $op = $__CARET__ ] || [ $op = $_LSHIFT ] || [ $op = $_RSHIFT ] || [ $op = $__COMMA__ ] ; then
      _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _op_to_str __t1 $op
      _string_concat3 __t1 $sub1 $__t1 $sub2
      _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $__EQ__ ] || [ $op = $_AMP_EQ ] || [ $op = $_BAR_EQ ] || [ $op = $_CARET_EQ ] || [ $op = $_LSHIFT_EQ ] || [ $op = $_MINUS_EQ ] || [ $op = $_PERCENT_EQ ] || [ $op = $_PLUS_EQ ] || [ $op = $_RSHIFT_EQ ] || [ $op = $_SLASH_EQ ] || [ $op = $_STAR_EQ ] ; then
      _comp_lvalue sub1 $child0
      _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _op_to_str __t1 $op
      _string_concat3 __t1 $sub1 $__t1 $sub2
      _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $__LBRACK__ ] ; then
      _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $__PLUS__
      _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $__PLUS__
      defstr __str_229 "))"
      defstr __str_247 " + "
      defstr __str_246 "_\$(("
      _wrap_str_lit __t1 $__str_246
      _wrap_str_lit __t2 $__str_247
      _wrap_str_lit __t3 $__str_229
      _string_concat5 __t1 $__t1 $sub1 $__t2 $sub2 $__t3
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $_ARROW ] ; then
      _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _struct_member_var sub2 $child1
      defstr __str_229 "))"
      defstr __str_247 " + "
      defstr __str_246 "_\$(("
      _wrap_str_lit __t1 $__str_246
      _wrap_str_lit __t2 $__str_247
      _wrap_str_lit __t3 $__str_229
      _string_concat5 __t1 $__t1 $sub1 $__t2 $sub2 $__t3
      _wrap_if_needed $1 0 $context $test_side_effects $__t1 $outer_op $op
    elif [ $op = $_EQ_EQ ] || [ $op = $_EXCL_EQ ] || [ $op = $_LT_EQ ] || [ $op = $_GT_EQ ] || [ $op = $__LT__ ] || [ $op = $__GT__ ] ; then
      if [ $context = $_RVALUE_CTX_TEST ] ; then
        _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_BASE 0 $op
        _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_BASE 0 $op
        defstr __str_248 " ]"
        defstr __str_235 "[ "
        _wrap_str_lit __t1 $__str_235
        _test_op_to_str __t2 $op
        _wrap_str_lit __t3 $__str_248
        _string_concat5 __t1 $__t1 $sub1 $__t2 $sub2 $__t3
        _with_prefixed_side_effects $1 $test_side_effects $__t1
      else
        _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
        _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
        _op_to_str __t1 $op
        _string_concat3 __t1 $sub1 $__t1 $sub2
        _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
      fi
    elif [ $op = $_CAST ] ; then
      _comp_rvalue_go $1 $child1 $context 0 $op
    else
      defstr __str_249 "comp_rvalue_go: unknown rvalue with 2 children"
      _fatal_error __ $__str_249
      : $(($1 = 0))
    fi
  elif [ $nb_children = 3 ] && [ $op = $__QUESTION__ ] ; then
    _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
    _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
    _comp_rvalue_go sub3 $child2 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
    defstr __str_250 ": "
    _op_to_str __t1 $op
    _wrap_str_lit __t2 $__str_250
    _string_concat5 __t1 $sub1 $__t1 $sub2 $__t2 $sub3
    _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
    endlet $1 __t3 __t2 __t1 child3 child2 child1 child0 sub3 sub2 sub1 nb_children op outer_op test_side_effects context node
    return
  elif [ $nb_children = 4 ] && { [ $op = $_AMP_AMP ] || [ $op = $_BAR_BAR ]; } ; then
    if [ $context = $_RVALUE_CTX_TEST ] ; then
      _non_parenthesized_operand sub1 $child0
      _non_parenthesized_operand sub2 $child1
      if { { _get_op __t1 $sub1; [ $__t1 = $_AMP_AMP ]; } || { _get_op __t1 $sub1; [ $__t1 = $_BAR_BAR ]; }; } && { _get_op __t1 $sub1; [ $__t1 != $op ]; } ; then
        _comp_rvalue_go sub1 $sub1 $_RVALUE_CTX_TEST $child2 $op
        defstr __str_231 "{ "
        _wrap_str_lit __t1 $__str_231
        defstr __str_232 "; }"
        _wrap_str_lit __t2 $__str_232
        _string_concat3 sub1 $__t1 $sub1 $__t2
      else
        _comp_rvalue_go sub1 $sub1 $_RVALUE_CTX_TEST $child2 $op
      fi
      if { { _get_op __t1 $sub2; [ $__t1 = $_AMP_AMP ]; } || { _get_op __t1 $sub2; [ $__t1 = $_BAR_BAR ]; }; } && { _get_op __t1 $sub2; [ $__t1 != $op ]; } ; then
        _comp_rvalue_go sub2 $sub2 $_RVALUE_CTX_TEST $child3 $op
        defstr __str_231 "{ "
        _wrap_str_lit __t1 $__str_231
        defstr __str_232 "; }"
        _wrap_str_lit __t2 $__str_232
        _string_concat3 sub2 $__t1 $sub2 $__t2
      else
        _comp_rvalue_go sub2 $sub2 $_RVALUE_CTX_TEST $child3 $op
      fi
      _op_to_str __t1 $op
      _string_concat3 $1 $sub1 $__t1 $sub2
      endlet $1 __t3 __t2 __t1 child3 child2 child1 child0 sub3 sub2 sub1 nb_children op outer_op test_side_effects context node
      return
    else
      if [ $test_side_effects != 0 ] || [ $child2 != 0 ] || [ $child3 != 0 ] ; then
        defstr __str_251 "comp_rvalue_go: && and || with function calls can only be used in tests"
        _fatal_error __ $__str_251
      fi
      _comp_rvalue_go sub1 $child0 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _comp_rvalue_go sub2 $child1 $_RVALUE_CTX_ARITH_EXPANSION 0 $op
      _op_to_str __t1 $op
      _string_concat3 __t1 $sub1 $__t1 $sub2
      _wrap_if_needed $1 1 $context $test_side_effects $__t1 $outer_op $op
      endlet $1 __t3 __t2 __t1 child3 child2 child1 child0 sub3 sub2 sub1 nb_children op outer_op test_side_effects context node
      return
    fi
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    printf "\n"
    defstr __str_252 "comp_rvalue_go: unknown rvalue"
    _fatal_error __ $__str_252
    : $(($1 = 0))
    endlet $1 __t3 __t2 __t1 child3 child2 child1 child0 sub3 sub2 sub1 nb_children op outer_op test_side_effects context node
    return
  fi
  endlet $1 __t3 __t2 __t1 child3 child2 child1 child0 sub3 sub2 sub1 nb_children op outer_op test_side_effects context node
}

: $((__t2 = __t1 = side_effect = result = fun_call_decl_start = contains_side_effects2 = replaced_fun_calls2 = simple_ast = context = node = 0))
_comp_rvalue() { let node $2; let context $3
  let simple_ast; let replaced_fun_calls2; let contains_side_effects2; let fun_call_decl_start; let result; let side_effect; let __t1; let __t2
  _handle_side_effects simple_ast $node
  replaced_fun_calls2=$_replaced_fun_calls
  contains_side_effects2=$_contains_side_effects
  fun_call_decl_start=$_glo_decl_ix
  while [ $_literals_inits != 0 ]; do
    _car_ side_effect $__EQ__ $_literals_inits
    _get_child __t1 $side_effect 0
    _get_child __t2 $side_effect 1
    _comp_defstr __ $__t1 $__t2 -1
    _cdr_ _literals_inits $_LIST $_literals_inits
  done
  if [ $context != $_RVALUE_CTX_TEST_ELSEIF ] ; then
    fun_call_decl_start=$_glo_decl_ix
  fi
  while [ $replaced_fun_calls2 != 0 ]; do
    _car_ side_effect $__EQ__ $replaced_fun_calls2
    _get_child __t1 $side_effect 1
    _get_child __t2 $side_effect 0
    _comp_fun_call __ $__t1 $__t2
    _cdr_ replaced_fun_calls2 $_LIST $replaced_fun_calls2
  done
  if [ $context = $_RVALUE_CTX_TEST ] || [ $context = $_RVALUE_CTX_TEST_ELSEIF ] ; then
    _undo_glo_decls __ $fun_call_decl_start
    _replay_glo_decls_inline result $fun_call_decl_start $_glo_decl_ix
    _comp_rvalue_go __t1 $simple_ast $_RVALUE_CTX_TEST 0 0
    _string_concat result $result $__t1
  else
    _comp_rvalue_go result $simple_ast $context 0 0
  fi
  : $((_contains_side_effects |= contains_side_effects2))
  : $(($1 = result))
  endlet $1 __t2 __t1 side_effect result fun_call_decl_start contains_side_effects2 replaced_fun_calls2 simple_ast context node
}

: $((__t1 = sub2 = sub1 = op = node = 0))
_comp_lvalue_address() { let node $2
  let op; let sub1; let sub2; let __t1
  _get_op op $node
  if [ $op = $_IDENTIFIER ] ; then
    defstr __str_253 "comp_rvalue_go: can't take the address of a local variable"
    _fatal_error __ $__str_253
    : $(($1 = 0))
  elif [ $op = $__LBRACK__ ] ; then
    _get_child __t1 $node 0
    _comp_rvalue sub1 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    _get_child __t1 $node 1
    _comp_rvalue sub2 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    defstr __str_247 " + "
    _wrap_str_lit __t1 $__str_247
    _string_concat3 $1 $sub1 $__t1 $sub2
  elif [ $op = $__STAR__ ] ; then
    _get_child __t1 $node 0
    _comp_rvalue $1 $__t1 $_RVALUE_CTX_BASE
  elif [ $op = $_ARROW ] ; then
    _get_child __t1 $node 0
    _comp_rvalue sub1 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    _get_child __t1 $node 1
    _struct_member_var sub2 $__t1
    defstr __str_247 " + "
    _wrap_str_lit __t1 $__str_247
    _string_concat3 $1 $sub1 $__t1 $sub2
  elif [ $op = $_CAST ] ; then
    _get_child __t1 $node 1
    _comp_lvalue_address $1 $__t1
  elif [ $op = $_PARENS ] ; then
    _get_child __t1 $node 0
    _comp_lvalue_address $1 $__t1
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    printf "\n"
    defstr __str_254 "comp_lvalue_address: unknown lvalue"
    _fatal_error __ $__str_254
    : $(($1 = 0))
  fi
  endlet $1 __t1 sub2 sub1 op node
}

: $((__t3 = __t2 = __t1 = sub2 = sub1 = op = node = 0))
_comp_lvalue() { let node $2
  let op; let sub1; let sub2; let __t1; let __t2; let __t3
  _get_op op $node
  if [ $op = $_IDENTIFIER ] || [ $op = $_IDENTIFIER_INTERNAL ] || [ $op = $_IDENTIFIER_STRING ] || [ $op = $_IDENTIFIER_DOLLAR ] ; then
    _env_var_with_prefix $1 $node 0
  elif [ $op = $__LBRACK__ ] ; then
    _get_child __t1 $node 0
    _comp_rvalue sub1 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    _get_child __t1 $node 1
    _comp_rvalue sub2 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    defstr __str_246 "_\$(("
    _wrap_str_lit __t1 $__str_246
    defstr __str_247 " + "
    _wrap_str_lit __t2 $__str_247
    defstr __str_229 "))"
    _wrap_str_lit __t3 $__str_229
    _string_concat5 $1 $__t1 $sub1 $__t2 $sub2 $__t3
  elif [ $op = $__STAR__ ] ; then
    _get_child __t1 $node 0
    _comp_rvalue sub1 $__t1 $_RVALUE_CTX_BASE
    _string_concat $1 $((-__UNDERSCORE__)) $sub1
  elif [ $op = $_ARROW ] ; then
    _get_child __t1 $node 0
    _comp_rvalue sub1 $__t1 $_RVALUE_CTX_ARITH_EXPANSION
    _get_child __t1 $node 1
    _struct_member_var sub2 $__t1
    defstr __str_246 "_\$(("
    _wrap_str_lit __t1 $__str_246
    defstr __str_247 " + "
    _wrap_str_lit __t2 $__str_247
    defstr __str_229 "))"
    _wrap_str_lit __t3 $__str_229
    _string_concat5 $1 $__t1 $sub1 $__t2 $sub2 $__t3
  elif [ $op = $_CAST ] ; then
    _get_child __t1 $node 1
    _comp_lvalue $1 $__t1
  elif [ $op = $_PARENS ] ; then
    _get_child __t1 $node 0
    _comp_lvalue_address $1 $__t1
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    printf "\n"
    defstr __str_255 "comp_lvalue: unknown lvalue"
    _fatal_error __ $__str_255
    : $(($1 = 0))
  fi
  endlet $1 __t3 __t2 __t1 sub2 sub1 op node
}

: $((__t1 = code_params = param = params = 0))
_fun_call_params() { let params $2
  let param; let code_params; let __t1
  code_params=0
  while [ $params != 0 ]; do
    _car __t1 $params
    _comp_rvalue param $__t1 $_RVALUE_CTX_BASE
    _concatenate_strings_with code_params $code_params $param $((-__SPACE__))
    _cdr_ params $_LIST $params
  done
  : $(($1 = code_params))
  endlet $1 __t1 code_params param params
}

: $((__t4 = __t3 = __t2 = __t1 = ident = res = param = 0))
_comp_putchar_inline() { let param $2
  let res; let ident; let __t1; let __t2; let __t3; let __t4
  if { _get_op __t1 $param; [ $__t1 = $_CHARACTER ]; } && { { { _get_val __t1 $param; [ $__t1 -ge 32 ]; } && { _get_val __t1 $param; [ $__t1 -le 126 ]; }; } || { _get_val __t1 $param; [ $__t1 = $__NEWLINE__ ]; }; } ; then
    defstr __str_256 "printf \""
    _wrap_str_lit __t1 $__str_256
    _get_val __t2 $param
    _escape_text __t2 $((-__t2)) 1
    _string_concat3 $1 $__t1 $__t2 $((-__DQUOTE__))
    endlet $1 __t4 __t3 __t2 __t1 ident res param
    return
  fi
  _comp_rvalue res $param $_RVALUE_CTX_ARITH_EXPANSION
  if [ $_contains_side_effects != 0 ] ; then
    _fresh_ident ident
    defstr __str_229 "))"
    defstr __str_257 "=\$(("
    _comp_lvalue __t1 $ident
    _wrap_str_lit __t2 $__str_257
    _wrap_str_lit __t3 $__str_229
    _string_concat4 __t1 $__t1 $__t2 $res $__t3
    _append_glo_decl __ $__t1
    _comp_lvalue res $ident
  elif _get_op __t1 $param; [ $__t1 != $_IDENTIFIER ] ; then
    _string_concat3 res $((-__LPAREN__)) $res $((-__RPAREN__))
  fi
  defstr __str_258 "/64))"
  defstr __str_228 "\$(("
  _wrap_str_lit __t1 $__str_228
  _wrap_str_lit __t2 $__str_258
  _string_concat3 __t1 $__t1 $res $__t2
  defstr __str_259 "/8%8))"
  defstr __str_228 "\$(("
  _wrap_str_lit __t2 $__str_228
  _wrap_str_lit __t3 $__str_259
  _string_concat3 __t2 $__t2 $res $__t3
  defstr __str_260 "%8))"
  defstr __str_228 "\$(("
  _wrap_str_lit __t3 $__str_228
  _wrap_str_lit __t4 $__str_260
  _string_concat3 __t3 $__t3 $res $__t4
  _string_concat3 res $__t1 $__t2 $__t3
  defstr __str_261 "printf \\\\\\\\"
  _wrap_str_lit __t1 $__str_261
  _string_concat $1 $__t1 $res
  endlet $1 __t4 __t3 __t2 __t1 ident res param
}

: $((__t3 = __t2 = __t1 = escape = params_text = format_str_end = format_str = 0))
_printf_call() { let format_str $2; let format_str_end $3; let params_text $4; let escape $5
  let __t1; let __t2; let __t3
  if [ $format_str = $format_str_end ] ; then
    : $(($1 = 0))
  else
    defstr __str_256 "printf \""
    defstr __str_262 "printf -- \""
    _wrap_str_lit __t1 $(((_$((format_str + 0)) == __MINUS__) ? __str_262: __str_256))
    _wrap_str_imm __t2 $format_str $format_str_end
    _escape_text __t2 $__t2 $escape
    _concatenate_strings_with __t3 $((-__DQUOTE__)) $params_text $((-__SPACE__))
    _string_concat3 $1 $__t1 $__t2 $__t3
  fi
  endlet $1 __t3 __t2 __t1 escape params_text format_str_end format_str
}

# PRINTF_STATE enum declaration
readonly _PRINTF_STATE_FLAGS=0
readonly _PRINTF_STATE_WIDTH=1
readonly _PRINTF_STATE_PRECISION=2
readonly _PRINTF_STATE_SPECIFIER=3
: $((__t3 = __t2 = __t1 = state = has_precision = has_width = mod = precision_text = width_text = params_text = specifier_start = format_start = param = params = format_str = 0))
_handle_printf_call() { let format_str $2; let params $3
  let param; let format_start; let specifier_start; let params_text; let width_text; let precision_text; let mod; let has_width; let has_precision; let state; let __t1; let __t2; let __t3
  param=0
  format_start=$format_str
  params_text=0
  width_text=0
  precision_text=0
  mod=0
  has_width=0
  has_precision=0
  state=$_PRINTF_STATE_FLAGS
  while [ $((_$format_str)) != $__NUL__ ]; do
    if [ $param = 0 ] && [ $params != 0 ] ; then
      _car param $params
      _cdr_ params $_LIST $params
    fi
    if [ $mod != 0 ] ; then
      case $((_$format_str)) in
        $__SPACE__|$__SHARP__|$__PLUS__|$__MINUS__|$__0__)
          if [ $state != $_PRINTF_STATE_FLAGS ] ; then
            defstr __str_263 "Invalid printf format: Flags must come before width and precision"
            _fatal_error __ $__str_263
          fi
        ;;
        $__1__|$__2__|$__3__|$__4__|$__5__|$__6__|$__7__|$__8__|$__9__)
          if [ $state != $_PRINTF_STATE_FLAGS ] && [ $state != $_PRINTF_STATE_PRECISION ] ; then
            defstr __str_264 "Invalid printf format: Width or precision already specified by a number"
            _fatal_error __ $__str_264
          fi
          while [ $__0__ -le $((_$format_str)) ] && [ $((_$format_str)) -le $__9__ ]; do
            : $((format_str += 1))
          done
          has_width=$(((state == _PRINTF_STATE_FLAGS) ? 1: has_width))
          has_precision=$(((state == _PRINTF_STATE_PRECISION) ? 1: has_precision))
          : $((state += 1))
          : $((format_str -= 1))
        ;;
        $__PERIOD__)
          if [ $state -ge $_PRINTF_STATE_PRECISION ] ; then
            defstr __str_265 "Invalid printf format: precision already specified"
            _fatal_error __ $__str_265
          fi
          state=$_PRINTF_STATE_PRECISION
        ;;
        $__STAR__)
          if [ $param = 0 ] ; then
            defstr __str_266 "Not enough parameters for printf"
            _fatal_error __ $__str_266
          fi
          if [ $state = $_PRINTF_STATE_FLAGS ] ; then
            _comp_rvalue width_text $param $_RVALUE_CTX_BASE
            has_width=1
          elif [ $state = $_PRINTF_STATE_PRECISION ] ; then
            _comp_rvalue precision_text $param $_RVALUE_CTX_BASE
            has_precision=1
          else
            defstr __str_267 "Width or precision already specified by a number"
            _fatal_error __ $__str_267
          fi
          param=0
        ;;
        $__PERCENT__)
          if [ $state != $_PRINTF_STATE_FLAGS ] ; then
            defstr __str_268 "Cannot use flags, width or precision with %%"
            _fatal_error __ $__str_268
          fi
          mod=0
        ;;
        $__l__|$__d__|$__i__|$__o__|$__u__|$__x__|$__X__)
          if [ $((_$format_str)) = $__l__ ] ; then
            while [ $((_$format_str)) = $__l__ ]; do
              : $((format_str += 1))
            done
            if [ $((_$format_str)) != $__d__ ] && [ $((_$format_str)) != $__i__ ] && [ $((_$format_str)) != $__o__ ] && [ $((_$format_str)) != $__u__ ] && [ $((_$format_str)) != $__x__ ] && [ $((_$format_str)) != $__X__ ] ; then
              printf "*format_str="
              printf \\$(((_$((format_str - 1)))/64))$(((_$((format_str - 1)))/8%8))$(((_$((format_str - 1)))%8))
              printf \\$(((_$format_str)/64))$(((_$format_str)/8%8))$(((_$format_str)%8))
              printf "\n"
              defstr __str_269 "Invalid printf format: Unsupported long specifier"
              _fatal_error __ $__str_269
            fi
          fi
          if [ $param = 0 ] ; then
            defstr __str_266 "Not enough parameters for printf"
            _fatal_error __ $__str_266
          fi
          _concatenate_strings_with params_text $params_text $width_text $((-__SPACE__))
          _concatenate_strings_with params_text $params_text $precision_text $((-__SPACE__))
          _comp_rvalue __t1 $param $_RVALUE_CTX_BASE
          _concatenate_strings_with params_text $params_text $__t1 $((-__SPACE__))
          param=0
          mod=0
        ;;
        $__c__)
          if [ $param = 0 ] ; then
            defstr __str_266 "Not enough parameters for printf"
            _fatal_error __ $__str_266
          fi
          if [ $has_width != 0 ] ; then
            defstr __str_270 "Width not supported for %c"
            _fatal_error __ $__str_270
          fi
          _printf_call __t1 $format_start $specifier_start $params_text 0
          _append_glo_decl __ $__t1
          format_start=$((format_str + 1))
          _comp_putchar_inline __t1 $param
          _append_glo_decl __ $__t1
          param=0
          params_text=0
          mod=0
        ;;
        $__s__)
          if [ $param = 0 ] ; then
            defstr __str_266 "Not enough parameters for printf"
            _fatal_error __ $__str_266
          fi
          _runtime_use_put_pstr=1
          if [ $has_width != 0 ] || [ $has_precision != 0 ] ; then
            _concatenate_strings_with params_text $params_text $width_text $((-__SPACE__))
            _concatenate_strings_with params_text $params_text $precision_text $((-__SPACE__))
            defstr __str_272 ")\""
            defstr __str_271 "\"\$(_put_pstr __ "
            _wrap_str_lit __t1 $__str_271
            _comp_rvalue __t2 $param $_RVALUE_CTX_BASE
            _wrap_str_lit __t3 $__str_272
            _string_concat3 __t1 $__t1 $__t2 $__t3
            _concatenate_strings_with params_text $params_text $__t1 $((-__SPACE__))
          else
            _printf_call __t1 $format_start $specifier_start $params_text 0
            _append_glo_decl __ $__t1
            format_start=$((format_str + 1))
            defstr __str_273 "_put_pstr __ "
            _wrap_str_lit __t1 $__str_273
            _comp_rvalue __t2 $param $_RVALUE_CTX_BASE
            _string_concat __t1 $__t1 $__t2
            _append_glo_decl __ $__t1
          fi
          param=0
          mod=0
        ;;
        *)
          printf "specifier="
          _put_pstr __ $specifier_start
          printf "\n"
          printf "format char='"
          printf \\$(((_$format_str)/64))$(((_$format_str)/8%8))$(((_$format_str)%8))
          printf "'\n"
          defstr __str_274 "Unsupported format specifier"
          _fatal_error __ $__str_274
        ;;
      esac
    elif [ $((_$format_str)) = $__PERCENT__ ] ; then
      mod=1
      specifier_start=$format_str
      : $((width_text = precision_text = has_width = has_precision = 0))
      state=$_PRINTF_STATE_FLAGS
    fi
    : $((format_str += 1))
  done
  _printf_call __t1 $format_start $format_str $params_text 0
  _append_glo_decl __ $__t1
  endlet $1 __t3 __t2 __t1 state has_precision has_width mod precision_text width_text params_text specifier_start format_start param params format_str
}

: $((__t2 = __t1 = res = param = name_id = params = name = assign_to = node = 0))
_comp_fun_call_code() { let node $2; let assign_to $3
  let name; let params; let name_id; let param; let res; let __t1; let __t2
  _get_child name $node 0
  _get_child params $node 1
  _get_val name_id $name
  if [ $assign_to = 0 ] ; then
    if { [ $name_id = $_PUTS_ID ] || [ $name_id = $_PUTSTR_ID ] || [ $name_id = $_PRINTF_ID ]; } && { _list_singleton param $params; [ $param != 0 ]; } && { _get_op __t1 $param; [ $__t1 = $_STRING ]; } ; then
      _get_val __t1 $param
      _printf_call $1 $((_string_pool + _$((_heap + __t1 + 1)))) 0 0 1
      endlet $1 __t2 __t1 res param name_id params name assign_to node
      return
    elif [ $name_id = $_PRINTF_ID ] && [ $params != 0 ] && { _car __t2 $params; _get_op __t2 $__t2; [ $__t2 = $_STRING ]; } ; then
      _car __t1 $params
      _get_val __t1 $__t1
      _cdr_ __t2 $_LIST $params
      _handle_printf_call __ $((_string_pool + _$((_heap + __t1 + 1)))) $__t2
      : $(($1 = 0))
      endlet $1 __t2 __t1 res param name_id params name assign_to node
      return
    elif [ $name_id = $_PUTCHAR_ID ] && { _list_singleton param $params; [ $param != 0 ]; } ; then
      _comp_putchar_inline $1 $param
      endlet $1 __t2 __t1 res param name_id params name assign_to node
      return
    elif [ $name_id = $_EXIT_ID ] && { _list_singleton param $params; [ $param != 0 ]; } ; then
      _comp_rvalue res $param $_RVALUE_CTX_BASE
      defstr __str_275 "exit "
      _wrap_str_lit __t1 $__str_275
      _string_concat $1 $__t1 $res
      endlet $1 __t2 __t1 res param name_id params name assign_to node
      return
    fi
  fi
  if [ $name_id = $_PUTCHAR_ID ] ; then
    _runtime_use_putchar=1
  elif [ $name_id = $_GETCHAR_ID ] ; then
    _runtime_use_getchar=1
  elif [ $name_id = $_EXIT_ID ] ; then
    _runtime_use_exit=1
  elif [ $name_id = $_MALLOC_ID ] ; then
    _runtime_use_malloc=1
  elif [ $name_id = $_FREE_ID ] ; then
    _runtime_use_free=1
  elif [ $name_id = $_PRINTF_ID ] ; then
    _runtime_use_printf=1
  elif [ $name_id = $_FOPEN_ID ] ; then
    _runtime_use_fopen=1
  elif [ $name_id = $_FCLOSE_ID ] ; then
    _runtime_use_fclose=1
  elif [ $name_id = $_FGETC_ID ] ; then
    _runtime_use_fgetc=1
  elif [ $name_id = $_READ_ID ] ; then
    _runtime_use_read=1
  elif [ $name_id = $_WRITE_ID ] ; then
    _runtime_use_write=1
  elif [ $name_id = $_OPEN_ID ] ; then
    _runtime_use_open=1
  elif [ $name_id = $_CLOSE_ID ] ; then
    _runtime_use_close=1
  fi
  if [ $assign_to != 0 ] ; then
    _comp_lvalue res $assign_to
  else
    defstr __str_148 "__"
    _wrap_str_lit res $__str_148
  fi
  _get_val __t1 $name
  _function_name __t1 $__t1
  _fun_call_params __t2 $params
  _concatenate_strings_with __t2 $res $__t2 $((-__SPACE__))
  _string_concat3 $1 $__t1 $((-__SPACE__)) $__t2
  endlet $1 __t2 __t1 res param name_id params name assign_to node
}

: $((res = assign_to = node = 0))
_comp_fun_call() { let node $2; let assign_to $3
  let res
  _comp_fun_call_code res $node $assign_to
  if [ $res != 0 ] ; then
    _append_glo_decl __ $res
  fi
  endlet $1 res assign_to node
}

: $((__t5 = __t4 = __t3 = __t2 = __t1 = lhs_op = rhs = lhs = 0))
_comp_assignment() { let lhs $2; let rhs $3
  let lhs_op; let __t1; let __t2; let __t3; let __t4; let __t5
  _get_op lhs_op $lhs
  if [ $lhs_op = $_IDENTIFIER ] || [ $lhs_op = $__LBRACK__ ] || [ $lhs_op = $__STAR__ ] || [ $lhs_op = $_ARROW ] ; then
    if _get_op __t1 $rhs; [ $__t1 = $__LPAREN__ ] ; then
      _comp_fun_call __ $rhs $lhs
    else
      if [ $lhs_op = $_IDENTIFIER ] && { _get_op __t1 $rhs; [ $__t1 != $__EQ__ ]; } ; then
        _comp_lvalue __t1 $lhs
        _comp_rvalue __t2 $rhs $_RVALUE_CTX_BASE
        _string_concat3 __t1 $__t1 $((-__EQ__)) $__t2
        _append_glo_decl __ $__t1
      else
        defstr __str_229 "))"
        defstr __str_176 " = "
        defstr __str_276 ": \$(("
        _wrap_str_lit __t1 $__str_276
        _comp_lvalue __t2 $lhs
        _wrap_str_lit __t3 $__str_176
        _comp_rvalue __t4 $rhs $_RVALUE_CTX_ARITH_EXPANSION
        _wrap_str_lit __t5 $__str_229
        _string_concat5 __t1 $__t1 $__t2 $__t3 $__t4 $__t5
        _append_glo_decl __ $__t1
      fi
    fi
  else
    printf "lhs_op=%d " $lhs_op
    printf \\$((lhs_op/64))$((lhs_op/8%8))$((lhs_op%8))
    printf "\n"
    defstr __str_277 "unknown lhs"
    _fatal_error __ $__str_277
  fi
  endlet $1 __t5 __t4 __t3 __t2 __t1 lhs_op rhs lhs
}

: $((__t1 = start_cgc_locals = start_in_tail_position = stmt_ctx = node = 0))
_comp_body() { let node $2; let stmt_ctx $3
  let start_in_tail_position; let start_cgc_locals; let __t1
  start_in_tail_position=$_in_tail_position
  start_cgc_locals=$_cgc_locals
  _in_tail_position=0
  while [ $node != 0 ]; do
    if _get_child __t1 $node 1; _get_op __t1 $__t1; [ $__t1 != $__LBRACE__ ] ; then
      _in_tail_position=$start_in_tail_position
    fi
    if _get_child __t1 $node 0; _comp_statement __t1 $__t1 $stmt_ctx; [ $__t1 != 0 ] ; then
      break
    fi
    _get_child node $node 1
  done
  _cgc_locals=$start_cgc_locals
  : $(($1 = node != 0))
  endlet $1 __t1 start_cgc_locals start_in_tail_position stmt_ctx node
}

_last_stmt=0
: $((__t1 = str = statement = 0))
_make_switch_pattern() { let statement $2
  let str; let __t1
  str=0
  while [ 1 != 0 ]; do
    _get_op __t1 $statement
    case $__t1 in
      $_DEFAULT_KW)
        str=$((-__STAR__))
        _get_child statement $statement 0
      ;;
      $_CASE_KW)
        _get_child __t1 $statement 0
        _comp_rvalue __t1 $__t1 $_RVALUE_CTX_BASE
        _concatenate_strings_with str $str $__t1 $((-__BAR__))
        _get_child statement $statement 1
      ;;
      *)
        if [ $str = 0 ] ; then
          defstr __str_278 "Expected case in switch. Fallthrough is not supported."
          _fatal_error __ $__str_278
        fi
        _last_stmt=$statement
        _string_concat $1 $str $((-__RPAREN__))
        break
      ;;
    esac
  done
  endlet $1 __t1 str statement
}

: $((__t3 = __t2 = __t1 = start_cgc_locals = statement = node = 0))
_comp_switch() { let node $2
  let statement; let start_cgc_locals; let __t1; let __t2; let __t3
  start_cgc_locals=$_cgc_locals
  defstr __str_280 " in"
  defstr __str_279 "case "
  _wrap_str_lit __t1 $__str_279
  _get_child __t2 $node 0
  _comp_rvalue __t2 $__t2 $_RVALUE_CTX_BASE
  _wrap_str_lit __t3 $__str_280
  _string_concat3 __t1 $__t1 $__t2 $__t3
  _append_glo_decl __ $__t1
  _cgc_add_enclosing_switch __ $_in_tail_position
  : $((_nest_level += 1))
  _get_child node $node 1
  if _get_op __t1 $node; [ $__t1 = $_CASE_KW ] ; then
    _new_ast2 node $__LBRACE__ $node 0
  fi
  if [ $node = 0 ] || { _get_op __t1 $node; [ $__t1 != $__LBRACE__ ]; } ; then
    defstr __str_281 "comp_statement: switch without body"
    _fatal_error __ $__str_281
  fi
  while _get_op __t1 $node; [ $__t1 = $__LBRACE__ ]; do
    _get_child statement $node 0
    _get_child node $node 1
    _make_switch_pattern __t1 $statement
    _append_glo_decl __ $__t1
    statement=$_last_stmt
    : $((_nest_level += 1))
    _in_tail_position=0
    if _comp_statement __t1 $statement $_STMT_CTX_SWITCH; [ $((!__t1)) != 0 ] ; then
      while _get_op __t1 $node; [ $__t1 = $__LBRACE__ ]; do
        _get_child statement $node 0
        _get_child node $node 1
        if _comp_statement __t1 $statement $_STMT_CTX_SWITCH; [ $__t1 != 0 ] ; then
          break
        fi
      done
    fi
    : $((_nest_level -= 1))
    defstr __str_282 ";;"
    _wrap_str_lit __t1 $__str_282
    _append_glo_decl __ $__t1
  done
  : $((_nest_level -= 1))
  defstr __str_283 "esac"
  _wrap_str_lit __t1 $__str_283
  _append_glo_decl __ $__t1
  _cgc_locals=$start_cgc_locals
  : $(($1 = 0))
  endlet $1 __t3 __t2 __t1 start_cgc_locals statement node
}

: $((__t3 = __t2 = __t1 = else_if = start_cgc_locals = termination_rhs = termination_lhs = start_glo_decl_idx = stmt_ctx = node = 0))
_comp_if() { let node $2; let stmt_ctx $3
  let start_glo_decl_idx; let termination_lhs; let termination_rhs; let start_cgc_locals; let else_if; let __t1; let __t2; let __t3
  termination_lhs=0
  termination_rhs=0
  start_cgc_locals=$_cgc_locals
  else_if=$((stmt_ctx & _STMT_CTX_ELSE_IF))
  stmt_ctx=$((stmt_ctx & ~ _STMT_CTX_ELSE_IF))
  defstr __str_286 " ; then"
  defstr __str_285 "if "
  defstr __str_284 "elif "
  _wrap_str_lit __t1 $((else_if ? __str_284: __str_285))
  _get_child __t2 $node 0
  _comp_rvalue __t2 $__t2 $((else_if ? _RVALUE_CTX_TEST_ELSEIF: _RVALUE_CTX_TEST))
  _wrap_str_lit __t3 $__str_286
  _string_concat3 __t1 $__t1 $__t2 $__t3
  _append_glo_decl __ $__t1
  : $((_nest_level += 1))
  start_glo_decl_idx=$_glo_decl_ix
  _get_child __t1 $node 1
  _comp_statement termination_lhs $__t1 $stmt_ctx
  if _any_active_glo_decls __t1 $start_glo_decl_idx; [ $((!__t1)) != 0 ] ; then
    _append_glo_decl __ $((-__COLON__))
  fi
  : $((_nest_level -= 1))
  if _get_child __t1 $node 2; [ $__t1 != 0 ] ; then
    if _get_child __t1 $node 2; _get_op __t1 $__t1; [ $__t1 = $_IF_KW ] ; then
      _get_child __t1 $node 2
      _comp_if termination_rhs $__t1 $((stmt_ctx | _STMT_CTX_ELSE_IF))
    else
      defstr __str_31 "else"
      _wrap_str_lit __t1 $__str_31
      _append_glo_decl __ $__t1
      : $((_nest_level += 1))
      start_glo_decl_idx=$_glo_decl_ix
      _get_child __t1 $node 2
      _comp_statement termination_rhs $__t1 $((stmt_ctx & ~ _STMT_CTX_ELSE_IF))
      if _any_active_glo_decls __t1 $start_glo_decl_idx; [ $((!__t1)) != 0 ] ; then
        _append_glo_decl __ $((-__COLON__))
      fi
      : $((_nest_level -= 1))
    fi
  fi
  if [ $((! else_if)) != 0 ] ; then
    defstr __str_287 "fi"
    _wrap_str_lit __t1 $__str_287
    _append_glo_decl __ $__t1
  fi
  if [ $((stmt_ctx & _STMT_CTX_SWITCH)) != 0 ] && [ $((termination_lhs ^ termination_rhs)) != 0 ] ; then
    defstr __str_288 "Early break out of a switch case is unsupported"
    _fatal_error __ $__str_288
  fi
  _cgc_locals=$start_cgc_locals
  : $(($1 = termination_lhs && termination_rhs))
  endlet $1 __t3 __t2 __t1 else_if start_cgc_locals termination_rhs termination_lhs start_glo_decl_idx stmt_ctx node
}

: $((__t2 = __t1 = loop_binding = always_returns = start_glo_decl_idx = start_cgc_locals = stmt_ctx = last_line = loop_end_stmt = body = cond = 0))
_comp_loop() { let cond $2; let body $3; let loop_end_stmt $4; let last_line $5; let stmt_ctx $6
  let start_cgc_locals; let start_glo_decl_idx; let always_returns; let loop_binding; let __t1; let __t2
  start_cgc_locals=$_cgc_locals
  always_returns=0
  _cgc_add_enclosing_loop __
  loop_binding=$_cgc_locals
  if [ $loop_end_stmt != 0 ] ; then
    : $((_$((_heap + loop_binding + 2)) = _glo_decl_ix))
    _comp_statement __ $loop_end_stmt $stmt_ctx
    _undo_glo_decls __ $((_$((_heap + loop_binding + 2))))
    : $((_$((_heap + loop_binding + 3)) = _glo_decl_ix))
  fi
  defstr __str_290 "; do"
  defstr __str_289 "while "
  _wrap_str_lit __t1 $__str_289
  _wrap_str_lit __t2 $__str_290
  _string_concat3 __t1 $__t1 $((cond ? cond: -__COLON__)) $__t2
  _append_glo_decl __ $__t1
  : $((_nest_level += 1))
  start_glo_decl_idx=$_glo_decl_ix
  _comp_statement always_returns $body $stmt_ctx
  _append_glo_decl __ $last_line
  _replay_glo_decls __ $((_$((_heap + loop_binding + 2)))) $((_$((_heap + loop_binding + 3)))) 1
  if _any_active_glo_decls __t1 $start_glo_decl_idx; [ $((!__t1)) != 0 ] ; then
    _append_glo_decl __ $((-__COLON__))
  fi
  : $((_nest_level -= 1))
  defstr __str_291 "done"
  _wrap_str_lit __t1 $__str_291
  _append_glo_decl __ $__t1
  _cgc_locals=$start_cgc_locals
  : $(($1 = (cond == -__COLON__) && always_returns))
  endlet $1 __t2 __t1 loop_binding always_returns start_glo_decl_idx start_cgc_locals stmt_ctx last_line loop_end_stmt body cond
}

: $((__t1 = binding = 0))
_comp_break() {
  let binding; let __t1
  _cgc_lookup_enclosing_loop_or_switch binding $_cgc_locals
  if [ $binding = 0 ] ; then
    defstr __str_292 "comp_statement: break not in loop or switch"
    _fatal_error __ $__str_292
  fi
  if [ $((_$((_heap + binding + 1)))) = $_BINDING_LOOP ] ; then
    defstr __str_23 "break"
    _wrap_str_lit __t1 $__str_23
    _append_glo_decl __ $__t1
  fi
  : $(($1 = 1))
  endlet $1 __t1 binding
}

: $((__t1 = binding = 0))
_comp_continue() {
  let binding; let __t1
  _cgc_lookup_enclosing_loop binding $_cgc_locals
  if [ $binding = 0 ] ; then
    defstr __str_293 "comp_statement: continue not in loop"
    _fatal_error __ $__str_293
  fi
  _replay_glo_decls __ $((_$((_heap + binding + 2)))) $((_$((_heap + binding + 3)))) 1
  defstr __str_27 "continue"
  _wrap_str_lit __t1 $__str_27
  _append_glo_decl __ $__t1
  : $(($1 = 0))
  endlet $1 __t1 binding
}

: $((__t3 = __t2 = __t1 = loop_depth = binding = return_value = 0))
_comp_return() { let return_value $2
  let binding; let loop_depth; let __t1; let __t2; let __t3
  _cgc_lookup_enclosing_loop_or_switch binding $_cgc_locals
  _cgc_loop_depth loop_depth $binding
  if [ $return_value != 0 ] ; then
    if _get_op __t1 $return_value; [ $__t1 = $__LPAREN__ ] ; then
      _new_dollar_ident __t1 1
      _comp_fun_call __ $return_value $__t1
    else
      defstr __str_229 "))"
      defstr __str_294 ": \$((\$1 = "
      _wrap_str_lit __t1 $__str_294
      _comp_rvalue __t2 $return_value $_RVALUE_CTX_ARITH_EXPANSION
      _wrap_str_lit __t3 $__str_229
      _string_concat3 __t1 $__t1 $__t2 $__t3
      _append_glo_decl __ $__t1
    fi
  fi
  if [ $binding != 0 ] && [ $((_$((_heap + binding + 1)))) = $_BINDING_SWITCH ] ; then
    : $((_in_tail_position |= _$((_heap + binding + 2))))
  fi
  if [ $_in_tail_position != 0 ] && [ $binding != 0 ] ; then
    if [ $loop_depth -ge 2 ] ; then
      defstr __str_295 "break "
      _wrap_str_lit __t1 $__str_295
      _wrap_int __t2 $loop_depth
      _string_concat __t1 $__t1 $__t2
      _append_glo_decl __ $__t1
    elif [ $loop_depth = 1 ] ; then
      defstr __str_23 "break"
      _wrap_str_lit __t1 $__str_23
      _append_glo_decl __ $__t1
    fi
  elif [ $((! _in_tail_position)) != 0 ] ; then
    _append_glo_decl_fixup __t1
    _cons _rest_loc_var_fixups $__t1 $_rest_loc_var_fixups
    defstr __str_42 "return"
    _wrap_str_lit __t1 $__str_42
    _append_glo_decl __ $__t1
  fi
  : $(($1 = 1))
  endlet $1 __t3 __t2 __t1 loop_depth binding return_value
}

: $((__t2 = __t1 = var_decl = node = 0))
_comp_var_decls() { let node $2
  let var_decl; let __t1; let __t2
  _get_child __t1 $node 1
  case $__t1 in
    $_EXTERN_KW|$_STATIC_KW)
      defstr __str_296 "Extern and static storage class specifier not supported on local variables"
      _fatal_error __ $__str_296
    ;;
  esac
  _get_child node $node 0
  while [ $node != 0 ]; do
    _car_ var_decl $_DECL $node
    _assert_var_decl_is_safe __ $var_decl 1
    _add_var_to_local_env __ $var_decl $_BINDING_VAR_LOCAL
    if _get_child __t1 $var_decl 2; [ $__t1 != 0 ] ; then
      _get_child __t1 $var_decl 0
      _get_child __t2 $var_decl 2
      _comp_assignment __ $__t1 $__t2
    fi
    _cdr_ node $_LIST $node
  done
  endlet $1 __t2 __t1 var_decl node
}

: $((__t4 = __t3 = __t2 = __t1 = str = op = stmt_ctx = node = 0))
_comp_statement() { let node $2; let stmt_ctx $3
  let op; let str; let __t1; let __t2; let __t3; let __t4
  if [ $node = 0 ] ; then
    : $(($1 = 0))
    endlet $1 __t4 __t3 __t2 __t1 str op stmt_ctx node
    return
  fi
  _get_op op $node
  _gensym_ix=0
  if [ $op = $_IF_KW ] ; then
    _comp_if $1 $node $stmt_ctx
  elif [ $op = $_WHILE_KW ] ; then
    _get_child __t1 $node 0
    _comp_rvalue __t1 $__t1 $_RVALUE_CTX_TEST
    _get_child __t2 $node 1
    _comp_loop $1 $__t1 $__t2 0 0 $stmt_ctx
  elif [ $op = $_DO_KW ] ; then
    defstr __str_297 ":"
    _wrap_str_lit __t1 $__str_297
    _get_child __t2 $node 0
    defstr __str_298 " || break"
    _get_child __t3 $node 1
    _comp_rvalue __t3 $__t3 $_RVALUE_CTX_TEST
    _wrap_str_lit __t4 $__str_298
    _string_concat __t3 $__t3 $__t4
    _comp_loop $1 $__t1 $__t2 0 $__t3 $stmt_ctx
  elif [ $op = $_FOR_KW ] ; then
    _get_child __t1 $node 0
    _comp_statement __ $__t1 $_STMT_CTX_DEFAULT
    str=$((-__COLON__))
    if _get_child __t1 $node 1; [ $__t1 != 0 ] ; then
      _get_child __t1 $node 1
      _comp_rvalue str $__t1 $_RVALUE_CTX_TEST
    fi
    _get_child __t1 $node 3
    _get_child __t2 $node 2
    _comp_loop $1 $str $__t1 $__t2 0 $stmt_ctx
  elif [ $op = $_SWITCH_KW ] ; then
    _comp_switch $1 $node
  elif [ $op = $_BREAK_KW ] ; then
    _comp_break $1
  elif [ $op = $_CONTINUE_KW ] ; then
    _comp_continue $1
  elif [ $op = $_RETURN_KW ] ; then
    _get_child __t1 $node 0
    _comp_return $1 $__t1
  elif [ $op = $__LPAREN__ ] ; then
    _comp_fun_call __ $node 0
    : $(($1 = 0))
  elif [ $op = $__LBRACE__ ] ; then
    _comp_body $1 $node $stmt_ctx
  elif [ $op = $__EQ__ ] ; then
    _get_child __t1 $node 0
    _get_child __t2 $node 1
    _comp_assignment __ $__t1 $__t2
    : $(($1 = 0))
  elif [ $op = $__COLON__ ] ; then
    defstr __str_299 "# "
    _wrap_str_lit __t1 $__str_299
    _get_child __t2 $node 0
    _get_val __t2 $__t2
    _wrap_str_pool __t2 $__t2
    _string_concat3 __t1 $__t1 $__t2 $((-__COLON__))
    _append_glo_decl __ $__t1
    _get_child __t1 $node 1
    _comp_statement $1 $__t1 $stmt_ctx
  elif [ $op = $_GOTO_KW ] ; then
    defstr __str_300 "goto statements not supported"
    _fatal_error __ $__str_300
    : $(($1 = 0))
  elif { _get_op __t1 $node; [ $__t1 = $_CASE_KW ]; } || { _get_op __t1 $node; [ $__t1 = $_DEFAULT_KW ]; } ; then
    defstr __str_301 "case/default must be at the beginning of a switch conditional block"
    _fatal_error __ $__str_301
    : $(($1 = 0))
  elif [ $op = $_DECLS ] ; then
    _comp_var_decls __ $node
    : $(($1 = 0))
  else
    _comp_rvalue str $node $_RVALUE_CTX_BASE
    if [ $_contains_side_effects != 0 ] ; then
      defstr __str_250 ": "
      _wrap_str_lit __t1 $__str_250
      _string_concat __t1 $__t1 $str
      _append_glo_decl __ $__t1
    fi
    : $(($1 = 0))
  fi
  endlet $1 __t4 __t3 __t2 __t1 str op stmt_ctx node
}

: $((__t3 = __t2 = __t1 = start_glo_decl_idx = save_loc_vars_fixup = decl = params_ix = function_comment = let_params_text = params = fun_type = name_probe = body = fun_decl = node = 0))
_comp_glo_fun_decl() { let node $2
  let fun_decl; let body; let name_probe; let fun_type; let params; let let_params_text; let function_comment; let params_ix; let decl; let save_loc_vars_fixup; let start_glo_decl_idx; let __t1; let __t2; let __t3
  _get_child fun_decl $node 0
  _get_child body $node 1
  _get_child __t1 $fun_decl 0
  _get_val name_probe $__t1
  _get_child fun_type $fun_decl 1
  _get_child params $fun_type 1
  let_params_text=0
  function_comment=0
  params_ix=2
  if [ $body = -1 ] ; then
    endlet $1 __t3 __t2 __t1 start_glo_decl_idx save_loc_vars_fixup decl params_ix function_comment let_params_text params fun_type name_probe body fun_decl node
    return
  fi
  _top_level_stmt=0
  _check_decls __ $params
  _add_fun_params_to_local_env __ $params
  if [ $name_probe = $_MAIN_ID ] ; then
    _main_defined=1
    if [ $params != 0 ] ; then
      _runtime_use_make_argv=1
    fi
  fi
  _let_params let_params_text $params
  while [ $params != 0 ]; do
    _car_ decl $_DECL $params
    if _get_child __t1 $decl 1; _is_constant_type __t1 $__t1; [ $__t1 != 0 ] ; then
      defstr __str_302 ": \$"
      _get_child __t1 $decl 0
      _get_val __t1 $__t1
      _wrap_str_pool __t1 $__t1
      _wrap_str_lit __t2 $__str_302
      _wrap_int __t3 $params_ix
      _string_concat3 __t1 $__t1 $__t2 $__t3
      defstr __str_303 ", "
      _wrap_str_lit __t2 $__str_303
      _concatenate_strings_with function_comment $function_comment $__t1 $__t2
    fi
    _cdr_ params $_LIST $params
    : $((params_ix += 1))
  done
  if [ $function_comment != 0 ] ; then
    defstr __str_304 " # "
    _wrap_str_lit __t1 $__str_304
    _string_concat function_comment $__t1 $function_comment
  fi
  defstr __str_305 "() {"
  _function_name __t1 $name_probe
  _wrap_str_lit __t2 $__str_305
  _string_concat4 __t1 $__t1 $__t2 $let_params_text $function_comment
  _append_glo_decl __ $__t1
  _in_tail_position=1
  : $((_nest_level += 1))
  start_glo_decl_idx=$_glo_decl_ix
  _append_glo_decl_fixup save_loc_vars_fixup
  _comp_body __ $body $_STMT_CTX_DEFAULT
  _cgc_locals=$_cgc_locals_fun
  _restore_local_vars __t1 $((params_ix - 1))
  _append_glo_decl __ $__t1
  _save_local_vars __t1
  _fixup_glo_decl __ $save_loc_vars_fixup $__t1
  while [ $_rest_loc_var_fixups != 0 ]; do
    _car __t1 $_rest_loc_var_fixups
    _restore_local_vars __t2 $((params_ix - 1))
    _fixup_glo_decl __ $__t1 $__t2
    _cdr_ _rest_loc_var_fixups $_LIST $_rest_loc_var_fixups
  done
  if _any_active_glo_decls __t1 $start_glo_decl_idx; [ $((!__t1)) != 0 ] ; then
    _append_glo_decl __ $((-__COLON__))
  fi
  : $((_nest_level -= 1))
  defstr __str_306 "}\n"
  _wrap_str_lit __t1 $__str_306
  _append_glo_decl __ $__t1
  endlet $1 __t3 __t2 __t1 start_glo_decl_idx save_loc_vars_fixup decl params_ix function_comment let_params_text params fun_type name_probe body fun_decl node
}

: $((__t3 = __t2 = __t1 = args = init_len = arr_len = init = type = name = node = 0))
_comp_glo_var_decl() { let node $2
  let name; let type; let init; let arr_len; let init_len; let args; let __t1; let __t2; let __t3
  _get_child name $node 0
  _get_child type $node 1
  _get_child init $node 2
  args=0
  if _get_op __t1 $type; [ $__t1 = $__LPAREN__ ] ; then
    endlet $1 __t3 __t2 __t1 args init_len arr_len init type name node
    return
  fi
  _assert_var_decl_is_safe __ $node 0
  if _get_op __t1 $type; [ $__t1 = $__LBRACK__ ] ; then
    _get_child arr_len $type 1
    if [ $init != 0 ] && { _get_op __t1 $init; [ $__t1 = $_STRING ]; } ; then
      _get_val __t1 $init
      init_len=$((_$((_heap + __t1 + 4)) + 1))
      if [ $arr_len != 0 ] && [ $arr_len -lt $init_len ] ; then
        defstr __str_307 "Array type is too small for initializer"
        _fatal_error __ $__str_307
      fi
      _get_val __t1 $init
      _comp_defstr __ $name $__t1 $(((arr_len != 0) ? arr_len: init_len))
    else
      if [ $init != 0 ] ; then
        if _get_op __t1 $init; [ $__t1 != $_INITIALIZER_LIST ] ; then
          defstr __str_308 "Array declaration with invalid initializer"
          _fatal_error __ $__str_308
        fi
        _get_child init $init 0
        _runtime_use_initialize=1
        _initializer_list_len init_len $init
        _comp_initializer_list args $init $arr_len
        if [ $arr_len = 0 ] ; then
          arr_len=$init_len
        elif [ $arr_len -lt $init_len ] ; then
          defstr __str_307 "Array type is too small for initializer"
          _fatal_error __ $__str_307
        fi
      fi
      if [ $arr_len = 0 ] ; then
        defstr __str_309 "Array declaration without size or initializer list"
        _fatal_error __ $__str_309
      fi
      _runtime_defarr __
      defstr __str_310 "defarr "
      _wrap_str_lit __t1 $__str_310
      _get_val __t2 $name
      _global_var __t2 $__t2
      _wrap_int __t3 $arr_len
      _string_concat4 __t1 $__t1 $__t2 $((-__SPACE__)) $__t3
      _concatenate_strings_with __t1 $__t1 $args $((-__SPACE__))
      _append_glo_decl __ $__t1
    fi
  else
    if [ $init = 0 ] ; then
      _new_ast0 init $_INTEGER 0
    fi
    _comp_assignment __ $name $init
  fi
  endlet $1 __t3 __t2 __t1 args init_len arr_len init type name node
}

: $((__t2 = __t1 = rhs = constant_name = 0))
_comp_assignment_constant() { let constant_name $2; let rhs $3
  let __t1; let __t2
  defstr __str_311 "readonly "
  _wrap_str_lit __t1 $__str_311
  _comp_rvalue __t2 $rhs $_RVALUE_CTX_BASE
  _string_concat4 __t1 $__t1 $constant_name $((-__EQ__)) $__t2
  _append_glo_decl __ $__t1
  endlet $1 __t2 __t1 rhs constant_name
}

: $((__t3 = __t2 = __t1 = cas = cases = ident = 0))
_comp_enum_cases() { let ident $2; let cases $3
  let cas; let __t1; let __t2; let __t3
  if [ $ident != 0 ] ; then
    defstr __str_312 " enum declaration"
    defstr __str_299 "# "
    _wrap_str_lit __t1 $__str_299
    _get_val __t2 $ident
    _wrap_str_pool __t2 $__t2
    _wrap_str_lit __t3 $__str_312
    _string_concat3 __t1 $__t1 $__t2 $__t3
    _append_glo_decl __ $__t1
  else
    defstr __str_313 "# Enum declaration"
    _wrap_str_lit __t1 $__str_313
    _append_glo_decl __ $__t1
  fi
  while [ $cases != 0 ]; do
    _car_ cas $__EQ__ $cases
    _get_child __t1 $cas 0
    _get_val __t1 $__t1
    _global_var __t1 $__t1
    _get_child __t2 $cas 1
    _comp_assignment_constant __ $__t1 $__t2
    _cdr_ cases $_LIST $cases
  done
  endlet $1 __t3 __t2 __t1 cas cases ident
}

: $((__t3 = __t2 = __t1 = field_type = offset = decl = members = ident = 0))
_comp_struct() { let ident $2; let members $3
  let decl; let offset; let field_type; let __t1; let __t2; let __t3
  _new_ast0 offset $_INTEGER 0
  if [ $ident != 0 ] ; then
    defstr __str_314 " struct member declarations"
    defstr __str_299 "# "
    _wrap_str_lit __t1 $__str_299
    _get_val __t2 $ident
    _wrap_str_pool __t2 $__t2
    _wrap_str_lit __t3 $__str_314
    _string_concat3 __t1 $__t1 $__t2 $__t3
    _append_glo_decl __ $__t1
  else
    defstr __str_315 "# Struct member declarations"
    _wrap_str_lit __t1 $__str_315
    _append_glo_decl __ $__t1
  fi
  while [ $members != 0 ]; do
    _car_ decl $_DECL $members
    _cdr_ members $_LIST $members
    _get_child field_type $decl 1
    if { _get_op __t1 $field_type; [ $__t1 = $__LBRACK__ ]; } || { _get_op __t1 $field_type; [ $__t1 = $_STRUCT_KW ]; } ; then
      defstr __str_316 "Nested structures not supported by shell backend. Use a reference type instead."
      _fatal_error __ $__str_316
    fi
    _get_child __t1 $decl 0
    _struct_member_var __t1 $__t1
    _comp_assignment_constant __ $__t1 $offset
    _get_val __t1 $offset
    _set_val __ $offset $((__t1 - 1))
  done
  if [ $ident != 0 ] ; then
    _struct_sizeof_var __t1 $ident
    _comp_assignment_constant __ $__t1 $offset
  fi
  _append_glo_decl __ 0
  endlet $1 __t3 __t2 __t1 field_type offset decl members ident
}

: $((__t3 = __t2 = __t1 = type = 0))
_handle_enum_struct_union_type_decl() { let type $2
  let __t1; let __t2; let __t3
  if _get_op __t1 $type; [ $__t1 = $_ENUM_KW ] ; then
    _get_child __t1 $type 1
    _get_child __t2 $type 2
    _comp_enum_cases __ $__t1 $__t2
  elif _get_op __t3 $type; [ $__t3 = $_STRUCT_KW ] ; then
    _get_child __t1 $type 1
    _get_child __t2 $type 2
    _comp_struct __ $__t1 $__t2
  elif _get_op __t3 $type; [ $__t3 = $_UNION_KW ] ; then
    defstr __str_317 "handle_enum_struct_union_type_decl: union not supported"
    _fatal_error __ $__str_317
  fi
  endlet $1 __t3 __t2 __t1 type
}

: $((__t1 = type = decl = decls = node = 0))
_handle_typedef() { let node $2
  let decls; let decl; let type; let __t1
  _get_child decls $node 0
  _car_ decl $_DECL $decls
  _get_child type $decl 1
  _get_type_specifier __t1 $type
  _handle_enum_struct_union_type_decl __ $__t1
  endlet $1 __t1 type decl decls node
}

: $((__t2 = __t1 = op = declarations = node = 0))
_comp_glo_decl() { let node $2
  let declarations; let op; let __t1; let __t2
  _get_op op $node
  _fun_gensym_ix=0
  _top_level_stmt=1
  if [ $op = $__EQ__ ] ; then
    _get_child __t1 $node 0
    _get_child __t2 $node 1
    _comp_assignment __ $__t1 $__t2
  elif [ $op = $_DECLS ] ; then
    if _get_child __t1 $node 1; [ $__t1 = $_EXTERN_KW ] ; then
      defstr __str_318 "Extern storage class specifier not supported"
      _fatal_error __ $__str_318
    fi
    _get_child declarations $node 0
    while [ $declarations != 0 ]; do
      _car_ __t1 $_DECL $declarations
      _comp_glo_var_decl __ $__t1
      _cdr_ declarations $_LIST $declarations
    done
  elif [ $op = $_FUN_DECL ] ; then
    _comp_glo_fun_decl __ $node
  elif [ $op = $_TYPEDEF_KW ] ; then
    _handle_typedef __ $node
  elif [ $op = $_ENUM_KW ] || [ $op = $_STRUCT_KW ] || [ $op = $_UNION_KW ] ; then
    _handle_enum_struct_union_type_decl __ $node
  else
    printf "op=%d " $op
    printf \\$((op/64))$((op/8%8))$((op%8))
    _get_nb_children __t1 $node
    printf " with %d children\n" $__t1
    defstr __str_319 "comp_glo_decl: unexpected declaration"
    _fatal_error __ $__str_319
  fi
  endlet $1 __t2 __t1 op declarations node
}

_prologue() {
  printf "#!/bin/sh\n"
  printf "set -e -u -f\n"
  printf "LC_ALL=C\n\n"
}

: $((__t1 = c = 0))
_epilogue() {
  let c; let __t1
  if [ $_any_character_used != 0 ] ; then
    printf "# Character constants\n"
    c=0
    while [ $c -lt 256 ]; do
      if [ $((_$((_characters_useds + (c / 16))) & (1 << (c % 16)))) != 0 ] ; then
        printf "readonly "
        _character_ident __t1 $c
        _print_text __ $__t1
        printf "="
        printf "%d" $c
        printf "\n"
      fi
      : $((c += 1))
    done
  fi
  printf "# Runtime library\n"
  _produce_runtime __
  if [ $_main_defined != 0 ] ; then
    printf "__code=0; # Exit code\n"
    if [ $_runtime_use_make_argv != 0 ] ; then
      printf "make_argv \$((\$# + 1)) \"\$0\" \"\$@\" # Setup argc/argv\n"
      printf "_main __code \$((\$# + 1)) \$__argv"
    else
      printf "_main __code"
    fi
    printf "\nexit \$__code\n"
  fi
  endlet $1 __t1 c
}

: $((__t2 = __t1 = counter = res = ident = env = 0))
_initialize_function_variables() {
  let env; let ident; let res; let counter; let __t1; let __t2
  env=$_cgc_locals_fun
  res=0
  counter=$_fun_gensym_ix
  while [ $counter -gt 0 ]; do
    _new_fresh_ident ident $counter
    _format_special_var __t1 $ident 0
    defstr __str_176 " = "
    _wrap_str_lit __t2 $__str_176
    _concatenate_strings_with res $res $__t1 $__t2
    : $((counter -= 1))
  done
  while [ $env != 0 ]; do
    ident=$((_$((_heap + env + 2))))
    if [ $((_$((_heap + env + 1)))) != $_BINDING_PARAM_LOCAL ] || { _is_constant_type __t1 $((_$((_heap + env + 4)))); [ $((!__t1)) != 0 ]; } ; then
      _local_var __t1 $ident
      defstr __str_176 " = "
      _wrap_str_lit __t2 $__str_176
      _concatenate_strings_with res $res $__t1 $__t2
    fi
    env=$((_$((_heap + env))))
  done
  if [ $res != 0 ] ; then
    defstr __str_276 ": \$(("
    _wrap_str_lit __t1 $__str_276
    defstr __str_320 " = 0))"
    _wrap_str_lit __t2 $__str_320
    _string_concat3 res $__t1 $res $__t2
    _print_text __ $res
    printf "\n"
  fi
  endlet $1 __t2 __t1 counter res ident env
}

_codegen_begin() {
  _init_comp_context __
  _prologue __
}

_max_text_alloc=0
_cumul_text_alloc=0
: $((decl = 0))
_codegen_glo_decl() { let decl $2
  _glo_decl_ix=0
  _text_alloc=1
  : $((_cgc_locals = _cgc_locals_fun = 0))
  _cgc_fs=1
  _comp_glo_decl __ $decl
  _initialize_function_variables __
  _print_glo_decls __
  _max_text_alloc=$(((_max_text_alloc > _text_alloc) ? _max_text_alloc: _text_alloc))
  : $((_cumul_text_alloc += _text_alloc))
  endlet $1 decl
}

_codegen_end() {
  _epilogue __
}

: $((decl = i = argv_ = argc = 0))
_main() { let argc $2; let argv_ $3
  let i; let decl
  _init_ident_table __
  _init_pnut_macros __
  i=1
  while [ $i -lt $argc ]; do
    if [ $((_$((_$((argv_ + i)) + 0)))) = $__MINUS__ ] ; then
      case $((_$((_$((argv_ + i)) + 1)))) in
        $__D__)
          _init_builtin_int_macro __ $((_$((argv_ + i)) + 2)) 1
        ;;
        $__U__)
          if [ $((_$((_$((argv_ + i)) + 2)))) = 0 ] ; then
            : $((i += 1))
            _init_ident __ $_IDENTIFIER $((_$((argv_ + i))))
          else
            _init_ident __ $_IDENTIFIER $((_$((argv_ + i)) + 2))
          fi
        ;;
        $__I__)
          if [ $_include_search_path != 0 ] ; then
            defstr __str_321 "only one include path allowed"
            _fatal_error __ $__str_321
          fi
          if [ $((_$((_$((argv_ + i)) + 2)))) = 0 ] ; then
            : $((i += 1))
            _include_search_path=$((_$((argv_ + i))))
          else
            _include_search_path=$((_$((argv_ + i)) + 2))
          fi
        ;;
        *)
          printf "Option "
          _putstr __ $((_$((argv_ + i))))
          printf "\n"
          defstr __str_322 "unknown option"
          _fatal_error __ $__str_322
        ;;
      esac
    else
      _include_file __ $((_$((argv_ + i)))) 0
    fi
    : $((i += 1))
  done
  if [ $_fp = 0 ] ; then
    printf "Usage: "
    _putstr __ $((_$((argv_ + 0))))
    printf " <filename>\n"
    defstr __str_323 "no input file"
    _fatal_error __ $__str_323
  fi
  _ch=$__NEWLINE__
  _codegen_begin __
  _get_tok __
  while [ $_tok != -1 ]; do
    _parse_declaration decl 0
    _codegen_glo_decl __ $decl
  done
  _codegen_end __
  : $(($1 = 0))
  endlet $1 decl i argv_ argc
}

# Character constants
readonly __NUL__=0
readonly __ALARM__=7
readonly __BACKSPACE__=8
readonly __TAB__=9
readonly __NEWLINE__=10
readonly __VTAB__=11
readonly __PAGE__=12
readonly __RET__=13
readonly __SPACE__=32
readonly __EXCL__=33
readonly __DQUOTE__=34
readonly __SHARP__=35
readonly __DOLLAR__=36
readonly __PERCENT__=37
readonly __AMP__=38
readonly __QUOTE__=39
readonly __LPAREN__=40
readonly __RPAREN__=41
readonly __STAR__=42
readonly __PLUS__=43
readonly __COMMA__=44
readonly __MINUS__=45
readonly __PERIOD__=46
readonly __SLASH__=47
readonly __0__=48
readonly __1__=49
readonly __2__=50
readonly __3__=51
readonly __4__=52
readonly __5__=53
readonly __6__=54
readonly __7__=55
readonly __8__=56
readonly __9__=57
readonly __COLON__=58
readonly __SEMICOLON__=59
readonly __LT__=60
readonly __EQ__=61
readonly __GT__=62
readonly __QUESTION__=63
readonly __AT__=64
readonly __A__=65
readonly __D__=68
readonly __I__=73
readonly __U__=85
readonly __X__=88
readonly __Z__=90
readonly __LBRACK__=91
readonly __BACKSLASH__=92
readonly __RBRACK__=93
readonly __CARET__=94
readonly __UNDERSCORE__=95
readonly __BACKTICK__=96
readonly __a__=97
readonly __b__=98
readonly __c__=99
readonly __d__=100
readonly __f__=102
readonly __i__=105
readonly __l__=108
readonly __n__=110
readonly __o__=111
readonly __r__=114
readonly __s__=115
readonly __t__=116
readonly __u__=117
readonly __v__=118
readonly __x__=120
readonly __z__=122
readonly __LBRACE__=123
readonly __BAR__=124
readonly __RBRACE__=125
readonly __TILDE__=126
# Runtime library
__c2i_0=48
__c2i_1=49
__c2i_2=50
__c2i_3=51
__c2i_4=52
__c2i_5=53
__c2i_6=54
__c2i_7=55
__c2i_8=56
__c2i_9=57
__c2i_a=97
__c2i_b=98
__c2i_c=99
__c2i_d=100
__c2i_e=101
__c2i_f=102
__c2i_g=103
__c2i_h=104
__c2i_i=105
__c2i_j=106
__c2i_k=107
__c2i_l=108
__c2i_m=109
__c2i_n=110
__c2i_o=111
__c2i_p=112
__c2i_q=113
__c2i_r=114
__c2i_s=115
__c2i_t=116
__c2i_u=117
__c2i_v=118
__c2i_w=119
__c2i_x=120
__c2i_y=121
__c2i_z=122
__c2i_A=65
__c2i_B=66
__c2i_C=67
__c2i_D=68
__c2i_E=69
__c2i_F=70
__c2i_G=71
__c2i_H=72
__c2i_I=73
__c2i_J=74
__c2i_K=75
__c2i_L=76
__c2i_M=77
__c2i_N=78
__c2i_O=79
__c2i_P=80
__c2i_Q=81
__c2i_R=82
__c2i_S=83
__c2i_T=84
__c2i_U=85
__c2i_V=86
__c2i_W=87
__c2i_X=88
__c2i_Y=89
__c2i_Z=90
char_to_int() {
  case $1 in
    [[:alnum:]]) __c=$((__c2i_$1)) ;;
    ' ') __c=32 ;;
    '!') __c=33 ;;
    '"') __c=34 ;;
    '#') __c=35 ;;
    '$') __c=36 ;;
    '%') __c=37 ;;
    '&') __c=38 ;;
    "'") __c=39 ;;
    '(') __c=40 ;;
    ')') __c=41 ;;
    '*') __c=42 ;;
    '+') __c=43 ;;
    ',') __c=44 ;;
    '-') __c=45 ;;
    '.') __c=46 ;;
    '/') __c=47 ;;
    ':') __c=58 ;;
    ';') __c=59 ;;
    '<') __c=60 ;;
    '=') __c=61 ;;
    '>') __c=62 ;;
    '?') __c=63 ;;
    '@') __c=64 ;;
    '[') __c=91 ;;
    '\') __c=92 ;;
    ']') __c=93 ;;
    '^') __c=94 ;;
    '_') __c=95 ;;
    '`') __c=96 ;;
    '{') __c=123 ;;
    '|') __c=124 ;;
    '}') __c=125 ;;
    '~') __c=126 ;;
    *)
      __c=$(printf "%d" "'$1"); __c=$((__c > 0 ? __c : 256 + __c)) ;;
  esac
}

unpack_escaped_string() { # $1 = string, $2 = size (optional)
  __str="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${2:-${#__str} + 1}))
  __ptr=$__addr
  __end=$((__ptr + ${2:-${#__str} + 1})) # End of allocated memory
  while [ -n "$__str" ] ; do
    case "$__str" in
      '\'*)
        __str="${__str#?}" # Remove the current char from $__str
        case "$__str" in
          '0'*) __c=0 ;;
          'a'*) __c=7 ;;
          'b'*) __c=8 ;;
          'f'*) __c=12 ;;
          'n'*) __c=10 ;;
          'r'*) __c=13 ;;
          't'*) __c=9 ;;
          'v'*) __c=11 ;;
          '\'*) __c=92 ;;
          '"'*) __c=34 ;;
          "'"*) __c=39 ;;
          '?'*) __c=63 ;;
          '$'*) __c=36 ;; # Not in C, used to escape variable expansion between double quotes
          *) echo "invalid escape in string: $__str"; exit 1 ;;
        esac
        __str="${__str#?}" # Remove the current char from $__str
        ;;
      *)
        char_to_int "${__str%"${__str#?}"}"
        __str="${__str#?}" # Remove the current char from $__str
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  while [ $__ptr -le $__end ]; do
    : $((_$__ptr = 0))
    : $((__ptr += 1))
  done
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string, $3 = size (optional)
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2" $3
    : $(($1 = __addr))
  fi
  set -u
}

_free() { # $2 = object to free
  __ptr=$(($2 - 1))          # Start of object
  __end=$((__ptr + _$__ptr)) # End of object
  while [ $__ptr -lt $__end ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
  done
  : $(($1 = 0))              # Return 0
}

_put_pstr() {
  : $(($1 = 0)); shift # Return 0
  __addr=$1; shift
  while [ $((__c = _$__addr)) != 0 ]; do
    printf \\$((__c/64))$((__c/8%8))$((__c%8))
    : $((__addr += 1))
  done
}

_malloc __buffer_fd0 1000   # Allocate buffer
: $((_$__buffer_fd0 = 0))   # Init buffer to ""
: $((__cursor_fd0 = 0))     # Make buffer empty
: $((__buflen_fd0 = 1000))  # Init buffer length
__state_fd0=0 # stdin
__state_fd1=1 # stdout
__state_fd2=2 # stderr
__state_fd3=-1
__state_fd4=-1
__state_fd5=-1
__state_fd6=-1
__state_fd7=-1
__state_fd8=-1
__state_fd9=-1

_open() { # $2: filename, $3: flags, $4: mode
  # Get available fd
  __fd=0
  while [ $__fd -lt 10 ]; do
    if [ $((__state_fd$__fd)) -lt 0 ]; then
      break
    fi
    : $((__fd += 1))
  done
  if [ $__fd -gt 9 ] ; then
    # Some shells don't support fd > 9
    echo "No more file descriptors available to open $(_put_pstr __ $2)" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    _malloc __addr 1000                 # Allocate buffer
    : $((_$__addr = 0))                 # Init buffer to ""
    : $((__buffer_fd$__fd = __addr))    # Buffer address
    : $((__cursor_fd$__fd = 0))         # Buffer cursor
    : $((__buflen_fd$__fd = 1000))      # Buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    __res=$(_put_pstr __ $2)
    if [ $3 = 0 ] ; then
      case $__fd in
        0) exec 0< "$__res" ;; 1) exec 1< "$__res" ;; 2) exec 2< "$__res" ;;
        3) exec 3< "$__res" ;; 4) exec 4< "$__res" ;; 5) exec 5< "$__res" ;;
        6) exec 6< "$__res" ;; 7) exec 7< "$__res" ;; 8) exec 8< "$__res" ;;
        9) exec 9< "$__res" ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> "$__res" ;; 1) exec 1> "$__res" ;; 2) exec 2> "$__res" ;;
        3) exec 3> "$__res" ;; 4) exec 4> "$__res" ;; 5) exec 5> "$__res" ;;
        6) exec 6> "$__res" ;; 7) exec 7> "$__res" ;; 8) exec 8> "$__res" ;;
        9) exec 9> "$__res" ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> "$__res" ;; 1) exec 1>> "$__res" ;; 2) exec 2>> "$__res" ;;
        3) exec 3>> "$__res" ;; 4) exec 4>> "$__res" ;; 5) exec 5>> "$__res" ;;
        6) exec 6>> "$__res" ;; 7) exec 7>> "$__res" ;; 8) exec 8>> "$__res" ;;
        9) exec 9>> "$__res" ;;
      esac
    else
      echo "Unknown file mode" ; exit 1
    fi
  fi
  : $(($1 = __fd))
}

# Open the file and return a FILE* for the file.
# The FILE structure contains the file descriptor.
_fopen() { # $2: File name, $3: Mode
  _open __fd $2 $((_$3 == 119)) 511
  _malloc __file 1        # Allocate FILE structure
  : $((_$__file = __fd))  # Save fd
  : $(($1 = __file))
}

_close() { # $2: fd
  __fd=$2
  __buf=$((__buffer_fd$__fd))  # Get buffer
  _free __ $__buf              # Release buffer
  : $((__state_fd$__fd = -1))  # Mark the fd as closed
  case $__fd in
    0) exec 0<&- ;; 1) exec 1<&- ;; 2) exec 2<&- ;;
    3) exec 3<&- ;; 4) exec 4<&- ;; 5) exec 5<&- ;;
    6) exec 6<&- ;; 7) exec 7<&- ;; 8) exec 8<&- ;;
    9) exec 9<&- ;;
  esac
  : $(($1 = 0))
}

_fclose() { # $2: file
  __file=$2
  __fd=$((_$__file))  # Get fd
  _free __ $__file    # Release FILE structure
  _close $1 $__fd
}

# Unpack a Shell string into an appropriately sized buffer
unpack_string() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  while [ ! -z "$__fgetc_buf" ]; do
    case "$__fgetc_buf" in
      " "*) : $((_$__buffer = 32))  ;;
      "e"*) : $((_$__buffer = 101)) ;;
      "="*) : $((_$__buffer = 61))  ;;
      "t"*) : $((_$__buffer = 116)) ;;
      ";"*) : $((_$__buffer = 59))  ;;
      "i"*) : $((_$__buffer = 105)) ;;
      ")"*) : $((_$__buffer = 41))  ;;
      "("*) : $((_$__buffer = 40))  ;;
      "n"*) : $((_$__buffer = 110)) ;;
      "s"*) : $((_$__buffer = 115)) ;;
      "l"*) : $((_$__buffer = 108)) ;;
      "+"*) : $((_$__buffer = 43))  ;;
      "p"*) : $((_$__buffer = 112)) ;;
      "a"*) : $((_$__buffer = 97))  ;;
      "r"*) : $((_$__buffer = 114)) ;;
      "f"*) : $((_$__buffer = 102)) ;;
      "d"*) : $((_$__buffer = 100)) ;;
      "*"*) : $((_$__buffer = 42))  ;;
      *)
        char_to_int "${__fgetc_buf%"${__fgetc_buf#?}"}"
        : $((_$__buffer = __c))
        ;;
    esac
    __fgetc_buf=${__fgetc_buf#?}      # Remove the first character
    : $((__buffer += 1))              # Move to the next buffer position
  done

  if [ $__ends_with_eof -eq 0 ]; then # Ends with newline and not EOF?
    : $((_$__buffer = 10))            # Line ends with newline
    : $((__buffer += 1))
  fi
  : $((_$__buffer = 0))               # Then \0
}

refill_buffer() { # $1: fd
  __fd=$1
  __buffer=$((__buffer_fd$__fd))

  IFS=
  __ends_with_eof=0
  read -r __temp_buf <&$__fd || __ends_with_eof=1

  # Check that the buffer is large enough to unpack the line
  __buflen=$((__buflen_fd$__fd - 2)) # Minus 2 to account for newline and \0
  __len=${#__temp_buf}
  if [ $__len -gt $__buflen ]; then
    # Free buffer and reallocate a new one double the line size
    __buflen=$((__len * 2))
    _free __ $__buffer
    _malloc __buffer $__buflen
    : $((__buffer_fd$__fd = __buffer))
    : $((__buflen_fd$__fd = __buflen))
  fi
  unpack_string "$__temp_buf" $__buffer $__ends_with_eof
}

read_byte() { # $2: fd
  __fd=$2
  : $((__buffer=__buffer_fd$__fd))
  : $((__cursor=__cursor_fd$__fd))
  # The cursor is at the end of the buffer, we need to read the next line
  if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
    # Buffer has been read completely, read next line
    refill_buffer $__fd
    __cursor=0 # Reset cursor and reload buffer
    : $((__buffer=__buffer_fd$__fd))
    if [ $((_$((__buffer + __cursor)))) -eq 0 ]; then
      : $(($1 = -1)) # EOF
      return
    fi
  fi
  : $(($1 = _$((__buffer + __cursor))))
  : $((__cursor_fd$__fd = __cursor + 1))  # Increment cursor
}

_fgetc() { # $2: file
  __file=$2
  __fd=$((_$__file))
  read_byte $1 $__fd
}

make_argv() {
  __argc=$1; shift;
  _malloc __argv $__argc # Allocate enough space for all elements. No need to initialize.
  __argv_ptr=$__argv

  while [ $# -ge 1 ]; do
    _malloc _$__argv_ptr $((${#1} + 1))
    unpack_string "$1" $((_$__argv_ptr)) 1
    : $((__argv_ptr += 1))
    shift
  done
}

# Local variables
__=0
__SP=0
let() { # $1: variable name, $2: value (optional)
  : $((__$((__SP += 1))=$1)) # Push
  : $(($1=${2-0}))           # Init
}
endlet() { # $1: return variable
           # $2...: function local variables
  __ret=$1 # Don't overwrite return value
  : $((__tmp = $__ret))
  while [ $# -ge 2 ]; do
    : $(($2 = __$(((__SP -= 1) + 1)))) # Pop
    shift;
  done
  : $(($__ret=__tmp))   # Restore return value
}

__code=0; # Exit code
make_argv $(($# + 1)) "$0" "$@" # Setup argc/argv
_main __code $(($# + 1)) $__argv
exit $__code

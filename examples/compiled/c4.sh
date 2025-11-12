#!/bin/sh
set -e -f
LC_ALL=C

: $((p = len = c = b = 0))
_memset() { # b: $2, c: $3, len: $4
  set $@ $b $c $len $p
  b=$2
  c=$3
  len=$4
  p=$b
  while [ $(((len -= 1) + 1)) != 0 ]; do
    : $((_$(((p += 1) - 1)) = c))
  done
  : $(($1 = b))
  : $((__tmp = $1)) $((b = $5)) $((c = $6)) $((len = $7)) $((p = $8)) $(($1 = __tmp))
}

: $((r = l = n = vr = vl = 0))
_memcmp() { # vl: $2, vr: $3, n: $4
  set $@ $vl $vr $n $l $r
  vl=$2
  vr=$3
  n=$4
  l=$vl
  r=$vr
  while [ $n != 0 ] && [ $((_$l)) = $((_$r)) ]; do
    : $((n -= 1))
    : $((l += 1))
    : $((r += 1))
  done
  : $(($1 = n ? ((_$l & 255) - (_$r & 255)): 0))
  : $((__tmp = $1)) $((vl = $5)) $((vr = $6)) $((n = $7)) $((l = $8)) $((r = $9)) $(($1 = __tmp))
}

_p=0
_lp=0
_data=0
_e=0
_le=0
_id=0
_sym=0
_tk=0
_ival=0
_ty=0
_loc=0
_line=0
_src=0
_debug=0
# Enum declaration
readonly _Num=128
readonly _Fun=129
readonly _Sys=130
readonly _Glo=131
readonly _Loc=132
readonly _Id=133
readonly _Char=134
readonly _Else=135
readonly _Enum=136
readonly _If=137
readonly _Int=138
readonly _Return=139
readonly _Sizeof=140
readonly _While=141
readonly _Assign=142
readonly _Cond=143
readonly _Lor=144
readonly _Lan=145
readonly _Or=146
readonly _Xor=147
readonly _And=148
readonly _Eq=149
readonly _Ne=150
readonly _Lt=151
readonly _Gt=152
readonly _Le=153
readonly _Ge=154
readonly _Shl=155
readonly _Shr=156
readonly _Add=157
readonly _Sub=158
readonly _Mul=159
readonly _Div=160
readonly _Mod=161
readonly _Inc=162
readonly _Dec=163
readonly _Brak=164
# Enum declaration
readonly _LEA=0
readonly _IMM=1
readonly _JMP=2
readonly _JSR=3
readonly _BZ=4
readonly _BNZ=5
readonly _ENT=6
readonly _ADJ=7
readonly _LEV=8
readonly _LI=9
readonly _LC=10
readonly _SI=11
readonly _SC=12
readonly _PSH=13
readonly _OR=14
readonly _XOR=15
readonly _AND=16
readonly _EQ=17
readonly _NE=18
readonly _LT=19
readonly _GT=20
readonly _LE=21
readonly _GE=22
readonly _SHL=23
readonly _SHR=24
readonly _ADD=25
readonly _SUB=26
readonly _MUL=27
readonly _DIV=28
readonly _MOD=29
readonly _OPEN=30
readonly _READ=31
readonly _CLOS=32
readonly _PRTF=33
readonly _MALC=34
readonly _FREE=35
readonly _MSET=36
readonly _MCMP=37
readonly _EXIT=38
# Enum declaration
readonly _CHAR=0
readonly _INT=1
readonly _PTR=2
# Enum declaration
readonly _Tk=0
readonly _Hash=1
readonly _Name=2
readonly _Class=3
readonly _Type=4
readonly _Val=5
readonly _HClass=6
readonly _HType=7
readonly _HVal=8
readonly _Idsz=9
: $((__t1 = pp = 0))
_next() {
  set $@ $pp $__t1
  while [ $((_tk = _$_p)) != 0 ]; do
    : $((_p += 1))
    if [ $_tk = $__LF__ ] ; then
      if [ $_src != 0 ] ; then
        printf "%d: (%d - %d = %d) %.*s" $_line $_p $_lp $((_p - _lp)) $((_p - _lp)) "$(_put_pstr __ $_lp)"
        _lp=$_p
        while [ $_le -lt $_e ]; do
          defstr __str_0 "LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"
          printf "%8.4s" "$(_put_pstr __ $((__str_0 + _$((_le += 1)) * 5)))"
          if [ $((_$_le)) -le $_ADJ ] ; then
            printf " %d\n" $((_$((_le += 1))))
          else
            printf "\n"
          fi
        done
      fi
      : $((_line += 1))
    elif [ $_tk = $__HASH__ ] ; then
      while [ $((_$_p)) != 0 ] && [ $((_$_p)) != $__LF__ ]; do
        : $((_p += 1))
      done
    elif { [ $_tk -ge $__a__ ] && [ $_tk -le $__z__ ]; } || { [ $_tk -ge $__A__ ] && [ $_tk -le $__Z__ ]; } || [ $_tk = $__UNDERSCORE__ ] ; then
      pp=$((_p - 1))
      while { [ $((_$_p)) -ge $__a__ ] && [ $((_$_p)) -le $__z__ ]; } || { [ $((_$_p)) -ge $__A__ ] && [ $((_$_p)) -le $__Z__ ]; } || { [ $((_$_p)) -ge $__0__ ] && [ $((_$_p)) -le $__9__ ]; } || [ $((_$_p)) = $__UNDERSCORE__ ]; do
        _tk=$(((_tk * 147) + _$(((_p += 1) - 1))))
      done
      _tk=$(((_tk << 6) + (_p - pp)))
      _id=$_sym
      while [ $((_$((_id + _Tk)))) != 0 ]; do
        if [ $_tk = $((_$((_id + _Hash)))) ] && { _memcmp __t1 $((_$((_id + _Name)))) $pp $((_p - pp)); [ $((!__t1)) != 0 ]; } ; then
          _tk=$((_$((_id + _Tk))))
          : $((__tmp = $1)) $((pp = $2)) $((__t1 = $3)) $(($1 = __tmp))
          return
        fi
        _id=$((_id + _Idsz))
      done
      : $((_$((_id + _Name)) = pp))
      : $((_$((_id + _Hash)) = _tk))
      : $((_tk = _$((_id + _Tk)) = _Id))
      break
    elif [ $_tk -ge $__0__ ] && [ $_tk -le $__9__ ] ; then
      if [ $((_ival = _tk - __0__)) != 0 ] ; then
        while [ $((_$_p)) -ge $__0__ ] && [ $((_$_p)) -le $__9__ ]; do
          _ival=$((((_ival * 10) + _$(((_p += 1) - 1))) - __0__))
        done
      elif [ $((_$_p)) = $__x__ ] || [ $((_$_p)) = $__X__ ] ; then
        while [ $((_tk = _$((_p += 1)))) != 0 ] && { { [ $_tk -ge $__0__ ] && [ $_tk -le $__9__ ]; } || { [ $_tk -ge $__a__ ] && [ $_tk -le $__f__ ]; } || { [ $_tk -ge $__A__ ] && [ $_tk -le $__F__ ]; }; }; do
          _ival=$(((_ival * 16) + (_tk & 15) + ((_tk >= __A__) ? 9: 0)))
        done
      else
        while [ $((_$_p)) -ge $__0__ ] && [ $((_$_p)) -le $__7__ ]; do
          _ival=$((((_ival * 8) + _$(((_p += 1) - 1))) - __0__))
        done
      fi
      _tk=$_Num
      break
    elif [ $_tk = $__SLASH__ ] ; then
      if [ $((_$_p)) = $__SLASH__ ] ; then
        : $((_p += 1))
        while [ $((_$_p)) != 0 ] && [ $((_$_p)) != $__LF__ ]; do
          : $((_p += 1))
        done
      else
        _tk=$_Div
        break
      fi
    elif [ $_tk = $__QUOTE__ ] || [ $_tk = $__DQUOTE__ ] ; then
      pp=$_data
      while [ $((_$_p)) != 0 ] && [ $((_$_p)) != $_tk ]; do
        if [ $((_ival = _$(((_p += 1) - 1)))) = $__BACKSLASH__ ] ; then
          if [ $((_ival = _$(((_p += 1) - 1)))) = $__n__ ] ; then
            _ival=$__LF__
          fi
        fi
        if [ $_tk = $__DQUOTE__ ] ; then
          : $((_$(((_data += 1) - 1)) = _ival))
        fi
      done
      : $((_p += 1))
      if [ $_tk = $__DQUOTE__ ] ; then
        _ival=$pp
      else
        _tk=$_Num
      fi
      break
    elif [ $_tk = $__EQ__ ] ; then
      if [ $((_$_p)) = $__EQ__ ] ; then
        : $((_p += 1))
        _tk=$_Eq
      else
        _tk=$_Assign
      fi
      break
    elif [ $_tk = $__PLUS__ ] ; then
      if [ $((_$_p)) = $__PLUS__ ] ; then
        : $((_p += 1))
        _tk=$_Inc
      else
        _tk=$_Add
      fi
      break
    elif [ $_tk = $__MINUS__ ] ; then
      if [ $((_$_p)) = $__MINUS__ ] ; then
        : $((_p += 1))
        _tk=$_Dec
      else
        _tk=$_Sub
      fi
      break
    elif [ $_tk = $__EXCL__ ] ; then
      if [ $((_$_p)) = $__EQ__ ] ; then
        : $((_p += 1))
        _tk=$_Ne
      fi
      break
    elif [ $_tk = $__LT__ ] ; then
      if [ $((_$_p)) = $__EQ__ ] ; then
        : $((_p += 1))
        _tk=$_Le
      elif [ $((_$_p)) = $__LT__ ] ; then
        : $((_p += 1))
        _tk=$_Shl
      else
        _tk=$_Lt
      fi
      break
    elif [ $_tk = $__GT__ ] ; then
      if [ $((_$_p)) = $__EQ__ ] ; then
        : $((_p += 1))
        _tk=$_Ge
      elif [ $((_$_p)) = $__GT__ ] ; then
        : $((_p += 1))
        _tk=$_Shr
      else
        _tk=$_Gt
      fi
      break
    elif [ $_tk = $__BAR__ ] ; then
      if [ $((_$_p)) = $__BAR__ ] ; then
        : $((_p += 1))
        _tk=$_Lor
      else
        _tk=$_Or
      fi
      break
    elif [ $_tk = $__AMP__ ] ; then
      if [ $((_$_p)) = $__AMP__ ] ; then
        : $((_p += 1))
        _tk=$_Lan
      else
        _tk=$_And
      fi
      break
    elif [ $_tk = $__CARET__ ] ; then
      _tk=$_Xor
      break
    elif [ $_tk = $__PERCENT__ ] ; then
      _tk=$_Mod
      break
    elif [ $_tk = $__STAR__ ] ; then
      _tk=$_Mul
      break
    elif [ $_tk = $__LBRACK__ ] ; then
      _tk=$_Brak
      break
    elif [ $_tk = $__QUESTION__ ] ; then
      _tk=$_Cond
      break
    elif [ $_tk = $__TILDE__ ] || [ $_tk = $__SEMICOLON__ ] || [ $_tk = $__LBRACE__ ] || [ $_tk = $__RBRACE__ ] || [ $_tk = $__LPAREN__ ] || [ $_tk = $__RPAREN__ ] || [ $_tk = $__RBRACK__ ] || [ $_tk = $__COMMA__ ] || [ $_tk = $__COLON__ ] ; then
      break
    fi
  done
  : $((__tmp = $1)) $((pp = $2)) $((__t1 = $3)) $(($1 = __tmp))
}

: $((d = t = lev = 0))
_expr() { # lev: $2
  set $@ $lev $t $d
  lev=$2
  if [ $((! _tk)) != 0 ] ; then
    printf "%d: unexpected eof in expression\n" $_line
    exit -1
  elif [ $_tk = $_Num ] ; then
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = _ival))
    _next __
    _ty=$_INT
  elif [ $_tk = $__DQUOTE__ ] ; then
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = _ival))
    _next __
    while [ $_tk = $__DQUOTE__ ]; do
      _next __
    done
    _data=$(((_data + 1) & -1))
    _ty=$_PTR
  elif [ $_tk = $_Sizeof ] ; then
    _next __
    if [ $_tk = $__LPAREN__ ] ; then
      _next __
    else
      printf "%d: open paren expected in sizeof\n" $_line
      exit -1
    fi
    _ty=$_INT
    if [ $_tk = $_Int ] ; then
      _next __
    elif [ $_tk = $_Char ] ; then
      _next __
      _ty=$_CHAR
    fi
    while [ $_tk = $_Mul ]; do
      _next __
      _ty=$((_ty + _PTR))
    done
    if [ $_tk = $__RPAREN__ ] ; then
      _next __
    else
      printf "%d: close paren expected in sizeof\n" $_line
      exit -1
    fi
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = (_ty == _CHAR) ? 1: 1))
    _ty=$_INT
  elif [ $_tk = $_Id ] ; then
    d=$_id
    _next __
    if [ $_tk = $__LPAREN__ ] ; then
      _next __
      t=0
      while [ $_tk != $__RPAREN__ ]; do
        _expr __ $_Assign
        : $((_$((_e += 1)) = _PSH))
        : $((t += 1))
        if [ $_tk = $__COMMA__ ] ; then
          _next __
        fi
      done
      _next __
      if [ $((_$((d + _Class)))) = $_Sys ] ; then
        : $((_$((_e += 1)) = _$((d + _Val))))
      elif [ $((_$((d + _Class)))) = $_Fun ] ; then
        : $((_$((_e += 1)) = _JSR))
        : $((_$((_e += 1)) = _$((d + _Val))))
      else
        printf "%d: bad function call\n" $_line
        exit -1
      fi
      if [ $t != 0 ] ; then
        : $((_$((_e += 1)) = _ADJ))
        : $((_$((_e += 1)) = t))
      fi
      _ty=$((_$((d + _Type))))
    elif [ $((_$((d + _Class)))) = $_Num ] ; then
      : $((_$((_e += 1)) = _IMM))
      : $((_$((_e += 1)) = _$((d + _Val))))
      _ty=$_INT
    else
      if [ $((_$((d + _Class)))) = $_Loc ] ; then
        : $((_$((_e += 1)) = _LEA))
        : $((_$((_e += 1)) = _loc - _$((d + _Val))))
      elif [ $((_$((d + _Class)))) = $_Glo ] ; then
        : $((_$((_e += 1)) = _IMM))
        : $((_$((_e += 1)) = _$((d + _Val))))
      else
        printf "%d: undefined variable\n" $_line
        exit -1
      fi
      : $((_$((_e += 1)) = ((_ty = _$((d + _Type))) == _CHAR) ? _LC: _LI))
    fi
  elif [ $_tk = $__LPAREN__ ] ; then
    _next __
    if [ $_tk = $_Int ] || [ $_tk = $_Char ] ; then
      t=$(((_tk == _Int) ? _INT: _CHAR))
      _next __
      while [ $_tk = $_Mul ]; do
        _next __
        t=$((t + _PTR))
      done
      if [ $_tk = $__RPAREN__ ] ; then
        _next __
      else
        printf "%d: bad cast\n" $_line
        exit -1
      fi
      _expr __ $_Inc
      _ty=$t
    else
      _expr __ $_Assign
      if [ $_tk = $__RPAREN__ ] ; then
        _next __
      else
        printf "%d: close paren expected\n" $_line
        exit -1
      fi
    fi
  elif [ $_tk = $_Mul ] ; then
    _next __
    _expr __ $_Inc
    if [ $_ty -gt $_INT ] ; then
      _ty=$((_ty - _PTR))
    else
      printf "%d: bad dereference\n" $_line
      exit -1
    fi
    : $((_$((_e += 1)) = (_ty == _CHAR) ? _LC: _LI))
  elif [ $_tk = $_And ] ; then
    _next __
    _expr __ $_Inc
    if [ $((_$_e)) = $_LC ] || [ $((_$_e)) = $_LI ] ; then
      : $((_e -= 1))
    else
      printf "%d: bad address-of\n" $_line
      exit -1
    fi
    _ty=$((_ty + _PTR))
  elif [ $_tk = $__EXCL__ ] ; then
    _next __
    _expr __ $_Inc
    : $((_$((_e += 1)) = _PSH))
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = 0))
    : $((_$((_e += 1)) = _EQ))
    _ty=$_INT
  elif [ $_tk = $__TILDE__ ] ; then
    _next __
    _expr __ $_Inc
    : $((_$((_e += 1)) = _PSH))
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = -1))
    : $((_$((_e += 1)) = _XOR))
    _ty=$_INT
  elif [ $_tk = $_Add ] ; then
    _next __
    _expr __ $_Inc
    _ty=$_INT
  elif [ $_tk = $_Sub ] ; then
    _next __
    : $((_$((_e += 1)) = _IMM))
    if [ $_tk = $_Num ] ; then
      : $((_$((_e += 1)) = - _ival))
      _next __
    else
      : $((_$((_e += 1)) = -1))
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Inc
      : $((_$((_e += 1)) = _MUL))
    fi
    _ty=$_INT
  elif [ $_tk = $_Inc ] || [ $_tk = $_Dec ] ; then
    t=$_tk
    _next __
    _expr __ $_Inc
    if [ $((_$_e)) = $_LC ] ; then
      : $((_$_e = _PSH))
      : $((_$((_e += 1)) = _LC))
    elif [ $((_$_e)) = $_LI ] ; then
      : $((_$_e = _PSH))
      : $((_$((_e += 1)) = _LI))
    else
      printf "%d: bad lvalue in pre-increment\n" $_line
      exit -1
    fi
    : $((_$((_e += 1)) = _PSH))
    : $((_$((_e += 1)) = _IMM))
    : $((_$((_e += 1)) = (_ty > _PTR) ? 1: 1))
    : $((_$((_e += 1)) = (t == _Inc) ? _ADD: _SUB))
    : $((_$((_e += 1)) = (_ty == _CHAR) ? _SC: _SI))
  else
    printf "%d: bad expression\n" $_line
    exit -1
  fi
  while [ $_tk -ge $lev ]; do
    t=$_ty
    if [ $_tk = $_Assign ] ; then
      _next __
      if [ $((_$_e)) = $_LC ] || [ $((_$_e)) = $_LI ] ; then
        : $((_$_e = _PSH))
      else
        printf "%d: bad lvalue in assignment\n" $_line
        exit -1
      fi
      _expr __ $_Assign
      : $((_$((_e += 1)) = ((_ty = t) == _CHAR) ? _SC: _SI))
    elif [ $_tk = $_Cond ] ; then
      _next __
      : $((_$((_e += 1)) = _BZ))
      d=$((_e += 1))
      _expr __ $_Assign
      if [ $_tk = $__COLON__ ] ; then
        _next __
      else
        printf "%d: conditional missing colon\n" $_line
        exit -1
      fi
      : $((_$d = (_e + 3)))
      : $((_$((_e += 1)) = _JMP))
      d=$((_e += 1))
      _expr __ $_Cond
      : $((_$d = (_e + 1)))
    elif [ $_tk = $_Lor ] ; then
      _next __
      : $((_$((_e += 1)) = _BNZ))
      d=$((_e += 1))
      _expr __ $_Lan
      : $((_$d = (_e + 1)))
      _ty=$_INT
    elif [ $_tk = $_Lan ] ; then
      _next __
      : $((_$((_e += 1)) = _BZ))
      d=$((_e += 1))
      _expr __ $_Or
      : $((_$d = (_e + 1)))
      _ty=$_INT
    elif [ $_tk = $_Or ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Xor
      : $((_$((_e += 1)) = _OR))
      _ty=$_INT
    elif [ $_tk = $_Xor ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_And
      : $((_$((_e += 1)) = _XOR))
      _ty=$_INT
    elif [ $_tk = $_And ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Eq
      : $((_$((_e += 1)) = _AND))
      _ty=$_INT
    elif [ $_tk = $_Eq ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Lt
      : $((_$((_e += 1)) = _EQ))
      _ty=$_INT
    elif [ $_tk = $_Ne ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Lt
      : $((_$((_e += 1)) = _NE))
      _ty=$_INT
    elif [ $_tk = $_Lt ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Shl
      : $((_$((_e += 1)) = _LT))
      _ty=$_INT
    elif [ $_tk = $_Gt ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Shl
      : $((_$((_e += 1)) = _GT))
      _ty=$_INT
    elif [ $_tk = $_Le ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Shl
      : $((_$((_e += 1)) = _LE))
      _ty=$_INT
    elif [ $_tk = $_Ge ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Shl
      : $((_$((_e += 1)) = _GE))
      _ty=$_INT
    elif [ $_tk = $_Shl ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Add
      : $((_$((_e += 1)) = _SHL))
      _ty=$_INT
    elif [ $_tk = $_Shr ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Add
      : $((_$((_e += 1)) = _SHR))
      _ty=$_INT
    elif [ $_tk = $_Add ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Mul
      if [ $((_ty = t)) -gt $_PTR ] ; then
        : $((_$((_e += 1)) = _PSH))
        : $((_$((_e += 1)) = _IMM))
        : $((_$((_e += 1)) = 1))
        : $((_$((_e += 1)) = _MUL))
      fi
      : $((_$((_e += 1)) = _ADD))
    elif [ $_tk = $_Sub ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Mul
      if [ $t -gt $_PTR ] && [ $t = $_ty ] ; then
        : $((_$((_e += 1)) = _SUB))
        : $((_$((_e += 1)) = _PSH))
        : $((_$((_e += 1)) = _IMM))
        : $((_$((_e += 1)) = 1))
        : $((_$((_e += 1)) = _DIV))
        _ty=$_INT
      elif [ $((_ty = t)) -gt $_PTR ] ; then
        : $((_$((_e += 1)) = _PSH))
        : $((_$((_e += 1)) = _IMM))
        : $((_$((_e += 1)) = 1))
        : $((_$((_e += 1)) = _MUL))
        : $((_$((_e += 1)) = _SUB))
      else
        : $((_$((_e += 1)) = _SUB))
      fi
    elif [ $_tk = $_Mul ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Inc
      : $((_$((_e += 1)) = _MUL))
      _ty=$_INT
    elif [ $_tk = $_Div ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Inc
      : $((_$((_e += 1)) = _DIV))
      _ty=$_INT
    elif [ $_tk = $_Mod ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Inc
      : $((_$((_e += 1)) = _MOD))
      _ty=$_INT
    elif [ $_tk = $_Inc ] || [ $_tk = $_Dec ] ; then
      if [ $((_$_e)) = $_LC ] ; then
        : $((_$_e = _PSH))
        : $((_$((_e += 1)) = _LC))
      elif [ $((_$_e)) = $_LI ] ; then
        : $((_$_e = _PSH))
        : $((_$((_e += 1)) = _LI))
      else
        printf "%d: bad lvalue in post-increment\n" $_line
        exit -1
      fi
      : $((_$((_e += 1)) = _PSH))
      : $((_$((_e += 1)) = _IMM))
      : $((_$((_e += 1)) = (_ty > _PTR) ? 1: 1))
      : $((_$((_e += 1)) = (_tk == _Inc) ? _ADD: _SUB))
      : $((_$((_e += 1)) = (_ty == _CHAR) ? _SC: _SI))
      : $((_$((_e += 1)) = _PSH))
      : $((_$((_e += 1)) = _IMM))
      : $((_$((_e += 1)) = (_ty > _PTR) ? 1: 1))
      : $((_$((_e += 1)) = (_tk == _Inc) ? _SUB: _ADD))
      _next __
    elif [ $_tk = $_Brak ] ; then
      _next __
      : $((_$((_e += 1)) = _PSH))
      _expr __ $_Assign
      if [ $_tk = $__RBRACK__ ] ; then
        _next __
      else
        printf "%d: close bracket expected\n" $_line
        exit -1
      fi
      if [ $t -gt $_PTR ] ; then
        : $((_$((_e += 1)) = _PSH))
        : $((_$((_e += 1)) = _IMM))
        : $((_$((_e += 1)) = 1))
        : $((_$((_e += 1)) = _MUL))
      elif [ $t -lt $_PTR ] ; then
        printf "%d: pointer type expected\n" $_line
        exit -1
      fi
      : $((_$((_e += 1)) = _ADD))
      : $((_$((_e += 1)) = ((_ty = t - _PTR) == _CHAR) ? _LC: _LI))
    else
      printf "%d: compiler error tk=%d\n" $_line $_tk
      exit -1
    fi
  done
  : $((__tmp = $1)) $((lev = $3)) $((t = $4)) $((d = $5)) $(($1 = __tmp))
}

: $((b = a = 0))
_stmt() {
  set $@ $a $b
  if [ $_tk = $_If ] ; then
    _next __
    if [ $_tk = $__LPAREN__ ] ; then
      _next __
    else
      printf "%d: open paren expected\n" $_line
      exit -1
    fi
    _expr __ $_Assign
    if [ $_tk = $__RPAREN__ ] ; then
      _next __
    else
      printf "%d: close paren expected\n" $_line
      exit -1
    fi
    : $((_$((_e += 1)) = _BZ))
    b=$((_e += 1))
    _stmt __
    if [ $_tk = $_Else ] ; then
      : $((_$b = (_e + 3)))
      : $((_$((_e += 1)) = _JMP))
      b=$((_e += 1))
      _next __
      _stmt __
    fi
    : $((_$b = (_e + 1)))
  elif [ $_tk = $_While ] ; then
    _next __
    a=$((_e + 1))
    if [ $_tk = $__LPAREN__ ] ; then
      _next __
    else
      printf "%d: open paren expected\n" $_line
      exit -1
    fi
    _expr __ $_Assign
    if [ $_tk = $__RPAREN__ ] ; then
      _next __
    else
      printf "%d: close paren expected\n" $_line
      exit -1
    fi
    : $((_$((_e += 1)) = _BZ))
    b=$((_e += 1))
    _stmt __
    : $((_$((_e += 1)) = _JMP))
    : $((_$((_e += 1)) = a))
    : $((_$b = (_e + 1)))
  elif [ $_tk = $_Return ] ; then
    _next __
    if [ $_tk != $__SEMICOLON__ ] ; then
      _expr __ $_Assign
    fi
    : $((_$((_e += 1)) = _LEV))
    if [ $_tk = $__SEMICOLON__ ] ; then
      _next __
    else
      printf "%d: semicolon expected\n" $_line
      exit -1
    fi
  elif [ $_tk = $__LBRACE__ ] ; then
    _next __
    while [ $_tk != $__RBRACE__ ]; do
      _stmt __
    done
    _next __
  elif [ $_tk = $__SEMICOLON__ ] ; then
    _next __
  else
    _expr __ $_Assign
    if [ $_tk = $__SEMICOLON__ ] ; then
      _next __
    else
      printf "%d: semicolon expected\n" $_line
      exit -1
    fi
  fi
  : $((__tmp = $1)) $((a = $2)) $((b = $3)) $(($1 = __tmp))
}

: $((__t1 = t = i = cycle = a = bp = sp = pc = idmain = poolsz = ty = bt = fd = argv_ = argc = 0))
_main() { # argc: $2, argv: $3
  set $@ $argc $argv_ $fd $bt $ty $poolsz $idmain $pc $sp $bp $a $cycle $i $t $__t1
  argc=$2
  argv_=$3
  : $((argc -= 1))
  : $((argv_ += 1))
  if [ $argc -gt 0 ] && [ $((_$((_$argv_)))) = $__MINUS__ ] && [ $((_$((_$argv_ + 1)))) = $__s__ ] ; then
    _src=1
    : $((argc -= 1))
    : $((argv_ += 1))
  fi
  if [ $argc -gt 0 ] && [ $((_$((_$argv_)))) = $__MINUS__ ] && [ $((_$((_$argv_ + 1)))) = $__d__ ] ; then
    _debug=1
    : $((argc -= 1))
    : $((argv_ += 1))
  fi
  if [ $argc -lt 1 ] ; then
    printf "usage: c4 [-s] [-d] file ...\n"
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if _open fd $((_$argv_)) 0; [ $fd -lt 0 ] ; then
    printf "could not open("
    _put_pstr __ $((_$argv_))
    printf ")\n"
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  poolsz=$((256 * 1024))
  if _malloc _sym $poolsz; [ $((! _sym)) != 0 ] ; then
    printf "could not malloc(%d) symbol area\n" $poolsz
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if _malloc _e $poolsz; [ $((!(_le = _e))) != 0 ] ; then
    printf "could not malloc(%d) text area\n" $poolsz
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if _malloc _data $poolsz; [ $((! _data)) != 0 ] ; then
    printf "could not malloc(%d) data area\n" $poolsz
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if _malloc sp $poolsz; [ $((! sp)) != 0 ] ; then
    printf "could not malloc(%d) stack area\n" $poolsz
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  defstr __str_1 "char else enum if int return sizeof while open read close printf malloc free memset memcmp exit void main"
  _p=$__str_1
  i=$_Char
  while [ $i -le $_While ]; do
    _next __
    : $((_$((_id + _Tk)) = (i += 1) - 1))
  done
  i=$_OPEN
  while [ $i -le $_EXIT ]; do
    _next __
    : $((_$((_id + _Class)) = _Sys))
    : $((_$((_id + _Type)) = _INT))
    : $((_$((_id + _Val)) = (i += 1) - 1))
  done
  _next __
  : $((_$((_id + _Tk)) = _Char))
  _next __
  idmain=$_id
  if _malloc _p $poolsz; [ $((!(_lp = _p))) != 0 ] ; then
    printf "could not malloc(%d) source area\n" $poolsz
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if _read i $fd $_p $((poolsz - 1)); [ $i -le 0 ] ; then
    printf "read() returned %d\n" $i
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  : $((_$((_p + i)) = 0))
  _close __ $fd
  _line=1
  _next __
  while [ $_tk != 0 ]; do
    bt=$_INT
    if [ $_tk = $_Int ] ; then
      _next __
    elif [ $_tk = $_Char ] ; then
      _next __
      bt=$_CHAR
    elif [ $_tk = $_Enum ] ; then
      _next __
      if [ $_tk != $__LBRACE__ ] ; then
        _next __
      fi
      if [ $_tk = $__LBRACE__ ] ; then
        _next __
        i=0
        while [ $_tk != $__RBRACE__ ]; do
          if [ $_tk != $_Id ] ; then
            printf "%d: bad enum identifier %d\n" $_line $_tk
            : $(($1 = -1))
            : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
            return
          fi
          _next __
          if [ $_tk = $_Assign ] ; then
            _next __
            if [ $_tk != $_Num ] ; then
              printf "%d: bad enum initializer\n" $_line
              : $(($1 = -1))
              : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
              return
            fi
            i=$_ival
            _next __
          fi
          : $((_$((_id + _Class)) = _Num))
          : $((_$((_id + _Type)) = _INT))
          : $((_$((_id + _Val)) = (i += 1) - 1))
          if [ $_tk = $__COMMA__ ] ; then
            _next __
          fi
        done
        _next __
      fi
    fi
    while [ $_tk != $__SEMICOLON__ ] && [ $_tk != $__RBRACE__ ]; do
      ty=$bt
      while [ $_tk = $_Mul ]; do
        _next __
        ty=$((ty + _PTR))
      done
      if [ $_tk != $_Id ] ; then
        printf "%d: bad global declaration\n" $_line
        : $(($1 = -1))
        : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
        return
      fi
      if [ $((_$((_id + _Class)))) != 0 ] ; then
        printf "%d: duplicate global definition\n" $_line
        : $(($1 = -1))
        : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
        return
      fi
      _next __
      : $((_$((_id + _Type)) = ty))
      if [ $_tk = $__LPAREN__ ] ; then
        : $((_$((_id + _Class)) = _Fun))
        : $((_$((_id + _Val)) = (_e + 1)))
        _next __
        i=0
        while [ $_tk != $__RPAREN__ ]; do
          ty=$_INT
          if [ $_tk = $_Int ] ; then
            _next __
          elif [ $_tk = $_Char ] ; then
            _next __
            ty=$_CHAR
          fi
          while [ $_tk = $_Mul ]; do
            _next __
            ty=$((ty + _PTR))
          done
          if [ $_tk != $_Id ] ; then
            printf "%d: bad parameter declaration\n" $_line
            : $(($1 = -1))
            : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
            return
          fi
          if [ $((_$((_id + _Class)))) = $_Loc ] ; then
            printf "%d: duplicate parameter definition\n" $_line
            : $(($1 = -1))
            : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
            return
          fi
          : $((_$((_id + _HClass)) = _$((_id + _Class))))
          : $((_$((_id + _Class)) = _Loc))
          : $((_$((_id + _HType)) = _$((_id + _Type))))
          : $((_$((_id + _Type)) = ty))
          : $((_$((_id + _HVal)) = _$((_id + _Val))))
          : $((_$((_id + _Val)) = (i += 1) - 1))
          _next __
          if [ $_tk = $__COMMA__ ] ; then
            _next __
          fi
        done
        _next __
        if [ $_tk != $__LBRACE__ ] ; then
          printf "%d: bad function definition\n" $_line
          : $(($1 = -1))
          : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
          return
        fi
        _loc=$((i += 1))
        _next __
        while [ $_tk = $_Int ] || [ $_tk = $_Char ]; do
          bt=$(((_tk == _Int) ? _INT: _CHAR))
          _next __
          while [ $_tk != $__SEMICOLON__ ]; do
            ty=$bt
            while [ $_tk = $_Mul ]; do
              _next __
              ty=$((ty + _PTR))
            done
            if [ $_tk != $_Id ] ; then
              printf "%d: bad local declaration\n" $_line
              : $(($1 = -1))
              : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
              return
            fi
            if [ $((_$((_id + _Class)))) = $_Loc ] ; then
              printf "%d: duplicate local definition\n" $_line
              : $(($1 = -1))
              : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
              return
            fi
            : $((_$((_id + _HClass)) = _$((_id + _Class))))
            : $((_$((_id + _Class)) = _Loc))
            : $((_$((_id + _HType)) = _$((_id + _Type))))
            : $((_$((_id + _Type)) = ty))
            : $((_$((_id + _HVal)) = _$((_id + _Val))))
            : $((_$((_id + _Val)) = i += 1))
            _next __
            if [ $_tk = $__COMMA__ ] ; then
              _next __
            fi
          done
          _next __
        done
        : $((_$((_e += 1)) = _ENT))
        : $((_$((_e += 1)) = i - _loc))
        while [ $_tk != $__RBRACE__ ]; do
          _stmt __
        done
        : $((_$((_e += 1)) = _LEV))
        _id=$_sym
        while [ $((_$((_id + _Tk)))) != 0 ]; do
          if [ $((_$((_id + _Class)))) = $_Loc ] ; then
            : $((_$((_id + _Class)) = _$((_id + _HClass))))
            : $((_$((_id + _Type)) = _$((_id + _HType))))
            : $((_$((_id + _Val)) = _$((_id + _HVal))))
          fi
          _id=$((_id + _Idsz))
        done
      else
        : $((_$((_id + _Class)) = _Glo))
        : $((_$((_id + _Val)) = _data))
        _data=$((_data + 1))
      fi
      if [ $_tk = $__COMMA__ ] ; then
        _next __
      fi
    done
    _next __
  done
  if [ $((!(pc = _$((idmain + _Val))))) != 0 ] ; then
    printf "main() not defined\n"
    : $(($1 = -1))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  if [ $_src != 0 ] ; then
    : $(($1 = 0))
    : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
    return
  fi
  : $((bp = sp = (sp + poolsz)))
  : $((_$((sp -= 1)) = _EXIT))
  : $((_$((sp -= 1)) = _PSH))
  t=$sp
  : $((_$((sp -= 1)) = argc))
  : $((_$((sp -= 1)) = argv_))
  : $((_$((sp -= 1)) = t))
  cycle=0
  while [ 1 != 0 ]; do
    i=$((_$(((pc += 1) - 1))))
    : $((cycle += 1))
    if [ $_debug != 0 ] ; then
      defstr __str_2 "LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"
      printf "%d> %.4s" $cycle "$(_put_pstr __ $((__str_2 + i * 5)))"
      if [ $i -le $_ADJ ] ; then
        printf " %d\n" $((_$pc))
      else
        printf "\n"
      fi
    fi
    if [ $i = $_LEA ] ; then
      a=$((bp + _$(((pc += 1) - 1))))
    elif [ $i = $_IMM ] ; then
      a=$((_$(((pc += 1) - 1))))
    elif [ $i = $_JMP ] ; then
      pc=$((_$pc))
    elif [ $i = $_JSR ] ; then
      : $((_$((sp -= 1)) = (pc + 1)))
      pc=$((_$pc))
    elif [ $i = $_BZ ] ; then
      pc=$((a ? (pc + 1): _$pc))
    elif [ $i = $_BNZ ] ; then
      pc=$((a ? _$pc: (pc + 1)))
    elif [ $i = $_ENT ] ; then
      : $((_$((sp -= 1)) = bp))
      bp=$sp
      sp=$((sp - _$(((pc += 1) - 1))))
    elif [ $i = $_ADJ ] ; then
      sp=$((sp + _$(((pc += 1) - 1))))
    elif [ $i = $_LEV ] ; then
      sp=$bp
      bp=$((_$(((sp += 1) - 1))))
      pc=$((_$(((sp += 1) - 1))))
    elif [ $i = $_LI ] ; then
      a=$((_$a))
    elif [ $i = $_LC ] ; then
      a=$((_$a))
    elif [ $i = $_SI ] ; then
      : $((_$((_$(((sp += 1) - 1)))) = a))
    elif [ $i = $_SC ] ; then
      : $((a = _$((_$(((sp += 1) - 1)))) = a))
    elif [ $i = $_PSH ] ; then
      : $((_$((sp -= 1)) = a))
    elif [ $i = $_OR ] ; then
      a=$((_$(((sp += 1) - 1)) | a))
    elif [ $i = $_XOR ] ; then
      a=$((_$(((sp += 1) - 1)) ^ a))
    elif [ $i = $_AND ] ; then
      a=$((_$(((sp += 1) - 1)) & a))
    elif [ $i = $_EQ ] ; then
      a=$((_$(((sp += 1) - 1)) == a))
    elif [ $i = $_NE ] ; then
      a=$((_$(((sp += 1) - 1)) != a))
    elif [ $i = $_LT ] ; then
      a=$((_$(((sp += 1) - 1)) < a))
    elif [ $i = $_GT ] ; then
      a=$((_$(((sp += 1) - 1)) > a))
    elif [ $i = $_LE ] ; then
      a=$((_$(((sp += 1) - 1)) <= a))
    elif [ $i = $_GE ] ; then
      a=$((_$(((sp += 1) - 1)) >= a))
    elif [ $i = $_SHL ] ; then
      a=$((_$(((sp += 1) - 1)) << a))
    elif [ $i = $_SHR ] ; then
      a=$((_$(((sp += 1) - 1)) >> a))
    elif [ $i = $_ADD ] ; then
      a=$((_$(((sp += 1) - 1)) + a))
    elif [ $i = $_SUB ] ; then
      a=$((_$(((sp += 1) - 1)) - a))
    elif [ $i = $_MUL ] ; then
      a=$((_$(((sp += 1) - 1)) * a))
    elif [ $i = $_DIV ] ; then
      a=$((_$(((sp += 1) - 1)) / a))
    elif [ $i = $_MOD ] ; then
      a=$((_$(((sp += 1) - 1)) % a))
    elif [ $i = $_OPEN ] ; then
      _open a $((_$((sp + 1)))) $((_$sp))
    elif [ $i = $_READ ] ; then
      _read a $((_$((sp + 2)))) $((_$((sp + 1)))) $((_$sp))
    elif [ $i = $_CLOS ] ; then
      _close a $((_$sp))
    elif [ $i = $_PRTF ] ; then
      t=$((sp + _$((pc + 1))))
      _printf a $((_$((t + -1)))) $((_$((t + -2)))) $((_$((t + -3)))) $((_$((t + -4)))) $((_$((t + -5)))) $((_$((t + -6))))
    elif [ $i = $_MALC ] ; then
      _malloc __t1 $((_$sp))
      a=$__t1
    elif [ $i = $_FREE ] ; then
      _free __ $((_$sp))
    elif [ $i = $_MSET ] ; then
      _memset __t1 $((_$((sp + 2)))) $((_$((sp + 1)))) $((_$sp))
      a=$__t1
    elif [ $i = $_MCMP ] ; then
      _memcmp a $((_$((sp + 2)))) $((_$((sp + 1)))) $((_$sp))
    elif [ $i = $_EXIT ] ; then
      printf "exit(%d) cycle = %d\n" $((_$sp)) $cycle
      : $(($1 = _$sp))
      break
    else
      printf "unknown instruction = %d! cycle = %d\n" $i $cycle
      : $(($1 = -1))
      break
    fi
  done
  : $((__tmp = $1)) $((argc = $4)) $((argv_ = $5)) $((fd = $6)) $((bt = $7)) $((ty = $8)) $((poolsz = $9)) $((idmain = ${10})) $((pc = ${11})) $((sp = ${12})) $((bp = ${13})) $((a = ${14})) $((cycle = ${15})) $((i = ${16})) $((t = ${17})) $((__t1 = ${18})) $(($1 = __tmp))
}

# Character constants
readonly __LF__=10
readonly __EXCL__=33
readonly __DQUOTE__=34
readonly __HASH__=35
readonly __PERCENT__=37
readonly __AMP__=38
readonly __QUOTE__=39
readonly __LPAREN__=40
readonly __RPAREN__=41
readonly __STAR__=42
readonly __PLUS__=43
readonly __COMMA__=44
readonly __MINUS__=45
readonly __SLASH__=47
readonly __0__=48
readonly __7__=55
readonly __9__=57
readonly __COLON__=58
readonly __SEMICOLON__=59
readonly __LT__=60
readonly __EQ__=61
readonly __GT__=62
readonly __QUESTION__=63
readonly __A__=65
readonly __F__=70
readonly __X__=88
readonly __Z__=90
readonly __LBRACK__=91
readonly __BACKSLASH__=92
readonly __RBRACK__=93
readonly __CARET__=94
readonly __UNDERSCORE__=95
readonly __a__=97
readonly __d__=100
readonly __f__=102
readonly __n__=110
readonly __s__=115
readonly __x__=120
readonly __z__=122
readonly __LBRACE__=123
readonly __BAR__=124
readonly __RBRACE__=125
readonly __TILDE__=126
# Runtime library
__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $(($1 = $__ALLOC + 1))
  : $((__ALLOC += $2 + 1))
}

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

next_sub_buffer() {
  if [ -z "$__us_buf256" ]; then
    if [ ${#__str} -ge 256 ]; then
      __temp="${__str#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????}"
      __us_buf256="${__str%"$__temp"}"
      __str="$__temp"
    else
      __us_buf256="$__str"
      __str=
    fi
  fi
  if [ -z "$__us_buf16" ]; then
    if [ ${#__us_buf256} -ge 16 ]; then
      __temp="${__us_buf256#????????????????}"
      __us_buf16="${__us_buf256%"$__temp"}"
      __us_buf256="$__temp"
    else
      __us_buf16="$__us_buf256"
      __us_buf256=
    fi
  fi
}
unpack_escaped_string() { # $1 = string, $2 = size (optional)
  __str="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${2:-${#__str} + 1}))
  __ptr=$__addr
  __end=$((__ptr + ${2:-${#__str} + 1})) # End of allocated memory
  __us_buf16=
  __us_buf256=
  while [ ! -z "$__str" ] || [ ! -z "$__us_buf256" ] ; do
    next_sub_buffer
    while [ ! -z "$__us_buf16" ]; do
      case "$__us_buf16" in
        '\'*)
          __us_buf16="${__us_buf16#?}" # Remove the current char from $__us_buf16
          if [ -z "$__us_buf16" ]; then next_sub_buffer; fi
          case "$__us_buf16" in
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
            *) echo "invalid escape in string: $__us_buf16"; exit 1 ;;
          esac
          __us_buf16="${__us_buf16#?}" # Remove the current char from $__us_buf16
          ;;
        *)
          char_to_int "${__us_buf16%"${__us_buf16#?}"}"
          __us_buf16="${__us_buf16#?}" # Remove the current char from $__us_buf16
          ;;
      esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
    done
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
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2" $3
    : $(($1 = __addr))
  fi
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

# Unpack a Shell string into an appropriately sized buffer
unpack_string() { # $1: Shell string, $2: Buffer, $3: Ends with EOF?
  __fgetc_buf=$1
  __buffer=$2
  __ends_with_eof=$3
  __fgetc_buf16=
  __stdin_buf256=
  __continue=1
  while [ $__continue != 0 ] ; do
    if [ -z "$__stdin_buf256" ]; then
      if [ ${#__fgetc_buf} -ge 256 ]; then
        __temp="${__fgetc_buf#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????}"
        __stdin_buf256="${__fgetc_buf%"$__temp"}"
        __fgetc_buf="$__temp"
      else
        __stdin_buf256="$__fgetc_buf"
        __fgetc_buf=
      fi
    fi
    if [ -z "$__fgetc_buf16" ]; then
      if [ ${#__stdin_buf256} -ge 16 ]; then
        __temp="${__stdin_buf256#????????????????}"
        __fgetc_buf16="${__stdin_buf256%"$__temp"}"
        __stdin_buf256="$__temp"
      else
        __fgetc_buf16="$__stdin_buf256"
        __stdin_buf256=
        __continue=0
      fi
    fi
    while [ ! -z "$__fgetc_buf16" ]; do
      case "$__fgetc_buf16" in
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
          char_to_int "${__fgetc_buf16%"${__fgetc_buf16#?}"}"
          : $((_$__buffer = __c))
          ;;
      esac
      __fgetc_buf16=${__fgetc_buf16#?}  # Remove the first character
      : $((__buffer += 1))              # Move to the next buffer position
    done
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

_read() { : $((__fd = $2)) $((__buf = $3)) $((__count = $4))
  : $((__i = 0))
  while [ $__i -lt $__count ] ; do
    read_byte __byte $__fd
    if [ $__byte -lt 0 ] ; then break; fi
    : $((_$((__buf + __i)) = __byte))
    : $((__i += 1))
  done
  : $(($1 = __i))
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

read_int() {
  __int=
  while [ $((__c = _$__fmt_ptr)) != 0 ] && [ $__c -ge 48 ] && [ $__c -le 57 ]; do
    __int="$__int$((__c - 48))"
    : $((__fmt_ptr += 1))
  done
}

pad() {
  if [ $(($1 - 1)) -ge 1 ]; then
    printf "%$(($1 - 1))s" ""
  fi
}

printf_invalid_format_error() {
  printf "Invalid format specifier. %%"
  : $((_$__fmt_ptr = 0)) # Terminate string after %...
  _put_pstr __ $__mod_start
  printf "\n"
  exit 1
}

printf_reset() {
  __mod=0
  __flags=
  __width=
  __precision=
}

_printf() { # $1 = printf format string, $2... = printf args
  : $(($1 = 0)); shift # Return 0
  __fmt_ptr=$1; shift
  __mod_start=0
  printf_reset
  while [ "$((__head = _$__fmt_ptr))" != 0 ] ; do
    __fmt_ptr=$((__fmt_ptr + 1))
    if [ $__mod -eq 1 ] ; then
      __char=$(printf "\\$(($__head/64))$(($__head/8%8))$(($__head%8))")
      __head_char=$__char
      case $__head_char in
        '%')
          if [ -n "${__flags}${__width}${__precision}" ]; then printf_invalid_format_error; fi
          printf "%%"
          printf_reset
          ;;
        'd'|'i'|'o'|'u'|'x'|'X')
          printf "%${__flags}${__width}${__precision}${__head_char}" $1
          shift
          printf_reset
          ;;
        'c')
          case "$__flags" in
            *'-'*)
              printf \\$(($1/64))$(($1/8%8))$(($1%8)); pad ${__width:-0} ;;
            *) pad ${__width:-0}; printf \\$(($1/64))$(($1/8%8))$(($1%8)) ;;
          esac
          shift
          printf_reset
          ;;
        's')
          # We only want to use the shell's native printf (and _put_pstr subshell) if %s has sub-specifiers
          if [ -z "{__flags}${__width}{__precision}" ]; then
            _put_pstr __ $1
          else
            printf "%${__flags}${__width}${__precision}s" "$(_put_pstr __ $1)"
          fi
          shift
          printf_reset
          ;;
        '-'|'+'|' '|'#'|'0')
          if [ -n "${__width}${__precision}" ]; then printf_invalid_format_error; fi
          __flags="$__flags$__head_char"
          ;;
        [0-9])
          if [ -n "${__width}${__precision}" ]; then printf_invalid_format_error; fi
          read_int
          __width="$__head_char$__int"
          ;;
        '*')
          if [ -n "${__width}${__precision}" ]; then printf_invalid_format_error; fi
          __width=$1
          shift
          ;;
        '.')
          __head=$((_$__fmt_ptr))
          if [ $__head = 42 ]; then # 42 = '*'
            __fmt_ptr=$((__fmt_ptr + 1))
            __precision=".$1"
            shift
          elif [ $__head -ge 48 ] && [ $__head -le 57 ]; then
            read_int
            __precision=".$__int"
          else
            printf_invalid_format_error
          fi
          ;;
        *)
          echo "4: Unknown format specifier %$__head_char"; exit 1
      esac
    elif [ $__head = 37 ]; then # 37 == '%'
      __mod=1; __mod_start=$__fmt_ptr
    else
      printf \\$(($__head/64))$(($__head/8%8))$(($__head%8))
    fi
  done
}

__code=0; # Exit code
make_argv $(($# + 1)) "$0" "$@" # Setup argc/argv
_main __code $(($# + 1)) $__argv
exit $__code

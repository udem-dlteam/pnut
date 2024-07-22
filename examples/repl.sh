set -e -u

__ALLOC=1 # Starting heap at 1 because 0 is the null pointer.

_malloc() { # $2 = object size
  : $((_$__ALLOC = $2)) # Track object size
  : $((__ALLOC += 1))
  : $(($1 = $__ALLOC))
  : $((__ALLOC += $2))
}

char_to_int() {
  case $1 in
    [0-9]) __c=$((48 + $1)) ;;
    'a') __c=97 ;;
    'b') __c=98 ;;
    'c') __c=99 ;;
    'd') __c=100 ;;
    'e') __c=101 ;;
    'f') __c=102 ;;
    'g') __c=103 ;;
    'h') __c=104 ;;
    'i') __c=105 ;;
    'j') __c=106 ;;
    'k') __c=107 ;;
    'l') __c=108 ;;
    'm') __c=109 ;;
    'n') __c=110 ;;
    'o') __c=111 ;;
    'p') __c=112 ;;
    'q') __c=113 ;;
    'r') __c=114 ;;
    's') __c=115 ;;
    't') __c=116 ;;
    'u') __c=117 ;;
    'v') __c=118 ;;
    'w') __c=119 ;;
    'x') __c=120 ;;
    'y') __c=121 ;;
    'z') __c=122 ;;
    'A') __c=65 ;;
    'B') __c=66 ;;
    'C') __c=67 ;;
    'D') __c=68 ;;
    'E') __c=69 ;;
    'F') __c=70 ;;
    'G') __c=71 ;;
    'H') __c=72 ;;
    'I') __c=73 ;;
    'J') __c=74 ;;
    'K') __c=75 ;;
    'L') __c=76 ;;
    'M') __c=77 ;;
    'N') __c=78 ;;
    'O') __c=79 ;;
    'P') __c=80 ;;
    'Q') __c=81 ;;
    'R') __c=82 ;;
    'S') __c=83 ;;
    'T') __c=84 ;;
    'U') __c=85 ;;
    'V') __c=86 ;;
    'W') __c=87 ;;
    'X') __c=88 ;;
    'Y') __c=89 ;;
    'Z') __c=90 ;;
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
      __c=$(LC_CTYPE=C printf "%d" "'$1")
  esac
}

unpack_escaped_string() {
  __buf="$1"
  # Allocates enough space for all characters, assuming that no character is escaped
  _malloc __addr $((${#__buf} + 1))
  __ptr=$__addr
  while [ -n "$__buf" ] ; do
    case "$__buf" in
      '\'*)
        __buf="${__buf#?}"               # remove the current char from $__buf
        case "$__buf" in
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
          *) echo "invalid escape in string: $__char"; exit 1 ;;
        esac
        __buf="${__buf#?}"               # remove the current char from $__buf
        ;;
      *)
        char_to_int "${__buf%"${__buf#?}"}"
        __buf="${__buf#?}"                  # remove the current char from $__buf
        ;;
    esac
    : $((_$__ptr = __c))
    : $((__ptr += 1))
  done
  : $((_$__ptr = 0))
}

# Define a string, and return a reference to it in the varible taken as argument.
# If the variable is already defined, this function does nothing.
# Note that it's up to the caller to ensure that no 2 strings share the same variable.
defstr() { # $1 = variable name, $2 = string
  set +u # Necessary to allow the variable to be empty
  if [ $(($1)) -eq 0 ]; then
    unpack_escaped_string "$2"
    : $(($1 = __addr))
  fi
  set -u
}

defstr __str_0 "RD?naeloob,?xelpmoc,cc/llac,?citebahpla-rahc,<,dnuor,dna,etouq,gnirts,raaaac,radddc,raaddc,?=>ic-gnirts,esle,>,!rdc-tes,?qe,oludom,tsil>-rotcev,htgnel-gnirts,?tsil,fer-gnirts,-,nruter,esac,rddddc,certel,gnicilps-etouqnu,!tes,?rebmun,daol,rdadac,ro,rac,tneitouq,?orez,xam,/,?evitisop,?=>ic-rahc,ecaps,roolf,?tcaxe,?laer,?=>gnirts,rdaddc,raddac,adbmal,gniliec,rdaadc,elif-tuptuo-htiw-llac,fi,mcl,tropxe,gnirtsbus,!tes-gnirts,?evitagen,+,raaadc,!rac-tes,*,etouqnu,?=<ic-rahc,enifed,?=ic-gnirts,etouqisauq,elif-tupni-htiw-llac,?tcaxeni,rdc,!tes-rotcev,rddadc,rddaac,nigeb,radaac,rotcev-ekam,tel,etacnurt,rdaaac,?=gnirts,?=<rahc,htgnel-rotcev,=<,raadac,?lanoitar,_,?=<gnirts,fer-rotcev,=>,bat,?ddo,noitaunitnoc-tnerruc-htiw-llac,nim,?=>rahc,gnirts-ekam,?=<ic-gnirts,regetni>-rahc,?ciremun-rahc,radadc,dnoc,=,dneppa-gnirts,trop-tuptuo-esolc,cossa,dcg,qmem,?>ic-gnirts,hcae-rof,redniamer,?<ic-gnirts,?<gnirts,?=rahc,?=ic-rahc,gnirts>-lobmys,?>ic-rahc,?>gnirts,rebmem,?neve,vmem,?ecapsetihw-rahc,elif-tuptuo-nepo,lave,?>rahc,?esac-rewol-rahc,raadc,?esac-reppu-rahc,?trop-tuptuo,?trop-tupni,elif-tupni-nepo,trop-tupni-esolc,tsil>-gnirts,raaac,raddc,rdddac,?<ic-rahc,?<rahc,gnirts>-rebmun,rebmun>-gnirts,enilwen,raac,esacnwod-rahc,qssa,vssa,?regetni,?vqe,fer-tsil,trop-tupni-tnerruc,radac,dneppa,rotcev,?erudecorp,radc,rdddc,sba,trop-tuptuo-tnerruc,rotcev>-tsil,ylppa,esrever,lobmys>-gnirts,rdadc,rdaac,esacpu-rahc,rddac,rahc-etirw,?gnirts,gnirts>-tsil,?rotcev,etirw,?rahc,?tcejbo-foe,?lauqe,rahc-keep,rahc>-regetni,?lobmys,pam,htgnel,daer,,,,,ton,,,yalpsid,rdac,rddc,rahc-daer,tsil,,?llun,,,,?riap,,,snoc,,,,,,,,,;8V1k!T1)li%zAmk!TH7%lYAl_Im^[\$Kl7(lYAlbAmZJl^)li&AmZBlb~YHl^{i\$ZCl^{!V18V1kAmZ9kAmYJlZJl^99k~YHl^YAkAmPliW#y]J7\$kWmi&:nHniW%ai&kk{!@#niVK\`^}'!W%Rm:nkw)iW)l!U9)l_,mYU9ma?l_>l^~Fl^}'!UG)l^8UGnUmlb?l\`^)l\`~Bm_>l_~Fl_})!V)8;mVma_8;mVmaUm\`l~Z4l_cYUGnka_?lb>la1nYV)ofd?lbCmai\$>l\`^~Fl_}+!V##n:nckiVE#nQla~i\$#n:nckiVE#nQla~i\$#n:nckiVE#nQla~i\$#n:nckiVE#nQla~YU>lQla~Bmw*?la~BmiW)>la~YU>la_iVB}'!?#na_iW)#nk_iW)~BmiW%_}'!=1nb1nRm:nTng?lecw*iW)m~Fl?la>l\`^})!UI8=nebb1nYUIqRm:nh4w2iW)m1nYUIqh1~BmiW%h1Cmh0eh-?lf?ldCmci\$_\`>la>l_~Fl_}/!V?7&ml_Im^[\$Kl8U;nX,mb?l\`X+ma>l_wVL8U;nX,mUmlb?l\`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l\`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l\`Z0l_wVJ~Bml_~i\$8U;nX,mb?l\`X+ma>l_wVL8U;nX,mUmlb?l\`YUJlwT%wVL~BmwT%>l^8U;nX,mb?l\`YU;nX-mGmlcZ,laYUJlwTKwVLwVL8U;nX,mb?l\`Z0l_wVJ~Bml_~BmwTKZ8l^~Fl>l^8U;nX,mGmlb?l\`YUJlwT)wVL88l^~Bml_~BmwT)>l^8UJl^8U;mX+ma>l_wW'~YKl^~SlFl^}'i\${!UJ8U;m_wU1{!UD,mLnca_wT3})!1#nb\`iVE8UIqgdLlCmbwS@LlaiVC\`8V)oCmfbYBl\`_\`~YDl_?l\`1nei\$1neImYUDnCmCm?lddwTNCm?l\`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l\`~BmwTN^1nci\$1ncYUDnCmNldwS2CmZ\$lcwSLZ#la1ncCmZ\$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci\$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm\`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni\$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl\`>l_wTJ{YCmaKl5mi\$>l^{wSI\`Ol\`~BmwTL^8UIqgdNldYCmai8YCm\`iTE\`Ol\`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^Im^~^YDl^Ol\`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc\`YOlb\`1nYV#meYUGnlc>l\`CmCmNld?l\`wT7\`~Fl^Ol\`~ImBmwT'_8UIqgdLlCmbwS@LlaiVC\`8V)oCmfbYBl\`_\`~YDl_?l\`1nei\$1neImYUDnCmCm?lddwTNCm?l\`wSLCmCmLlLm>lawU1bxM1neImCm?l_wSL~BmwU+>l^>l_~Fl_a?l?la>l?l\`~BmwTN^1nci\$1ncYUDnCmNldwS2CmZ\$lcwSLZ#la1ncCmZ\$lbwSL~BmwU+Z#la~Fl?la_~BmwS2^1nci\$1ncIm^1ncImLnYUDnCmNlgwTFwS@wS@LlLm\`wS@wSI~FlNlbOla~Fl?la_~BmwTF^1nci%1ncIm^1ncImYUDni\$CmNldwU2^~FlNlbOla~Fl?la_~BmwU2^1ndCmCmZ/mNleYCmbKl5nOl\`>l_wTJ{YCmaKl5mi\$>l^{wSI\`Ol\`~BmwTL^8UIqgdNldYCmai8YCm\`iTE\`Ol\`~BmwSI^8=nc?la_~BmwSL^#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^ImSlJlYV*l_#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_YU:mYBl\`m#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVE\`#nRmVmh.w4l#ng~JleWmi&:nTniW%Nlh-YU9mCmCmh.i\$i\$ak\`iVEYU@mi&\`~_UmYU:mYUNlaml~^Im^~^YDl^Ol\`~BmwT7^1n:nHngZ>lec1n:nHngkc~JlZ+ldHnfYOldbiVIOla_~BmwT3^1nYV#meYUGnlc\`YOlb\`1nYV#meYUGnlc>l\`CmCmNld?l\`wT7\`~Fl^Ol\`~Im^~^BmwTJ^1ncYV?lOla_~BmwT%^#ncOlaiVE~BmwU1^>l_~Fl_#nbYUGnk\`\`iVF~YDl_})!V*)l^8V*l?l^~Fl^{!U@9&lCm\`^8U@mCma>l_?l^~Fl^}'!UN)lk8>mYUNl?l_l~Fl^{!;#na_iVE}'!VKl!VIo!VEn!VFm!VBl!W)k!V'7&mZAlaZAl_[\$Kl)lk)liVD~Fl_)ll)ll)liVD~Z=m__7,m?lb?l\`~YS%m__>l\`>l^~Fl_~Fl^}'i\$}'!V27&m>la>l_[\$Kl)lk)liVD~Fl_)ll7,m?lb?l\`)ll~Em\`^)liVD~Em__>l\`>l^~Fl_~Fl^}'i\$}'!S08LlZ'mYCm\`jAj/z!T07(oi&ca_[\$Kl8V&la7/oCmfZ2mb>lb\`a_Gml\`~Ema_}+i\$})!U,8<lYS(m\`^}'!S68<lYS+m\`^}'!S+.mYV'ma_k}'!S(.mkYV'm\`^}'!T&+mkYV'm\`^}'!T:8<lYS'm\`^}'!S?8<lZPm\`^}']P.mYV2ma_k}'!S'.mkYV2m\`^}'!SF8Gm\`^}'!U08Ll^z!S78LlYV+m__Im^[&?l\`>l_8LlYV+m__iW*~Bmi&_|!T/8V3n>lb\`>l^})!U#8ElZ2m\`>l^}'!U%(l^{!UL8V<liVAy!V<8V@llAmZ9kAmYS*m_Kl89liW\$AmPl^{z!U=)li\$8U=nca?l_AmDm>lb>l?l^AmDm>lbvS#8U=nca?l_AmDm>lb>l_~Sl^Z6m\`>l^~Fl^})!U<7'ma^AmDm>lavCAmDm>lavR#AmDm>lavC)li\$~Jl^8U<nb\`?l^AmX'ma>l^AmDm>lavC~Fl^})!98ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~ImZEl\`8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Im^[&?l\`>l_8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~ImZEl\`8ULk89lLnQla?l\`>l_~YU>l_-m>l_vS7AmDm>l_vF~Z-l_-m>l_vLAmi\$-m>l_vLAmImYU<nai9?l^AmPm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_8U=n\`i&>l_~YMl_8U=n\`i&>l?l_~YDl_-m>l_vLAmYU<n\`i9?l_AmPm_>l_AmDm>l_vK~Fl_8Nm__~YIl_89lZ.mQl\`Ql?l_~Im^~^ZDl_89m_Z;l_~Z4l_-m>l_vLAmDm>l_vK~Jl_-m>l_vS,AmDm>l_vF~YHl_-m>l_vS;AmDm>l_vF~Bmi%_-m>l_vS-AmDm>l_vF~Sl_Z)k~Bmi&_|!J89m__-m>l_vLAmi\$-m>l_vLAmImYU<naiJ?l^AmYJm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n\`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm\`Ol^8Nm\`\`~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n\`iW&>l_AmDm>l_vE~YMl_Im^[&?l\`>l_89m__-m>l_vLAmi\$-m>l_vLAmImYU<naiJ?l^AmYJm\`>l^>l_~Em?l\`kAmDm>l_vKAmDm>l_vF~YKl_-m>l_vLAmYU<n\`iJ?l_AmYJm_>l_AmDm>l_vK~Fl_8Jm\`Ol^8Nm\`\`~Sl^Z5mYCmiVHj&>l_AmDm>l_vS#AmDm>l_vF~YIl_-m>l_vEAmYU=n\`iW&>l_AmDm>l_vE~YMl_Z)k~Bmi&_|]9-m>l_uIm^[%?l_>l^-m>l_uZ)k~Bmi&^z!T4)l^AmYS/l_X'l^ZKl^}'!V08V0l_8UMl_~Bmu>l^)l^~YHl^Ml^{!UM'l^8V0l_~BmvR0>l^8UMl_AmMl_~ZLl^)l^~YHl^YFl^{!UH8UHmaCm\`^8UHmbCma^8UHmbCmat~BmvS;^8UHmbCmav0~BmvS9^8UHmbCmau~BmvS5^>lMl\`~BmvS#^9&l_~BmvE^)li&~Bmk^>lMl_}'!UA8UAmCmbZ7lMl\`_8LlZ&l\`~ImImEmvD\`8UAmCmbZ7lMl\`_8LlZ&l\`~ImIm^~^BmvL_8UAmCmbZ7lMl\`_8LlZ&l\`~Im^~^BmvK^>lYFl^}'!UO,mYUOla^)l^AmMlaYAl\`~i\$,mYUOla^)l^AmMlaYAl\`~YGmiVOYS\$l^~YDl^YAl_)li&AmMl_~BmvL^YUMl^{!A9%l\`)l^~^^Z:l^YUAmi&_8V&lYUHm\`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl\`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl\`wT%AmMl_~BmvS'^5mYAl\`wU1AmMl_~BmvJ^9%lYUAmLliVP\`9(lYAl\`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl\`AmMl\`~BmvS#^)li%AmMl\`~BmvS;^)li\$AmMl\`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Im^[%?l_>l^9%l\`)l^~^^Z:l^YUAmi&_8V&lYUHm\`i&AmMl_~BmvE^5mYAlawT)5mYAlawTKAmMl\`~BmvR5^>lYFl_AmMl_~BmvO^5mYAl\`wT%AmMl_~BmvS'^5mYAl\`wU1AmMl_~BmvJ^9%lYUAmLliVP\`9(lYAl\`~BmvK^8ElOlZ6miVHZ%l^)l_~Bml?l^6lb~Bmk?l^YUAmi&aYFl\`AmMl\`~BmvS#^)li%AmMl\`~BmvS;^)li\$AmMl\`~BmvS-^>lYFl_AmMl_~BmvF^8UOl_AmMl_~BmvK^)l^~YHl^YUMl^Z1k~Bmi&^z!F)l^AmYUKm__Ml^Im^[%?l_>l^)l^AmYUKm__Ml^Z1k~Bmi&^z!T\$)l^AmZBl_X'l^ZCl^}']))liW(y]1)liVGy!N-m>l_>l_Im^[&?l\`>l_-m>l_>l_Z)k~Bmi&_|!S/)li\$8V=l>l^AmYV\$mi\$^~?l^{]K#nti%YV9l^{!6)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami\$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Im^[%?l_>l^)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^Ami\$)l^AmYUKmi&_8El^)liW+~Jl^YUPl>l_~Jl^YV%l^AmYULk~YV-l^Z1k~Bmi&^z!V-8<l?l^{!UK8V\$m\`^}'!V%(l^{]B)li\$8V.l>l^AmYV\$mi\$^~?l^{]C#nsi&YV>l^{!W(:nti%YV5k!VG:nsi&YV:kAmk!H+miW+^{!W+:npkk!V=8V.l^{!V.:nlkl!-:nlkm!UP:nlkn!V9:nlko!V>:nlkp!V5:nlkq!V::nlkr!U3iT=!SHiT=!T6iT=!T=)l^{!T28U?nal^[\$Kl8U:m_YUCmYS-m\`\`_Z*l\`Z*l^)lk~Bmk_}'i\$z!S-8U?nakKl7,m\`^7,m__~Em__Z*l\`Z*l^}'[\$Kl7*m_YS)m__)l_~Bmk^}'i\$z!U'8>mb^)l^~BmEmkbEmk\`)lk~Bmk^GmYU:m\`a_YUCm\`^}'!S)0mYU:mYUCmb\`\`^}']*)l^0m_k~Emk^{!S98U?na_Kl)l^)l_~Em__}'|!TB8U?na_Kl)l_)l^~Em__}'|!S;8<lZNl^{]N+mYU:mYUCmm\`m^{!T..mk^{!T@.m_k{!TC+mk^{!S=8UFobi%_Kl8<lEm\`^}'|!SC8UFobi%_Kl8<lEm__}'|!U*8UFobi%_Kl.m__}'|!U48UFobi%_Kl.m\`^}'|!S18UFobi%_j3|!T#)li\${!T<)li%{!TD8UCm\`^}'!TA8U?na_iTD8UCm_l~Jl_|!TP8U?na_Kl0m\`^}'0m_k~Jl_|!T*8U?n\`lKl8U:m\`^}'z!T-8U?n\`kKl8>m\`^}'z!UF)l\`8UFo?ldX(m>lda>lb^~i\$)l\`8UFo?ldX(m>lda>lb^~\`~Fla}+!U?)l_8U?n?lbX'm>lb\`^~Fl\`})!S:iU6!U67&lKl)l_AmYV6mQlc^AmYV(m>lc^?l?lKli\${?l?lKli\${!S*)li\$9'mZ/mYUEmbiSPLl_iS*AmZ'mYUEmaiTE^~Fl>l_|!C)li&,mZ'mZ/mYUEmciSPLl\`iCZ'mYUEmaiTE^~Fl>l_|!UE)li&,mYUEm?la_X%l>l_~Fl_}']'8V;m\`^}'!V;:nlks]7)l^8ElUmvC>l^~ZFl^{!P)l^8ElGmvC>l^~ZHl^{]F)li\$.mvRP>l^~Em>l_vR5{]H)li\$.mvSB>l^~Em>l_vS'{]L/lYS,miVM>l^{!S4)li\$.mvR/>l^~Em>l_vR\${!U59Fl_)l^~^ZHl^{!T?8<lZ=m\`^}'!T(8<lYS#m\`^}'!S#9ImYPl\`YPl^}']=9<mYPl\`YPl^}'!S%8S&mYPl\`YPl^}'!S88<lZ<m\`^}'!SE8<lZIm\`^}']I.m>l_>l_}']<.m>l\`>l^}'!S&93m\`^}'].9(l^z!SJ9(lYV+mk^{!SO8V3nb\`>l^})!S>92m\`>l^}'!SD(l^{!VJj/!VLi,!W'9(l^{!U;i5!V/)l\`8V/nCmca\`Gml^~Em_k})!V+8V/ni&\`^}'!S.)li\$8S.m?la_)l^~YGm>l__>l_~Fl_}']6j5]5)li\$95m?la_)l^~Z3m>l__>l_~Fl_}']O)li\$9Om?l\`^)l_~YGm>l\`^~Fl_}'!S,jM]M)li\$9Mm?l\`^)l_~Z3m>l\`^~Fl_}'!V,)l^8V,mGml\`?l^~Em\`k}'!V38V(maYV,m\`^})]2'lYV,m\`^}'!V4)l_8V4mCma>l_?l^~Fl^}']&8V4mi&^{]/7%l_[\$Kl)li&7)l?l_)l^~Jl?l_,mX*lCm?la?l_>l^~Fl^>l^~Fl^{i\$z!B)lk8>mYBl?l_l~Fl^{!5)l^z!TM9+l?l^{!U.9+l>l^{!T99?l?l^{!U-9?l>l^{!SN9\$l?l^{!S39\$l>l^{!T59Gl?l^{!T,9Gl>l^{!T88Ol>l^{!TG90l?l^{!SB90l>l^{!SM9#l?l^{!SK9#l>l^{!SG9@l?l^{!U/9@l>l^{]+87l?l^{]?87l>l^{]\$9,l?l^{]G9,l>l^{]088l>l^{]#98l?l^{]@98l>l^{],(l>l^{]8'l>l^{]>8Ol?l^{!O88l?l^{!7(l?l^{!8'l?l^{!U)8V\$m\`^}'!T+8V(m\`^}'!SP(l^{!TE'l^{!,#nk\`^}'!G)li\$)li\$)li\$)li\$8Gm>la>l_~YGm?la?l_~YGmQlaQl_~YU>l\`)li\$~BmpQl_~YU>l_)l^~^Bm\`^}'!<+mi\$^{]:7'l^)li\$0m_k~^X'l?l^~BmvP>l^)li\$~Jl^>lc[\$Kl)l_)li\$7.ndUm\`YU:mch/?l\`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li\$~Jl^{[&Kl)li\$0mvR%^~i\$)li\$0mvR%^~EmvR/^~Em_vR\${['Kl)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR\${i\$i\$i\$i\$Im^[&?l\`>l_7'l^)li\$0m_k~^X'l?l^~BmvP>l^)li\$~Jl^>lc[\$Kl)l_)li\$7.ndUm\`YU:mch/?l\`~^X(l^>l^~Fl^})[%Kl7*ne7*nf~Bmv3gk^)li\$~Jl^{[&Kl)li\$0mvR%^~i\$)li\$0mvR%^~EmvR/^~Em_vR\${['Kl)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~i\$)li\$0mvS(^~i\$)li\$0mvS(^~EmvS.^~Em_vS'0mvR6^~EmvR<^~Em_vR50mvR%^~EmvR/^~Em_vR\${i\$i\$i\$i\$u~Bmi&_|];#nnYBl_^X&mi&\`#nnYBl_^:nkX'mi&GmbkvP~Emk\`[\$Kl)l^7-m_\`~Emak:nkbUm_vR%)l^7-m_\`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i\$Im^[&?l\`>l_#nnYBl_^X&mi&\`#nnYBl_^:nkX'mi&GmbkvP~Emk\`[\$Kl)l^7-m_\`~Emak:nkbUm_vR%)l^7-m_\`~Emak:nkbUmGmu_vR6~Em_tGmYU:mf__YUCmd^}'i\$u~Bmi&_|!V8#nmYS0l_i\${]%7&miVN_[\$Kl)l^!VN:nkiVN^YV8l^7+m?la_)l^~YGm\`?l^>l_~Fl_}'i\${!VN?li#!S\$(l^{!U&'l^{](#noYBl_^{]A8UEmYV7l_iE{!L8V&lYUEm_iS5{!UE)li&#nkYUEm?la_X%l>l_~Fl_}'!V7'l^{!V&#nnYBl_^{!E#nqk^{!S5'l^{Amk!B)lk8>mYBl?l_l~Fl^{!U\$7&m\`_[\$Kl3l^3l^7+m?la?l^~Fl^)li\$~Bma^?l^~Fl^}'i\${!U7iT;!T;iSA!SAj4!TIj4]48<lYU>l^{!3+mi&^{!U(+m\`^}']3+m\`^+m>l\`>l^~i\$+m\`^+m>l\`>l^~YIl_~YIl^}']EYUBlt]DYUBls!U8+mi\$_)l^~^Bmi%^{!IYUBlq]-YUBll!KYUBlo!MYUBln!DYUBlm!/YUBlk!UB4l)li\$+mbQl^~YU>l^{{AmkAmkAmkAmkAmkAmkAmkAmkAmkAmk!):nlkt!2:nlkv.!4:nlkv/!U>:nlkv0!':nlkv1!(:nlkv2!::nlkv3!V(:nlkv4!V\$:nlkv5!V6:nlkv6!+:nlkv7!.:nlkv8!>:nlkv9!U::nlkv;!UC:nlkv<!V@:nlkv=!W#:nnm:nk:nki&vCvR3!VC:nki&wS@!VDGmlk!W*:nqkvC!VA:nnq:nk:nk:nk:nk:nk:nki&vLvJvR#vS&vR#vK!W\$:nnl:nki&vC!W&:nk:nk:nk:nki&:nk:nki&vEvE:nk:nki&vS#vS#:nk:nki&vS;v0:nk:nki&vS5u!VO:nnl:nki&vR#!VP:nqkvF!VH:nk:nk:nk:nki&:nk:nki&v0wTO:nk:nki&twS<:nk:nki&vCwT>:nk:nki&ux9!VM:nk:nk:nk:nk:nki&v0v.utvC!0:nlkv:!*:nlkuy"
_input=$__str_0
# rib struct member declarations
readonly __field0=0
readonly __field1=1
readonly __field2=2
readonly __sizeof__rib=3
_heap_start=0
_alloc_limit=0
_pc=$(((0 << 1) | 1))
_FALSE=$(((0 << 1) | 1))
_symbol_table=$(((0 << 1) | 1))
_pos=0
_alloc=0
_alloc_limit=0
_scan=0
_init_heap() {
  _malloc _heap_start $((1 * ((100000 * 3) << 1)))
  if [ $((!_heap_start)) != 0 ] ; then
    exit 7
  fi
  _alloc=_heap_start
  _alloc_limit=$((_heap_start + (100000 * 3)))
  _stack=$(((0 << 1) | 1))
}

: $((copy = field0 = ptr = o = 0))
_copy() { let o $2
  let_ ptr; let_ field0; let_ copy
  if [ $((!(o & 1))) != 0 ] ; then
    ptr=o
    field0=$((_$((ptr + 0))))
    if [ $field0 = _NULL ] ; then
      copy=$((_$((ptr + 1))))
    else
      copy=_alloc
      : $((_$(((ptr += 1) - 1)) = _NULL))
      : $((_$(((_alloc += 1) - 1)) = field0))
      : $((_$(((_alloc += 1) - 1)) = _$(((ptr += 1) - 1))))
      : $((_$(((_alloc += 1) - 1)) = _$ptr))
      : $((_$((ptr + -1)) = copy))
    fi
    : $(($1 = copy))
    endlet $1 copy field0 ptr o
    return
  fi
  : $(($1 = o))
  endlet $1 copy field0 ptr o
}

: $((to_space = 0))
_gc() {
  let_ to_space
  to_space=$(((_alloc_limit == (_heap_start + (100000 * 3))) ? (_heap_start + (100000 * 3)): _heap_start))
  _alloc_limit=$((to_space + (100000 * 3)))
  _alloc=$to_space
  _copy _stack $_stack
  _copy _pc $_pc
  _copy _FALSE $_FALSE
  _scan=$to_space
  while [ $_scan != $_alloc ] ; do
    _copy _$_scan $((_$_scan))
    : $(((_scan += 1) - 1))
  done
  endlet $1 to_space
}

: $((x = 0))
_pop() {
  let_ x
  x=$((_$((_stack + __field0))))
  _stack=$((_$((_stack + __field1))))
  : $(($1 = x))
  endlet $1 x
}

: $((tag = car = 0))
_push2() { let car $2; let tag $3
  : $((_$(((_alloc += 1) - 1)) = car))
  : $((_$(((_alloc += 1) - 1)) = _stack))
  : $((_$(((_alloc += 1) - 1)) = tag))
  _stack=(_alloc - 3)
  if [ $_alloc = $_alloc_limit ] ; then
    _gc __ 
  fi
  endlet $1 tag car
}

: $((allocated = old_stack = tag = cdr = car = 0))
_alloc_rib() { let car $2; let cdr $3; let tag $4
  let_ old_stack; let_ allocated
  _push2 __ $car $cdr
  old_stack=$((_$((_stack + __field1))))
  allocated=$_stack
  : $((_$((allocated + __field1)) = _$((allocated + __field2))))
  : $((_$((allocated + __field2)) = tag))
  _stack=$old_stack
  : $(($1 = allocated))
  endlet $1 allocated old_stack tag cdr car
}

: $((allocated = old_stack = tag = cdr = car = 0))
_alloc_rib2() { let car $2; let cdr $3; let tag $4
  let_ old_stack; let_ allocated
  _push2 __ $car $tag
  old_stack=$((_$((_stack + __field1))))
  allocated=$_stack
  : $((_$((allocated + __field1)) = cdr))
  _stack=$old_stack
  : $(($1 = allocated))
  endlet $1 allocated old_stack tag cdr car
}

_get_byte() {
  : $(($1 = _$((_input + (_pos += 1) - 1))))
}

: $((__g1 = x = 0))
_get_code() {
  let_ x; let_ __g1
  _get_byte __g1 
  x=$((__g1 - 35))
  : $(($1 = (x < 0) ? 57: x))
  endlet $1 __g1 x
}

: $((x = n = 0))
_get_int() { let n $2
  let_ x
  _get_code x 
  : $((n *= (92 / 2)))
  if [ $x -lt $((92 / 2)) ] ; then
    : $(($1 = n + x))
  else
    _get_int $1 $(((n + x) - (92 / 2)))
  fi
  endlet $1 x n
}

: $((i = lst = 0))
_list_tail() { let lst $2; let i $3
  if [ $i = 0 ] ; then
    : $(($1 = lst))
  else
    _list_tail $1 _$((lst + __field1)) $((i - 1))
  fi
  endlet $1 i lst
}

: $((i = lst = 0))
_inst_tail() { let lst $2; let i $3
  if [ $i = 0 ] ; then
    : $(($1 = lst))
  else
    _inst_tail $1 _$((lst + __field2)) $((i - 1))
  fi
  endlet $1 i lst
}

: $((__g1 = i = lst = 0))
_list_ref() { let lst $2; let i $3
  let_ __g1
  _list_tail __g1 $lst $i
  : $(($1 = _$((__g1 + __field0))))
  endlet $1 __g1 i lst
}

: $((__g1 = return_value = o = 0))
_get_opnd() { let o $2
  let_ return_value; let_ __g1
  if [ $((o & 1)) != 0 ] ; then
    _list_tail __g1 _stack (o >> 1)
    return_value=__g1
  else
    return_value=o
  fi
  : $(($1 = _$((return_value + __field0))))
  endlet $1 __g1 return_value o
}

: $((s = 0))
_get_cont() {
  let_ s
  s=$_stack
  while [ $((!(_$((s + __field2)) >> 1))) != 0 ] ; do
    s=$((_$((s + __field1))))
  done
  : $(($1 = s))
  endlet $1 s
}

: $((str = current = length = i = s = 0))
_scm2str() { let s $2
  let_ i; let_ length; let_ current; let_ str
  i=0
  length=(_$((s + __field1)) >> 1)
  current=$((_$((s + __field0))))
  _malloc str $((length + 1))
  i=0
  while [ $i -lt $length ] ; do
    : $((_$((str + i)) = (_$((current + __field0)) >> 1)))
    current=$((_$((current + __field1))))
    : $(((i += 1) - 1))
  done
  : $((_$((str + length)) = __NUL__))
  : $(($1 = str))
  endlet $1 str current length i s
}

: $((x = 0))
_bool2scm() { let x $2
  if [ $x != 0 ] ; then
    : $(($1 = _$((_FALSE + __field0))))
  else
    : $(($1 = _FALSE))
  fi
  endlet $1 x
}

: $((__g1 = filename = num_args = bytes_read = success = buffer = file = arg = new_rib = z = y = x = no = 0))
_prim() { let no $2
  let_ x; let_ y; let_ z; let_ new_rib; let_ arg; let_ file; let_ buffer; let_ success; let_ bytes_read; let_ num_args; let_ filename; let_ __g1
  _malloc buffer 1
  if [ $no = 0 ] ; then
    _alloc_rib __g1 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((0 << 1) | 1))
    new_rib=__g1
    _pop z 
    _pop y 
    _pop x 
    : $((_$((new_rib + __field0)) = x))
    : $((_$((new_rib + __field1)) = y))
    : $((_$((new_rib + __field2)) = z))
    _push2 __ $new_rib $(((0 << 1) | 1))
  elif [ $no = 1 ] ; then
    _pop x 
    file=(x ^ 1)
    _fclose __ $file
  elif [ $no = 2 ] ; then
    _pop y 
    _pop x 
    file=(y ^ 1)
    : $((_$((buffer + 0)) = (x >> 1)))
    _fwrite success $buffer 1 1 $file
    if [ $success != 1 ] ; then
      defstr __str_1 "Cannot write to file."
      _perror __ $__str_1
    fi
    _fflush __ $file
    _push2 __ $((_$((_FALSE + __field0)))) $(((0 << 1) | 1))
  elif [ $no = 3 ] ; then
    _pop x 
    file=(x ^ 1)
    _fread bytes_read $buffer 1 1 $file
    if [ $((!bytes_read)) != 0 ] ; then
      _push2 __ $((_$((_FALSE + __field1)))) $(((0 << 1) | 1))
    else
      _push2 __ $(((_$((buffer + 0)) << 1) | 1)) $(((0 << 1) | 1))
    fi
  elif [ $no = 4 ] ; then
    _pop x 
    _scm2str filename $x
    defstr __str_2 "w"
    _fopen file $filename $__str_2
    _push2 __ $((file | 1)) $(((0 << 1) | 1))
    _free __ filename
  elif [ $no = 5 ] ; then
    _pop x 
    _scm2str filename $x
    defstr __str_3 "r"
    _fopen file $filename $__str_3
    _push2 __ $((file ? (file | 1): _FALSE)) $(((0 << 1) | 1))
    _free __ filename
  elif [ $no = 6 ] ; then
    defstr __str_4 "w"
    _fdopen file 1 $__str_4
    _push2 __ $((file | 1)) $(((0 << 1) | 1))
  elif [ $no = 7 ] ; then
    defstr __str_5 "r"
    _fdopen file 0 $__str_5
    _push2 __ $((file | 1)) $(((0 << 1) | 1))
  elif [ $no = 8 ] ; then
    _pop y 
    _pop x 
    num_args=0
    : $((_$((_$((_FALSE + __field0)) + __field0)) = x))
    arg=y
    while [ $arg != $((_$((_FALSE + __field1)))) ] ; do
      _push2 __ $arg $(((0 << 1) | 1))
      arg=$((_$((_stack + __field0))))
      : $((_$((_stack + __field0)) = _$((arg + __field0))))
      arg=_$((arg + __field1))
      : $(((num_args += 1) - 1))
    done
    _push2 __ $(((num_args << 1) | 1)) $(((0 << 1) | 1))
    x=$((_$((_$((_FALSE + __field0)) + __field0))))
    : $(($1 = x))
    endlet $1 __g1 filename num_args bytes_read success buffer file arg new_rib z y x no
    return
  elif [ $no = 9 ] ; then
    _pop x 
    _push2 __ $x $(((0 << 1) | 1))
  elif [ $no = 10 ] ; then
    _pop __ 
  elif [ $no = 11 ] ; then
    _pop x 
    _pop __ 
    _push2 __ $x $(((0 << 1) | 1))
  elif [ $no = 12 ] ; then
    x=$((_$((_$((_stack + __field0)) + __field0))))
    y=$((_$((_stack + __field1))))
    _alloc_rib __g1 $x $y $(((1 << 1) | 1))
    : $((_$((_stack + __field0)) = __g1))
  elif [ $no = 13 ] ; then
    _pop x 
    _bool2scm __g1 $((!(x & 1)))
    _push2 __ $__g1 $(((0 << 1) | 1))
  elif [ $no = 14 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field0)))) $(((0 << 1) | 1))
  elif [ $no = 15 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field1)))) $(((0 << 1) | 1))
  elif [ $no = 16 ] ; then
    _pop x 
    _push2 __ $((_$((x + __field2)))) $(((0 << 1) | 1))
  elif [ $no = 17 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field0)) = y)) $(((0 << 1) | 1))
  elif [ $no = 18 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field1)) = y)) $(((0 << 1) | 1))
  elif [ $no = 19 ] ; then
    _pop y 
    _pop x 
    _push2 __ $((_$((x + __field2)) = y)) $(((0 << 1) | 1))
  elif [ $no = 20 ] ; then
    _pop y 
    _pop x 
    _bool2scm __g1 $((x == y))
    _push2 __ $__g1 $(((0 << 1) | 1))
  elif [ $no = 21 ] ; then
    _pop y 
    _pop x 
    _bool2scm __g1 $(((x >> 1) < (y >> 1)))
    _push2 __ $__g1 $(((0 << 1) | 1))
  elif [ $no = 22 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((x + y) - 1)) $(((0 << 1) | 1))
  elif [ $no = 23 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((x - y) + 1)) $(((0 << 1) | 1))
  elif [ $no = 24 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((((x >> 1) * (y >> 1)) << 1) | 1)) $(((0 << 1) | 1))
  elif [ $no = 25 ] ; then
    _pop y 
    _pop x 
    _push2 __ $(((((x >> 1) / (y >> 1)) << 1) | 1)) $(((0 << 1) | 1))
  elif [ $no = 26 ] ; then
    _pop x 
    exit (x >> 1)
  else
    exit 6
  fi
  _free __ $buffer
  : $(($1 = (0 << 1) | 1))
  endlet $1 __g1 filename num_args bytes_read success buffer file arg new_rib z y x no
}

: $((__g2 = __g1 = opnd = x = p = rest = new_pc = k = c2 = s2 = proc = jump = vari = nparams = nparams_vari = nargs = instr = i = 0))
_run() {
  let_ i; let_ instr; let_ nargs; let_ nparams_vari; let_ nparams; let_ vari; let_ jump; let_ proc; let_ s2; let_ c2; let_ k; let_ new_pc; let_ rest; let_ p; let_ x; let_ opnd; let_ __g1; let_ __g2
  while [ 1 != 0 ] ; do
    instr=(_$((_pc + __field0)) >> 1)
    if [ $instr = 5 ] ; then
      exit 0
    elif [ $instr = 0 ] ; then
      jump=$((_$((_pc + __field2)) == ((0 << 1) | 1)))
      _get_opnd proc $((_$((_pc + __field1))))
      while [ 1 != 0 ] ; do
        if [ $((_$((proc + __field0)) & 1)) != 0 ] ; then
          _pop __ 
          _prim proc (_$((proc + __field0)) >> 1)
          if [ $((!(proc & 1))) != 0 ] ; then
            continue
          fi
          if [ $jump != 0 ] ; then
            _get_cont _pc 
            : $((_$((_stack + __field1)) = _$((_pc + __field0))))
          fi
          _pc=$((_$((_pc + __field2))))
        else
          _pop __g1 
          nargs=(__g1 >> 1)
          _alloc_rib __g1 $(((0 << 1) | 1)) $proc $(((0 << 1) | 1))
          s2=__g1
          proc=$((_$((s2 + __field1))))
          : $((_$((_pc + __field0)) = _$((proc + __field0))))
          nparams_vari=(_$((_$((proc + __field0)) + __field0)) >> 1)
          nparams=$((nparams_vari >> 1))
          vari=$((nparams_vari & 1))
          if [ $((vari ? (nparams > nargs): (nparams != nargs))) != 0 ] ; then
            defstr __str_6 "*** Unexpected number of arguments nargs: %ld nparams: %ld vari: %ld\n"
            _printf __ $__str_6 $nargs $nparams $vari
            exit 1
          fi
          : $((nargs -= nparams))
          if [ $vari != 0 ] ; then
            rest=$((_$((_FALSE + __field1))))
            i=0
            while [ $i -lt $nargs ] ; do
              _pop __g2 
              _alloc_rib __g1 $__g2 $rest $s2
              rest=__g1
              s2=$((_$((rest + __field2))))
              : $((_$((rest + __field2)) = (0 << 1) | 1))
              : $((i += 1))
            done
            _alloc_rib __g1 $rest $s2 $(((0 << 1) | 1))
            s2=__g1
          fi
          i=0
          while [ $i -lt $nparams ] ; do
            _pop __g2 
            _alloc_rib __g1 $__g2 $s2 $(((0 << 1) | 1))
            s2=__g1
            : $((i += 1))
          done
          nparams=$((nparams + vari))
          _list_tail __g1 s2 $nparams
          c2=__g1
          if [ $jump != 0 ] ; then
            _get_cont k 
            : $((_$((c2 + __field0)) = _$((k + __field0))))
            : $((_$((c2 + __field2)) = _$((k + __field2))))
          else
            : $((_$((c2 + __field0)) = _stack))
            : $((_$((c2 + __field2)) = _$((_pc + __field2))))
          fi
          _stack=$s2
          new_pc=$((_$((_pc + __field0))))
          : $((_$((_pc + __field0)) = (instr << 1) | 1))
          _pc=$((_$((new_pc + __field2))))
        fi
        break
      done
    elif [ $instr = 1 ] ; then
      x=$((_$((_stack + __field0))))
      if [ $((_$((_pc + __field1)) & 1)) != 0 ] ; then
        _list_tail __g1 _stack (_$((_pc + __field1)) >> 1)
        opnd=__g1
      else
        opnd=_$((_pc + __field1))
      fi
      : $((_$((opnd + __field0)) = x))
      _stack=$((_$((_stack + __field1))))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 2 ] ; then
      _get_opnd __g1 $((_$((_pc + __field1))))
      _push2 __ $__g1 $(((0 << 1) | 1))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 3 ] ; then
      _push2 __ $((_$((_pc + __field1)))) $(((0 << 1) | 1))
      _pc=$((_$((_pc + __field2))))
    elif [ $instr = 4 ] ; then
      _pop p 
      if [ $p != $_FALSE ] ; then
        _pc=$((_$((_pc + __field1))))
      else
        _pc=$((_$((_pc + __field2))))
      fi
    else
      exit 6
    fi
  done
  endlet $1 __g2 __g1 opnd x p rest new_pc k c2 s2 proc jump vari nparams nparams_vari nargs instr i
}

: $((__g1 = n = 0))
_symbol_ref() { let n $2
  let_ __g1
  _list_ref __g1 _symbol_table $n
  : $(($1 = __g1))
  endlet $1 __g1 n
}

: $((l = list = 0))
_lst_length() { let list $2
  let_ l
  l=0
  while [ $((!(list & 1))) != 0 ] && [ (_$((list + __field2)) >> 1) = 0 ] ; do
    : $((l += 1))
    list=$((_$((list + __field1))))
  done
  : $(($1 = (l << 1) | 1))
  endlet $1 l list
}

: $((__g1 = root = sym = list = name = 0))
_create_sym() { let name $2
  let_ list; let_ sym; let_ root; let_ __g1
  _lst_length __g1 $name
  _alloc_rib list $name $__g1 $(((3 << 1) | 1))
  _alloc_rib sym $_FALSE list $(((2 << 1) | 1))
  _alloc_rib root sym $_symbol_table $(((0 << 1) | 1))
  : $(($1 = root))
  endlet $1 __g1 root sym list name
}

: $((__g1 = c = accum = n = 0))
_build_sym_table() {
  let_ n; let_ accum; let_ c; let_ __g1
  _get_int n 0
  while [ $n -gt 0 ] ; do
    : $(((n -= 1) + 1))
    _create_sym __g1 $((_$((_FALSE + __field1))))
    _symbol_table=__g1
  done
  accum=$((_$((_FALSE + __field1))))
  while [ 1 != 0 ] ; do
    _get_byte c 
    if [ $c = 44 ] ; then
      _create_sym __g1 $accum
      _symbol_table=__g1
      accum=$((_$((_FALSE + __field1))))
      continue
    fi
    if [ $c = 59 ] ; then
      break
    fi
    _alloc_rib __g1 $(((c << 1) | 1)) accum $(((0 << 1) | 1))
    accum=__g1
  done
  _create_sym __g1 $accum
  _symbol_table=__g1
  endlet $1 __g1 c accum n
}

: $((c = 0))
_set_global() { let c $2
  : $((_$((_$((_symbol_table + __field0)) + __field0)) = c))
  _symbol_table=$((_$((_symbol_table + __field1))))
  endlet $1 c
}

# Initialize the memory to 0
initialize_memory() { # $1 = address, $2 = length
  __ix=$1
  __last=$(($1 + $2))
  while [ $__ix -lt $__last ]; do
    : $((_$__ix=0))
    : $((__ix += 1))
  done
}

defarr() { _malloc $1 $2; ; initialize_memory $(($1)) $2; }

defarr _weights 6
_init_weights() {
  : $((_$((_weights + 0)) = 20))
  : $((_$((_weights + 1)) = 30))
  : $((_$((_weights + 2)) = 0))
  : $((_$((_weights + 3)) = 10))
  : $((_$((_weights + 4)) = 11))
  : $((_$((_weights + 5)) = 4))
}

: $((__g3 = __g2 = __g1 = c = x = op = d = n = 0))
_decode() {
  let_ n; let_ d; let_ op; let_ x; let_ c; let_ __g1; let_ __g2; let_ __g3
  while [ 1 != 0 ] ; do
    _get_code x 
    n=$x
    op=-1
    while [ $n -gt $((2 + (d = _$((_weights + (op += 1)))))) ] ; do
      : $((n -= (d + 3)))
    done
    if [ $x -gt 90 ] ; then
      op=4
      _pop n 
    else
      if [ $((!op)) != 0 ] ; then
        _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
      fi
      if [ $n -ge $d ] ; then
        if [ $n = $d ] ; then
          _get_int __g1 0
          n=$(((__g1 << 1) | 1))
        else
          _get_int __g2 $(((n - d) - 1))
          _symbol_ref __g1 $__g2
          n=__g1
        fi
      else
        if [ $op -lt 3 ] ; then
          _symbol_ref __g1 $n
          n=__g1
        else
          n=$(((n << 1) | 1))
        fi
      fi
      if [ $op -gt 4 ] ; then
        _pop __g3 
        _alloc_rib2 __g2 $n $(((0 << 1) | 1)) $__g3
        _alloc_rib __g1 __g2 $((_$((_FALSE + __field1)))) $(((1 << 1) | 1))
        n=__g1
        if [ $_stack = $(((0 << 1) | 1)) ] ; then
          break
        fi
        op=3
      elif [ $op -gt 0 ] ; then
        : $(((op -= 1) + 1))
      else
        op=0
      fi
    fi
    _alloc_rib c $(((op << 1) | 1)) $n 0
    : $((_$((c + __field2)) = _$((_stack + __field0))))
    : $((_$((_stack + __field0)) = c))
  done
  _pc=$((_$((_$((n + __field0)) + __field2))))
  endlet $1 __g3 __g2 __g1 c x op d n
}

: $((first = 0))
_setup_stack() {
  let_ first
  _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
  _push2 __ $(((0 << 1) | 1)) $(((0 << 1) | 1))
  first=$((_$((_stack + __field1))))
  : $((_$((_stack + __field1)) = (0 << 1) | 1))
  : $((_$((_stack + __field2)) = first))
  : $((_$((first + __field0)) = (5 << 1) | 1))
  : $((_$((first + __field1)) = (0 << 1) | 1))
  : $((_$((first + __field2)) = (0 << 1) | 1))
  endlet $1 first
}

: $((__g3 = __g2 = __g1 = 0))
_init() {
  let_ __g1; let_ __g2; let_ __g3
  _init_weights __ 
  _init_heap __ 
  _alloc_rib __g2 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((5 << 1) | 1))
  _alloc_rib __g3 $(((0 << 1) | 1)) $(((0 << 1) | 1)) $(((5 << 1) | 1))
  _alloc_rib __g1 __g2 __g3 $(((5 << 1) | 1))
  _FALSE=__g1
  _build_sym_table __ 
  _decode __ 
  _alloc_rib __g1 $(((0 << 1) | 1)) $_symbol_table $(((1 << 1) | 1))
  _set_global __ __g1
  _set_global __ $_FALSE
  _set_global __ $((_$((_FALSE + __field0))))
  _set_global __ $((_$((_FALSE + __field1))))
  _setup_stack __ 
  _run __ 
  endlet $1 __g3 __g2 __g1
}

_main() {
  _init __ 
}

# Character constants
readonly __NUL__=0
# Runtime library
_free() { # $1 = pointer to object to free
  : $(($1 = 0)); shift # Return 0
  __ptr=$1
  __size=$((_$((__ptr - 1)))) # Get size of allocation
  while [ $__size -gt 0 ]; do
    unset "_$__ptr"
    : $((__ptr += 1))
    : $((__size -= 1))
  done
}

int_to_char() {
  case $1 in
    48|49|50|51|52|53|54|55|56|57) __char=$(($1 - 48)) ;;
    97)  __char="a" ;;
    98)  __char="b" ;;
    99)  __char="c" ;;
    100) __char="d" ;;
    101) __char="e" ;;
    102) __char="f" ;;
    103) __char="g" ;;
    104) __char="h" ;;
    105) __char="i" ;;
    106) __char="j" ;;
    107) __char="k" ;;
    108) __char="l" ;;
    109) __char="m" ;;
    110) __char="n" ;;
    111) __char="o" ;;
    112) __char="p" ;;
    113) __char="q" ;;
    114) __char="r" ;;
    115) __char="s" ;;
    116) __char="t" ;;
    117) __char="u" ;;
    118) __char="v" ;;
    119) __char="w" ;;
    120) __char="x" ;;
    121) __char="y" ;;
    122) __char="z" ;;
    65)  __char="A" ;;
    66)  __char="B" ;;
    67)  __char="C" ;;
    68)  __char="D" ;;
    69)  __char="E" ;;
    70)  __char="F" ;;
    71)  __char="G" ;;
    72)  __char="H" ;;
    73)  __char="I" ;;
    74)  __char="J" ;;
    75)  __char="K" ;;
    76)  __char="L" ;;
    77)  __char="M" ;;
    78)  __char="N" ;;
    79)  __char="O" ;;
    80)  __char="P" ;;
    81)  __char="Q" ;;
    82)  __char="R" ;;
    83)  __char="S" ;;
    84)  __char="T" ;;
    85)  __char="U" ;;
    86)  __char="V" ;;
    87)  __char="W" ;;
    88)  __char="X" ;;
    89)  __char="Y" ;;
    90)  __char="Z" ;;
    32)  __char=" " ;;
    33)  __char="!" ;;
    34)  __char="\"" ;;
    35)  __char="#" ;;
    36)  __char="$" ;;
    37)  __char="%" ;;
    38)  __char="&" ;;
    39)  __char="'" ;;
    40)  __char="(" ;;
    41)  __char=")" ;;
    42)  __char="*" ;;
    43)  __char="+" ;;
    44)  __char="," ;;
    45)  __char="-" ;;
    46)  __char="." ;;
    47)  __char="/" ;;
    58)  __char=":" ;;
    59)  __char=";" ;;
    60)  __char="<" ;;
    61)  __char="=" ;;
    62)  __char=">" ;;
    63)  __char="?" ;;
    64)  __char="@" ;;
    91)  __char="[" ;;
    92)  __char="\\" ;;
    93)  __char="]" ;;
    94)  __char="^" ;;
    95)  __char="_" ;;
    96)  __char="\`" ;;
    123) __char="{" ;;
    124) __char="|" ;;
    125) __char="}" ;;
    126) __char="~" ;;
    10)  __char="\n" ;;
    *)
      echo "Invalid character code: $1" ; exit 1
      __char=$(printf "\\$(printf "%o" "$1")") ;;
  esac
}

# Emit a C-string line by line so that whitespace isn't mangled
print_string() {
  __addr=$1; shift
  __max_len=100000000
  __delim=0
  __len=0
  __acc=""
  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) # 10 == '\n'
        printf "%s\n" "$__acc"
        __acc="" ;;
      *)
        int_to_char "$__char"
        __acc="$__acc$__char" ;;
    esac
  done
  printf "%s" "$__acc"
}

# Convert a VM string reference to a Shell string.
# $__res is set to the result, and $__len is set to the length of the string.
pack_string() {
  __addr=$1; shift
  __max_len=100000000
  __delim=0
  __len=0
  __res=""
  if [ $# -ge 1 ] ; then __delim=$1   ; shift ; fi # Optional end of string delimiter
  if [ $# -ge 1 ] ; then __max_len=$1 ; shift ; fi # Optional max length
  while [ $((_$__addr)) != $__delim ] && [ $__max_len -gt $__len ] ; do
    __char=$((_$__addr))
    __addr=$((__addr + 1))
    __len=$((__len + 1))
    case $__char in
      10) __res="$__res\n" ;; # 10 == '\n'
      *)        int_to_char "$__char"
        __res="$__res$__char" ;;
    esac
  done
}

_printf() { # $1 = printf format string, $2... = printf args
  : $(($1 = 0)); shift # Return 0
  __fmt_ptr=$1; shift
  __mod=0
  while [ "$((_$__fmt_ptr))" != 0 ] ; do
    __head=$((_$__fmt_ptr))
    __fmt_ptr=$((__fmt_ptr + 1))
    if [ $__mod -eq 1 ] ; then
      int_to_char "$__head"
      __head_char=$__char
      case $__head_char in
        'd') # 100 = 'd' Decimal integer
          printf "%d" $1
          shift
          ;;
        'c') # 99 = 'c' Character
          # Don't need to handle non-printable characters the only use of %c is for printable characters
          printf \\$(($1/64))$(($1/8%8))$(($1%8))
          shift
          ;;
        'x') # 120 = 'x' Hexadecimal integer
          printf "%x" $1
          shift
          ;;
        's') # 115 = 's' String
          print_string $1
          shift
          ;;
        '.') # String with length. %.*s will print the first 4 characters of the string
          pack_string $__fmt_ptr 0 2 # Read next 2 characters
          __fmt_ptr=$((__fmt_ptr + 2))
          if [ "$__res" = "*s" ]; then
            print_string $2 0 $1
            shift 2
          else
            echo "Unknown format specifier: %.$__res" ; exit 1
          fi
          ;;
        [0-9])                         # parse integer
          # Get max length (with padding)
          pack_string $__fmt_ptr 46 # Read until '.' or end of string
          __fmt_ptr=$((__fmt_ptr + __len + 1))
          __min_len="$__head_char$__res" # Don't forget the first digit we've already read
          # Get string length
          pack_string $__fmt_ptr 115 # Read until 's' or end of string
          __fmt_ptr=$((__fmt_ptr + __len))
          __str_len=$__res
          __head=$((_$__fmt_ptr))
          int_to_char "$__head"
          __head_char=$__char
          __fmt_ptr=$((__fmt_ptr + 1))
          if [ "$__head_char" = 's' ]; then
            __str_ref=$1; shift
            # Count length of string with pack_string but don't use packed string
            pack_string $__str_ref 0 $__str_len
            __pad=""
            __padlen=$((__min_len - __len)) # Pad string so it has at least $__min_len characters
            while [ $__padlen -gt 0 ]; do
              __pad=" $__pad"
              : $((__padlen -= 1))
              done
            printf "%s" "$__pad" # Pad string
            print_string $__str_ref 0 $__str_len # Print string
          else
            echo "Unknown format specifier: '%$__min_len.$__str_len$__head_char'" ; exit 1;
          fi
          ;;
        *)
          echo "Unknown format specifier %$__head_char"; exit 1
      esac
      __mod=0
    else
      case $__head in
        10) printf "\n" ;;  # 10 == '\n'
        37) __mod=1 ;; # 37 == '%'
        *) printf \\$(($__head/64))$(($__head/8%8))$(($__head%8)) ;;
      esac
    fi
  done
}

__state_fd0=0;
_malloc __buffer_fd0 1000   # Allocate buffer
: $((_$__buffer_fd0 = 0))   # Init buffer to ""
: $((__cursor_fd0 = 0))     # Make buffer empty
: $((__buflen_fd0 = 1000))  # Init buffer length
__state_fd1=1
__state_fd2=1
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
    echo "No more file descriptors available" ; exit 1
  else
    # Because the file must be read line-by-line, and string
    # values can't be assigned to dynamic variables, each line
    # is read and then unpacked in the buffer.
    _malloc __addr 1000                   # Allocate buffer
    : $((_$__addr = 0))                 # Init buffer to ""
    : $((__buffer_fd$__fd = __addr))    # Save buffer address
    : $((__cursor_fd$__fd = 0))         # Make buffer empty
    : $((__buflen_fd$__fd = 1000))      # Init buffer length
    : $((__state_fd$__fd = $3))         # Mark the fd as opened
    pack_string $2
    if [ $3 = 0 ] ; then
      case $__fd in
        0) exec 0< $__res ;; 1) exec 1< $__res ;; 2) exec 2< $__res ;;
        3) exec 3< $__res ;; 4) exec 4< $__res ;; 5) exec 5< $__res ;;
        6) exec 6< $__res ;; 7) exec 7< $__res ;; 8) exec 8< $__res ;;
        9) exec 9< $__res ;;
      esac
    elif [ $3 = 1 ] ; then
      case $__fd in
        0) exec 0> $__res ;; 1) exec 1> $__res ;; 2) exec 2> $__res ;;
        3) exec 3> $__res ;; 4) exec 4> $__res ;; 5) exec 5> $__res ;;
        6) exec 6> $__res ;; 7) exec 7> $__res ;; 8) exec 8> $__res ;;
        9) exec 9> $__res ;;
      esac
    elif [ $3 = 2 ] ; then
      case $__fd in
        0) exec 0>> $__res ;; 1) exec 1>> $__res ;; 2) exec 2>> $__res ;;
        3) exec 3>> $__res ;; 4) exec 4>> $__res ;; 5) exec 5>> $__res ;;
        6) exec 6>> $__res ;; 7) exec 7>> $__res ;; 8) exec 8>> $__res ;;
        9) exec 9>> $__res ;;
      esac
    else
      echo "Unknow file mode" ; exit 1
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

# Local variables
__=0
__SP=0
let() { : $((__SP += 1)) $((__$__SP=$1)) $(($1=$2)) ; } 
let_() { : $((__SP += 1)) $((__$__SP=$1)); } 
endlet() {
  __ret=$1; : $((__tmp = $__ret)) # Save return value so it's not overwritten
  while [ $# -ge 2 ]; do : $(($2 = __$__SP)) $((__SP -= 1)); shift; done
  : $(($__ret=__tmp))
}

__code=0; # Success exit code
_main __code; exit $__code

# string_pool_alloc=14298 heap_alloc=25677 max_text_alloc=7865 cumul_text_alloc=27900
